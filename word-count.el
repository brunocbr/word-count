;;; word-count.el --- Word counter with daily logging -*- lexical-binding: t; -*-

;; Author: Bruno Loureiro Conte
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools, writing
;; URL: https://github.com/word-count/word-count.el

;;; Commentary:

;; This mode counts words typed in real-time in buffers where
;; `word-count-mode` is activated and logs the count to a file
;; based on the date.

;;; Code:

(defcustom word-count-log-files "~/.emacs.d/word-count-log-%s-%s.txt"
  "A string format defining the path for logging daily word counts.
The first `%s' will be replaced with the current date (ISO 8601).
The second `%s' will be replaced with a unique identifier for the current Emacs
instance.
Setting this variable to nil disables logging, and no word
count data will be saved.
The logs are stored in tab-separated values (TSV) format, facilitating
easy analysis with spreadsheets or other tools."
  :type 'file
  :group 'word-count)

(defcustom word-count-word-delimiters '(" " "." "," ";" ":" "!" "?")
  "List of characters used to delimit words in the buffer.
These characters are considered as boundaries for word counting."
  :type 'list
  :group 'word-count)

(defcustom word-count-save-interval 120
  "Time interval for saving the word count, in seconds."
  :type 'integer
  :group 'word-count)

(defvar word-count-my-word-count nil
  "Count of words typed.")

(defvar word-count-other-word-counts nil
  "Sum of counts from other instances.")

(defvar word-count-source-id nil
  "Unique source id for the local instance.")

(defvar word-count-last-date nil
  "The last date for which the word count was recorded.")

(defvar word-count-count-words-hook nil
  "Hook run when counting words.
Functions added to this hook will be called after the word count is updated.")

(defvar word-count-save-count-hook nil
  "Hook run when saving the word count.
Functions added to this hook will be called before the count is saved.")

(defvar-local word-count-last-char nil
  "Stores the last character typed to check for word changes.")

(defvar word-count-last-saved-count nil
  "Stores the last count persisted.")

(defvar word-count-timer nil
  "The timer for word count saves and updates from other sources.")

(define-minor-mode word-count-mode
  "Word counter while typing."
  :lighter (:eval (format " WordsToday[%d]"
                          (word-count-total-words-today))) ;; Displays the word count
  (if word-count-mode
      (progn
        (unless word-count-source-id
          (setq word-count-source-id (word-count-source-id)))
        (unless word-count-my-word-count
          (setq word-count-my-word-count (word-count-load-count))) ;; Load count from file on first activation
        (unless word-count-last-date
          (setq word-count-last-date (word-count-today-date)))
        (setq word-count-last-char " ")
        (unless word-count-other-word-counts
          (word-count-update-other-counts))
        (when (= (word-count-active-buffers) 1) ;; set timer if this is the first activation of the minor mode
          (setq word-count-timer (run-at-time "1 min" word-count-save-interval
                                              #'word-count-background-tasks)))
        ;; Add local hooks
        (add-hook 'post-self-insert-hook 'word-count-count-words nil t)
        (add-hook 'kill-buffer-hook 'word-count-disable))
    (remove-hook 'post-self-insert-hook 'word-count-count-words t)
    (remove-hook 'kill-buffer-hook 'word-count-disable)
    (word-count-deactivate)))

(defun word-count-total-words-today ()
  "Return the sum of the local count with counts from other instances for
today."
  (+ word-count-other-word-counts word-count-my-word-count))

(defun word-count-deactivate ()
  "Deactivate the word count minor mode and clean up resources.

If there are no more active buffers, it cancels the running
timer for periodic tasks, saves the current word count to a log file,
and resets the internal state variable `word-count-my-word-count` to nil."
  (when (= (word-count-active-buffers) 0)
    (cancel-timer word-count-timer)
    ;; Save count to file on deactivation
    (word-count-save-count)
    (setq word-count-my-word-count nil)))

(defun word-count-active-buffers ()
  "Count the buffers where the minor mode is active."
  (let ((count 0))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when word-count-mode
          (setq count (1+ count)))))
    count))

(defun word-count--base64-to-alphabetical (str)
  "Transform Base64 encoded string STR to use only lowercase letters and numbers."
  (let ((alphabet "abcdefghijklmnopqrstuvwxyz0123456789"))
    (apply #'concat
           (mapcar (lambda (c)
                     (string (aref alphabet (mod (string-to-number (format "%d" c)) 36))))
                   (string-to-list str)))))

(defun word-count-source-id ()
  "Generate a unique identifier for the current instance of Emacs.

The identifier is formatted as 'username@hostname_hash', where:
- 'username' is the current user's login name.
- 'hostname' is the name of the system on which Emacs is running.
- 'hash' is a compact hash based on the current username,
hostname, process ID, and a timestamp, ensuring uniqueness."
  (let* ((username (user-login-name))
         (hostname (system-name))
         (pid-str (number-to-string (emacs-pid)))
         (timestamp (format-time-string "%Y%m%dT%H%M%S" (current-time)))
         (raw-id (concat username "@" hostname ":" pid-str ":" timestamp))
         (hash (secure-hash 'sha256 raw-id))
         (base64-encoded (base64-encode-string hash t))
         (truncated (substring base64-encoded 0 (min 24 (length base64-encoded)))))
    (concat (user-login-name) "@" (system-name) "_"
            (word-count--base64-to-alphabetical truncated))))

(defun word-count-increment ()
  "Increment the global word count by one."
  (setq word-count-my-word-count (1+ word-count-my-word-count)))

(defun word-count-count-words ()
  "Count words based on typing and update the word count."
  (let ((current-char (this-command-keys)))
    (when (char-or-string-p current-char)
      (when (and (member current-char word-count-word-delimiters)
                 (not (member word-count-last-char word-count-word-delimiters)))
        (word-count-increment)
        (run-hooks 'word-count-count-words-hook)
        (force-mode-line-update))
      (setq word-count-last-char current-char))))

(defun word-count-update-log-entry (date value)
  "Insert or update the logfile entry for DATE with VALUE."
  (let* ((count-str (number-to-string value))
         (source word-count-source-id)
         (entry (concat date "\t" count-str "\n"))
         (logfile (format word-count-log-files date source)))
    (with-temp-file logfile
      (insert entry))))

(defun word-count-today-date ()
  (format-time-string "%Y-%m-%d" (current-time)))

(defun word-count-save-count ()
  "Save the word count to the log file."
  (interactive)
  (run-hooks 'word-count-save-count-hook)
  (let ((today (word-count-today-date)))
    ;; Handle current date change
    (when (not (string= today word-count-last-date))
      (word-count-update-log-entry word-count-last-date word-count-my-word-count)
      (setq word-count-my-word-count 0)) ;; reset the word count
    (word-count-update-log-entry today word-count-my-word-count)
    (setq word-count-last-date today)
    (setq word-count-last-saved-count word-count-my-word-count)))

(defun word-count-save-count-maybe ()
  "Save the word count if it is different from the last saved value,
stored in `word-count-last-saved-count`."
  (unless (eql word-count-last-saved-count word-count-my-word-count)
    (word-count-save-count)))

(defun word-count-load-from-file ()
  "Helper function to load the word count from log file."
  (let* ((today (word-count-today-date))
         (source word-count-source-id)
         (entry (concat today "\t"))
         (logfile (format word-count-log-files today source)))
    (if (file-exists-p logfile)
        (with-temp-buffer
          (insert-file-contents logfile)
          (goto-char (point-max))
          (when (search-backward entry nil t)
            (let* ((line (thing-at-point 'line t))
                   (parts (split-string line "\t")))
              (when parts
                (string-to-number (nth 1 parts)))))))))

(defun word-count-load-count ()
  "Load the word count from the log file if `word-count-log-files`
is set and the appropriate file exists."
  (interactive)
  (if word-count-log-files
      (or (word-count-load-from-file) 0)
    0))

(defun word-count-load-other-counts ()
  "Load the sum of word counts coming from other instances."
  (let* ((today (word-count-today-date))
         (this-source (word-count-source-id))
         (other-counts 0)
         (logfile-pattern (format word-count-log-files today "*"))
         (my-logfile (expand-file-name (format word-count-log-files today this-source)))
         (files (file-expand-wildcards logfile-pattern)))
    (message "Pattern: %s\nThe files: %s\n" logfile-pattern files)
    (dolist (logfile files)
      (let ((full-logfile (expand-file-name logfile)))
        (when (and (file-exists-p full-logfile) ;; check that file exists
                   ;; ignore the log file for the current instance
                   (not (file-equal-p my-logfile full-logfile)))
          (with-temp-buffer
            (insert-file-contents full-logfile)
            (goto-char (point-min))
            (let* ((line (thing-at-point 'line t))
                   (parts (split-string line "\t")))
              (when parts
                (setq other-counts (+ other-counts
                                      (string-to-number (nth 1 parts))))))))))
    other-counts))

(defun word-count-update-other-counts ()
  "Update the global variable `word-count-other-word-counts`."
  (setq word-count-other-word-counts (word-count-load-other-counts)))

(defun word-count-background-tasks ()
  "Save the local word count to the log file, and update the other word counts."
  (progn
    (word-count-save-count-maybe)
    (word-count-update-other-counts)))

;;;###autoload
(defun word-count-enable ()
  "Enable `word-count-mode` in the current buffer."
  (interactive)
  (word-count-mode 1))

;;;###autoload
(defun word-count-disable ()
  "Disable `word-count-mode` in the current buffer."
  (interactive)
  (word-count-mode -1))

(provide 'word-count)
