;;; word-count.el --- Word counter with daily logging -*- lexical-binding: t; -*-

;; Author: Bruno Loureiro Conte
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools, writing
;; URL: https://github.com/word-count/word-count.el

;;; Commentary:

;; This mode counts words typed in real-time in buffers where
;; `word-count-mode` is activated and logs the count to a file
;; based on the date.

;;; Code:

(defcustom word-count-log-file "~/.emacs.d/word-count-log.txt"
  "Path to the file where the daily word count will be logged.
If set to nil, logging will be disabled, and no word count will be saved.
The file will be created if it does not already exist. The log is stored in a
tab-separated values (TSV) format, making it easy to analyze with
spreadsheets or other tools."
  :type 'file
  :group 'word-count)

(defcustom word-count-word-delimiters '(" " "." "," ";" ":" "!" "?")
  "List of characters used to delimit words in the buffer.
These characters are considered as boundaries for word counting."
  :type 'list
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

(defvar word-count-timer nil
  "The timer for word count saves and updates from other sources.")

(define-minor-mode word-count-mode
  "Word counter while typing."
  :lighter (:eval (format " WordsToday[%d]"
                          (+ word-count-other-word-counts word-count-my-word-count))) ;; Displays the word count
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
          (setq word-count-timer (run-at-time "1 min" 60 #'word-count-background-tasks)))
        ;; Add local hooks
        (add-hook 'post-self-insert-hook 'word-count-count-words nil t)
        (add-hook 'kill-buffer-hook 'word-count-disable))
    (remove-hook 'post-self-insert-hook 'word-count-count-words t)
    (remove-hook 'kill-buffer-hook 'word-count-disable)
    (word-count-deactivate)))


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

(defun word-count-source-id ()
  "Generate a unique identifier for the current instance of Emacs.

The identifier is formatted as 'username@hostname:PID', where:
- 'username' is the current user's login name.
- 'hostname' is the name of the system on which Emacs is running.
- 'PID' is the process ID of the current Emacs instance."
  (concat (user-login-name) "@" (system-name) ":" (number-to-string (emacs-pid))))

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
         (entry (concat date "\t" source)))
    (with-temp-buffer
      (if (file-exists-p word-count-log-file)
          (insert-file-contents word-count-log-file))
      (goto-char (point-max))
      (if (search-backward (concat entry "\t") nil t)
          (delete-region (point) (1+ (line-end-position))))  ;; Remove previous entry for the date
      (insert (concat entry "\t" count-str "\n"))
      (write-region (point-min) (point-max) word-count-log-file nil))))

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
    (setq word-count-last-date today)))

(defun word-count-load-from-file ()
  "Helper function to load the word count from log file."
  (let* ((today (word-count-today-date))
         (source word-count-source-id)
         (entry (concat today "\t" source)))
    (if (file-exists-p word-count-log-file)
        (with-temp-buffer
          (insert-file-contents word-count-log-file)
          (goto-char (point-max))
          (when (search-backward (concat entry "\t") nil t)
            (let* ((line (thing-at-point 'line t))
                   (parts (split-string line "\t")))
              (when parts
                (string-to-number (nth 2 parts)))))))))

(defun word-count-load-count ()
  "Load the word count from the log file if `word-count-log-file`
is set and the file exists."
  (interactive)
  (if word-count-log-file
      (or (word-count-load-from-file) 0)
    0))

(defun word-count-load-other-counts ()
  "Load the sum of word counts coming from other instances."
  (let* ((today (word-count-today-date))
         (this-source word-count-source-id)
         (other-counts 0))
    (if (file-exists-p word-count-log-file)
        (with-temp-buffer
          (insert-file-contents word-count-log-file)
          (goto-char (point-max))
          (while (search-backward (concat today "\t") nil t)
            (let* ((line (thing-at-point 'line t))
                   (parts (split-string line "\t")))
              (when (and parts
                         (not (string= this-source (nth 1 parts))))
                (setq other-counts (+ other-counts
                                      (string-to-number (nth 2 parts)))))))
          other-counts)
      0)))

(defun word-count-update-other-counts ()
  "Update the global variable `word-count-other-word-counts`."
  (setq word-count-other-word-counts (word-count-load-other-counts)))

(defun word-count-background-tasks ()
  "Save the local word count to the log file, and update the other word counts."
  (progn
    (word-count-save-count)
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
