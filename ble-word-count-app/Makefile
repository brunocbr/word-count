# -------- configuration --------

APP_NAME := BLE\ Word\ Count
APP_BUNDLE := dist/$(APP_NAME).app
VENV := venv
PYTHON := $(VENV)/bin/python
PIP := $(VENV)/bin/pip
DIST := dist
BUILD := build

# -------- phony targets --------

.PHONY: help venv install build clean rebuild run

# -------- help --------

help:
	@echo ""
	@echo "Targets:"
	@echo "  make venv      Create virtual environment"
	@echo "  make install   Install Python dependencies"
	@echo "  make build     Build the macOS .app using py2app"
	@echo "  make run       Run the built app manually (test)"
	@echo "  make clean     Remove build artifacts"
	@echo "  make rebuild   Clean and build again"
	@echo ""

# -------- environment --------

venv:
	python3 -m venv $(VENV)

install: venv
	$(PIP) install --upgrade pip
	$(PIP) install -r requirements.txt

# -------- build --------

build: install
	$(PYTHON) setup.py py2app

run:
	open "$(APP_BUNDLE)" --args 1234

# -------- cleanup --------

clean:
	rm -rf $(BUILD) $(DIST) $(VENV) *.egg-info

rebuild: clean build
