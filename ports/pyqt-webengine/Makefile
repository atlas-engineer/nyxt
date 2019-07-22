PYTHON ?= python
PIP ?= pip


install:
	$(PIP) install -r requirements.txt

run:
	$(PYTHON) next-pyqt.py

isort:
	isort -y --atomic

qa:
	flake8 *.py
