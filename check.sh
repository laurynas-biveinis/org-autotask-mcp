#!/bin/bash

EXIT_CODE=0

# Run elisp-lint on Emacs Lisp files
echo "Running elisp-lint..."
emacs -Q --batch --eval "(progn \
  (add-to-list 'load-path \
    (expand-file-name \"~/.emacs.d/elpa/mcp/\")) \
  (add-to-list 'load-path \
    (expand-file-name \"~/.emacs.d/elpa/elisp-lint-20220419.252\")) \
  (add-to-list 'load-path \
    (expand-file-name \"~/.emacs.d/elpa/package-lint-0.26\")) \
  (add-to-list 'load-path \
    (expand-file-name \"~/.emacs.d/elpa/dash-20250312.1307\")) \
  (add-to-list 'load-path default-directory) \
  (require 'elisp-lint) \
  (dolist (file '(\"org-autotask-mcp.el\" \"org-autotask-mcp-test.el\")) \
    (message \"Linting %s...\" file) \
    (elisp-lint-file file)))"

# Check linting exit code
if [ $? -ne 0 ]; then
    echo "Elisp linting failed!"
    EXIT_CODE=1
fi

# Run Elisp tests
echo "Running tests..."
emacs -Q --batch --eval "(progn \
  (add-to-list 'load-path \
    (expand-file-name \"~/.emacs.d/elpa/mcp/\")) \
  (add-to-list 'load-path default-directory) \
  (require 'ert) \
  (load \"org-autotask-mcp-test.el\") \
  (ert-run-tests-batch-and-exit))"

# Check test exit code
if [ $? -ne 0 ]; then
    echo "Tests failed!"
    EXIT_CODE=1
fi

# Run Markdown linting
echo "Running Markdown linter..."
mdl ./*.md
if [ $? -ne 0 ]; then
    echo "Markdown linting failed!"
    EXIT_CODE=1
fi

# Run terminology check
echo "Running terminology check..."
textlint --rule terminology ./*.md
if [ $? -ne 0 ]; then
    echo "Terminology check failed!"
    EXIT_CODE=1
fi

# Run prettier on Markdown files
echo "Running prettier on Markdown files..."
prettier --check ./*.md
if [ $? -ne 0 ]; then
    echo "Prettier check failed for Markdown files!"
    EXIT_CODE=1
fi

# Run GitHub Actions workflow linting
echo "Running GitHub Actions workflow linter..."
actionlint .github/workflows/*.yml
if [ $? -ne 0 ]; then
    echo "GitHub Actions workflow linting failed!"
    EXIT_CODE=1
fi

# Run prettier on YAML files
echo "Running prettier on YAML files..."
prettier --check .github/workflows/*.yml
if [ $? -ne 0 ]; then
    echo "Prettier check failed for YAML files!"
    EXIT_CODE=1
fi

# Final result
if [ $EXIT_CODE -eq 0 ]; then
    echo "OK to continue!"
fi

exit $EXIT_CODE