#!/bin/bash

EXIT_CODE=0

# Check Elisp syntax via byte compilation
echo "Checking Elisp syntax via byte compilation..."
if ! emacs -Q --batch \
	--eval "(setq byte-compile-warnings nil)" \
	--eval "(add-to-list 'load-path \".\")" \
	--eval "(add-to-list 'load-path (expand-file-name \"~/.emacs.d/elpa/mcp/\"))" \
	--eval "(dolist (file '(\"org-autotask-mcp.el\" \"org-autotask-mcp-test.el\"))
      (message \"Checking syntax of %s...\" file)
      (if (not (byte-compile-file file))
          (kill-emacs 1)))"; then
	echo "Elisp byte compilation check failed"
	EXIT_CODE=1
fi

# Format Elisp files using elisp-autofmt
if [ $EXIT_CODE -eq 0 ]; then
	echo "Running elisp-autofmt on Elisp files..."
	if ! emacs -Q --batch --eval "(let ((pkg-dirs (list (locate-user-emacs-file \"elpa/elisp-autofmt-20250421.1112\")
                                          (expand-file-name \".\"))))
                         (dolist (dir pkg-dirs)
                           (add-to-list 'load-path dir))
                         (require 'elisp-autofmt)
                         (dolist (file '(\"org-autotask-mcp.el\" \"org-autotask-mcp-test.el\"))
                           (message \"Formatting %s...\" file)
                           (find-file file)
                           (elisp-autofmt-buffer-to-file)
                           (message \"Formatted %s\" file)))"; then
		echo "elisp-autofmt failed"
		EXIT_CODE=1
	fi
else
	echo "Skipping formatting due to syntax errors"
fi

# Run elisp-lint on Emacs Lisp files
echo "Running elisp-lint..."
if ! emacs -Q --batch --eval "(progn \
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
    (elisp-lint-file file)))"; then
	echo "Elisp linting failed!"
	EXIT_CODE=1
fi

rm org-autotask-mcp.elc
rm org-autotask-mcp-test.elc

# Run Elisp tests
echo "Running tests..."
if ! emacs -Q --batch --eval "(progn \
  (add-to-list 'load-path \
    (expand-file-name \"~/.emacs.d/elpa/mcp/\")) \
  (add-to-list 'load-path default-directory) \
  (require 'ert) \
  (load \"org-autotask-mcp-test.el\") \
  (ert-run-tests-batch-and-exit))"; then
	echo "Tests failed!"
	EXIT_CODE=1
fi

# Run Markdown linting
echo "Running Markdown linter..."
if ! mdl ./*.md; then
	echo "Markdown linting failed!"
	EXIT_CODE=1
fi

# Run terminology check
echo "Running terminology check..."
if ! textlint --rule terminology ./*.md; then
	echo "Terminology check failed!"
	EXIT_CODE=1
fi

# Run prettier on Markdown files
echo "Running prettier on Markdown files..."
if ! prettier --check ./*.md; then
	echo "Prettier check failed for Markdown files!"
	EXIT_CODE=1
fi

# Run GitHub Actions workflow linting
echo "Running GitHub Actions workflow linter..."
if ! actionlint .github/workflows/*.yml; then
	echo "GitHub Actions workflow linting failed!"
	EXIT_CODE=1
fi

# Run shellcheck on shell scripts
echo "Running shellcheck..."
if ! shellcheck ./*.sh; then
	echo "Shellcheck failed!"
	EXIT_CODE=1
fi

# Run prettier on YAML files
echo "Running prettier on YAML files..."
if ! prettier --check .github/workflows/*.yml; then
	echo "Prettier check failed for YAML files!"
	EXIT_CODE=1
fi

# If all checks passed, run shfmt to format shell scripts
if [ $EXIT_CODE -eq 0 ]; then
	echo "Running shfmt to format shell scripts..."
	if ! shfmt -w ./*.sh; then
		echo "shfmt failed!"
		EXIT_CODE=1
	fi
fi

# Final result
if [ $EXIT_CODE -eq 0 ]; then
	echo "OK to continue!"
fi

exit $EXIT_CODE
