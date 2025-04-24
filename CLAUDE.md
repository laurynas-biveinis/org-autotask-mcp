# Commands for Claude

## Standing Orders

- Run elisp-lint on any edited Elisp files (see the Elisp Lint section below).
- Run the Markdown linter on any edited Markdown files: `mdl filename.md`
- Run textlint terminology check on Markdown files:
  `textlint --rule terminology filename.md`
- Run the GitHub Actions workflow linter on any edited workflow files:
  `actionlint filename.yml`
- Run prettier to check and format YAML and Markdown files:
  `prettier --check filename.yml|md` and `prettier --write filename.yml|md` to fix

## Byte Compilation

To byte-compile all Elisp files:

```bash
emacs -Q --batch --eval "(progn \
  (add-to-list 'load-path \
    (expand-file-name \"~/.emacs.d/elpa/mcp/\")) \
  (add-to-list 'load-path \
    (expand-file-name \"~/.emacs.d/elpa/simple-httpd-1.5.1\")) \
  (add-to-list 'load-path default-directory))" \
  -f batch-byte-compile *.el
```

## Testing

To run the tests:

```bash
emacs -Q --batch --eval "(progn \
  (add-to-list 'load-path \
    (expand-file-name \"~/.emacs.d/elpa/mcp/\")) \
  (add-to-list 'load-path \
    (expand-file-name \"~/.emacs.d/elpa/simple-httpd-1.5.1\")) \
  (add-to-list 'load-path default-directory) \
  (require 'ert) \
  (load \"org-autotask-mcp-test.el\") \
  (ert-run-tests-batch-and-exit))"
```

## Elisp Lint

To run elisp-lint on all Elisp files:

```bash
emacs -Q --batch --eval "(progn \
  (add-to-list 'load-path \
    (expand-file-name \"~/.emacs.d/elpa/mcp/\")) \
  (add-to-list 'load-path \
    (expand-file-name \"~/.emacs.d/elpa/simple-httpd-1.5.1\")) \
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
```
