# Commands for Claude

## Standing Orders

- After each code change to Elisp files, run the byte compilation command to
  verify there are no compilation errors.
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
