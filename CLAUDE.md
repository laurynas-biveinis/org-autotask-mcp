# Commands for Claude

## Standing Orders

- After each code change to Elisp files, run the byte compilation command to
  verify there are no compilation errors.
- Run the Markdown linter on any edited Markdown files: `mdl filename.md`

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