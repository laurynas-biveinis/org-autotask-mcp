;;; org-autotask-mcp-test.el --- Test for org-autotask-mcp -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; URL: https://github.com/laurynas-biveinis/org-autotask-mcp

;;; Commentary:
;; Test starting a real MCP server for Org tasks

;;; Code:

(require 'ert)
(require 'mcp)
(require 'org-autotask-mcp)

(ert-deftest org-autotask-mcp-test-server-starts ()
  "Test that the MCP server starts successfully."
  (unwind-protect
      (progn
        ;; Start the server
        (org-autotask-mcp-start-server)

        ;; Verify that the server is running
        (should org-autotask-mcp-server-running))

    ;; Always stop the server after test
    (org-autotask-mcp-stop-server)))

(ert-deftest org-autotask-mcp-test-list-files-empty ()
  "Test list-available-org-files with empty `org-autotask-mcp-files'."
  (let ((org-autotask-mcp-files nil)
        (result nil))
    (unwind-protect
        (progn
          ;; Start the server
          (org-autotask-mcp-start-server)
          (should org-autotask-mcp-server-running)

          (let* ((request
                  (json-encode
                   `((jsonrpc . "2.0")
                     (method . "tools/call")
                     (id . 1)
                     (params . ((name . "list-available-org-files")
                                (arguments . ()))))))
                 ;; Process the request directly through mcp-process-jsonrpc
                 (response-string (mcp-process-jsonrpc request))
                 ;; Parse the response
                 (response-data (json-read-from-string response-string)))

            ;; Set result for verification
            (setq result response-data)

            ;; Verify response has expected structure
            (let ((result-obj (assoc-default 'result result)))
              ;; Check content structure with expected format
              (let ((content (assoc-default 'content result-obj)))
                (should (vectorp content))
                (should (= (length content) 1))
                (let ((text-obj (aref content 0)))
                  (should (equal (assoc-default 'type text-obj) "text"))
                  (should (stringp (assoc-default 'text text-obj)))))
              (should (eq (assoc-default 'isError result-obj) :json-false))
              ;; Check that only these two fields exist
              (should (= (length result-obj) 2)))))

      ;; Clean up
      (org-autotask-mcp-stop-server))))

(defun org-autotask-mcp-test--create-get-file-request (file-path)
  "Create a JSON-RPC request to get content of FILE-PATH."
  (json-encode
   `((jsonrpc . "2.0")
     (method . "tools/call")
     (id . 1)
     (params . ((name . "get-org-file-content")
                (arguments . ((file-path . ,file-path))))))))

(ert-deftest org-autotask-mcp-test-get-file-content-empty ()
  "Test get-org-file-content with empty `org-autotask-mcp-files'."
  (let ((org-autotask-mcp-files nil)
        (result nil))
    (unwind-protect
        (progn
          ;; Start the server
          (org-autotask-mcp-start-server)
          (should org-autotask-mcp-server-running)

          (let* ((request
                  (org-autotask-mcp-test--create-get-file-request
                   "/path/to/nonexistent.org"))
                 ;; Process the request directly through mcp-process-jsonrpc
                 (response-string (mcp-process-jsonrpc request))
                 ;; Parse the response
                 (response-data (json-read-from-string response-string)))

            ;; Set result for verification
            (setq result response-data)

            ;; Verify response has expected structure for error
            (let ((result-obj (assoc-default 'result result)))
              ;; Check that isError is true for this case
              (should (eq (assoc-default 'isError result-obj) t))
              ;; Check content structure with expected error format
              (let ((content (assoc-default 'content result-obj)))
                (should (vectorp content))
                (should (= (length content) 1))
                (let ((text-obj (aref content 0)))
                  (should (equal (assoc-default 'type text-obj) "text"))
                  (should (stringp (assoc-default 'text text-obj)))
                  ;; Check for the exact error message
                  (should (equal "No org files in allowed list"
                                 (assoc-default 'text text-obj))))))))

      ;; Clean up
      (org-autotask-mcp-stop-server))))

(ert-deftest org-autotask-mcp-test-get-file-content-success ()
  "Test get-org-file-content with a file in `org-autotask-mcp-files'."
  (let* ((temp-file (make-temp-file "org-test" nil ".org"))
         (test-content "* Test Org File\n** Test Heading\nTest content")
         (org-autotask-mcp-files (list temp-file))
         (result nil))
    (unwind-protect
        (progn
          ;; Write test content to the file
          (with-temp-file temp-file
            (insert test-content))
          ;; Start the server
          (org-autotask-mcp-start-server)
          (should org-autotask-mcp-server-running)

          (let* ((request
                  (org-autotask-mcp-test--create-get-file-request temp-file))
                 ;; Process the request directly through mcp-process-jsonrpc
                 (response-string (mcp-process-jsonrpc request))
                 ;; Parse the response
                 (response-data (json-read-from-string response-string)))

            ;; Set result for verification
            (setq result response-data)

            ;; Verify response has expected structure for success
            (let ((result-obj (assoc-default 'result result)))
              ;; Check that isError is false for this case
              (should (eq (assoc-default 'isError result-obj) :json-false))
              ;; Check content structure with expected format
              (let ((content (assoc-default 'content result-obj)))
                (should (vectorp content))
                (should (= (length content) 1))
                (let ((text-obj (aref content 0)))
                  (should (equal (assoc-default 'type text-obj) "text"))
                  (should (stringp (assoc-default 'text text-obj)))
                  ;; Check that the content matches what we expect
                  (should (equal test-content
                                 (assoc-default 'text text-obj))))))))

      ;; Clean up
      (org-autotask-mcp-stop-server)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(provide 'org-autotask-mcp-test)
;;; org-autotask-mcp-test.el ends here