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

(defun org-autotask-mcp-test--create-tool-request
    (tool-name &optional arguments)
  "Create JSON-RPC request to call TOOL-NAME with ARGUMENTS."
  (json-encode
   `((jsonrpc . "2.0")
     (method . "tools/call")
     (id . 1)
     (params . ((name . ,tool-name) (arguments . ,(or arguments ())))))))

(defun org-autotask-mcp-test--send-request (request)
  "Send REQUEST to MCP server and return parsed response data."
  (let* ((response-string (mcp-process-jsonrpc request))
         (response-data (json-read-from-string response-string)))
    response-data))

(ert-deftest org-autotask-mcp-test-enable-disable ()
  "Test that the MCP tools are enabled and disabled successfully."
  (unwind-protect
      (progn
        ;; Start the MCP server first
        (mcp-start)

        ;; Enable MCP tools
        (org-autotask-mcp-enable)

        ;; Verify that tools are enabled
        (should org-autotask-mcp-enabled)
        ;; Verify tools are registered by making a call
        (let* ((request
                (org-autotask-mcp-test--create-tool-request
                 "list-available-org-files"))
               (response-data (org-autotask-mcp-test--send-request request)))

          ;; Verify we got a valid response (not an error about missing tool)
          (should (assoc 'result response-data))
          (should-not (assoc 'error response-data)))

        ;; Disable tools
        (org-autotask-mcp-disable)

        ;; Verify that tools are disabled
        (should-not org-autotask-mcp-enabled))
    ;; Always clean up after test
    (when (boundp 'org-autotask-mcp-enabled)
      (when org-autotask-mcp-enabled
        (org-autotask-mcp-disable)))
    (mcp-stop)))

(ert-deftest org-autotask-mcp-test-list-files-empty ()
  "Test list-available-org-files with empty `org-autotask-mcp-files'."
  (let ((org-autotask-mcp-files nil)
        (result nil))
    (unwind-protect
        (progn
          ;; Start the server and enable tools
          (mcp-start)
          (org-autotask-mcp-enable)

          (let* ((request
                  (org-autotask-mcp-test--create-tool-request
                   "list-available-org-files"))
                 (response-data (org-autotask-mcp-test--send-request request)))

            ;; Set result for verification
            (setq result response-data)

            ;; Verify response has expected structure
            (let ((result-obj (assoc-default 'result result)))
              ;; Check content structure with expected format
              (org-autotask-mcp-test--verify-content-structure result-obj)
              (should (eq (assoc-default 'isError result-obj) :json-false))
              ;; Check that only these two fields exist
              (should (= (length result-obj) 2)))))

      ;; Clean up
      (org-autotask-mcp-disable)
      (mcp-stop))))

(defun org-autotask-mcp-test--create-get-file-request (file-path)
  "Create a JSON-RPC request to get content of FILE-PATH."
  (org-autotask-mcp-test--create-tool-request "get-org-file-content"
                                              `((file-path . ,file-path))))

(defun org-autotask-mcp-test--verify-content-structure (result-obj)
  "Verify the content structure in RESULT-OBJ has expected format."
  (let ((content (assoc-default 'content result-obj)))
    (should (vectorp content))
    (should (= (length content) 1))
    (let ((text-obj (aref content 0)))
      (should (equal (assoc-default 'type text-obj) "text"))
      (should (stringp (assoc-default 'text text-obj))))))

(ert-deftest org-autotask-mcp-test-get-file-content-empty ()
  "Test get-org-file-content with empty `org-autotask-mcp-files'."
  (let ((org-autotask-mcp-files nil)
        (result nil))
    (unwind-protect
        (progn
          ;; Start the server and enable tools
          (mcp-start)
          (org-autotask-mcp-enable)

          (let* ((request
                  (org-autotask-mcp-test--create-get-file-request
                   "/path/to/nonexistent.org"))
                 (response-data (org-autotask-mcp-test--send-request request)))

            ;; Set result for verification
            (setq result response-data)

            ;; Verify response has expected structure for error
            (let ((result-obj (assoc-default 'result result)))
              ;; Check that isError is true for this case
              (should (eq (assoc-default 'isError result-obj) t))
              ;; Check content structure with expected error format
              (org-autotask-mcp-test--verify-content-structure result-obj)
              ;; Check for the exact error message
              (let ((text-obj (aref (assoc-default 'content result-obj) 0)))
                (should
                 (equal
                  "No org files in allowed list"
                  (assoc-default 'text text-obj)))))))

      ;; Clean up
      (org-autotask-mcp-disable)
      (mcp-stop))))

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
          ;; Start the server and enable tools
          (mcp-start)
          (org-autotask-mcp-enable)

          (let* ((request
                  (org-autotask-mcp-test--create-get-file-request temp-file))
                 (response-data (org-autotask-mcp-test--send-request request)))

            ;; Set result for verification
            (setq result response-data)

            ;; Verify response has expected structure for success
            (let ((result-obj (assoc-default 'result result)))
              ;; Check that isError is false for this case
              (should (eq (assoc-default 'isError result-obj) :json-false))
              ;; Check content structure with expected format
              (org-autotask-mcp-test--verify-content-structure result-obj)
              ;; Check that the content matches what we expect
              (let ((text-obj (aref (assoc-default 'content result-obj) 0)))
                (should (equal test-content (assoc-default 'text text-obj)))))))

      ;; Clean up
      (org-autotask-mcp-disable)
      (mcp-stop)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(provide 'org-autotask-mcp-test)
;;; org-autotask-mcp-test.el ends here
