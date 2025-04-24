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

        ;; Verify that the server instance exists
        (should org-autotask-mcp-server))

    ;; Always stop the server after test
    (org-autotask-mcp-stop-server)))

(ert-deftest org-autotask-mcp-test-list-files-empty ()
  "Test list-available-org-files with empty `org-autotask-mcp-files'."
  (let ((org-autotask-mcp-files nil)
        (result nil))
    (unwind-protect
        (progn
          ;; Start the server on default port 8000
          (org-autotask-mcp-start-server)
          (should org-autotask-mcp-server)

          ;; Connect as a client and make the request
          (let* ((url-request-method "POST")
                 (url-request-extra-headers
                  '(("Content-Type" . "application/json")))
                 (url-request-data
                  (json-encode
                   `((jsonrpc . "2.0")
                     (method . "mcp.server.invoke_tool")
                     (id . 1)
                     (params . ((tool_name . "list-available-org-files")
                                (tool_input . ()))))))
                 (url "http://localhost:8000/mcp")
                 (response-buffer (url-retrieve-synchronously url t)))

            (with-current-buffer response-buffer
              (goto-char (point-min))
              (re-search-forward "\n\n")
              ;; Get the response as a string and convert it to JSON
              (let ((json-string (buffer-substring-no-properties
                                  (point) (point-max))))
                (setq result (json-read-from-string json-string))))

            ;; Verify response has expected structure with empty files list
            (let ((files (assoc-default 'files (assoc-default 'result result))))
              (should (equal files nil)))))

      ;; Clean up
      (org-autotask-mcp-stop-server))))

(provide 'org-autotask-mcp-test)
;;; org-autotask-mcp-test.el ends here