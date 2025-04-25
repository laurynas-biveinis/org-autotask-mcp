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

            ;; Verify response has expected structure with empty files list
            (let ((files (assoc-default 'files (assoc-default 'result result))))
              (should (equal files [])))))

      ;; Clean up
      (org-autotask-mcp-stop-server))))

(provide 'org-autotask-mcp-test)
;;; org-autotask-mcp-test.el ends here