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

(provide 'org-autotask-mcp-test)
;;; org-autotask-mcp-test.el ends here