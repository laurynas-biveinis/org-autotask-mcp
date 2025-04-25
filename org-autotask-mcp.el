;;; org-autotask-mcp.el --- Model Context Protocol server for Org tasks -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Laurynas Biveinis

;; Author: Laurynas Biveinis
;; Keywords: tools, ai
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (mcp "0.1.0"))
;; URL: https://github.com/laurynas-biveinis/org-autotask-mcp

;;; Commentary:

;; This package provides a Model Context Protocol (MCP) server implementation
;; for Emacs that integrates with Org mode for task management.

;;; Code:

(require 'org)
(require 'json)
(require 'mcp)

;;;###autoload
(defgroup org-autotask-mcp nil
  "Model Context Protocol server for Org tasks."
  :group 'org
  :prefix "org-autotask-mcp-")

(defcustom org-autotask-mcp-files nil
  "List of Org files to include in task management."
  :type '(repeat file)
  :group 'org-autotask-mcp)

(defvar org-autotask-mcp-server-running nil
  "Flag indicating whether the MCP server for Org tasks is running.")

(defun org-autotask-mcp-list-files ()
  "MCP tool handler to list available Org files."
  (if org-autotask-mcp-files
      (mapconcat #'identity org-autotask-mcp-files " ")
    ""))

(defun org-autotask-mcp-get-file-content (file-path)
  "Return the content of org file at FILE-PATH if it's in the allowed list.
Only files listed in `org-autotask-mcp-files` can be accessed."
  (cond
   ((null org-autotask-mcp-files)
    (mcp-tool-throw "No org files in allowed list"))
   ((member file-path org-autotask-mcp-files)
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string)))
   (t
    (mcp-tool-throw (format "File not in allowed list: %s" file-path)))))

;;;###autoload
(defun org-autotask-mcp-start-server ()
  "Start the MCP server for Org tasks."
  (interactive)
  (unless org-autotask-mcp-server-running
    ;; Start the MCP server
    (mcp-start)

    ;; Register tools
    (mcp-register-tool
     "list-available-org-files"
     "List available Org files for task management"
     #'org-autotask-mcp-list-files)

    (mcp-register-tool
     "get-org-file-content"
     "Get the full content of an Org file"
     #'org-autotask-mcp-get-file-content)

    ;; Update server status
    (setq org-autotask-mcp-server-running t)
    (message "Org-AutoTask MCP server started")))

;;;###autoload
(defun org-autotask-mcp-stop-server ()
  "Stop the MCP server for Org tasks."
  (interactive)
  (when org-autotask-mcp-server-running
    (mcp-stop)
    (setq org-autotask-mcp-server-running nil)
    (message "Org-AutoTask MCP server stopped")))

(provide 'org-autotask-mcp)
;;; org-autotask-mcp.el ends here