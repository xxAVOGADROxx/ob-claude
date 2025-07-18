;;; ob-mcp.el --- Org babel functions for MCP (Model Context Protocol) servers -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; URL: https://github.com/xxAVOGADROxx/ob-claude
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (ob-claude "1.0.0"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; MCP (Model Context Protocol) integration for org-babel.
;; Extends ob-claude functionality to work with MCP servers.
;;
;; Features:
;; - Execute MCP tool calls in org-babel blocks
;; - Multiple MCP server configuration
;; - Automatic server process management
;; - Integration with Claude for analysis
;; - Support for Charli3 TradingView server and others
;;
;; Setup:
;;   (require 'ob-mcp)
;;   (ob-mcp-setup)
;;
;; Usage:
;;   #+begin_src mcp-charli3 :tool get_token_data :policy-id "ada_policy" :asset-name "ada_asset"
;;   #+end_src

;;; Code:

(require 'ob)
(require 'json)
(require 'org-element)

;;; Customizable Variables

(defgroup ob-mcp nil
  "Org-babel integration for MCP servers."
  :group 'org-babel)

(defcustom ob-mcp-servers
  '((charli3-tradingview
     :command "node"
     :args ("dist/index.js")
     :working-directory "/home/jsrqv/code/typescript/tradingview-demo/mcp-server"
     :env (("CHARLI3_API_URL" . "https://dev-api.charli3.io")
           ("CHARLI3_BEARER_TOKEN" . ""))
     :tools (get_groups get_symbols get_token_data get_historical_data 
                        search_tokens get_server_info update_config test_connection)))
  "Configuration for MCP servers.
Each server is defined as:
  (SERVER-NAME
   :command \"executable\"
   :args (\"arg1\" \"arg2\")
   :env ((\"VAR1\" . \"value1\") (\"VAR2\" . \"value2\"))
   :tools (tool1 tool2 tool3))"
  :type '(alist :key-type symbol
          :value-type (plist :options
                             ((:command string)
                              (:args (repeat string))
                              (:env (alist :key-type string :value-type string))
                              (:tools (repeat symbol)))))
  :group 'ob-mcp)

(defcustom ob-mcp-timeout 30
  "Timeout in seconds for MCP tool calls."
  :type 'integer
  :group 'ob-mcp)

;;; Server Process Management

(defvar ob-mcp--active-processes nil
  "Alist of active MCP server processes.")

(defun ob-mcp--get-process (server-name)
  "Get or start MCP server process for SERVER-NAME."
  (let ((proc (cdr (assoc server-name ob-mcp--active-processes))))
    (if (and proc (process-live-p proc))
        proc
      (ob-mcp--start-server server-name))))

(defun ob-mcp--start-server (server-name)
  "Start MCP server for SERVER-NAME."

  (let* ((config (cdr (assoc server-name ob-mcp-servers)))
         (command (plist-get config :command))
         (args (plist-get config :args))
         (env (plist-get config :env))
         (working-dir (plist-get config :working-directory))
         (default-directory (or working-dir default-directory))
         (process-environment 
          (append (mapcar (lambda (pair) (format "%s=%s" (car pair) (cdr pair))) env)
                  process-environment))
         (proc (apply #'start-process 
                      (format "mcp-%s" server-name)
                      (format "*mcp-%s*" server-name)
                      command args)))
    (when proc
      (set-process-filter proc #'ob-mcp--process-filter)
      (setf (alist-get server-name ob-mcp--active-processes) proc)
      (message "Started MCP server: %s in directory: %s" server-name default-directory)
      proc)))

(defun ob-mcp--process-filter (proc string)
  "Process filter for MCP server output."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (insert string))))

(defun ob-mcp--stop-server (server-name)
  "Stop MCP server for SERVER-NAME."
  (let ((proc (cdr (assoc server-name ob-mcp--active-processes))))
    (when (and proc (process-live-p proc))
      (delete-process proc)
      (setf (alist-get server-name ob-mcp--active-processes) nil)
      (message "Stopped MCP server: %s" server-name))))

(defun ob-mcp-stop-all-servers ()
  "Stop all active MCP servers."
  (interactive)
  (dolist (server ob-mcp--active-processes)
    (ob-mcp--stop-server (car server))))

;;; Org-babel Configuration

(defvar org-babel-default-header-args:mcp-charli3
  '((:results . "output")
    (:exports . "both")
    (:tool . nil))
  "Default header arguments for MCP Charli3 blocks.")

;;; Core Functions

(defun ob-mcp-setup ()
  "Set up org-babel MCP integration."
  (interactive)
  (add-to-list 'org-babel-load-languages '(mcp-charli3 . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-src-lang-modes '("mcp-charli3" . json))
  (message "MCP integration setup complete"))

(defun org-babel-execute:mcp-charli3 (body params)
  "Execute MCP tool call for Charli3 server in BODY with PARAMS."
  (ob-mcp--execute-tool 'charli3-tradingview body params))

(defun ob-mcp--execute-tool (server-name body params)
  "Execute MCP tool call for SERVER-NAME with BODY and PARAMS."
  (let* ((tool-param (cdr (assoc :tool params)))
         (tool (if (stringp tool-param) tool-param (symbol-name tool-param)))
         (args (ob-mcp--extract-tool-args params))
         (proc (ob-mcp--get-process server-name)))
    (unless tool
      (error "No tool specified. Use :tool parameter"))
    (unless proc
      (error "Failed to start MCP server: %s" server-name))
    (ob-mcp--call-tool proc tool args body)))

(defun ob-mcp--extract-tool-args (params)
  "Extract tool arguments from babel PARAMS."
  (let ((args '()))
    (dolist (param params)
      (let ((key (car param))
            (value (cdr param)))
        (when (and (not (eq key :tool))
                   (not (eq key :results))
                   (not (eq key :exports))
                   (string-match "^:" (symbol-name key)))
          (push (cons (substring (symbol-name key) 1) value) args))))
    args))

(defun ob-mcp--call-tool (proc tool args body)
  "Call MCP tool via PROC with TOOL, ARGS, and BODY."
  (let* ((request-id (format "req-%d" (random 10000)))
         (tool-name (if (stringp tool) tool (symbol-name tool)))
         (request (json-encode
                   `((jsonrpc . "2.0")
                     (id . ,request-id)
                     (method . "tools/call")
                     (params . ((name . ,tool-name)
                                (arguments . ,(ob-mcp--format-args args body)))))))
         (response-buffer (get-buffer-create (format "*mcp-response-%s*" tool-name))))
    
    ;; Send request
    (process-send-string proc (concat request "\n"))
    
    ;; Wait for response
    (with-timeout (ob-mcp-timeout
                   (error "MCP tool call timeout: %s" tool-name))
      (while (not (ob-mcp--response-ready-p proc request-id))
        (sleep-for 0.1)))
    
    ;; Parse and return response
    (ob-mcp--parse-response proc request-id)))

(defun ob-mcp--format-args (args body)
  "Format ARGS and BODY for MCP tool call."
  (let ((formatted-args '()))
    (dolist (arg args)
      (push (cons (car arg) (cdr arg)) formatted-args))
    (when (and body (not (string-empty-p (string-trim body))))
      (push (cons "query" (string-trim body)) formatted-args))
    formatted-args))

(defun ob-mcp--response-ready-p (proc request-id)
  "Check if response is ready for REQUEST-ID from PROC."
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (format "\"id\":\"%s\"" request-id) nil t))))

(defun ob-mcp--parse-response (proc request-id)
  "Parse MCP response for REQUEST-ID from PROC."
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (format "\"id\":\"%s\"" request-id) nil t)
        (beginning-of-line)
        (let* ((response-line (buffer-substring-no-properties 
                               (line-beginning-position) 
                               (line-end-position)))
               (response-json (json-read-from-string response-line))
               (result (cdr (assoc 'result response-json)))
               (error-info (cdr (assoc 'error response-json))))
          (cond
           (error-info
            (format "Error: %s" (cdr (assoc 'message error-info))))
           (result
            (ob-mcp--format-result result))
           (t "No response received")))))))

(defun ob-mcp--format-result (result)
  "Format MCP tool RESULT for display."
  (cond
   ;; Handle content array with text responses
   ((and (listp result) (vectorp (cdr (assoc 'content result))))
    (let ((content (cdr (assoc 'content result))))
      (mapconcat (lambda (item)
                   (let ((text (cdr (assoc 'text item))))
                     (if text
                         (ob-mcp--pretty-format-json text)
                       (json-encode item))))
                 content "\n")))
   ;; Handle error responses
   ((and (listp result) (cdr (assoc 'error result)))
    (let ((error-info (cdr (assoc 'error result))))
      (format "ERROR: %s\nType: %s\nDetails: %s"
              (cdr (assoc 'message error-info))
              (cdr (assoc 'type error-info))
              (cdr (assoc 'details error-info)))))
   ;; Handle direct content
   ((vectorp result)
    (mapconcat (lambda (item)
                 (if (and (listp item) (cdr (assoc 'text item)))
                     (ob-mcp--pretty-format-json (cdr (assoc 'text item)))
                   (json-encode item)))
               result "\n"))
   ;; Fallback
   (t (json-encode result))))

(defun ob-mcp--pretty-format-json (json-string)
  "Pretty format JSON string for better readability."
  (condition-case nil
      (let ((parsed (json-read-from-string json-string)))
        (cond
         ;; Handle simple objects with formatted output
         ((and (listp parsed)
               (cdr (assoc 'name parsed))
               (cdr (assoc 'version parsed)))
          (ob-mcp--format-server-info parsed))
         ;; Handle token data
         ((and (listp parsed)
               (cdr (assoc 'ticker parsed))
               (cdr (assoc 'price parsed)))
          (ob-mcp--format-token-data parsed))
         ;; Handle arrays of objects
         ((vectorp parsed)
          (ob-mcp--format-array parsed))
         ;; Handle errors
         ((and (listp parsed) (cdr (assoc 'error parsed)))
          (let ((error-info (cdr (assoc 'error parsed))))
            (format "ERROR: %s" (cdr (assoc 'message error-info)))))
         ;; Default: pretty print JSON
         (t (with-temp-buffer
              (insert json-string)
              (json-pretty-print-buffer)
              (buffer-string)))))
    (error json-string)))

(defun ob-mcp--format-server-info (server-info)
  "Format server info for display."
  (format "Server: %s v%s (%s)\nAPI: %s (Token: %s)\nCache: %d entries\nQueue: %d/%d requests\nAPI Calls: %d\nSupported DEXes: %s"
          (cdr (assoc 'name server-info))
          (cdr (assoc 'version server-info))
          (cdr (assoc 'status server-info))
          (cdr (assoc 'apiUrl (cdr (assoc 'config server-info))))
          (if (cdr (assoc 'hasToken (cdr (assoc 'config server-info)))) "✓" "✗")
          (cdr (assoc 'size (cdr (assoc 'cache server-info))))
          (cdr (assoc 'running (cdr (assoc 'queue server-info))))
          (cdr (assoc 'maxConcurrent (cdr (assoc 'queue server-info))))
          (cdr (assoc 'count (cdr (assoc 'api server-info))))
          (mapconcat 'identity (cdr (assoc 'supportedDexes server-info)) ", ")))

(defun ob-mcp--format-token-data (token-data)
  "Format token data for display."
  (format "%s (%s)\nPrice: $%.6f (%.2f%%)\nVolume 24h: $%.2f\nTVL: $%.2f\nLast Updated: %s"
          (cdr (assoc 'ticker token-data))
          (cdr (assoc 'name token-data))
          (cdr (assoc 'price token-data))
          (cdr (assoc 'price_change_percentage_24h token-data))
          (cdr (assoc 'volume_24h token-data))
          (cdr (assoc 'tvl token-data))
          (cdr (assoc 'timestamp token-data))))

(defun ob-mcp--format-array (array)
  "Format array of objects for display."
  (mapconcat (lambda (item)
               (cond
                ;; Group info
                ((and (listp item) (cdr (assoc 'name item)) (cdr (assoc 'id item)))
                 (format "• %s (%s) - %d symbols"
                         (cdr (assoc 'name item))
                         (cdr (assoc 'id item))
                         (or (cdr (assoc 'symbols_count item)) 0)))
                ;; Symbol info
                ((and (listp item) (cdr (assoc 'ticker item)) (cdr (assoc 'dex item)))
                 (format "• %s - %s (%s)"
                         (cdr (assoc 'ticker item))
                         (cdr (assoc 'name item))
                         (cdr (assoc 'dex item))))
                ;; Historical data
                ((and (listp item) (cdr (assoc 'time item)) (cdr (assoc 'close item)))
                 (format "Time: %s, Close: %.6f, Volume: %.2f"
                         (format-time-string "%Y-%m-%d %H:%M" 
                                             (seconds-to-time (cdr (assoc 'time item))))
                         (cdr (assoc 'close item))
                         (or (cdr (assoc 'volume item)) 0)))
                ;; Default
                (t (json-encode item))))
             array "\n"))

;;; Interactive Functions

(defun ob-mcp-list-tools (server-name)
  "List available tools for SERVER-NAME."
  (interactive
   (list (intern (completing-read "MCP Server: " 
                                  (mapcar #'car ob-mcp-servers)))))
  (let* ((config (cdr (assoc server-name ob-mcp-servers)))
         (tools (plist-get config :tools)))
    (message "Available tools for %s: %s" 
             server-name 
             (mapconcat #'symbol-name tools ", "))))

(defun ob-mcp-test-connection (server-name)
  "Test connection to SERVER-NAME."
  (interactive
   (list (intern (completing-read "MCP Server: " 
                                  (mapcar #'car ob-mcp-servers)))))
  (condition-case err
      (let ((proc (ob-mcp--get-process server-name)))
        (if proc
            (message "Successfully connected to %s" server-name)
          (error "Failed to connect to %s" server-name)))
    (error (message "Connection failed: %s" (error-message-string err)))))

;;; Utility Functions

(defun ob-mcp-insert-tool-template (server-name tool)
  "Insert template for TOOL from SERVER-NAME."
  (interactive
   (let* ((server (intern (completing-read "MCP Server: " 
                                           (mapcar #'car ob-mcp-servers))))
          (config (cdr (assoc server ob-mcp-servers)))
          (tools (plist-get config :tools))
          (tool (intern (completing-read "Tool: " 
                                         (mapcar #'symbol-name tools)))))
     (list server tool)))
  
  (let ((template (ob-mcp--get-tool-template server-name tool)))
    (insert template)))

(defun ob-mcp--get-tool-template (server-name tool)
  "Get template for TOOL from SERVER-NAME."
  (cond
   ((eq server-name 'charli3-tradingview)
    (ob-mcp--charli3-tool-template tool))
   (t (format "#+begin_src mcp-%s :tool %s\n#+end_src" server-name tool))))

(defun ob-mcp--charli3-tool-template (tool)
  "Get template for Charli3 TOOL."
  (pcase tool
    ('get_groups
     "#+begin_src mcp-charli3 :tool get_groups\n#+end_src")
    ('get_symbols
     "#+begin_src mcp-charli3 :tool get_symbols :group \"Aggregate\"\n#+end_src")
    ('get_token_data
     "#+begin_src mcp-charli3 :tool get_token_data :policy-id \"policy_id\" :asset-name \"asset_name\"\n#+end_src")
    ('get_historical_data
     "#+begin_src mcp-charli3 :tool get_historical_data :symbol \"ADAUSD:Aggregate\" :resolution \"1d\" :from 1704067200 :to 1704153600\n#+end_src")
    ('search_tokens
     "#+begin_src mcp-charli3 :tool search_tokens :group \"Aggregate\" :query \"ADA\" :limit 10\n#+end_src")
    ('get_server_info
     "#+begin_src mcp-charli3 :tool get_server_info\n#+end_src")
    ('update_config
     "#+begin_src mcp-charli3 :tool update_config :api-url \"https://api.charli3.io\" :bearer-token \"your_token\"\n#+end_src")
    ('test_connection
     "#+begin_src mcp-charli3 :tool test_connection\n#+end_src")
    (_ (format "#+begin_src mcp-charli3 :tool %s\n#+end_src" tool))))

;;; Configuration Helpers

(defun ob-mcp-configure-charli3 (api-url bearer-token &optional server-path)
  "Configure Charli3 TradingView MCP server."
  (interactive "sAPI URL: \nsBearerToken: \nsServer Path (optional): ")
  (let* ((server-config (assoc 'charli3-tradingview ob-mcp-servers))
         (config (cdr server-config))
         (env (plist-get config :env))
         (args (plist-get config :args)))
    
    ;; Update environment variables
    (setf (alist-get "CHARLI3_API_URL" env nil nil #'string=) api-url)
    (setf (alist-get "CHARLI3_BEARER_TOKEN" env nil nil #'string=) bearer-token)
    
    ;; Update server path if provided
    (when (and server-path (not (string-empty-p server-path)))
      (setf (car args) server-path))
    
    ;; Update configuration
    (setf (cdr server-config) 
          (plist-put (plist-put config :env env) :args args))
    
    (message "Charli3 TradingView MCP server configured")))

(provide 'ob-mcp)

;;; ob-mcp.el ends here
