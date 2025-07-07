;;; ob-charli3.el --- Natural language interface for Charli3 trading data -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Jose Seraquive
;; URL: https://github.com/xxAVOGADROxx/ob-claude
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (ob-mcp "1.0.0"))

;;; Commentary:

;; Natural language interface for Charli3 trading data.
;; 
;; Instead of technical parameters, use simple queries like:
;; - "ADA price"
;; - "ADA/C3 history last month"
;; - "top tokens by volume"
;; - "SUNDAE vs ADA comparison"
;;
;; Results are formatted as beautiful org-mode tables and reports.

;;; Code:

(load-file "/home/jsrqv/code/elisp/ob-claude/ob-mcp.el")
(require 'org-table)

;;; Token Symbol Mapping

(defvar ob-charli3-token-map
  '(("ADA" . "ADAUSD")
    ("C3" . "C3USD") 
    ("CHARLI3" . "C3USD")
    ("SUNDAE" . "SUNDAEUSD")
    ("MIN" . "MINUSD")
    ("WRT" . "WRTUSD")
    ("MELD" . "MELDUSD")
    ("HOSKY" . "HOSKYUSD")
    ("DJED" . "DJEDUSD")
    ("SHEN" . "SHENUSD"))
  "Mapping of common token symbols to their trading pairs.")

(defvar ob-charli3-dex-aliases
  '(("aggregate" . "Aggregate")
    ("minswap" . "Minswap") 
    ("sundae" . "SundaeSwap")
    ("wing" . "WingRiders")
    ("mueli" . "MuesliSwap")
    ("spectrum" . "Spectrum")
    ("splash" . "Splash")
    ("vyfi" . "VyFi"))
  "Aliases for DEX names.")

;;; Babel Configuration

(defvar org-babel-default-header-args:charli3
  '((:results . "output")
    (:exports . "both"))
  "Default header arguments for Charli3 blocks.")

;;; Core Functions

(defun ob-charli3-setup ()
  "Set up natural language Charli3 integration."
  (interactive)
  (add-to-list 'org-babel-load-languages '(charli3 . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-src-lang-modes '("charli3" . text))
  (message "Charli3 natural language interface setup complete"))

(defun org-babel-execute:charli3 (body params)
  "Execute natural language Charli3 query in BODY."
  (let* ((query (string-trim body))
         (parsed (ob-charli3--parse-query query)))
    (ob-charli3--execute-parsed-query parsed)))

;;; Query Parsing

(defun ob-charli3--parse-query (query)
  "Parse natural language QUERY into structured request."
  (let ((query-lower (downcase query)))
    (cond
     ;; Price queries: "ADA price", "price of ADA", "ADA current price" 
     ((string-match "\\(price\\|current\\)" query-lower)
      (let ((token (ob-charli3--extract-token query-lower)))
        `(:type price :token ,token)))
     
     ;; History queries: "ADA history", "ADA chart last month", "ADA/C3 history 1d"
     ((string-match "\\(history\\|chart\\|historical\\)" query-lower)
      (let ((token (ob-charli3--extract-token query-lower))
            (period (ob-charli3--extract-period query))
            (resolution (ob-charli3--extract-resolution query)))
        `(:type history :token ,token :period ,period :resolution ,resolution)))
     
     ;; Comparison queries: "ADA vs SUNDAE", "compare ADA and C3"
     ((string-match "\\b\\([a-z0-9]+\\).*\\(vs\\|versus\\|compared?\\).*\\b\\([a-z0-9]+\\)" query-lower)
      `(:type compare :token1 ,(upcase (match-string 1 query-lower)) :token2 ,(upcase (match-string 3 query-lower))))
     
     ;; Market overview: "market overview", "top tokens", "dex summary"
     ((string-match "\\(market\\|overview\\|top\\|summary\\)" query-lower)
      `(:type market))
     
     ;; DEX queries: "minswap tokens", "sundae pools"
     ((string-match "\\b\\([a-z]+\\).*\\(tokens\\|pools\\|symbols\\)" query-lower)
      `(:type dex :name ,(match-string 1 query-lower)))
     
     ;; Default: treat as search
     (t `(:type search :query ,query)))))

(defun ob-charli3--extract-token (query)
  "Extract the most likely token from QUERY, ignoring conversational words."
  (let* ((query-lower (downcase query))
         ;; Remove conversational words
         (stop-words '("hey" "hi" "hello" "tell" "me" "the" "a" "an" "what" "is" "are" 
                      "current" "price" "of" "for" "please" "can" "you" "show" "give"
                      "history" "chart" "last" "past" "and" "or" "vs" "versus" "compared"
                      "compare" "to" "with" "from" "get" "fetch" "find" "search"))
         ;; Split into words
         (words (split-string query-lower "[ .,!?;:]+" t))
         ;; Filter out stop words and short words
         (filtered-words (seq-filter 
                         (lambda (word) 
                           (and (>= (length word) 2)
                                (not (member word stop-words))
                                ;; Keep words that look like tokens (letters/numbers)
                                (string-match "^[a-z0-9]+$" word)))
                         words))
         ;; Look for known token patterns or compound tokens
         (candidates (seq-map 'upcase filtered-words)))
    
    ;; Find the best candidate
    (cond
     ;; Handle compound tokens like "C3.ADA" 
     ((string-match "\\b\\([A-Z0-9]+\\)\\.\\([A-Z0-9]+\\)\\b" (upcase query))
      (match-string 1 (upcase query)))
     ;; Look for known tokens first
     ((seq-find (lambda (token) (assoc token ob-charli3-token-map)) candidates)
      (seq-find (lambda (token) (assoc token ob-charli3-token-map)) candidates))
     ;; Return the first reasonable candidate
     ((> (length candidates) 0)
      (car candidates))
     ;; Fallback: try to extract any uppercase sequence from original query
     ((string-match "\\b\\([A-Z][A-Z0-9]*\\)\\b" query)
      (match-string 1 query))
     ;; Last resort: return "ADA"
     (t "ADA"))))

(defun ob-charli3--extract-period (query)
  "Extract time period from QUERY."
  (cond
   ((string-match "\\(last\\|past\\).*\\(week\\)" query) "1w")
   ((string-match "\\(last\\|past\\).*\\(month\\)" query) "1m") 
   ((string-match "\\(last\\|past\\).*\\(day\\)" query) "1d")
   ((string-match "\\(last\\|past\\).*\\(year\\)" query) "1y")
   ((string-match "\\([0-9]+\\).*\\(day\\|d\\)" query) 
    (format "%dd" (string-to-number (match-string 1 query))))
   (t "1w"))) ; default to 1 week

(defun ob-charli3--extract-resolution (query)
  "Extract resolution from QUERY."
  (cond
   ((string-match "\\b1m\\b\\|\\b1min\\b" query) "1min")
   ((string-match "\\b5m\\b\\|\\b5min\\b" query) "5min")
   ((string-match "\\b15m\\b\\|\\b15min\\b" query) "15min")
   ((string-match "\\b1h\\b\\|\\b1hour\\b" query) "60min")
   ((string-match "\\b1d\\b\\|\\bdaily\\b" query) "1d")
   (t "1d"))) ; default to daily

;;; Query Execution

(defun ob-charli3--execute-parsed-query (parsed)
  "Execute PARSED query and format results."
  (pcase (plist-get parsed :type)
    ('price (ob-charli3--get-price parsed))
    ('history (ob-charli3--get-history parsed))
    ('compare (ob-charli3--compare-tokens parsed))
    ('market (ob-charli3--market-overview))
    ('dex (ob-charli3--dex-info parsed))
    ('search (ob-charli3--search-tokens parsed))
    (_ "Unknown query type")))

;;; Price Queries

(defun ob-charli3--get-price (parsed)
  "Get current price for token in PARSED query."
  (let* ((token (plist-get parsed :token))
         (search-result (ob-charli3--search-token token))
         (data (if search-result
                   (ob-charli3--call-mcp 'get_token_data 
                                         `(("policyId" . ,(cdr (assoc 'policy_id search-result)))
                                           ("assetName" . ,(cdr (assoc 'asset_name search-result)))))
                 nil)))
    (if (and data (not (string-match "ERROR" data)))
        (ob-charli3--format-price-table token data search-result)
      (format "Unable to fetch price for %s. %s" token 
              (if search-result 
                  (format "API Error: %s" data)
                "Token not found in search results")))))

(defun ob-charli3--format-price-table (token data search-result)
  "Format price DATA for TOKEN as org table."
  (let ((parsed-data (ob-charli3--parse-json-response data)))
    (if parsed-data
        (format "** %s Price Summary\n\n| Metric | Value |\n|--------|-------|\n| Symbol | %s |\n| DEX | %s |\n| Policy ID | %s |\n| Asset Name | %s |\n| Price | $%.6f |\n| Change 24h | %.2f%% |\n| Volume 24h | $%.2f |\n| TVL | $%.2f |\n| Last Updated | %s |\n\n"
                token
                (or (cdr (assoc 'ticker search-result)) 
                    (cdr (assoc 'ticker parsed-data)) 
                    token)
                (or (cdr (assoc 'dex search-result)) "Aggregate")
                (or (cdr (assoc 'policy_id search-result)) "N/A")
                (or (cdr (assoc 'asset_name search-result)) "N/A")
                (or (cdr (assoc 'price parsed-data)) 0)
                (or (cdr (assoc 'price_change_percentage_24h parsed-data)) 0)
                (or (cdr (assoc 'volume_24h parsed-data)) 0)
                (or (cdr (assoc 'tvl parsed-data)) 0)
                (or (cdr (assoc 'timestamp parsed-data)) "Unknown"))
      (format "Unable to parse price data for %s" token))))

;;; History Queries

(defun ob-charli3--get-history (parsed)
  "Get historical data for token in PARSED query."
  (let* ((token (plist-get parsed :token))
         (period (plist-get parsed :period))
         (resolution (plist-get parsed :resolution))
         (search-result (ob-charli3--search-token token))
         (symbol (if search-result
                     (format "%s:Aggregate" (cdr (assoc 'ticker search-result)))
                   (ob-charli3--resolve-symbol token)))
         (time-range (ob-charli3--calculate-time-range period))
         (data (if search-result
                   (ob-charli3--call-mcp 'get_historical_data 
                                         `(("symbol" . ,symbol)
                                           ("resolution" . ,resolution)
                                           ("from" . ,(car time-range))
                                           ("to" . ,(cdr time-range))))
                 nil)))
    (if (and data (not (string-match "ERROR" data)))
        (ob-charli3--format-history-table token data resolution period)
      (format "Unable to fetch history for %s. %s" token 
              (if search-result 
                  (format "API Error: %s" data)
                "Token not found in search results")))))

(defun ob-charli3--format-history-table (token data resolution period)
  "Format historical DATA for TOKEN as org table."
  (let ((parsed-data (ob-charli3--parse-json-response data)))
    (if (and parsed-data (vectorp parsed-data))
        (concat
         (format "** %s Historical Data (%s, %s)\n\n" token resolution period)
         "| Date | Open | High | Low | Close | Volume |\n"
         "|------|------|------|-----|-------|--------|\n"
         (mapconcat 
          (lambda (item)
            (format "| %s | $%.6f | $%.6f | $%.6f | $%.6f | %.2f |"
                    (format-time-string "%Y-%m-%d %H:%M" 
                                        (seconds-to-time (cdr (assoc 'time item))))
                    (or (cdr (assoc 'open item)) 0)
                    (or (cdr (assoc 'high item)) 0)
                    (or (cdr (assoc 'low item)) 0)
                    (or (cdr (assoc 'close item)) 0)
                    (or (cdr (assoc 'volume item)) 0)))
          (seq-take parsed-data 10) ; Show last 10 entries
          "\n")
         "\n\n")
      (format "Unable to parse historical data for %s" token))))

;;; Market Overview

(defun ob-charli3--market-overview ()
  "Get market overview with top tokens and DEX summary."
  (let ((groups-data (ob-charli3--call-mcp 'get_groups nil)))
    (if (and groups-data (not (string-match "ERROR" groups-data)))
        (ob-charli3--format-market-table groups-data)
      (format "Unable to fetch market data. Error: %s" groups-data))))

(defun ob-charli3--format-market-table (data)
  "Format market DATA as org table."
  (let ((parsed-data (ob-charli3--parse-json-response data)))
    (if parsed-data
        (concat
         "** Market Overview\n\n"
         "*** Available DEXes\n\n"
         "| DEX | Status |\n"
         "|-----|--------|\n"
         (mapconcat 
          (lambda (item)
            (format "| %s | Active |" (cdr (assoc 'id item))))
          (if (vectorp parsed-data) parsed-data 
            (cdr (assoc 'groups parsed-data)))
          "\n")
         "\n\n")
      "Unable to parse market data")))

;;; Token Search Functions

(defun ob-charli3--search-token (token)
  "Search for TOKEN and return the best match (highest TVL)."
  (let* ((search-data (ob-charli3--call-mcp 'search_tokens 
                                            `(("group" . "Aggregate")
                                              ("query" . ,(upcase token))
                                              ("limit" . 10))))
         (parsed-results (ob-charli3--parse-json-response search-data)))
    ;; Debug information
    (message "Search Debug - Token: %s, Raw Data: %s, Parsed: %s" 
             token search-data parsed-results)
    (cond
     ;; Handle vector response (direct JSON array)
     ((and parsed-results (vectorp parsed-results) (> (length parsed-results) 0))
      (let ((first-result (aref parsed-results 0)))
        (message "Search Success - Found %d results, using first: %s" 
                 (length parsed-results) first-result)
        first-result))
     ;; Handle list response (converted from JSON)
     ((and parsed-results (listp parsed-results) (> (length parsed-results) 0))
      (let ((first-result (car parsed-results)))
        (message "Search Success - Found %d results (list), using first: %s" 
                 (length parsed-results) first-result)
        first-result))
     ;; Handle string response that might be JSON
     ((stringp search-data)
      (message "Search trying to parse string response: %s" search-data)
      (condition-case nil
          (let ((json-result (json-read-from-string search-data)))
            (message "Parsed JSON result: %s" json-result)
            (if (and json-result (vectorp json-result) (> (length json-result) 0))
                (aref json-result 0)
              nil))
        (error 
         (message "Search Failed - JSON parsing error for token: %s" token)
         nil)))
     ;; No valid results
     (t
      (message "Search Failed - No valid results for token: %s, got: %s" token parsed-results)
      nil))))

(defun ob-charli3--get-symbols-for-dex (dex)
  "Get all symbols for DEX."
  (let* ((symbols-data (ob-charli3--call-mcp 'get_symbols 
                                             `(("group" . ,dex)
                                               ("useCache" . t))))
         (parsed-symbols (ob-charli3--parse-json-response symbols-data)))
    parsed-symbols))

;;; Utility Functions

(defun ob-charli3--resolve-symbol (token)
  "Resolve TOKEN to trading symbol."
  (let ((resolved (cdr (assoc (upcase token) ob-charli3-token-map))))
    (if resolved
        (format "%s:Aggregate" resolved)
      (format "%sUSD:Aggregate" (upcase token)))))

(defun ob-charli3--calculate-time-range (period)
  "Calculate time range for PERIOD."
  (let* ((now (float-time))
         (seconds-ago 
          (pcase period
            ("1d" (* 24 60 60))
            ("1w" (* 7 24 60 60))
            ("1m" (* 30 24 60 60))
            ("1y" (* 365 24 60 60))
            (_ (if (string-match "\\([0-9]+\\)d" period)
                   (* (string-to-number (match-string 1 period)) 24 60 60)
                 (* 7 24 60 60))))))
    (cons (floor (- now seconds-ago)) (floor now))))

(defun ob-charli3--call-mcp (tool args)
  "Call MCP TOOL with ARGS."
  (condition-case err
      (let ((proc (ob-mcp--get-process 'charli3-tradingview)))
        (if proc
            (ob-charli3--call-tool-raw proc tool args "")
          "MCP server not available"))
    (error (format "MCP call failed: %s" err))))

(defun ob-charli3--call-tool-raw (proc tool args body)
  "Call MCP tool via PROC with TOOL, ARGS, and BODY, returning raw JSON."
  (let* ((request-id (format "req-%d" (random 10000)))
         (tool-name (if (stringp tool) tool (symbol-name tool)))
         (request (json-encode
                   `((jsonrpc . "2.0")
                     (id . ,request-id)
                     (method . "tools/call")
                     (params . ((name . ,tool-name)
                                (arguments . ,(ob-charli3--format-args args body)))))))
         (response-buffer (get-buffer-create (format "*mcp-response-%s*" tool-name))))
    
    ;; Send request
    (process-send-string proc (concat request "\n"))
    
    ;; Wait for response
    (with-timeout (ob-mcp-timeout
                   (error "MCP tool call timeout: %s" tool-name))
      (while (not (ob-charli3--response-ready-p proc request-id))
        (sleep-for 0.1)))
    
    ;; Parse and return RAW response
    (ob-charli3--parse-response-raw proc request-id)))

(defun ob-charli3--format-args (args body)
  "Format ARGS and BODY for MCP tool call."
  (let ((formatted-args '()))
    (dolist (arg args)
      (push (cons (car arg) (cdr arg)) formatted-args))
    (when (and body (not (string-empty-p (string-trim body))))
      (push (cons "query" (string-trim body)) formatted-args))
    formatted-args))

(defun ob-charli3--response-ready-p (proc request-id)
  "Check if response is ready for REQUEST-ID from PROC."
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward (format "\"id\":\"%s\"" request-id) nil t))))

(defun ob-charli3--parse-response-raw (proc request-id)
  "Parse MCP response for REQUEST-ID from PROC, returning raw JSON."
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
            ;; Extract raw JSON from content array
            (if (and (listp result) (vectorp (cdr (assoc 'content result))))
                (let ((content (cdr (assoc 'content result))))
                  (if (> (length content) 0)
                      (cdr (assoc 'text (aref content 0)))
                    "No content in response"))
              (json-encode result)))
           (t "No response received")))))))

(defun ob-charli3--parse-json-response (response)
  "Parse JSON RESPONSE string."
  (condition-case nil
      (cond
       ;; If it's already parsed, return as-is
       ((and (not (stringp response)) response)
        response)
       ;; If it's a string, try to parse it
       ((stringp response)
        (let ((trimmed (string-trim response)))
          (if (string-prefix-p "[" trimmed)
              (json-read-from-string trimmed)
            ;; Maybe it's wrapped in an object
            (let ((parsed (json-read-from-string trimmed)))
              (or (cdr (assoc 'data parsed))
                  (cdr (assoc 'result parsed))
                  parsed)))))
       ;; Return nil for anything else
       (t nil))
    (error 
     (message "JSON parsing error for: %s" response)
     nil)))

;;; Interactive Functions

(defun ob-charli3-insert-query-template ()
  "Insert a template for natural language queries."
  (interactive)
  (let ((templates '("ADA price"
                     "ADA history last month 1d" 
                     "market overview"
                     "ADA vs SUNDAE comparison"
                     "minswap tokens")))
    (insert (format "#+begin_src charli3\n%s\n#+end_src"
                    (completing-read "Query template: " templates)))))

(defun ob-charli3-debug-parser (query)
  "Debug the natural language parser for QUERY."
  (interactive "sQuery to debug: ")
  (let* ((parsed (ob-charli3--parse-query query))
         (debug-buffer (get-buffer-create "*Charli3 Debug*")))
    (with-current-buffer debug-buffer
      (erase-buffer)
      (insert (format "Query: %s\n" query))
      (insert (format "Parsed: %s\n" parsed))
      (insert (format "Type: %s\n" (plist-get parsed :type)))
      (insert (format "Token: %s\n" (plist-get parsed :token)))
      (when (plist-get parsed :period)
        (insert (format "Period: %s\n" (plist-get parsed :period))))
      (when (plist-get parsed :resolution)
        (insert (format "Resolution: %s\n" (plist-get parsed :resolution))))
      (display-buffer (current-buffer)))
    parsed))

(defun ob-charli3-quick-price (token)
  "Quick price lookup for TOKEN."
  (interactive "sToken symbol: ")
  (let ((result (ob-charli3--get-price `(:token ,token))))
    (with-current-buffer (get-buffer-create "*Charli3 Price*")
      (erase-buffer)
      (insert result)
      (org-mode)
      (display-buffer (current-buffer)))))

(provide 'ob-charli3)

;;; ob-charli3.el ends here