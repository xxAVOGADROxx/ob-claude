;;; ob-claude.el --- Org babel functions for Claude API -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; URL: https://github.com/xxAVOGADROxx/ob-claude
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; Simple org-babel integration for Claude API.
;;
;; Features:
;; - Execute Claude queries in org-babel blocks
;; - Context management for conversations
;; - System prompts, temperature, and model version control
;; - Secure API key handling
;;
;; Setup:
;;   (require 'ob-claude)
;;   (ob-claude-setup)
;;
;; Usage:
;;   #+begin_src claude
;;     Hello Claude!
;;   #+end_src

;;; Code:

(require 'ob)
(require 'json)
(require 'url)
(require 'org-element)

;;; Customizable Variables

(defgroup ob-claude nil
  "Org-babel integration for Claude API."
  :group 'org-babel)

(defcustom ob-claude-api-key nil
  "Claude API key.
Can be a string or a function that returns the API key."
  :type '(choice (string :tag "API Key")
          (function :tag "Function to get API Key"))
  :group 'ob-claude)

(defcustom ob-claude-default-model "claude-3-5-sonnet-20241022"
  "Default Claude model to use."
  :type 'string
  :group 'ob-claude)

(defcustom ob-claude-api-url "https://api.anthropic.com/v1/messages"
  "Claude API endpoint URL."
  :type 'string
  :group 'ob-claude)

(defcustom ob-claude-max-tokens 8192
  "Maximum tokens for Claude response."
  :type 'integer
  :group 'ob-claude)

;;; Org-babel Configuration

(defvar org-babel-default-header-args:claude
  '((:results . "output")
    (:exports . "both")
    (:model . nil)
    (:system . nil)
    (:temperature . nil)
    (:context . nil))
  "Default header arguments for Claude blocks.")

;;; Core Functions

(defun ob-claude-setup ()
  "Set up org-babel Claude integration."
  (add-to-list 'org-babel-load-languages '(claude . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-src-lang-modes '("claude" . text)))

(defun org-babel-execute:claude (body params)
  "Execute Claude query in BODY with PARAMS."
  (let* ((model (or (cdr (assoc :model params)) ob-claude-default-model))
         (system (cdr (assoc :system params)))
         (temperature (cdr (assoc :temperature params)))
         (context-name (cdr (assoc :context params)))
         (context (ob-claude--build-context body context-name))
         (messages (ob-claude--build-messages context)))
    (ob-claude--api-request model messages system temperature)))

;;; Context Management

(defun ob-claude--build-context (current-prompt context-name)
  "Build conversation context including CURRENT-PROMPT.
Efficient by default: only sends current block unless context specified.
Context modes:
- nil/missing: current block only (efficient, saves tokens/costs)
- \"all\": include all previous claude blocks
- \"context-name\": include blocks with matching context name"
  (let ((blocks (ob-claude--get-relevant-blocks context-name))
        (context '()))
    (dolist (block blocks)
      (let ((prompt (plist-get block :prompt))
            (response (plist-get block :response)))
        (when prompt
          (push (cons :user prompt) context))
        (when response
          (push (cons :assistant response) context))))
    (push (cons :user current-prompt) context)
    (nreverse context)))

(defun ob-claude--get-relevant-blocks (context-name)
  "Get relevant source blocks for context.
Context modes:
- nil or missing: current block only (default, efficient)
- \"all\": include all previous claude blocks
- \"t\": same as \"all\" (legacy compatibility)
- specific name: only blocks with matching context name"
  (let ((current-pos (point))
        (blocks '()))
    (when (and context-name
               (or (string= context-name "all")
                   (string= context-name "t")
                   (not (string= context-name "none"))))
      (org-element-map (org-element-parse-buffer) 'src-block
        (lambda (element)
          (when (and (< (org-element-property :begin element) current-pos)
                     (string= (org-element-property :language element) "claude"))
            (let* ((params (org-babel-parse-header-arguments
                            (or (org-element-property :parameters element) "")))
                   (block-context (cdr (assoc :context params))))
              (when (or (string= context-name "all")
                        (string= context-name "t")
                        (string= block-context context-name))
                (push (list :prompt (string-trim (org-element-property :value element))
                            :response (ob-claude--get-block-result element))
                      blocks)))))))
    (nreverse blocks)))

(defun ob-claude--get-block-result (element)
  "Get the result of a source block ELEMENT."
  (save-excursion
    (goto-char (org-element-property :begin element))
    (when (org-babel-where-is-src-block-result)
      (goto-char (org-babel-where-is-src-block-result))
      (org-babel-read-result))))

(defun ob-claude--build-messages (context)
  "Build messages array for Claude API from CONTEXT."
  (let ((messages '()))
    (dolist (item context)
      (let ((role (car item))
            (content (cdr item)))
        (push `((role . ,(substring (symbol-name role) 1))
                (content . ,content))
              messages)))
    (nreverse messages)))

;;; API Interface

(defun ob-claude--api-request (model messages system temperature)
  "Make request to Claude API with MODEL, MESSAGES, SYSTEM, and TEMPERATURE."
  (let ((api-key (ob-claude--get-api-key)))
    (unless api-key
      (error "Claude API key not set. Configure ob-claude-api-key"))
    (ob-claude--http-request
     :model model
     :messages messages
     :system system
     :temperature temperature
     :api-key api-key)))

(defun ob-claude--get-api-key ()
  "Get Claude API key from configuration."
  (cond
   ((functionp ob-claude-api-key)
    (funcall ob-claude-api-key))
   ((stringp ob-claude-api-key)
    ob-claude-api-key)
   ;; Fallback to chatgpt-shell-anthropic-key if available
   ((and (boundp 'chatgpt-shell-anthropic-key) chatgpt-shell-anthropic-key)
    (if (functionp chatgpt-shell-anthropic-key)
        (funcall chatgpt-shell-anthropic-key)
      chatgpt-shell-anthropic-key))
   (t nil)))

(cl-defun ob-claude--http-request (&key model messages system temperature api-key)
  "Make HTTP request to Claude API."
  (let* ((request-data (append
                        `((model . ,model)
                          (max_tokens . ,ob-claude-max-tokens)
                          (messages . ,(vconcat messages)))
                        (when system `((system . ,system)))
                        (when temperature `((temperature . ,temperature)))))
         (json-data (json-encode request-data))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("x-api-key" . ,api-key)
            ("anthropic-version" . "2023-06-01")))
         (url-request-data json-data))
    (with-current-buffer (url-retrieve-synchronously ob-claude-api-url)
      (goto-char (point-min))
      (re-search-forward "^$" nil t)
      (let* ((response-json (json-read))
             (error-msg (cdr (assoc 'message (cdr (assoc 'error response-json)))))
             (content (cdr (assoc 'content response-json))))
        (if error-msg
            (error "Claude API error: %s" error-msg)
          (if (and content (vectorp content) (> (length content) 0))
              (cdr (assoc 'text (aref content 0)))
            (error "Unexpected Claude API response format")))))))

;;; Setup function for compatibility

(defun ob-claude-use-chatgpt-shell-key ()
  "Configure ob-claude to use chatgpt-shell-anthropic-key."
  (setq ob-claude-api-key
        (lambda ()
          (cond
           ((functionp chatgpt-shell-anthropic-key)
            (funcall chatgpt-shell-anthropic-key))
           ((stringp chatgpt-shell-anthropic-key)
            chatgpt-shell-anthropic-key)
           (t (error "chatgpt-shell-anthropic-key not properly configured"))))))

(provide 'ob-claude)

;;; ob-claude.el ends here
