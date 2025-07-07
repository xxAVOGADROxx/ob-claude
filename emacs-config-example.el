;;; emacs-config-example.el --- Example Emacs configuration for Charli3 integration

;; Copy these lines to your Emacs configuration file (init.el or config.el)

;;; Basic Setup
;; Load the required files (adjust paths to your setup)
(load-file "/path/to/ob-claude/ob-mcp.el")
(load-file "/path/to/ob-claude/ob-charli3.el")

;; Setup the integrations
(ob-mcp-setup)
(ob-charli3-setup)

;;; Server Configuration
(setq ob-mcp-servers
  '((charli3-tradingview
     :command "node"
     :args ("dist/index.js")
     :working-directory "/path/to/your/tradingview-demo/mcp-server"  ; CHANGE THIS PATH
     :env (("CHARLI3_API_URL" . "https://dev-api.charli3.io")
           ("CHARLI3_BEARER_TOKEN" . ""))  ; Add your API key here or use environment variable
     :tools (get_groups get_symbols get_token_data get_historical_data 
                        search_tokens get_server_info update_config test_connection))))

;;; Alternative: Use environment variable for API key (recommended)
;; First set in your shell: export CHARLI3_BEARER_TOKEN="cta_your_api_key_here"
;; Then use:
;; ("CHARLI3_BEARER_TOKEN" . (getenv "CHARLI3_BEARER_TOKEN"))

;;; Optional: Debug mode (uncomment to enable)
;; (setq ob-mcp-debug t)

;;; Optional: Adjust timeout (default is 30 seconds)
;; (setq ob-mcp-timeout 60)

;;; Test your setup
;; After loading this config, test with:
;; 1. Open an org file
;; 2. Create a block: #+begin_src charli3
;;                    ADA price
;;                    #+end_src
;; 3. Run with C-c C-c

;;; emacs-config-example.el ends here