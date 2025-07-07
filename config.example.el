;;; config.example.el --- Example configuration for ob-mcp -*- lexical-binding: t; -*-

;; Copy this file to config.el and fill in your API credentials

(setq ob-mcp-servers
  '((charli3-tradingview
     :command "node"
     :args ("dist/index.js")
     :working-directory "/path/to/your/mcp-server"
     :env (("CHARLI3_API_URL" . "https://dev-api.charli3.io")
           ("CHARLI3_BEARER_TOKEN" . "your_api_key_here"))
     :tools (get_groups get_symbols get_token_data get_historical_data 
                        search_tokens get_server_info update_config test_connection))))

;;; config.example.el ends here