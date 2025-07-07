# Charli3 Trading Data Integration for Emacs

Natural language interface for Charli3 Cardano DeFi trading data in Emacs org-mode.

## 🎯 What This Does

Write natural language queries in Emacs and get beautiful trading data:

```org
#+begin_src charli3
Tell me the price of ADA.C3
#+end_src

#+begin_src charli3
What is the current SUNDAE price?
#+end_src

#+begin_src charli3
Search for C3 tokens
#+end_src
```

Instead of dealing with technical APIs, just ask in plain English!

## 🔧 Installation & Setup

### 1. Prerequisites

- Emacs with org-mode
- Node.js (for MCP server)
- Charli3 API key from https://charli3.io/api

### 2. Clone and Setup

```bash
# Clone the repository
git clone https://github.com/your-username/ob-claude.git
cd ob-claude

# Setup the MCP server (if you have the tradingview-demo)
cd /path/to/tradingview-demo/mcp-server
npm install
npm run build
```

### 3. Emacs Configuration

Add to your Emacs `config.el` or `init.el`:

```elisp
;; Load the Charli3 integration
(load-file "/path/to/ob-claude/ob-mcp.el")
(load-file "/path/to/ob-claude/ob-charli3.el")

;; Setup the integration
(ob-mcp-setup)
(ob-charli3-setup)

;; Configure the MCP server connection
(setq ob-mcp-servers
  '((charli3-tradingview
     :command "node"
     :args ("dist/index.js")
     :working-directory "/path/to/your/tradingview-demo/mcp-server"
     :env (("CHARLI3_API_URL" . "https://dev-api.charli3.io")
           ("CHARLI3_BEARER_TOKEN" . "your_api_key_here"))
     :tools (get_groups get_symbols get_token_data get_historical_data 
                        search_tokens get_server_info update_config test_connection))))
```

### 4. API Key Configuration

**Option A: Direct in config (not recommended for git)**
```elisp
("CHARLI3_BEARER_TOKEN" . "cta_your_actual_api_key_here")
```

**Option B: Environment variable (recommended)**
```bash
export CHARLI3_BEARER_TOKEN="cta_your_actual_api_key_here"
```

Then in Emacs config:
```elisp
("CHARLI3_BEARER_TOKEN" . (getenv "CHARLI3_BEARER_TOKEN"))
```

**Option C: .env file in MCP server directory**
Create `/path/to/mcp-server/.env`:
```
CHARLI3_API_URL=https://dev-api.charli3.io
CHARLI3_BEARER_TOKEN=cta_your_actual_api_key_here
```

## 🚀 Usage

### Natural Language Queries

#### Price Queries
```org
#+begin_src charli3
ADA price
#+end_src

#+begin_src charli3
What is the current C3 price?
#+end_src

#+begin_src charli3
Tell me the price of SUNDAE
#+end_src
```

#### Token Search
```org
#+begin_src charli3
Search for ADA tokens
#+end_src

#+begin_src charli3
Find C3 tokens
#+end_src
```

#### Market Overview
```org
#+begin_src charli3
Market overview
#+end_src

#+begin_src charli3
Show me the top tokens
#+end_src
```

#### Historical Data
```org
#+begin_src charli3
ADA history last week
#+end_src

#+begin_src charli3
C3 chart last month
#+end_src
```

### Technical MCP Calls (Advanced)

If you need direct API access:

```org
#+begin_src mcp-charli3 :tool get_groups
#+end_src

#+begin_src mcp-charli3 :tool search_tokens :group "Aggregate" :query "ADA" :limit 5
#+end_src

#+begin_src mcp-charli3 :tool get_token_data :policyId "LQ" :assetName "WC43ZB"
#+end_src
```

## 🎨 Example Outputs

### Price Query Result
```
💰 ADA Price Data

Current Price: $0.421337
24h Change: +2.34%
24h Volume: $1,234,567
TVL: $987,654
Last Updated: 2025-01-07 22:30:00 UTC

DEX: Aggregate
Policy ID: LQ
Asset Name: WC43ZB
```

### Search Results
```
🔍 Search Results for "ADA":

1. ADA (LQ/WC43ZB) - TVL: $987,654
2. ADA (WMT/5X4TJT) - TVL: $456,789  
3. ADA (SUNDAE/3376BU) - TVL: $234,567
```

## 🔧 Troubleshooting

### Common Issues

#### 1. "MCP tool call timeout"
- Check if MCP server is running
- Verify API key is correct
- Check network connection

```elisp
;; Debug: Check server buffer
(switch-to-buffer "*mcp-charli3-tradingview*")
```

#### 2. "Authentication failed"
- Verify your API key is valid
- Check the API URL is correct
- Ensure .env file is in the right directory

#### 3. "Token not found in search results"
- Try simpler token names (e.g., "ADA" instead of "ADA.C3")
- Check available tokens with market overview query
- Verify the token exists on Charli3

#### 4. Server starts but no response
- Check working directory is correct
- Verify all npm dependencies are installed
- Check server logs for errors

### Debug Mode

Enable debug output:
```elisp
(setq ob-mcp-debug t)
```

Check server buffer:
```elisp
(switch-to-buffer "*mcp-charli3-tradingview*")
```

Test connection manually:
```org
#+begin_src mcp-charli3 :tool test_connection
#+end_src
```

## 🏗️ Architecture

```
Emacs org-mode
    ↓
ob-charli3.el (Natural language parsing)
    ↓  
ob-mcp.el (MCP protocol communication)
    ↓
MCP Server (tradingview-demo/mcp-server)
    ↓
Charli3 API (https://dev-api.charli3.io)
```

## 📋 Available DEX Groups

- Aggregate
- Minswap  
- MinswapV2
- MuesliSwap
- Spectrum
- Splash
- SundaeSwap
- SundaeSwapV3
- VyFi
- WingRiders
- WingRidersV2

## 🔑 Supported Token Queries

The natural language parser recognizes:
- **ADA** - Cardano native token
- **C3** - Charli3 token
- **SUNDAE** - SundaeSwap token
- **MIN** - Minswap token
- **MELD** - MELD token
- **HOSKY** - HOSKY token
- **DJED** - DJED stablecoin
- And many more...

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test with your API key
5. Remove sensitive information
6. Submit a pull request

## 📄 License

MIT License - see LICENSE file for details.

## 🆘 Support

- Check the troubleshooting section above
- Review server logs in `*mcp-charli3-tradingview*` buffer
- Verify MCP server is running with `npm start`
- Test API connection directly with curl if needed

---

**Happy Trading! 🚀📈**