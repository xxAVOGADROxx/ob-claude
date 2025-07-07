# MCP Integration for ob-claude

This extension adds Model Context Protocol (MCP) support to ob-claude, enabling direct access to external data sources and tools from within Emacs org-mode documents.

## Features

- **Seamless Integration**: Works alongside existing ob-claude functionality
- **Multiple MCP Servers**: Support for various MCP servers with easy configuration
- **Charli3 TradingView**: Built-in support for Cardano DeFi market data
- **Process Management**: Automatic server startup and management
- **Template System**: Quick insertion of tool call templates
- **Error Handling**: Robust error handling and timeout management

## Installation

1. **Prerequisites**: Ensure you have `ob-claude.el` working first
2. **Load MCP Extension**:
   ```elisp
   (load-file "/path/to/ob-mcp.el")
   (ob-mcp-setup)
   ```

## Quick Start

### Configure Charli3 TradingView Server

```elisp
(ob-mcp-configure-charli3 
 "https://api.charli3.io"
 "your-bearer-token-here"
 "/path/to/mcp-server/dist/index.js")
```

### Basic Usage

```org
#+begin_src mcp-charli3 :tool get_groups
#+end_src

#+begin_src mcp-charli3 :tool get_token_data :policy-id "ada_policy" :asset-name "ada_asset"
#+end_src
```

## Charli3 TradingView Tools

### Market Data

#### `get_groups`
Get available trading groups/DEXes
```org
#+begin_src mcp-charli3 :tool get_groups
#+end_src
```

#### `get_symbols`  
Get symbols for a specific DEX
```org
#+begin_src mcp-charli3 :tool get_symbols :group "Aggregate"
#+end_src
```

#### `get_token_data`
Get current token price, volume, and TVL
```org
#+begin_src mcp-charli3 :tool get_token_data :policy-id "policy_id" :asset-name "asset_name"
#+end_src
```

#### `get_historical_data`
Get historical OHLCV data
```org
#+begin_src mcp-charli3 :tool get_historical_data :symbol "ADAUSD:Aggregate" :resolution "1d" :from 1704067200 :to 1704153600
#+end_src
```

#### `search_tokens`
Search for tokens
```org
#+begin_src mcp-charli3 :tool search_tokens :group "Aggregate" :query "ADA" :limit 10
#+end_src
```

### Server Management

#### `get_server_info`
Get server status and configuration
```org
#+begin_src mcp-charli3 :tool get_server_info
#+end_src
```

#### `test_connection`
Test API connectivity
```org
#+begin_src mcp-charli3 :tool test_connection
#+end_src
```

#### `update_config`
Update server configuration
```org
#+begin_src mcp-charli3 :tool update_config :api-url "https://api.charli3.io" :bearer-token "new_token"
#+end_src
```

## Advanced Usage

### Combining MCP with Claude Analysis

1. **Fetch Data**:
```org
#+begin_src mcp-charli3 :tool get_historical_data :symbol "ADAUSD:Aggregate" :resolution "1d" :from 1704067200 :to 1704153600
#+end_src
```

2. **Analyze with Claude**:
```org
#+begin_src claude :context "trading-analysis"
Based on the ADA price data above, provide technical analysis including trends, support/resistance levels, and trading recommendations.
#+end_src
```

### Multi-Token Analysis

```org
#+begin_src mcp-charli3 :tool search_tokens :group "Aggregate" :query "SUNDAE" :limit 3
#+end_src

#+begin_src mcp-charli3 :tool get_token_data :policy-id "sundae_policy" :asset-name "sundae_asset"
#+end_src

#+begin_src claude :context "portfolio-analysis"
Compare ADA vs SUNDAE performance and provide investment recommendations.
#+end_src
```

## Configuration

### MCP Server Configuration

Servers are configured in `ob-mcp-servers`:

```elisp
(setq ob-mcp-servers
  '((charli3-tradingview
     :command "node"
     :args ("/path/to/mcp-server/dist/index.js")
     :env (("CHARLI3_API_URL" . "https://api.charli3.io")
           ("CHARLI3_BEARER_TOKEN" . "your-token"))
     :tools (get_groups get_symbols get_token_data get_historical_data 
             search_tokens get_server_info update_config test_connection))))
```

### Environment Variables

You can also set environment variables:
```bash
export CHARLI3_API_URL="https://api.charli3.io"
export CHARLI3_BEARER_TOKEN="your-token-here"
```

## Interactive Commands

### `M-x ob-mcp-list-tools`
List available tools for a server

### `M-x ob-mcp-test-connection`
Test connection to an MCP server

### `M-x ob-mcp-insert-tool-template`
Insert a template for a specific tool

### `M-x ob-mcp-stop-all-servers`
Stop all running MCP server processes

## Supported Resolutions

- `1min` - 1 minute intervals
- `5min` - 5 minute intervals  
- `15min` - 15 minute intervals
- `60min` - 1 hour intervals
- `1d` - 1 day intervals

## Supported DEXes

- **Aggregate** - Synthetic pool combining all DEXes
- **Minswap** - V1 and V2 protocols
- **SundaeSwap** - V1 and V3 protocols
- **WingRiders** - V1 and V2 protocols
- **MuesliSwap** - Community DEX
- **Spectrum** - AMM protocol
- **Splash** - DEX platform
- **VyFi** - Yield farming platform

## Troubleshooting

### Common Issues

1. **Server Won't Start**
   - Check the server path in configuration
   - Verify Node.js is installed
   - Check MCP server dependencies

2. **Authentication Errors**
   - Verify your Charli3 API token
   - Check API URL configuration
   - Ensure sufficient API quota

3. **Tool Call Timeouts**
   - Increase `ob-mcp-timeout` value
   - Check network connectivity
   - Verify server is responding

### Debug Mode

View server output in the process buffer:
- `*mcp-charli3-tradingview*` - Server output and logs

### Server Status

Check server status:
```org
#+begin_src mcp-charli3 :tool get_server_info
#+end_src
```

## Workflow Examples

### Daily Market Analysis

1. Get market overview
2. Check top tokens  
3. Analyze trends with Claude
4. Generate trading recommendations

### Token Research

1. Search for tokens of interest
2. Gather current and historical data
3. Perform technical analysis
4. Create investment thesis

### Portfolio Monitoring

1. Track specific token positions
2. Monitor price movements
3. Set up alerts and notifications
4. Rebalance recommendations

## Security Considerations

- Store API keys securely (never commit to version control)
- Use environment variables for sensitive data
- Monitor API usage and costs
- Be aware of rate limits

## Cost Optimization

- Use appropriate time resolutions for your needs
- Cache frequently accessed data
- Monitor Charli3 API usage dashboard
- Use efficient context management with Claude

## Contributing

This MCP integration is designed to be extensible. To add support for new MCP servers:

1. Add server configuration to `ob-mcp-servers`
2. Create new babel language (e.g., `mcp-newserver`)
3. Add execution function (`org-babel-execute:mcp-newserver`)
4. Add tool templates
5. Update documentation

## Examples

See `examples-mcp.org` for comprehensive usage examples and workflows.

## Support

- [Charli3 Documentation](https://docs.charli3.io)
- [MCP Specification](https://spec.modelcontextprotocol.io/)
- [ob-claude Repository](https://github.com/xxAVOGADROxx/ob-claude)