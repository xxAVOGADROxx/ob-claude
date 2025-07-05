# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**chatgpt-shell** is a Claude-focused Emacs package that provides a comint-based shell interface for interacting with Anthropic's Claude models. This is a security-focused fork that includes only Claude/Anthropic integration. It's written in Emacs Lisp.

## Core Architecture

### Main Components

- **`chatgpt-shell.el`** - Main entry point and core functionality (cleaned up for Claude-only usage)
- **`chatgpt-shell-prompt-compose.el`** - Compose buffer implementation for enhanced query crafting
- **`chatgpt-shell-anthropic.el`** - Anthropic Claude integration (only remaining provider)

### Key Dependencies

- **shell-maker** (v0.78.1+) - Provides the underlying shell infrastructure
- **Emacs 28.1+** - Minimum required version
- Built-in modules: `comint`, `markdown-overlays`, `org-babel`, `smerge-mode`

### Provider Architecture

The Claude provider follows this pattern:
- Model definition via `chatgpt-shell-anthropic-make-model` functions
- HTTP request handling via curl to Anthropic's API
- Response streaming and parsing
- Anthropic-specific customizations and API endpoints

## Development Workflow

### Testing

Run tests using Emacs' built-in ERT framework:

```elisp
;; Load the test file
(load-file "test_chatgpt-shell.el")

;; Run all tests
(ert-run-tests-batch-and-exit)

;; Run specific test
(ert "test-chatgpt-shell--append-system-info-smoke-test")
```

### Code Structure Conventions

- **Naming**: All functions use `chatgpt-shell-` prefix
- **Provider functions**: Use `chatgpt-shell-[provider]-` prefix
- **Private functions**: Use `chatgpt-shell--` prefix (double dash)
- **Customizable variables**: Use `defcustom` with appropriate `:group 'chatgpt-shell`
- **API keys**: Store as functions or variables following pattern `chatgpt-shell-[provider]-key`

### Key Entry Points

- `chatgpt-shell` - Main shell interface
- `chatgpt-shell-prompt-compose` - Enhanced compose buffer
- `chatgpt-shell-swap-model` - Switch between Claude model versions
- `chatgpt-shell-quick-insert` - Quick insertion from minibuffer

### Configuration Pattern

Claude configuration requires:
1. API key configuration via `chatgpt-shell-anthropic-key`
2. Model version selection from available Claude models
3. Optional URL base customization for proxies via `chatgpt-shell-anthropic-api-url-base`
4. Claude-specific parameters (temperature, token limits, thinking mode, etc.)

### Code Execution & Babel Integration

The package integrates with org-babel for executing code blocks within responses:
- Uses `C-c C-c` to execute source blocks
- Supports multiple programming languages
- Leverages existing org-babel infrastructure

### Session Management

- Transcripts can be saved via `chatgpt-shell-save-session-transcript`
- Sessions can be restored via `chatgpt-shell-restore-session-from-transcript`
- Multiple shell instances supported with `C-u M-x chatgpt-shell`

## Important Implementation Details

### Streaming Architecture

Responses are streamed by default using curl and parsed incrementally. The streaming can be disabled via `chatgpt-shell-streaming`.

### Model Swapping

The interface allows runtime switching between different Claude model versions while maintaining conversation context.

### Compose Buffer Workflow

The compose buffer provides a hybrid experience:
1. Craft detailed queries in a dedicated buffer
2. Submit with `C-c C-c`
3. Navigate responses with single-key bindings (n/p, r, m, q, e)
4. Execute code blocks inline

### Security Considerations

- Store Anthropic API keys securely (auth-sources, pass, environment variables)
- Never commit API keys to version control
- This fork removes all non-Claude providers for security and simplicity
- Proxy support available for corporate environments via `chatgpt-shell-proxy`
- All HTTP requests go directly to Anthropic's API endpoints only
