# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with the ob-claude repository.

## Project Overview

**ob-claude** is an Emacs Org-mode babel extension that enables executing Claude AI queries as source blocks. It integrates with org-babel to provide seamless AI interactions within org documents.

## Core Architecture

### Main Components

- **`ob-claude.el`** - Main implementation file containing all functionality
- **Context Management** - Efficient system for managing conversation context
- **API Integration** - Direct HTTP requests to Anthropic's Claude API
- **Org-babel Integration** - Seamless integration with existing org-mode workflows

### Key Features

1. **Efficient by Default** - Only sends current block unless context specified
2. **Context Management** - Named contexts for conversation continuity
3. **Parameter Support** - Model, system prompts, temperature, etc.
4. **Cost Optimization** - Designed to minimize token usage and API costs

## Context System (Important)

The context system is the key feature that makes ob-claude efficient:

### Default Behavior (Efficient)
- No `:context` parameter → Only current block sent
- Saves tokens and money
- Suitable for independent queries

### Context Modes
- `:context "all"` → Include all previous claude blocks
- `:context "name"` → Include blocks with matching context name
- `:context nil` → Current block only (same as default)

### Implementation Details

**Functions:**
- `ob-claude--get-relevant-blocks` - Filters blocks based on context mode
- `ob-claude--build-context` - Builds message array for API
- `ob-claude--build-messages` - Converts context to Claude API format

**Logic:**
- Context filtering happens in `ob-claude--get-relevant-blocks`
- Default behavior (nil context) returns empty blocks list
- Only specified contexts include previous blocks

## Configuration

### Required
```elisp
(setq ob-claude-api-key "sk-ant-api03-...")
```

### Optional
```elisp
(setq ob-claude-default-model "claude-3-5-sonnet-20241022")
```

## Available Parameters

- `:model` - Claude model selection
- `:system` - System prompt
- `:temperature` - Response randomness (0.0-1.0)
- `:context` - Context management mode

## Common Issues and Solutions

### 1. "Multibyte text in HTTP request"
**Problem:** Characters with accents/special characters not properly encoded
**Solution:** Ensure UTF-8 encoding in HTTP requests

### 2. "Unexpected role" API error
**Problem:** Old cached version of code with incorrect role formatting
**Solution:** Restart Emacs or run `M-x eval-buffer`

### 3. Entire conversation history sent
**Problem:** Context system including too many previous blocks
**Solution:** Use default context mode (no `:context` parameter)

## Development Guidelines

### Code Structure
- All functions use `ob-claude-` prefix
- Private functions use `ob-claude--` prefix (double dash)
- Customizable variables use `defcustom`

### Key Functions
- `org-babel-execute:claude` - Main entry point
- `ob-claude--api-request` - HTTP request handling
- `ob-claude--build-context` - Context management
- `ob-claude--build-messages` - Message formatting

### Testing
- Test with different context modes
- Verify UTF-8 encoding handling
- Test API key validation
- Check parameter parsing

## Security Considerations

- Never commit API keys to version control
- Use environment variables or auth-sources for keys
- Monitor API usage to control costs
- Be aware of token counting for cost optimization

## Cost Optimization

The context system is designed for cost efficiency:
- Default behavior minimizes token usage
- Named contexts allow precise control
- Avoid `:context "all"` unless necessary
- Consider model selection (haiku vs opus) for cost vs capability tradeoffs

## Integration with Org-mode

- Uses standard org-babel infrastructure
- Supports `C-c C-c` execution
- Results inserted below source blocks
- Integrates with org-mode export systems
- Supports all standard org-babel parameters

## API Integration

- Direct HTTP requests to `api.anthropic.com`
- Uses `url-retrieve-synchronously` for HTTP
- JSON encoding/decoding for API communication
- Proper error handling and user feedback
- Follows Anthropic API specifications

This architecture ensures ob-claude is efficient, cost-effective, and well-integrated with the Emacs ecosystem while providing powerful AI capabilities within org documents.