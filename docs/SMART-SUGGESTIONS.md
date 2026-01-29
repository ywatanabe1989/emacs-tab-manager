# Smart Suggestions

## Overview

ETM's Smart Suggestions feature provides ML-inspired buffer recommendations based on your usage patterns.

## Features

- **Context-aware suggestions** based on:
  - Current project/directory
  - Major mode
  - Time of day
  - Remote host context

- **Privacy-focused**: All pattern tracking is local-only
- **Adaptive scoring**: Learns from your buffer switching habits

## Usage

Suggestions integrate with completion frameworks (ivy, helm, vertico) and appear when switching buffers.

## Configuration

```elisp
;; Enable/disable smart suggestions
(setq etm-smart-suggestions-enabled t)

;; Adjust suggestion count
(setq etm-smart-suggestions-max 5)
```

## How It Works

1. **Pattern Collection**: Tracks buffer switches within context
2. **Scoring**: Weighs recency, frequency, and context match
3. **Ranking**: Presents most relevant buffers first

The system respects your privacy - no data leaves your machine.
