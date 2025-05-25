# Smart Suggestions Manual Testing Checklist

## Setup
- [ ] Load ETM: `(require 'etm)`
- [ ] Initialize ETM: `(etm-init)`
- [ ] Enable Smart mode: `M-x etm-smart-mode`

## Basic Functionality

### Pattern Tracking
- [ ] Switch between two buffers multiple times
- [ ] Verify pattern is tracked: Check with `etm-smart-check-patterns`
- [ ] Confirm patterns are tab-specific (create new tab and verify no patterns)

### Suggestions
- [ ] After creating patterns, use `C-x t S s` to quick switch
- [ ] Verify it suggests the most frequent switch target
- [ ] Use `C-x t S S` to see all suggestions
- [ ] Verify suggestions are ranked by score

### UI Features
- [ ] Enable overlay: `(setq etm-smart-ui-use-overlay t)`
- [ ] Wait in a buffer with patterns - overlay should appear
- [ ] In suggestions buffer, press 1-9 to switch to suggestions
- [ ] Press q to quit suggestions buffer

## Context Awareness

### Project Context
- [ ] Create patterns in one project
- [ ] Open different project (different default-directory)
- [ ] Verify suggestions are project-specific

### Major Mode Context
- [ ] Create patterns between elisp files
- [ ] Switch to markdown file
- [ ] Verify elisp patterns have lower scores from markdown

### Time Context
- [ ] Create patterns at current time
- [ ] Change `current-time` if testing (mock)
- [ ] Verify time-based scoring works

### Remote Context
- [ ] Open remote file via TRAMP
- [ ] Create patterns with remote buffers
- [ ] Verify remote patterns are separate from local

## Privacy Features

### Blacklisting
- [ ] Open a *scratch* buffer
- [ ] Try to create patterns with it
- [ ] Verify it's not tracked (check blacklist)

### Data Management
- [ ] Use `C-x t S c` to clear patterns
- [ ] Confirm clearing with yes/no prompt
- [ ] Verify patterns are cleared

### Persistence
- [ ] Create some patterns
- [ ] Save data: `M-x etm-smart-save-data`
- [ ] Clear patterns: `C-x t S c`
- [ ] Load data: `M-x etm-smart-load-data`
- [ ] Verify patterns are restored

## Integration

### Completion Frameworks
- [ ] Test with default completion
- [ ] Test with ivy (if available)
- [ ] Test with helm (if available)  
- [ ] Test with vertico (if available)

### Minibuffer Enhancement
- [ ] Use `C-x b` to switch buffers
- [ ] Verify suggestions appear with annotations
- [ ] Check score display in annotations

### Tab Bar Integration
- [ ] Enable tab bar display
- [ ] Verify suggestion indicator in tab name (if implemented)

## Edge Cases

### No Patterns
- [ ] Fresh start with no patterns
- [ ] Try `C-x t S s` - should handle gracefully
- [ ] Try `C-x t S S` - should show "No suggestions"

### Single Buffer
- [ ] Have only one buffer open
- [ ] Verify no errors when trying suggestions

### Many Patterns
- [ ] Create 10+ different patterns
- [ ] Verify performance is acceptable
- [ ] Check suggestion limiting works

### Circular Patterns
- [ ] Create A->B->C->A pattern
- [ ] Verify suggestions handle cycles properly

## Performance

### Response Time
- [ ] Measure time for `C-x t S s` with 10 patterns
- [ ] Measure time for `C-x t S S` with 50 patterns
- [ ] Verify overlay appears within configured delay

### Memory Usage
- [ ] Create 100+ patterns
- [ ] Check memory usage remains reasonable
- [ ] Verify old patterns decay properly

## Error Handling

### Invalid Buffers
- [ ] Create pattern with buffer A->B
- [ ] Kill buffer B
- [ ] Try suggestions from A - should handle missing buffer

### Corrupted Data
- [ ] Manually edit saved data file
- [ ] Try loading - should handle errors gracefully

### Mode Not Enabled
- [ ] Disable Smart mode
- [ ] Try Smart commands - should give helpful message

## Keybindings

### All Commands Work
- [ ] `C-x t S s` - Quick switch
- [ ] `C-x t S S` - Show suggestions  
- [ ] `C-x t S o` - Show overlay
- [ ] `C-x t S t` - Toggle on/off
- [ ] `C-x t S c` - Clear patterns
- [ ] `C-x t S r` - Reset scores

### No Conflicts
- [ ] Verify Smart keybindings don't conflict with existing ETM bindings
- [ ] Check no system keybinding conflicts

## Configuration

### Variables
- [ ] Change `etm-smart-max-suggestions` and verify limit
- [ ] Change `etm-smart-min-confidence` and verify filtering
- [ ] Change `etm-smart-decay-factor` and verify decay
- [ ] Disable `etm-smart-ui-use-overlay` and verify no overlay

### Customization
- [ ] Use `M-x customize-group RET etm-smart`
- [ ] Verify all options are documented
- [ ] Save customization and verify persistence

## Demo

### Run Demo Script
- [ ] Load `examples/smart-suggestions-demo.el`
- [ ] Run `M-x etm-smart-demo`
- [ ] Complete all demo scenarios
- [ ] Verify each scenario works as described