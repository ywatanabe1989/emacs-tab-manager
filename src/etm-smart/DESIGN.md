# ETM Smart Suggestions - Technical Design Document

## Architecture Overview

ETM Smart Suggestions enhances buffer management by learning from user behavior and providing intelligent, context-aware suggestions for buffer switching and layout management.

## Core Components

### 1. Pattern Tracking Engine (`etm-smart-patterns.el`)

The pattern tracking engine monitors buffer switching behavior and builds a statistical model of user workflows.

#### Data Structures

```elisp
;; Global pattern storage
(defvar etm-smart-patterns (make-hash-table :test 'equal)
  "Hash table mapping tab names to their pattern data.")

;; Pattern entry structure
(cl-defstruct etm-smart-pattern
  from-buffer      ; Source buffer name
  to-buffer        ; Destination buffer name  
  count            ; Number of occurrences
  timestamps       ; List of recent timestamps (capped)
  context          ; Additional context (project, time, etc.)
  score)           ; Calculated relevance score

;; Context structure  
(cl-defstruct etm-smart-context
  project-root     ; Current project root
  major-mode       ; Major mode of source buffer
  time-of-day      ; Morning/afternoon/evening/night
  day-of-week      ; Weekday/weekend
  remote-host)     ; Remote host if applicable
```

#### Key Functions

```elisp
(defun etm-smart-track-switch (from-buffer to-buffer)
  "Track a buffer switch from FROM-BUFFER to TO-BUFFER.")

(defun etm-smart-get-patterns (buffer &optional context)
  "Get switching patterns for BUFFER with optional CONTEXT.")

(defun etm-smart-calculate-scores (patterns current-context)
  "Calculate relevance scores for PATTERNS given CURRENT-CONTEXT.")
```

### 2. Suggestion Engine (`etm-smart-suggest.el`)

The suggestion engine uses tracked patterns to provide intelligent buffer suggestions.

#### Scoring Algorithm

The suggestion score combines multiple factors:

```
score = w1 * frequency_score + 
        w2 * recency_score + 
        w3 * context_match_score +
        w4 * group_affinity_score
```

Where:
- `frequency_score`: Normalized occurrence count
- `recency_score`: Time-decayed recent usage
- `context_match_score`: Similarity of current context
- `group_affinity_score`: Buffer group relationships

#### Key Functions

```elisp
(defun etm-smart-suggest-buffers (&optional count)
  "Suggest COUNT buffers based on current context.")

(defun etm-smart-score-suggestion (pattern current-context)
  "Calculate suggestion score for PATTERN in CURRENT-CONTEXT.")

(defun etm-smart-filter-suggestions (suggestions)
  "Filter and sort suggestions by relevance.")
```

### 3. Learning System (`etm-smart-learn.el`)

The learning system continuously improves suggestions based on user feedback.

#### Feedback Mechanisms

1. **Implicit Feedback**: Accepting/rejecting suggestions
2. **Explicit Feedback**: User ratings or corrections
3. **Temporal Patterns**: Time-based usage patterns

#### Key Functions

```elisp
(defun etm-smart-learn-from-feedback (suggestion accepted-p)
  "Update patterns based on whether SUGGESTION was ACCEPTED-P.")

(defun etm-smart-adapt-weights (performance-data)
  "Adapt scoring weights based on PERFORMANCE-DATA.")

(defun etm-smart-prune-patterns ()
  "Remove outdated or low-value patterns.")
```

### 4. UI Integration (`etm-smart-ui.el`)

Seamless integration with ETM's existing interface.

#### Display Modes

1. **Inline Suggestions**: In minibuffer during completion
2. **Suggestion Buffer**: Dedicated buffer with detailed info
3. **Overlay Hints**: Subtle UI hints in tab-bar or mode-line

#### Key Functions

```elisp
(defun etm-smart-completing-read (prompt)
  "Enhanced completing-read with smart suggestions.")

(defun etm-smart-show-suggestions ()
  "Display suggestions in a dedicated buffer.")

(defun etm-smart-annotate-completion (candidate)
  "Add suggestion metadata to completion CANDIDATE.")
```

### 5. Storage and Persistence (`etm-smart-store.el`)

Efficient storage of pattern data with privacy in mind.

#### Storage Format

```elisp
;; Stored in user-emacs-directory/etm-smart/patterns.el
((version . 1)
 (patterns . ((tab-name . (pattern-list ...))
              ...))
 (config . ((last-prune . timestamp)
            (total-switches . count))))
```

#### Key Functions

```elisp
(defun etm-smart-save-patterns ()
  "Save patterns to persistent storage.")

(defun etm-smart-load-patterns ()
  "Load patterns from persistent storage.")

(defun etm-smart-migrate-patterns (old-version)
  "Migrate patterns from OLD-VERSION format.")
```

## Integration Architecture

### With ETM Core

```elisp
;; Hook into buffer switching
(add-hook 'etm-buffer-switch-hook 'etm-smart-track-switch)

;; Enhance existing commands
(advice-add 'etm-buffer-jump-home :before 'etm-smart-pre-jump)
(advice-add 'etm-buffer-jump-home :after 'etm-smart-post-jump)
```

### With Buffer Groups

```elisp
;; Group-aware suggestions
(defun etm-smart-suggest-from-group (group-name)
  "Suggest buffers from GROUP-NAME based on patterns.")

;; Learn group navigation patterns
(defun etm-smart-track-group-navigation (from-group to-group)
  "Track navigation between buffer groups.")
```

### With Remote Support

```elisp
;; Remote-aware patterns
(defun etm-smart-track-remote-pattern (local-buffer remote-buffer)
  "Track patterns between local and remote buffers.")

;; Host-specific suggestions
(defun etm-smart-suggest-for-host (hostname)
  "Suggest buffers specific to HOSTNAME.")
```

## Performance Considerations

### Memory Management

- Pattern storage capped at 1000 entries per tab
- LRU eviction for old patterns
- Compressed storage format
- Lazy loading of historical data

### CPU Optimization

- Asynchronous pattern analysis
- Cached suggestion results (invalidated on switch)
- Incremental score updates
- Batched persistence operations

### Benchmarks

Target performance metrics:
- Pattern tracking: < 1ms per switch
- Suggestion generation: < 10ms for 5 suggestions
- Storage size: < 1MB for typical usage
- Memory overhead: < 5MB runtime

## Privacy and Security

### Data Protection

1. **Local Storage Only**: No network transmission
2. **Encrypted Storage**: Optional encryption for pattern data
3. **Anonymization**: Buffer names hashed in storage
4. **Selective Tracking**: Blacklist for sensitive buffers

### User Controls

```elisp
(defcustom etm-smart-privacy-mode nil
  "When non-nil, limit data collection and storage.")

(defcustom etm-smart-blacklist-patterns '("\\*password\\*" "\\*private\\*")
  "Patterns for buffers to never track.")

(defun etm-smart-clear-history (&optional tab-name)
  "Clear pattern history for TAB-NAME or all tabs.")
```

## Configuration Options

```elisp
(defgroup etm-smart nil
  "Smart suggestions for ETM."
  :group 'etm)

(defcustom etm-smart-enable t
  "Enable smart buffer suggestions."
  :type 'boolean)

(defcustom etm-smart-min-confidence 0.3
  "Minimum confidence score for suggestions."
  :type 'float)

(defcustom etm-smart-max-suggestions 5
  "Maximum number of suggestions to show."
  :type 'integer)

(defcustom etm-smart-learning-rate 0.1
  "Rate at which patterns are learned."
  :type 'float)

(defcustom etm-smart-decay-factor 0.95
  "Daily decay factor for pattern scores."
  :type 'float)
```

## Testing Strategy

### Unit Tests

```elisp
;; Test pattern tracking
(ert-deftest test-etm-smart-track-switch ()
  "Test pattern tracking functionality.")

;; Test suggestion generation
(ert-deftest test-etm-smart-suggest ()
  "Test suggestion generation.")

;; Test score calculation
(ert-deftest test-etm-smart-scoring ()
  "Test scoring algorithm.")
```

### Integration Tests

```elisp
;; Test with ETM buffer system
(ert-deftest test-etm-smart-buffer-integration ()
  "Test integration with ETM buffer management.")

;; Test with remote support
(ert-deftest test-etm-smart-remote-integration ()
  "Test integration with remote buffers.")
```

### Performance Tests

```elisp
;; Benchmark pattern tracking
(ert-deftest test-etm-smart-performance ()
  "Test performance meets targets.")

;; Memory usage tests
(ert-deftest test-etm-smart-memory ()
  "Test memory usage stays within bounds.")
```

## Migration Path

### From v2.4.0 to v2.5.0

1. Smart features disabled by default initially
2. Gradual pattern collection begins
3. Suggestions appear after threshold met
4. Full features available after warm-up period

### Compatibility

- Fully backward compatible
- Graceful degradation if disabled
- No impact on existing workflows
- Optional for all users

## Future Enhancements

### v2.6.0 Possibilities

1. **Project Templates**: Pre-defined patterns for common projects
2. **Pattern Sharing**: Export/import pattern sets
3. **Advanced Analytics**: Usage reports and insights

### v3.0.0 Vision

1. **ML Integration**: Neural network-based predictions
2. **Collaborative Filtering**: Community patterns
3. **Workflow Automation**: Macro-like buffer sequences

## Conclusion

ETM Smart Suggestions represents a significant step toward intelligent buffer management. By learning from user behavior while respecting privacy, it can dramatically improve the Emacs buffer switching experience without adding complexity for users who prefer traditional methods.