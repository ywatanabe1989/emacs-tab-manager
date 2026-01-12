# Enhanced Remote Support Design

## Overview
This module enhances ETM's support for remote files and buffers accessed via TRAMP, providing robust connection management, visual indicators, and remote-aware navigation.

## Architecture

### 1. Connection Management (`etm-remote-connection.el`)
- **Connection Pool**: Maintain active TRAMP connections with health monitoring
- **Multi-Method Support**: Handle SSH, SCP, SUDO, and other TRAMP methods
- **Auto-Reconnect**: Automatic reconnection on failure with exponential backoff
- **Connection Info**: Track method, user, host, port for each connection

### 2. Visual Indicators (`etm-remote-indicators.el`)
- **Tab Names**: Show remote host in tab names (e.g., "Tab 1 [host]")
- **Buffer Names**: Prefix remote buffers with host (e.g., "[host] file.el")
- **Mode Line**: Add remote indicator with connection status
- **Tab Bar**: Color-code tabs with remote buffers

### 3. Remote Navigation (`etm-remote-navigation.el`)
- **Host-Aware Commands**: Navigate buffers filtered by host
- **Cross-Host Jumping**: Quick switch between hosts
- **Remote Buffer Groups**: Auto-group buffers by host
- **Connection Status**: Show connection health in buffer lists

### 4. Error Handling (`etm-remote-errors.el`)
- **Graceful Degradation**: Continue working when connections fail
- **User Notifications**: Clear messages about connection issues
- **Retry Logic**: Smart retry with backoff
- **Fallback Strategies**: Local cache for layout persistence

### 5. Layout Persistence (`etm-remote-layout.el`)
- **Enhanced Save Format**: Include TRAMP method and connection details
- **Connection Restoration**: Re-establish connections when loading layouts
- **Host Migration**: Handle host renames and IP changes
- **Offline Mode**: Load layouts without active connections

## Data Structures

```elisp
;; Connection info per tab
(defvar etm-remote-connections
  (make-hash-table :test 'equal)
  "Hash table mapping tab-id to connection alist.
Keys are tab IDs, values are alists of (host . connection-info).")

;; Connection info structure
(defstruct etm-remote-connection
  method     ; TRAMP method (ssh, scp, sudo, etc.)
  user       ; Username
  host       ; Hostname or IP
  port       ; Port number (optional)
  status     ; :connected, :disconnected, :connecting
  last-check ; Timestamp of last health check
  retries    ; Number of reconnection attempts
  properties ; Additional TRAMP properties
  )

;; Visual indicator settings
(defcustom etm-remote-indicator-format "[%h]"
  "Format for remote host indicators. %h is replaced with hostname.")

(defcustom etm-remote-tab-prefix t
  "Whether to show remote host in tab names.")

(defcustom etm-remote-buffer-prefix t
  "Whether to prefix buffer names with remote host.")
```

## API Functions

### Connection Management
- `etm-remote-connect(method user host &optional port)` - Establish connection
- `etm-remote-disconnect(host)` - Close connection
- `etm-remote-check-connection(host)` - Health check
- `etm-remote-reconnect(host)` - Force reconnection
- `etm-remote-cleanup-all()` - Cleanup all connections

### Navigation
- `etm-remote-switch-to-host(host)` - Switch to first buffer on host
- `etm-remote-next-buffer-on-host()` - Next buffer on same host
- `etm-remote-previous-buffer-on-host()` - Previous buffer on same host
- `etm-remote-list-buffers-by-host()` - Show buffers grouped by host
- `etm-remote-jump-to-host-home()` - Jump to home buffer on current host

### Visual Indicators
- `etm-remote-update-tab-names()` - Update all tab names with host info
- `etm-remote-format-buffer-name(buffer)` - Add host prefix to buffer name
- `etm-remote-mode-line-indicator()` - Generate mode line indicator
- `etm-remote-toggle-indicators()` - Toggle visual indicators on/off

## Integration Points

1. **Buffer Registration**: Extend `etm-buffer-register` to detect remote buffers
2. **Tab Creation**: Modify `etm-new-tab` to handle remote-specific tabs
3. **Layout Save/Load**: Enhance layout functions for remote persistence
4. **Keybindings**: Add remote-specific commands to `etm-command-map`

## Testing Strategy

1. **Mock TRAMP Functions**: Create test doubles for TRAMP APIs
2. **Connection Simulation**: Test connection lifecycle without real network
3. **Error Scenarios**: Test various failure modes
4. **Visual Verification**: Test indicator formatting
5. **Performance**: Ensure no slowdown for local buffers

## Implementation Phases

### Phase 1: Core Infrastructure (Priority: High)
- [ ] Create etm-remote module structure
- [ ] Implement connection data structures
- [ ] Add TRAMP method detection
- [ ] Basic connection health checking

### Phase 2: Visual Indicators (Priority: Medium)
- [ ] Tab name enhancement
- [ ] Buffer name prefixing
- [ ] Mode line indicator
- [ ] Color coding for remote tabs

### Phase 3: Navigation (Priority: Medium)
- [ ] Host-aware buffer filtering
- [ ] Cross-host navigation commands
- [ ] Remote buffer grouping
- [ ] Keybinding integration

### Phase 4: Error Handling (Priority: High)
- [ ] Connection failure detection
- [ ] Automatic reconnection
- [ ] User notifications
- [ ] Graceful degradation

### Phase 5: Layout Enhancement (Priority: Low)
- [ ] Extended save format
- [ ] Connection restoration
- [ ] Offline mode support
- [ ] Migration handling