#!/bin/bash

# Script to upgrade layout format in etm-open-*.el files
# Converts all layouts to use consistent format:
# - Window specs: (type path x y width height [path-host])
# - Function call: --etm-layout-create-from-positions "name" '(...) host

LAYOUT_DIR="/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/saved-layouts"

# Function to upgrade a single file
upgrade_file() {
    local file="$1"
    local base_name=$(basename "$file" .el)
    local layout_name=${base_name#etm-open-}
    
    echo "Upgrading $file..."
    
    # Create backup
    cp "$file" "$file.bak"
    
    # Read the current content
    content=$(cat "$file")
    
    # Extract window specs and host info
    # This regex matches the window-specs list and optional host
    if [[ $content =~ --etm-layout-create-from-positions[[:space:]]+\"([^\"]+)\"[[:space:]]+\'(\([^)]+\))[[:space:]]*([^)]*)\) ]]; then
        local tab_name="${BASH_REMATCH[1]}"
        local window_specs="${BASH_REMATCH[2]}"
        local host_info="${BASH_REMATCH[3]}"
        
        # Check if window specs have nil as last parameter
        if [[ $window_specs =~ nil\) ]]; then
            # Remove nil parameters from window specs
            window_specs=$(echo "$window_specs" | sed 's/ nil)/)/g')
        fi
        
        # Determine the host
        local host="nil"
        if [[ -n $host_info && $host_info != "" ]]; then
            # Remove quotes if present
            host=$(echo "$host_info" | sed 's/^"//' | sed 's/"$//')
        else
            # Check if window specs contain host information
            if [[ $window_specs =~ \"(sp|[a-zA-Z0-9.-]+)\" ]]; then
                host="${BASH_REMATCH[1]}"
            fi
        fi
        
        # Now rebuild the file with consistent format
        cat > "$file" << EOF
;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <$(date '+%Y-%m-%d %H:%M:%S')>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/etm-layout/saved-layouts/$base_name.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

(defun $base_name ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "$tab_name"
                                      $window_specs
                                      $(if [[ $host == "nil" ]]; then echo "nil"; else echo "\"$host\""; fi)))

(defalias '$layout_name '$base_name)


(provide '$base_name)

(when
    (not load-file-name)
  (message "$base_name.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
EOF
    fi
}

# Process all etm-open-*.el files
for file in "$LAYOUT_DIR"/etm-open-*.el; do
    if [[ -f "$file" ]]; then
        upgrade_file "$file"
    fi
done

echo "Layout format upgrade complete!"
echo "Backup files created with .bak extension"