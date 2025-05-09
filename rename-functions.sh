#!/bin/bash
# Script to rename internal function prefixes for consistency

# Function to display usage information
show_usage() {
    echo "Usage: $0 [options]"
    echo ""
    echo "Options:"
    echo "  -h, --help        Show this help message"
    echo "  -n, --dry-run     Show what would be changed without making changes"
    echo "  -v, --verbose     Show detailed information about changes"
    echo ""
    echo "Purpose:"
    echo "  Standardize function naming conventions across the ETM codebase"
    echo "  Changes --my/ prefixes to --etm- for consistency"
}

# Parse command line arguments
dry_run=true
verbose=false

while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help)
            show_usage
            exit 0
            ;;
        -n|--dry-run)
            dry_run=true
            shift
            ;;
        -v|--verbose)
            verbose=true
            shift
            ;;
        --no-dry-run)
            dry_run=false
            shift
            ;;
        *)
            echo "Error: Unknown option: $1"
            show_usage
            exit 1
            ;;
    esac
done

# Set dry run mode message
if $dry_run; then
    echo "DRY RUN MODE: No changes will be made (use --no-dry-run to apply changes)"
else
    echo "LIVE MODE: Changes will be applied"
fi

# Function to rename function prefixes in files
rename_function_prefixes() {
    echo "Renaming function prefixes..."
    
    # Find all .el files and process them
    find . -type f -name "*.el" -not -path "*/\.*" -not -path "*/saved-layouts/*" | while read file_path; do
        # Check if the file contains --my/ prefixes
        if grep -q "defun --my/" "$file_path"; then
            echo "Processing file: $file_path"
            
            # Show the changes that would be made
            if $verbose; then
                echo "  Changes:"
                grep -n "defun --my/" "$file_path" | while read -r line; do
                    old_func=$(echo "$line" | sed -E 's/.*defun (--my\/[a-z-]+).*/\1/')
                    new_func=$(echo "$old_func" | sed 's/--my\//--etm-/')
                    echo "    $old_func -> $new_func"
                done
            fi
            
            # Make the replacements if not in dry run mode
            if ! $dry_run; then
                # Replace function definitions
                sed -i 's/defun --my\//defun --etm-/g' "$file_path"
                
                # Replace function calls
                sed -i 's/--my\//--etm-/g' "$file_path"
            fi
        fi
    done
}

# Execute the functions
rename_function_prefixes

if $dry_run; then
    echo "Dry run completed. No actual changes were made."
else
    echo "Renaming completed successfully."
fi