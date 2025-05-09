#!/bin/bash
# Simple test script to verify core module loading

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"

EMACS_CMD="emacs -Q --batch"

# Add modules path
$EMACS_CMD \
  --eval "(add-to-list 'load-path \"$THIS_DIR\")" \
  --eval "(add-to-list 'load-path \"$THIS_DIR/etm-core\")" \
  --eval "(message \"Load paths: %s\" load-path)" \
  --eval "(condition-case err (progn (require 'etm-core-variables) (message \"etm-core-variables loaded successfully\")) (error (message \"Error loading etm-core-variables: %S\" err)))" \
  --eval "(condition-case err (progn (require 'etm-core-helpers) (message \"etm-core-helpers loaded successfully\")) (error (message \"Error loading etm-core-helpers: %S\" err)))" \
  --eval "(condition-case err (progn (require 'etm-core) (message \"etm-core loaded successfully\")) (error (message \"Error loading etm-core: %S\" err)))"