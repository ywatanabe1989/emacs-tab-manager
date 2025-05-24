;;; Test the layout upgrade functionality

(add-to-list 'load-path "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager")
(add-to-list 'load-path "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-core")
(add-to-list 'load-path "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout")

(require 'etm-core-variables)
(require 'etm-layout-default)
(require 'etm-layout-upgrade)

;; Initialize required variables
(setq etm-layout-save-dir "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/saved-layouts/")
(setq etm-localhost-names '("localhost" "127.0.0.1"))
(setq etm-ignored-host "ignore")
(setq etm-layout-default-hosts (make-hash-table :test 'equal))

;; Test on a single file first
(let ((test-file "/home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-layout/saved-layouts/etm-open-bashd.el"))
  (message "Testing on %s" test-file)
  (let ((parsed (etm-layout-parse-file test-file)))
    (message "Parsed result: %S" parsed)))

;; Run dry-run upgrade
(etm-layout-upgrade-all t)