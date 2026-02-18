<!-- ---
!-- Timestamp: 2026-01-23 23:40:47
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/README.md
!-- --- -->

# ETM Layout

## Vterm Init Commands

Configure commands to run in each vterm after `cd && clear`:

```elisp
;; Alist: (INDEX . COMMAND) where INDEX is 1-based vterm position
(setq etm-vterm-init-commands '((1 . "cc")))  ; default

;; Default command for vterms not in the list
(setq etm-vterm-init-command-default nil)
```

### Examples

```elisp
;; Only 1st vterm runs cc
(setq etm-vterm-init-commands '((1 . "cc")))

;; 1st: cc, 2nd: htop, 3rd: nothing
(setq etm-vterm-init-commands '((1 . "cc") (2 . "htop") (3 . nil)))

;; All run cc except 2nd
(setq etm-vterm-init-command-default "cc")
(setq etm-vterm-init-commands '((2 . nil)))

;; Disable all init commands
(setq etm-vterm-init-commands nil)
(setq etm-vterm-init-command-default nil)
```

<!-- EOF -->