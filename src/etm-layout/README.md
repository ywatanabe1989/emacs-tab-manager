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
(setq etm-vterm-init-commands '((1 . "cld")))  ; default

;; Default command for vterms not in the list
(setq etm-vterm-init-command-default nil)
```

### Examples

```elisp
;; Only 1st vterm runs cld
(setq etm-vterm-init-commands '((1 . "cld")))

;; 1st: cld, 2nd: htop, 3rd: nothing
(setq etm-vterm-init-commands '((1 . "cld") (2 . "htop") (3 . nil)))

;; All run cld except 2nd
(setq etm-vterm-init-command-default "cld")
(setq etm-vterm-init-commands '((2 . nil)))

;; Disable all init commands
(setq etm-vterm-init-commands nil)
(setq etm-vterm-init-command-default nil)
```

<!-- EOF -->