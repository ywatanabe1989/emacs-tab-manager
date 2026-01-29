;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-keys-command-map.el

(require 'ert)

(ert-deftest test-etm-keys-command-map-loads
    ()
  (require 'etm-keys-command-map)
  (should
   (featurep 'etm-keys-command-map)))

(ert-deftest test-etm-command-map-binding
    ()
  (let
      ((old-binding
        (key-binding
         (kbd "M-t"))))
    (unwind-protect
        (progn
          (global-set-key
           (kbd "M-t")
           'etm-command-map)
          (should
           (eq
            (key-binding
             (kbd "M-t"))
            'etm-command-map)))
      (global-set-key
       (kbd "M-t")
       old-binding))))

(provide 'test-etm-keys-command-map)

(when
    (not load-file-name)
  (message "test-etm-keys-command-map.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

(when (not load-file-name)
  (ert-run-tests-interactively t))

;; --------------------------------------------------------------------------------
;; Start of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-keys/etm-keys-command-map.el
;; --------------------------------------------------------------------------------
;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-02-13 14:44:53>
;; ;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/etm-keys/etm-keys-command-map.el
;; 
;; (define-prefix-command 'etm-command-map)
;; 
;; (global-set-key
;;  (kbd "M-t")
;;  'etm-command-map)
;; 
;; (provide 'etm-keys-command-map)
;; 
;; (when
;;     (not load-file-name)
;;   (message "etm-keys-command-map.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))
;; --------------------------------------------------------------------------------
;; End of Source Code from: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-keys/etm-keys-command-map.el
;; --------------------------------------------------------------------------------

;;; test-etm-keys-command-map.el ends here
