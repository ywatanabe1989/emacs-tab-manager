;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-13 15:29:50>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-tab-manager/tests/test-etm-buffer-kill-or-bury.el

(require 'ert)
(require 'etm-buffer-kill-or-bury)

(ert-deftest test-etm-buffer-kill-or-bury-registered
    ()
  (with-temp-buffer
    (let*
        ((buffer-name
          (buffer-name))
         (etm-registered-buffers
          `(("tab1" .
             (("home" . ,buffer-name))))))
      (etm-buffer-kill-or-bury)
      (should
       (buffer-live-p
        (get-buffer buffer-name))))))

(ert-deftest test-etm-buffer-kill-or-bury-protected
    ()
  (with-temp-buffer
    (let*
        ((buffer-name
          (buffer-name))
         (etm-protected-buffers
          (list buffer-name)))
      (etm-buffer-kill-or-bury)
      (should
       (buffer-live-p
        (get-buffer buffer-name))))))

(ert-deftest test-etm-buffer-kill-or-bury-unregistered
    ()
  (with-temp-buffer
    (let*
        ((buffer-name
          (buffer-name))
         (etm-registered-buffers nil)
         (etm-protected-buffers nil))
      (etm-buffer-kill-or-bury)
      (should-not
       (buffer-live-p
        (get-buffer buffer-name))))))

(provide 'test-etm-buffer-kill-or-bury)

(when
    (not load-file-name)
  (message "test-etm-buffer-kill-or-bury.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))