;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-12-29 07:59:21>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-tab-manager/src/etm-layout/saved-layouts/etm-open-figrecipe-demo-movie.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@scitex.ai)


(defun etm-open-figrecipe-demo-movie ()
  "Create tab layout for specific configuration."
  (interactive)
  (--etm-layout-create-from-positions "figrecipe-demo-movie"
                                      '((file "~/proj/figrecipe/feature-demo-movie/" 0 1 68 70 "localhost")
                                        (shell "~/proj/figrecipe/feature-demo-movie/" 68 1 68 70
                                               "localhost")
                                        (shell "~/proj/figrecipe/feature-demo-movie/" 136 1 69 70
                                               "localhost")
                                        (shell "~/proj/figrecipe/feature-demo-movie/" 205 1 69 70
                                               "localhost"))
                                      "localhost"))

(defalias 'figrecipe-demo-movie 'etm-open-figrecipe-demo-movie)


(provide 'etm-open-figrecipe-demo-movie)

(when
    (not load-file-name)
  (message "etm-open-figrecipe-demo-movie.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))