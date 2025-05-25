;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Run all etm-remote tests

(require 'ert)

;; Load all test files
(message "Loading test-etm-remote-connection...")
(load "tests/etm-remote/test-etm-remote-connection.el")
(message "Loading test-etm-remote-indicators...")
(load "tests/etm-remote/test-etm-remote-indicators.el")
(message "Loading test-etm-remote-navigation...")
(load "tests/etm-remote/test-etm-remote-navigation.el")
(message "Loading test-etm-remote-errors...")
(load "tests/etm-remote/test-etm-remote-errors.el")
(message "Loading test-etm-remote-layout...")
(load "tests/etm-remote/test-etm-remote-layout.el")

;; Run all tests
(ert-run-tests-batch-and-exit)