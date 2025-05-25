;;; test-debug-indicators.el --- Debug ETM indicators

(add-to-list 'load-path ".")
(add-to-list 'load-path "etm-core") 
(add-to-list 'load-path "etm-remote")

(require 'etm-core-variables)
(require 'etm-remote-connection)
(require 'etm-remote-indicators)

;; Set up mock environment
(setq etm-remote-connections (make-hash-table :test 'equal))
(setq etm-remote-global-connections (make-hash-table :test 'equal))
(puthash "test-tab" (make-hash-table :test 'equal) etm-remote-connections)

;; Mock tab-bar functions
(defun tab-bar--current-tab ()
  `((name . "test-tab")))

(defvar test-tab-name "test-tab")
(defun tab-bar-rename-tab (name)
  (setq test-tab-name name)
  (message "Tab renamed to: %s" name))

;; Create connections
(message "Creating first connection...")
(let ((conn1 (etm-remote-connect "ssh" "user" "example.com")))
  (setf (etm-remote-connection-status conn1) :connected)
  (message "First connection created"))

;; Check connections
(let ((tab-conns (etm-remote--get-tab-connections)))
  (message "Number of connections in tab: %d" (hash-table-count tab-conns))
  (maphash (lambda (host conn)
             (message "  Host: %s, Status: %s" host (etm-remote-connection-status conn)))
           tab-conns))

;; Enhance tab name  
(message "Enhancing tab name...")
(etm-remote-enhance-tab-name)
(message "Tab name after first enhance: %s" test-tab-name)

;; Create second connection
(message "\nCreating second connection...")
(let ((conn2 (etm-remote-connect "ssh" "user" "other.com")))
  (setf (etm-remote-connection-status conn2) :connected)
  (message "Second connection created"))

;; Check connections again
(let ((tab-conns (etm-remote--get-tab-connections)))
  (message "Number of connections in tab: %d" (hash-table-count tab-conns))
  (maphash (lambda (host conn)
             (message "  Host: %s, Status: %s" host (etm-remote-connection-status conn)))
           tab-conns))

;; Enhance tab name again
(message "Enhancing tab name again...")
(etm-remote-enhance-tab-name)
(message "Tab name after second enhance: %s" test-tab-name)

;;; test-debug-indicators.el ends here