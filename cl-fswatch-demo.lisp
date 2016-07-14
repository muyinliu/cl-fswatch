(defpackage #:cl-fswatch-demo
  (:use #:cl #:cl-fswatch))

(in-package :cl-fswatch-demo)

(defun fsw-event-callback (event-list)
  "flags: pointer, array of fsw-event-flag."
  (dolist (event event-list)
    (destructuring-bind (path time flag-list)
        event
      (format t "path: ~S, evt_time: ~S, flag-list: (~{~S~^, ~})~%"
              path
              (universal-time->iso-time-string time)
              flag-list))))

(defvar *session* (init-session))
(add-path *session* "/Users/muyinliu/Downloads/will_be_fswatch")
(add-path *session* "/Users/muyinliu/Downloads/will_be_fswatch2")
(set-latency *session* 0.1)
(set-callback *session* #'fsw-event-callback)
(start-monitor *session*)
