(defpackage #:cl-fswatch
  (:use #:cl)
  (:nicknames #:fswatch #:fsw)
  (:export #:init-session
           #:all-session
           #:get-session
           #:remove-session
           #:last-error
           #:verbose-p
           #:set-verbose
           ;; fswatch-session
           #:add-path
           #:add-property
           #:set-allow-overflow
           #:set-callback
           #:set-latency
           #:set-recursive
           #:set-directory-only
           #:set-follow-symlinks
           #:add-event-type-filter
           #:add-filter
           #:start-monitor
           #:destroy
           ;; utils
           #:unix-time->universal-time
           #:universal-time->iso-time-string
           ;; conditions
           #:fsw-status-error
           #:fsw-invalid-handle-error))
