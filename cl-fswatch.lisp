(in-package :fsw)

;;; condition

(define-condition fsw-status-error (error)
  ((code :initarg :code :reader code))
  (:report (lambda (condition stream)
             (format stream "fswatch C function returned error ~S(~A)"
                     (cffi:foreign-enum-keyword '%fsw-error-codes
                                                (code condition))
                     (code condition))))
  (:documentation "Signalled when a fswatch function return a status code other than FSW-OK"))

(defmacro with-fsw-error-code-handler (form)
  "Handle foreign call to libfswatch."
  `(let ((handle ,form))
     (if (zerop handle)
         t
         (error 'fsw-status-error :code handle))))

;;; fswatch-session

(defclass fswatch-session ()
  ((handle :initarg :handle :reader handle :type integer
           :initform (error "Must pass an integer as value of slot :hanle."))
   (path-list :initarg :path-list :accessor path-list)
   (property-list :initarg :property-list :accessor property-list)
   (allow-overflow-p :initarg :allow-overflow-p :accessor allow-overflow-p)
   (callback :initarg :callback :accessor callback)
   (latency :initarg :latency :accessor latency)
   (recursive-p :initarg :recursive-p :accessor recursive-p)
   (directory-only-p :initarg :directory-only-p :accessor directory-only-p)
   (follow-symlinks-p :initarg :follow-symlinks-p :accessor follow-symlinks-p)
   (event-type-filter-list :initarg :event-type-filter-list :accessor event-type-filter-list)
   (filter-list :initarg :filter-list :accessor filter-list)
   (thread :initarg :thread :accessor thread))
  (:default-initargs
   :path-list nil
   :property-list nil
   :allow-overflow-p nil
   :callback nil
   :latency nil
   :recursive-p t
   :directory-only-p nil
   :follow-symlinks-p nil
   :event-type-filter-list nil
   :filter-list nil
   :thread nil))

(defmethod initialize-instance :after ((session fswatch-session) &key)
  (set-latency session 0.1)
  (setf (latency session) 0.1))

(defmethod print-object ((session fswatch-session) stream)
  (with-slots (handle path-list property-list allow-overflow-p callback latency recursive-p directory-only-p follow-symlinks-p event-type-filter-list filter-list thread)
      session
    (print-unreadable-object (session stream :type t :identity t)
      (format stream ":handle ~S ~%  :path-list '(~{~S~^, ~}) ~%  :property-list '(~{~S~^, ~}) ~%  :allow-overflow-p ~S ~%  :callback ~S ~%  :latency ~S :recursive-p ~S :directory-only-p ~S :follow-symlinks-p ~S ~%  :event-type-filter-list '(~{~S~^, ~}) ~%  :filter-list '(~{~S~^, ~}) ~%  :thread ~S"
              handle
              path-list
              property-list
              allow-overflow-p
              callback
              latency
              recursive-p
              directory-only-p
              follow-symlinks-p
              event-type-filter-list
              filter-list
              thread))))

(defmethod add-path ((session fswatch-session) pathspec)
  (setf pathspec (namestring (pathname pathspec)))
  (with-fsw-error-code-handler
      (%fsw-add-path (handle session) pathspec))
  (push pathspec (path-list session)))

(defmethod add-property ((session fswatch-session) name value)
  (with-fsw-error-code-handler
      (%fsw-add-property (handle session) name value))
  (push (cons name value) (property-list session)))

(defmethod set-allow-overflow ((session fswatch-session) allow-overflow-p)
  (with-fsw-error-code-handler
      (%fsw-set-allow-overflow (handle session)
                               (cffi:convert-to-foreign allow-overflow-p :boolean)))
  (setf (allow-overflow-p session) allow-overflow-p))

(defmacro fsw-set-callback (handle callback)
  "Macro to generate callback function pointer and set to fsw."
  (let ((callback-name (gensym "fsw-cevent-callback")))
    `(progn
       ;; /**
       ;;  * A function pointer of type FSW_CEVENT_CALLBACK is used by the API as a
       ;;  * callback to provide information about received events.  The callback is
       ;;  * passed the following arguments:
       ;;  *   - events, a const pointer to an array of events of type const fsw_cevent.
       ;;  *   - event_num, the size of the *events array.
       ;;  *   - data, optional persisted data for a callback.
       ;;  *
       ;;  * The memory used by the fsw_cevent objects will be freed at the end of the
       ;;  * callback invocation.  A callback should copy such data instead of storing
       ;;  * a pointer to it.
       ;;  */
       ;; typedef void (*FSW_CEVENT_CALLBACK) (fsw_cevent const *const events,
       ;;                                      const unsigned int event_num,
       ;;                                      void *data);
       (cffi:defcallback ,callback-name :void ((events :pointer)
                                             (event_num :unsigned-int)
                                             (data :pointer))
       (declare (ignore data))
       (funcall ,callback
                (loop for i below event_num
                   collect 
                     (cffi:with-foreign-slots ((path evt_time flags flags_num)
                                               (cffi:mem-aptr events '(:struct %fsw-cevent) i)
                                               (:struct %fsw-cevent))
                       (list path
                             (unix-time->universal-time evt_time)
                             (loop for j below flags_num
                                collect (cffi:foreign-enum-keyword '%fsw-event-flag
                                                                   (cffi:mem-aref flags
                                                                                  :int
                                                                                  j))))))))
       ;; TODO data, optional persisted data for a callback.
       (cffi:with-foreign-pointer (data 32)
         (with-fsw-error-code-handler
             (%fsw-set-callback ,handle
                                (cffi:callback ,callback-name)
                                data))))))

(defmethod set-callback ((session fswatch-session) callback)
  (fsw-set-callback (handle session) callback)
  (setf (callback session)
        callback))

(defmethod set-latency ((session fswatch-session) latency)
  "Set fswatch event delay time."
  (with-fsw-error-code-handler
      (%fsw-set-latency (handle session)
                        (cffi:convert-to-foreign (coerce latency 'double-float)
                                                 :double)))
  (setf (latency session) latency))

(defmethod set-recursive ((session fswatch-session) recursive-p)
  (with-fsw-error-code-handler
      (%fsw-set-recursive (handle session)
                          (cffi:convert-to-foreign recursive-p :boolean)))
  (setf (recursive-p session) recursive-p))

(defmethod set-directory-only ((session fswatch-session) directory-only-p)
  (with-fsw-error-code-handler
      (%fsw-set-directory-only (handle session)
                               (cffi:convert-to-foreign directory-only-p :boolean)))
  (setf (directory-only-p session) directory-only-p))

(defmethod set-follow-symlinks ((session fswatch-session) follow-symlinks-p)
  (with-fsw-error-code-handler
      (%fsw-set-follow-symlinks (handle session)
                                (cffi:convert-to-foreign follow-symlinks-p :boolean)))
  (setf (follow-symlinks-p session) follow-symlinks-p))

(defmethod add-event-type-filter ((session fswatch-session) event-type)
  (assert (member event-type '(:Created :Updated :Removed :Rename :OwnerModified
                               :AttributeModified :MovedFrom :MoveTo
                               :IsFile :IsDir :IsSymLink
                               :Link :Overflow)))
  (cffi:with-foreign-object (pointer '(:struct %fsw-event-type-filter))
    (setf (cffi:foreign-slot-value pointer '(:struct %fsw-event-type-filter) 'flag)
          (cffi:foreign-enum-value '%fsw-event-flag event-type))
    (with-fsw-error-code-handler
        (%fsw-add-event-type-filter (handle session) pointer))))

(defmethod add-filter ((session fswatch-session) text type case-sensitive-p extended-p)
  (check-type text string)
  (assert (member type '(:filter_include :filter_exclude)))
  (cffi:with-foreign-object (pointer '(:struct %fsw-cmonitor-filter))
    (setf (cffi:foreign-slot-value pointer '(:struct %fsw-cmonitor-filter) 'text)
          text)
    (setf (cffi:foreign-slot-value pointer '(:struct %fsw-cmonitor-filter) 'type)
          (cffi:foreign-enum-value '%fsw-filter-type type))
    (setf (cffi:foreign-slot-value pointer '(:struct %fsw-cmonitor-filter) 'case_sensitive)
          (cffi:convert-to-foreign case-sensitive-p :boolean))
    (setf (cffi:foreign-slot-value pointer '(:struct %fsw-cmonitor-filter) 'extended)
          (cffi:convert-to-foreign extended-p))
    (with-fsw-error-code-handler
        (%fsw-add-filter (handle session) pointer))))

(defmethod start-monitor ((session fswatch-session))
  (unless (and (path-list session)
               (callback session))
    (error "A path to watch and a callback function are required."))
  (let ((thread
         (bt:make-thread #'(lambda ()
                             (with-fsw-error-code-handler
                                 (%fsw-start-monitor (handle session))))
                         :name (format nil "fswatch thread(handle: ~S)"
                                       (handle session)))))
    (setf (thread session) thread)))

(defun destroy (session)
  (check-type session fswatch-session)
  (let ((thread (thread session)))
    (when thread
      (bt:destroy-thread thread)
      
      (bt:make-thread #'(lambda ()
                          (with-fsw-error-code-handler
                              (%fsw-destroy-session (handle session)))))
      (setf (thread session) nil)
      (remove-session session)
      (setf session nil)
      t)))



;;; fswatch

(defvar *session-list* nil)

(defun init-library ()
  (with-fsw-error-code-handler
      (%fsw-init-library)))

;; initial library automatically
(init-library)

(define-condition fsw-init-session-error (error)
  ((code :initarg :code :reader code))
  (:report (lambda (condition stream)
             (format stream "fswatch C function fsw_init_session returned error ~:[UNKNOWN~;:FSW-INVALID-HANDLE~](~A)"
                     (equal %fsw-invalid-handle (code condition))
                     (code condition)))))

(defun init-session (&optional (monitor-type
                                (cffi:foreign-enum-value '%fsw-monitor-type
                                                         :system-default-monitor-type)))
  (let* ((handle (%fsw-init-session monitor-type)))
    (when (< handle 0)
      (error 'fsw-init-session-error :code handle))
    (let ((session (make-instance 'fswatch-session :handle handle)))
      (push session *session-list*)
      session)))

(defun all-sessions ()
  "List all sessions."
  *session-list*)

(defun get-session (handle)
  "Get fswatch session with handle(integer)."
  (loop for session in *session-list*
     do (when (eq handle (handle session))
          (return-from get-session session))))

(defun remove-session (session)
  (setf *session-list*
        (remove-if #'(lambda (item)
                       (equal (handle session)
                              (handle item)))
                   *session-list*)))

(defun last-error ()
  (with-fsw-error-code-handler
      (%fsw-last-error)))

(defun verbose-p ()
  (cffi:convert-from-foreign (%fsw-is-verbose) :boolean))

(defun set-verbose (verbose-p)
  (%fsw-set-verbose (cffi:convert-to-foreign verbose-p :boolean)))

