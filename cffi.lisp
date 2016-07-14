;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CFFI wrap of libfswatch

(in-package :fsw)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; libfswatch_types.h

;; typedef unsigned int FSW_HANDLE;
(cffi:defctype %fsw-handle :unsigned-int)

;; typedef int FSW_STATUS;
(cffi:defctype %fsw-status :int)

;; #define FSW_INVALID_HANDLE -1
(defconstant %fsw-invalid-handle -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cevent.sh

(cffi:defctype %time_t :long)

;; enum fsw_event_flag
;; {
;;   NoOp = 0,                     /**< No event has occurred. */
;;   PlatformSpecific = (1 << 0),  /**< Platform-specific placeholder for event type that cannot currently be mapped. */
;;   Created = (1 << 1),           /**< An object was created. */
;;   Updated = (1 << 2),           /**< An object was updated. */
;;   Removed = (1 << 3),           /**< An object was removed. */
;;   Renamed = (1 << 4),           /**< An object was renamed. */
;;   OwnerModified = (1 << 5),     /**< The owner of an object was modified. */
;;   AttributeModified = (1 << 6), /**< The attributes of an object were modified. */
;;   MovedFrom = (1 << 7),         /**< An object was moved from this location. */
;;   MovedTo = (1 << 8),           /**< An object was moved to this location. */
;;   IsFile = (1 << 9),            /**< The object is a file. */
;;   IsDir = (1 << 10),            /**< The object is a directory. */
;;   IsSymLink = (1 << 11),        /**< The object is a symbolic link. */
;;   Link = (1 << 12),             /**< The link count of an object has changed. */
;;   Overflow = (1 << 13)          /**< The event queue has overflowed. */
;; };
(cffi:defcenum %fsw-event-flag
  (:NoOp              0)
  (:PlatformSpecific  1)
  (:Created           2)
  (:Updated           4)
  (:Removed           8)
  (:Renamed           16)
  (:OwnerModified     32)
  (:AttributeModified 64)
  (:MovedFrom         128)
  (:MoveTo            256)
  (:IsFile            512)
  (:IsDir             1024)
  (:IsSymLink         2048)
  (:Link              4096)
  (:Overflow          8192))

;; FSW_STATUS fsw_get_event_flag_by_name(const char *name, fsw_event_flag *flag);
(cffi:defcfun ("fsw_get_event_flag_by_name" %fsw-get-event-flag-by-name)
    %fsw-status
  (name :string)
  (flag :pointer)) ;; %fsw-event-flag

;; char *fsw_get_event_flag_name(const fsw_event_flag flag);
(cffi:defcfun ("fsw_get_event_flag_name" %fsw-get-event-flag-name)
    :string
  (flag %fsw-event-flag))

;; typedef struct fsw_cevent
;; {
;;   char * path;
;;   time_t evt_time;
;;   fsw_event_flag * flags;
;;   unsigned int flags_num;
;; } fsw_cevent;
(cffi:defcstruct %fsw-cevent
  "FSW C event structure."
  (path :string)
  (evt_time %time_t)
  (flags :pointer)
  (flags_num :unsigned-int))

;; typedef void (*FSW_CEVENT_CALLBACK)(fsw_cevent const *const events,
;;                                     const unsigned int event_num,
;;                                     void *data);
;; Note: see macro fsw-set-callback in file cl-fswatch.lisp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cmonitor.h

;; enum fsw_monitor_type
;; {
;;   system_default_monitor_type = 0, /**< System default monitor. */
;;   fsevents_monitor_type,           /**< OS X FSEvents monitor. */
;;   kqueue_monitor_type,             /**< BSD `kqueue` monitor. */
;;   inotify_monitor_type,            /**< Linux `inotify` monitor. */
;;   windows_monitor_type,            /**< Windows monitor. */
;;   poll_monitor_type,               /**< `stat()`-based poll monitor. */
;;   fen_monitor_type                 /**< Solaris/Illumos monitor. */
;; };
(cffi:defcenum %fsw-monitor-type
  (:system-default-monitor-type 0)
  :fsevents-monitor-type
  :kqueue-monitor-type
  :inotify-monitor-type
  :windows-monitor-type
  :poll-monitor-type
  :fen-monitor-type)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cfilter.h

;; enum fsw_filter_type
;; {
;;   filter_include,
;;   filter_exclude
;; };
(cffi:defcenum %fsw-filter-type
  :filter_include
  :filter_exclude)

;; typedef struct fsw_cmonitor_filter
;; {
;;   char * text;
;;   fsw_filter_type type;
;;   bool case_sensitive;
;;   bool extended;
;; } fsw_cmonitor_filter;
(cffi:defcstruct %fsw-cmonitor-filter
  "FSW C monitor filter."
  (text :string)
  (type %fsw-filter-type)
  (case_sensitive :boolean)
  (extended :boolean))

;; typedef struct fsw_event_type_filter
;; {
;;   fsw_event_flag flag;
;; } fsw_event_type_filter;
(cffi:defcstruct %fsw-event-type-filter
  (flag %fsw-event-flag))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; error.h

;; Error codes
(cffi:defcenum %fsw-error-codes
  (:fsw-ok                            0    ) ;; The call was successful.
  (:fsw-err-unknown-error             1    ) ;; An unknown error has occurred.
  (:fsw-err-session-unknown           2    ) ;; The session specified by the handle is unknown.
  (:fsw-err-monitor-already-exists    4    ) ;; The session already contains a monitor.
  (:fsw-err-memory                    8    ) ;; An error occurred while invoking a memory management routine.
  (:fsw-err-unknown-monitor-type      16   ) ;; "The specified monitor type does not exist.
  (:fsw-err-callback-not-set          32   ) ;; The callback has not been set.
  (:fsw-err-paths-not-set             64   ) ;; The paths to watch have not been set.
  (:fsw-err-missing-context           128  ) ;; The callback context has not been set.
  (:fsw-err-invalid-path              256  ) ;; The path is invalid.
  (:fsw-err-invalid-callback          512  ) ;; The callback is invalid.
  (:fsw-err-invalid-latency           1024 ) ;; The latency is invalid.
  (:fsw-err-invalid-regex             2048 ) ;; The regular expression is invalid.
  (:fsw-err-monitor-already-running   4096 ) ;; A monitor is already running in the specified session.
  (:fsw-err-unknown-value             8192 ) ;; The value is unknown.
  (:fsw-err-invalid-property          16384));; The property is invalid.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; libfswatch.h

;; FSW_STATUS fsw_init_library();
(cffi:defcfun ("fsw_init_library" %fsw-init-library)
    %fsw-status)

;; FSW_HANDLE fsw_init_session(const fsw_monitor_type type = system_default_monitor_type);
(cffi:defcfun ("fsw_init_session" %fsw-init-session)
    %fsw-handle
  (fsw-monitor-type %fsw-monitor-type))

;; FSW_STATUS fsw_add_path(const FSW_HANDLE handle, const char * path);
(cffi:defcfun ("fsw_add_path" %fsw-add-path)
    %fsw-status
  (handle %fsw-handle)
  (path :string))

;; FSW_STATUS fsw_add_property(const FSW_HANDLE handle, const char * name, const char * value);
;; Note: require fswatch 1.6.0+
(cffi:defcfun ("fsw_add_property" %fsw-add-property)
    %fsw-status
  (handle %fsw-handle)
  (name :string)
  (value :string))

;; FSW_STATUS fsw_set_allow_overflow(const FSW_HANDLE handle, const bool allow_overflow);
;; Note: require fswatch 1.6.0+
(cffi:defcfun ("fsw_set_allow_overflow" %fsw-set-allow-overflow)
    %fsw-status
  (handle %fsw-handle)
  (allow_overflow :boolean))

;; FSW_STATUS fsw_set_callback(const FSW_HANDLE handle,
;;                             const FSW_CEVENT_CALLBACK callback,
;;                             void * data);
(cffi:defcfun ("fsw_set_callback" %fsw-set-callback)
    %fsw-status
  (handle %fsw-handle)
  (callback :pointer) ;; %fsw-cevent-callback (function pointer)
  (data :pointer))

;; FSW_STATUS fsw_set_latency(const FSW_HANDLE handle, const double latency);
;; Note: require fswatch 1.4.4+
(cffi:defcfun ("fsw_set_latency" %fsw-set-latency)
    %fsw-status
  (handle %fsw-handle)
  (latency :double))

;; FSW_STATUS fsw_set_recursive(const FSW_HANDLE handle, const bool recursive);
;; Note: require fswatch 1.4.4+
(cffi:defcfun ("fsw_set_recursive" %fsw-set-recursive)
    %fsw-status
  (handle %fsw-handle)
  (recursive :boolean))

;; FSW_STATUS fsw_set_directory_only(const FSW_HANDLE handle, const bool directory_only);
;; Note: require fswatch 1.7.0+
(cffi:defcfun ("fsw_set_directory_only" %fsw-set-directory-only)
    %fsw-status
  (handle %fsw-handle)
  (directory_only :boolean))

;; FSW_STATUS fsw_set_follow_symlinks(const FSW_HANDLE handle,
;;                                    const bool follow_symlinks);
;; Note: require fswatch 1.4.4+
(cffi:defcfun ("fsw_set_follow_symlinks" %fsw-set-follow-symlinks)
    %fsw-status
  (handle %fsw-handle)
  (follow_symlinks :boolean))

;; FSW_STATUS fsw_add_event_type_filter(const FSW_HANDLE handle,
;;                                      const fsw_event_type_filter event_type);
;; Note: require fswatch 1.5.0+
(cffi:defcfun ("fsw_add_event_type_filter" %fsw-add-event-type-filter)
    %fsw-status
  (handle %fsw-handle)
  (event_type :pointer)) ;; %fsw-event-type-filter

;; FSW_STATUS fsw_add_filter(const FSW_HANDLE handle,
;;                           const fsw_cmonitor_filter filter);
;; Note: require fswatch 1.4.4+
(cffi:defcfun ("fsw_add_filter" %fsw-add-filter)
    %fsw-status
  (handle %fsw-handle)
  (filter :pointer)) ;; %fsw-cmonitor-filter

;; FSW_STATUS fsw_start_monitor(const FSW_HANDLE handle);
;; Note: require fswatch 1.4.4+
(cffi:defcfun ("fsw_start_monitor" %fsw-start-monitor)
    %fsw-status
  (handle %fsw-handle))

;; FSW_STATUS fsw_destroy_session(const FSW_HANDLE handle);
;; Note: require fswatch 1.4.4+
(cffi:defcfun ("fsw_destroy_session" %fsw-destroy-session)
    %fsw-status
  (handle %fsw-handle))

;; FSW_STATUS fsw_last_error();
;; Note: require fswatch 1.4.4+
(cffi:defcfun ("fsw_last_error" %fsw-last-error)
    %fsw-status)

;; bool fsw_is_verbose();
;; Note: require fswatch 1.4.4+
(cffi:defcfun ("fsw_is_verbose" %fsw-is-verbose)
    :boolean)


;; void fsw_set_verbose(bool verbose);
;; Note: require fswatch 1.6.0+
(cffi:defcfun ("fsw_set_verbose" %fsw-set-verbose)
    :void
  (verbose :boolean))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; libfswatch_log.h
;;
;; Note: ignore libfswatch_log.h


;;;; End of CFFI wrap of libfswatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

