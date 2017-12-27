# cl-fswatch

`cl-fswatch` is CFFI wrap of [fswatch](https://github.com/emcrisostomo/fswatch), a cross-platform file change monitor.

## License

Copyright (c) 2016 Muyinliu Xing Released under the ISC License.

## Dependencies

All available vis Quicklisp:

* cffi
* bordeaux-threads


## build DLL
### Mac OS/Linux

```shell
wget --no-check-certificate https://github.com/emcrisostomo/fswatch/releases/download/1.9.2/fswatch-1.9.2.tar.gz
tar xzf fswatch-1.9.2.tar.gz
cd fswatch-1.9.2
./configure
make && make install
```

Note: current stable release is **fswatch v1.9.2**

### Windows

TODO Don't know how to build DLL for fswatch on Windows yet. Welcome to reply.

## Installation

In shell:
```shell
git clone https://github.com/muyinliu/cl-fswatch.git
cp -r cl-taobao ~/quicklisp/local-projects/
```

In Common Lisp:

```lisp
(ql:quickload 'cl-fswatch)
```

Note: package `cl-fswatch` has nicknames: `fswatch` `fsw`

## Usage

```lisp
(defvar *session* (fsw:init-session))

(fsw:add-path *session* "/dir/to/watch")
(fsw:add-path *session* "/file/to/watch")

(defun fsw-event-callback (event-list)
  "flags: pointer, array of fsw-event-flag."
  (dolist (event event-list)
    (destructuring-bind (path time flag-list)
        event
      (format t "path: ~S, evt_time: ~S, flag-list: (~{~S~^, ~})~%"
              path
              (universal-time->iso-time-string time)
              flag-list))))

(fsw:set-callback *session* #'fsw-event-callback)

(fsw:start-monitor *session*) ;; will create a thread here
```

## Known issue

### Unable to load foreign library

```lisp
(cffi:use-foreign-library %libfswatch)
```
=>
```=>
debugger invoked on a CFFI:LOAD-FOREIGN-LIBRARY-ERROR in thread
#<THREAD "main thread" RUNNING {1002A9C6E3}>:
  Unable to load foreign library (%LIBFSWATCH).
  Error opening shared object "libfswatch.so":
  libfswatch.so: cannot open shared object file: No such file or directory.
```

1. Make sure the DLL file(`libfswatch.so` on Linux, `libfswatch.dylib` on Mac OS X, `libfswatch.dll` on Windows) built success
2. Make sure CFFI can find the DLL file(copy/symlink DLL to `/usr/lib/` on Mac/Linux, copy DLL to `C:/Windows/System32` on Windows)

### Can NOT destroy fswatch-session

Looks like C function `fsw_destroy_session` from libfswatch didn't work...


## fswatch C function Compatibility

|           C API           | require |
|---------------------------|---------|
| fsw_init_library          | 1.4.4+  |
| fsw_init_session          | 1.4.4+  |
| fsw_add_path              | 1.4.4+  |
| fsw_add_property          | 1.6.0+  |
| fsw_set_allow_overflow    | 1.6.0+  |
| fsw_set_callback          | 1.4.4+  |
| fsw_set_latency           | 1.4.4+  |
| fsw_set_recursive         | 1.4.4+  |
| fsw_set_directory_only    | 1.7.0+  |
| fsw_set_follow_symlinks   | 1.4.4+  |
| fsw_add_event_type_filter | 1.5.0+  |
| fsw_add_filter            | 1.4.4+  |
| fsw_start_monitor         | 1.4.4+  |
| fsw_destroy_session       | 1.4.4+  |
| fsw_last_error            | 1.4.4+  |
| fsw_is_verbose            | 1.4.4+  |
| fsw_set_verbose           | 1.6.0+  |

Note: minimal required fswatch v1.4.4
Note: `fsw_add_property` requires fswatch v1.6.0+
