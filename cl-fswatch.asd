(asdf:defsystem :cl-fswatch
  :description "A CFFI wrap of fswatch(https://github.com/emcrisostomo/fswatch), a cross-platform file change monitor."
  :version "0.1.0"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :serial t
  :depends-on (#:cffi #:bordeaux-threads)
  :components ((:static-file "cl-fswatch.asd")
               (:file "packages")
               (:file "utils")
               (:file "preload")
               (:file "cffi")
               (:file "cl-fswatch")))
