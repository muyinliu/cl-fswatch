(in-package :fsw)

(cffi:define-foreign-library %libfswatch
  (t (:default "libfswatch")))

(cffi:use-foreign-library %libfswatch)
