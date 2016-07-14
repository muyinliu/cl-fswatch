(in-package :fsw)

(defun unix-time->universal-time (unix-time)
  "Convert unix-time to universal-time."
  (+ unix-time 2208988800))

(defun universal-time->iso-time-string (universal-time)
  "Format universal-time like yyyy-MM-dd HH:mm:ss(2014-03-30 21:03:42)"
  (multiple-value-bind (s m h d mm y) 
      (decode-universal-time universal-time)
    (format nil 
            "~A-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" 
            y mm d h m s)))
