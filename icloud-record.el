(defclass icloud:record () ())

(defmethod to-icloud ((it icloud:record))
  (object-class it)
  )


(defmethod get-date ((it icloud:record) date)
  (let ((year (aref date 1))
        (month (aref date 2))
        (day (aref date 3))
        (hour (aref date 4))
        (minute (aref date 5)))
    (encode-time 0 minute hour day month year)))

(defmethod format-time ((it icloud:record) slot &optional format)
  (format-time-string (or format "%Y-%m-%d %R") (eieio-oref it slot)))

(provide 'icloud-record)
