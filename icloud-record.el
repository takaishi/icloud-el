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

(provide 'icloud-record)
