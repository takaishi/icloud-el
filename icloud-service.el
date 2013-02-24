(defclass icloud:service (icloud:record)
  ((title :initarg :title :accessor title)
   (url :initarg :url :accessor url)
   (status :initarg :status :accessor status)))

(provide 'icloud-service)
