(defclass icloud:collection ()
  ((order :initarg :order :initform nil)
   (email-notification :initarg :email-notification  :initform nil)
   (guid :initarg :guid  :initform nil)
   (created-date :initarg :created-date  :initform nil)
   (collection-share-type :initarg :collection-share-type  :initform nil)
   (participants :initarg :participants  :initform nil)
   (completed-count :initarg :completed-count  :initform nil)
   (enabled :initarg :enabled  :initform nil)
   (created-date-extended :initarg :created-date-extended  :initform nil)
   (title :initarg :title  :initform nil)
   (ctag :initarg :ctag  :initform nil)))

(defmethod to-icloud ((c icloud:collection))
  (json-encode `(:Collections [,(icloud:alist-to-plist
                                 (mapcar (lambda (x)
                                           (cons x
                                                 (eieio-oref c (intern (concat ":" (symbol-name x))))))
                                         (object-slots c)))])))


(provide 'icloud-collection)
