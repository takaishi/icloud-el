(defclass icloud:reminder (icloud:record)
  ((title :initarg :title :initform "")
   (description :initarg :description :initform nil)
   (p-guid :initarg :p-guid :initform nil)
   (etag :initarg :etag :initform nil)
   (order :initarg :order)
   (guid :initarg :guid)
   (completed-date :initarg :completed-date)
   (start-date-is-all-day :initarg :start-date-is-all-day)
   (alarms :initarg :alarms)
   (created-date :initarg :created-date)
   (start-date-tz :initarg :start-date-tz)
   (due-date-is-all-day :initarg :due-date-is-all-day)
   (priority :initarg :priority)
   (due-date :initarg :due-date)
   (created-date-extended :initarg :created-date-extended)
   (last-modified-date :initarg :last-modified-date)
   (recurrence :initarg :recurrence)
   (start-date :initarg :start-date)))

(defmethod print-to-minibuffer ((r icloud:reminder))
  (concat (format "タイトル: %s\n" (oref r title))
          (format "メモ: %s\n" (oref r description))
          (format "作成日: %s\n" (oref r created-date))
          (format "期限: %s\n" (oref r due-date))
          ))

(provide 'icloud-reminder)
