(defclass icloud:session ()
  ((id :initarg :id
       :initform ""
       :type string)
   (pass :initarg :pass
         :initform ""
         :type string)
   (user :initarg :user
         :initform nil)
   (services :initarg :services
             :initform nil
             :accessor services)
   (cookies :initarg :cookies
            :initform nil)
   (collections :initarg :collections)
   (reminders :initarg :reminders)))

(defmethod request((s icloud:session) url method headers &optional data)
  (let* ((url url)
         (url-request-method method)
         (url-request-extra-headers (cons headers icloud:*default-header*))
         (url-request-data (or data ""))
         (response (url-retrieve-synchronously url))
         (body (icloud:json-from-response response)))
    (store-cookie s response)
    body))


(defmethod store-cookie ((s icloud:session) response)
  (with-current-buffer response
    (goto-char (point-min))
    (while (re-search-forward "^Set-Cookie: \\(.*\\)$" nil t)
      (let ((c (match-string-no-properties 1)))
        (oset s cookies (cons (mapcar (lambda (pair)
                                        (if (string-match "\\([^=]+\\)=\\(.*\\)" pair)
                                            (cons (match-string 1 pair) (match-string 2 pair))))
                                      (split-string c ";"))
                              (oref s cookies)))))))

(defmethod login ((s icloud:session))
  (let* ((response (request s
                            icloud:*login-url*
                            "POST"
                            nil
                            (json-encode `(:apple_id ,(oref s id) :password ,(oref s pass) :extended_login true)))))
    (oset s user (assoc-default 'dsInfo response))
    (oset s services (mapcar (lambda (service)
                               (let ((title (symbol-name (car service)))
                                     (url (assoc-default 'url (cdr service)))
                                     (status (assoc-default 'status (cdr service))))
                                 (make-instance icloud:service :title title :url url :status status)))
                             (assoc-default 'webservices response)))))

(defmethod service-url ((s icloud:session) service path &optional params)
  (concat (assoc-default 'url (assoc-default 'reminders (oref s services)))
          path
          (if params (concat "?" (icloud:generate-query params)))))

(defmethod startup-url ((s icloud:session))
  (service-url s 'reminders "/rd/startup" `(:lang "ja-jp" :usertz "Asia/Tokyo" :dsid ,(assoc-default 'dsid (oref s user)))))

(defmethod generate-cookie-header-line ((s icloud:session))
  `("Cookie" . ,(mapconcat 'identity
                           (mapcar (lambda (cookie)
                                     (concat (car (car cookie)) "=" (cdr (car cookie)) "; "))
                                   (oref s cookies))
                           "")))

(defmethod fetch-reminders ((s icloud:session))
  (let* ((response (request s (startup-url s) "GET" (generate-cookie-header-line s)))
         (collections (icloud:get-collections-from-response response))
         (reminders (icloud:get-reminders-from-response response)))
    (oset s collections collections)
    (oset s reminders reminders)
    (cons collections reminders)))


(defmethod completing-reminders ((s icloud:session))
  (let* ((reminders (or (oref s reminders) (cdr (fetch-reminders s))))
         (sources (mapcar (lambda (r) (eieio-oref r 'title)) reminders))
         (title (completing-read "Task: " sources)))
    (loop for r in reminders
          if (string= (oref r title) title)
          return r
          end)))

(defmethod create-reminder ((s icloud:session) r)
  (request s
           (concat (service-url s 'reminders "/rd/reminders/tasks")
                   (concat "?" "lang=ja-jp&usertz=Asia/Tokyo&dsid=" (assoc-default 'dsid (oref s user))))
           "POST"
           (generate-cookie-header-line s)
           (json-encode `(:Reminders (:title "huga" :pGuid "tasks" :priority 0 :guid ,(org-id-uuid))
                                     :ClientState (:Collections [(:guid "tasks" :ctag ,(oref (car (oref s collections)) ctag))])))))

(provide 'icloud-session)
