(defun icloud:symbol-to-keyword (symbol)
  (intern (concat ":" (icloud:to-chain (symbol-name symbol)))))

(defun icloud:convert-alist-keys-to-chain (alist)
  (mapcar (lambda (pair)
            (cons (icloud:generate-key pair)
                  (icloud:generate-value pair)))
          alist))

(defun icloud:alist-to-plist (alist)
  (let ((plist nil))
    (loop for a in (reverse alist)
          do
          (let ((key (car a))
                (value (cdr a)))
            (setq plist (cons value plist))
            (setq plist (cons key plist))))
    plist))

(defun icloud:generate-key (pair)
  (icloud:symbol-to-keyword (car pair)))

(defun icloud:generate-value (pair)
  (cdr pair))


(defun icloud:date-from-icloud (date)
  (let ((year (aref date 1))
        (month (aref date 2))
        (day (aref date 3))
        (hour (aref date 4))
        (minute (aref date 5)))
    (encode-time 0 minute hour day month year)))

(defun icloud:from-icloud (alist)
  (mapcar (lambda (pair)
            (if (equal (car pair) ':created-date)
                (cons (car pair) (icloud:date-from-icloud (cdr pair)))
              pair))
          alist))

(defun icloud:to-chain (str)
  (let ((case-fold-search nil))
    (mapconcat
     (lambda (c)
       (if (string-match "[A-Z]" c)
           (concat "-" (downcase c))
         c))
     (split-string str "" t) "")))

(defun icloud:json-from-response (response)
  (with-current-buffer response
    (goto-char (point-min))
    (re-search-forward "\n\n" nil t)
    (json-read)))

(defun icloud:generate-query (params)
  (let ((result nil))
    (while params
      (let ((k (symbol-name (car params)))
            (v (cadr params)))
        (setq result (cons (concat (substring k 1 (length k)) "=" v) result))
        (setq params (cddr params))))
    (mapconcat 'identity (reverse result) "&")))


(defun icloud:get-collections-from-response (response)
  (icloud:get-records-from-response response icloud:collection 'Collections))

(defun icloud:get-reminders-from-response (response)
  (icloud:get-records-from-response response icloud:reminder 'Reminders))

(defun icloud:get-records-from-response (response class key)
  (mapcar (lambda (c)
            (apply 'make-instance
                   class
                   (icloud:alist-to-plist
                    (icloud:from-icloud (icloud:convert-alist-keys-to-chain c)))))
          (assoc-default key response)))

(provide 'icloud-util)
