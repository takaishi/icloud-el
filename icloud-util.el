(defun icloud:alist-to-plist (alist)
  (let ((plist nil))
    (loop for a in (reverse alist)
          do (setq plist (cons (cdr a) plist))
          (setq plist (cons (intern (concat ":" (icloud:to-chain (symbol-name (car a))))) plist)))
    plist))

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
                   (icloud:alist-to-plist c)))
          (assoc-default key response)))

(provide 'icloud-util)
