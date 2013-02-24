(require 'json)
(require 'url-cookie)
(require 'eieio)
(require 'cl)

(require 'icloud-util)
(require 'icloud-session)
(require 'icloud-record)
(require 'icloud-collection)
(require 'icloud-reminder)
(require 'icloud-user)

(defvar icloud:*default-header* '(("origin" . "https://www.icloud.com") ("content-type" . "text/plain")))
(defvar icloud:*login-url* "https://setup.icloud.com/setup/ws/1/login")

(setq icloud:*id* nil)
(setq icloud:*password* nil)

(defvar *icloud-buffer* "*icloud*")

(setq icloud:*session* nil)

(defun icloud:reminders (s)
  (let* ((reminders (oref s reminders))
         (buf (get-buffer-create "*icloud:reminder*"))
         (title-length-max (number-to-string (apply 'max (mapcar (lambda (reminder) (length (oref reminder title))) (oref s reminders)))))
         (lines (mapcar (lambda (r)
                          (cons (format "%s" (oref r title)) r))
                        reminders)))
    (with-current-buffer buf
      (delete-region (point-min) (point-max))
      (icloud-mode)
      (loop for reminder in reminders
            do (let ((title (format (concat "%-" title-length-max "s | %s") (oref reminder title) (or (oref reminder description) "")))
                     (description (concat " - description: " (oref reminder description)))
                     (created (concat " - created: " (format-time reminder 'created-date))))
                 (insert title)
                 (add-text-properties (line-beginning-position) (line-end-position) `(reminder ,reminder))
                 (let ((ol (make-overlay (line-beginning-position) (line-end-position)))
                       begin
                       end)
                   (add-text-properties (line-beginning-position) (line-end-position) `(overlay ,ol))
                   (insert "\n")
                   (setq begin (point))
                   (insert description)
                   (insert "\n")
                   (insert created)
                   (insert "\n")
                   (setq end (point))
                   (move-overlay ol begin end)
                   (overlay-put  ol 'invisible t))))
      )
    (pop-to-buffer buf)))

(defun icloud:toggle-invisible ()
  (interactive)
  (let ((current-overlay (get-text-property (point) 'overlay)))
    (if (overlay-get current-overlay 'invisible)
        (overlay-put current-overlay 'invisible nil)
      (overlay-put current-overlay 'invisible t))))

(setq icloud-mode-map
      (let ((map (make-keymap)))
        (define-key map (kbd "q") 'icloud-quit)
        (define-key map (kbd "TAB") 'icloud:toggle-invisible)
        map))

(defun icloud ()
  (interactive)
  (unless (and icloud:*id* icloud:*password*)
    (setq icloud:*id* (read-string "ID: "))
    (setq icloud:*password* (read-string "PASSWORD: ")))
  (unless icloud:*session*
    (setq icloud:*session* (make-instance icloud:session :id icloud:*id* :pass icloud:*password*)))
  (login icloud:*session*)
  (fetch-reminders icloud:*session*)
  (icloud:reminders icloud:*session*)
  (icloud-mode))

(defun icloud-mode ()
  (kill-all-local-variables)
  (use-local-map icloud-mode-map)
  (setq major-mode 'iclud-mode)
  (setq mode-name "iCloud"))

(defun icloud-quit ()
  (interactive)
  (kill-buffer (current-buffer)))




(provide 'icloud)
