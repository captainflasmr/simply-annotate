;;; simple-annotate.el --- Simple annotation system for Emacs

(require 'json)

;;; Customization

(defgroup simple-annotate nil
  "Simple annotation system."
  :group 'text)

(defcustom simple-annotate-file 
  (expand-file-name "simple-annotations.json" user-emacs-directory)
  "File to store annotations."
  :type 'file
  :group 'simple-annotate)

(defcustom simple-annotate-highlight-face
  '(:background "yellow" :foreground "black")
  "Face for highlighted annotated text."
  :type 'plist
  :group 'simple-annotate)

;;; Variables

(defvar-local simple-annotate-overlays nil
  "List of annotation overlays in current buffer.")

;;; Core Functions

(defun simple-annotate-file-key ()
  "Get unique key for current file."
  (or (buffer-file-name) 
      (buffer-name)))

(defun simple-annotate-load-database ()
  "Load annotations from database file."
  (when (file-exists-p simple-annotate-file)
    (with-temp-buffer
      (insert-file-contents simple-annotate-file)
      (ignore-errors (json-read)))))

(defun simple-annotate-save-database (db)
  "Save database to file."
  (with-temp-file simple-annotate-file
    (insert (json-encode db))))

(defun simple-annotate-get-file-annotations (file-key db)
  "Get annotations for specific file from database."
  (cdr (assoc file-key db)))

(defun simple-annotate-update-database (file-key annotations)
  "Update database with annotations for file."
  (let ((db (or (simple-annotate-load-database) '())))
    (setf (alist-get file-key db nil nil #'string=) annotations)
    (simple-annotate-save-database db)))

(defun simple-annotate-create-overlay (start end text)
  "Create annotation overlay."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'simple-annotation text)
    (overlay-put overlay 'face simple-annotate-highlight-face)
    (overlay-put overlay 'help-echo text)
    (overlay-put overlay 'mouse-face 'highlight)
    (push overlay simple-annotate-overlays)
    overlay))

(defun simple-annotate-remove-overlay (overlay)
  "Remove annotation overlay."
  (setq simple-annotate-overlays 
        (delq overlay simple-annotate-overlays))
  (delete-overlay overlay))

(defun simple-annotate-clear-all-overlays ()
  "Remove all annotation overlays from buffer."
  (dolist (overlay simple-annotate-overlays)
    (delete-overlay overlay))
  (setq simple-annotate-overlays nil))

(defun simple-annotate-overlay-at-point (&optional pos)
  "Get annotation overlay at point."
  (let ((pos (or pos (point))))
    (cl-find-if (lambda (ov) 
                  (overlay-get ov 'simple-annotation))
                (overlays-at pos))))

(defun simple-annotate-serialize-annotations ()
  "Convert buffer annotations to serializable format."
  (mapcar (lambda (overlay)
            `((start . ,(overlay-start overlay))
              (end . ,(overlay-end overlay))
              (text . ,(overlay-get overlay 'simple-annotation))))
          simple-annotate-overlays))

(defun simple-annotate-deserialize-annotations (annotations)
  "Restore annotations from serialized format."
  (dolist (ann annotations)
    (let ((start (cdr (assoc 'start ann)))
          (end (cdr (assoc 'end ann)))
          (text (cdr (assoc 'text ann))))
      (when (and start end text 
                 (<= start (point-max))
                 (<= end (point-max)))
        (simple-annotate-create-overlay start end text)))))

;;; Interactive Commands

;;;###autoload
(defun simple-annotate-add ()
  "Add annotation to selected region or current line."
  (interactive)
  (let* ((start (if (use-region-p) 
                    (region-beginning) 
                    (line-beginning-position)))
         (end (if (use-region-p) 
                  (region-end) 
                  (line-end-position)))
         (text (read-string "Annotation: ")))
    (when (not (string-empty-p text))
      (simple-annotate-create-overlay start end text)
      (simple-annotate-save-annotations)
      (when (use-region-p) (deactivate-mark))
      (message "Annotation added"))))

;;;###autoload
(defun simple-annotate-remove ()
  "Remove annotation at point."
  (interactive)
  (let ((overlay (simple-annotate-overlay-at-point)))
    (if overlay
        (progn
          (simple-annotate-remove-overlay overlay)
          (simple-annotate-save-annotations)
          (message "Annotation removed"))
      (message "No annotation at point"))))

;;;###autoload
(defun simple-annotate-edit ()
  "Edit annotation at point."
  (interactive)
  (let ((overlay (simple-annotate-overlay-at-point)))
    (if overlay
        (let ((new-text (read-string "Edit annotation: " 
                                     (overlay-get overlay 'simple-annotation))))
          (if (string-empty-p new-text)
              (simple-annotate-remove)
            (overlay-put overlay 'simple-annotation new-text)
            (overlay-put overlay 'help-echo new-text)
            (simple-annotate-save-annotations)
            (message "Annotation updated")))
      (message "No annotation at point"))))

;;;###autoload
(defun simple-annotate-show ()
  "Show annotation at point in minibuffer."
  (interactive)
  (let ((overlay (simple-annotate-overlay-at-point)))
    (if overlay
        (message "Annotation: %s" (overlay-get overlay 'simple-annotation))
      (message "No annotation at point"))))

;;;###autoload
(defun simple-annotate-list ()
  "List all annotations in current buffer."
  (interactive)
  (if simple-annotate-overlays
      (let ((annotations (mapcar (lambda (ov)
                                   (format "Line %d: %s"
                                           (line-number-at-pos (overlay-start ov))
                                           (overlay-get ov 'simple-annotation)))
                                 (sort simple-annotate-overlays
                                       (lambda (a b) 
                                         (< (overlay-start a) 
                                            (overlay-start b)))))))
        (with-output-to-temp-buffer "*Annotations*"
          (dolist (ann annotations)
            (princ ann)
            (princ "\n"))))
    (message "No annotations in buffer")))

(defun simple-annotate-save-annotations ()
  "Save current buffer's annotations."
  (let ((file-key (simple-annotate-file-key)))
    (when file-key
      (simple-annotate-update-database 
       file-key 
       (simple-annotate-serialize-annotations)))))

(defun simple-annotate-load-annotations ()
  "Load annotations for current buffer."
  (let* ((file-key (simple-annotate-file-key))
         (db (simple-annotate-load-database))
         (annotations (simple-annotate-get-file-annotations file-key db)))
    (when annotations
      (simple-annotate-deserialize-annotations annotations))))

;;; Minor Mode

(defvar simple-annotate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a a") #'simple-annotate-add)
    (define-key map (kbd "C-c a d") #'simple-annotate-remove)
    (define-key map (kbd "C-c a e") #'simple-annotate-edit)
    (define-key map (kbd "C-c a s") #'simple-annotate-show)
    (define-key map (kbd "C-c a l") #'simple-annotate-list)
    map)
  "Keymap for simple-annotate-mode.")

;;;###autoload
(define-minor-mode simple-annotate-mode
  "Simple annotation mode."
  :lighter " Ann"
  :keymap simple-annotate-mode-map
  (if simple-annotate-mode
      (progn
        (simple-annotate-load-annotations)
        (add-hook 'before-save-hook #'simple-annotate-save-annotations nil t)
        (add-hook 'kill-buffer-hook #'simple-annotate-save-annotations nil t))
    (simple-annotate-clear-all-overlays)
    (remove-hook 'before-save-hook #'simple-annotate-save-annotations t)
    (remove-hook 'kill-buffer-hook #'simple-annotate-save-annotations t)))

(provide 'simple-annotate)
;;; simple-annotate.el ends here
