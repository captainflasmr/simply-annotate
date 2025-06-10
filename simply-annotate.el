;;; simply-annotate.el --- Simple annotation system for Emacs -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: applications, tools, convenience
;; URL: https://github.com/captainflasmr/simply-annotate
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; A lightweight annotation system for Emacs that allows
;; you to add persistent notes to any text file without modifying the
;; original content.
;;
;; Quick Start:
;;
;; (use-package simply-annotate
;;  :bind ("C-c A" . simply-annotate-mode))
;;
;; Usage:
;;
;; * Creating Annotations
;;
;; ** Single-line Annotations
;;
;; 1. Select text or place cursor on a line
;; 2. Press =C-c a a=
;; 3. Type your annotation in the minibuffer
;; 4. Press =Enter=
;;
;; ** Multi-line Annotations
;; 
;; 1. Select text or place cursor on a line
;; 2. Press =C-c a m=
;; 3. A dedicated buffer opens for editing
;; 4. Type your multi-line annotation
;; 5. Press =C-c C-c= to save or =C-c C-k= to cancel
;; 
;; * Viewing Annotations
;; 
;; When =simply-annotate-mode= is active:
;; 
;; - Annotated text is highlighted with a colored background
;; - The header line shows annotation count and available commands
;; - Moving your cursor to annotated text automatically displays the annotation content
;; - A dedicated annotation buffer can show detailed information
;; 
;; * Managing Annotations
;; 
;; ** Editing
;; 
;; - Place cursor on annotated text
;; - Press =C-c a e= to edit the annotation
;; - For multi-line annotations, a dedicated editing buffer opens
;; 
;; ** Deleting
;; 
;; - Place cursor on annotated text
;; - Press =C-c a d= to remove the annotation
;; 
;; ** Listing All Annotations
;; 
;; - Press =C-c a l= to open an org-mode buffer showing all annotations
;; - Click on line numbers to jump directly to annotations
;; - Perfect for getting an overview of all your notes
;;
;;; Code:

;;; Customization

(require 'cl-lib)

(defgroup simply-annotate nil
  "Simple annotation system."
  :group 'text)

(defcustom simply-annotate-file
  (expand-file-name "simply-annotations.el" user-emacs-directory)
  "File to store annotations."
  :type 'file
  :group 'simply-annotate)

(defcustom simply-annotate-highlight-face 'highlight
  "Face for highlighted annotated text."
  :type 'face
  :group 'simply-annotate)

(defcustom simply-annotate-buffer-name "*Annotation*"
  "Name of the buffer to display annotations."
  :type 'string
  :group 'simply-annotate)

(defcustom simply-annotate-auto-show-buffer t
  "Automatically show annotation buffer when point moves to an annotation."
  :type 'boolean
  :group 'simply-annotate)

(defcustom simply-annotate-buffer-height 0.3
  "Height of annotation buffer as fraction of frame height."
  :type 'float
  :group 'simply-annotate)

;;; Variables

(defvar-local simply-annotate-overlays nil
  "List of annotation overlays in current buffer.")

(defvar-local simply-annotate-original-header-line nil
  "Original `header-line-format' before annotation mode.")

(defvar-local simply-annotate-current-annotation nil
  "Current annotation text being displayed in header.")

(defvar-local simply-annotate-current-overlay nil
  "Current overlay being displayed/edited in annotation buffer.")

(defvar-local simply-annotate-source-buffer nil
  "Source buffer for annotation display.")

(defvar-local simply-annotate-header-end-pos nil
  "Position where the annotation content starts (after header).")

;;; Core Functions
(defun simply-annotate-file-key ()
  "Get unique key for current file."
  (or (buffer-file-name) (buffer-name)))

(defun simply-annotate-load-database ()
  "Load annotations from database file."
  (when (file-exists-p simply-annotate-file)
    (with-temp-buffer
      (insert-file-contents simply-annotate-file)
      (let ((content (string-trim (buffer-string))))
        (unless (string-empty-p content)
          (ignore-errors (car (read-from-string content))))))))

(defun simply-annotate-save-database (db)
  "Save database to file.
Argument DB database."
  (with-temp-file simply-annotate-file
    (insert ";;; Simply Annotate Database\n")
    (insert ";;; This file is auto-generated. Do not edit manually.\n\n")
    (prin1 db (current-buffer))
    (insert "\n")))

(defun simply-annotate-update-database (file-key annotations)
  "Update database with ANNOTATIONS for file.
Argument FILE-KEY ."
  (let ((db (or (simply-annotate-load-database) '())))
    (setf (alist-get (if (symbolp file-key) (symbol-name file-key) file-key)
                     db nil nil #'string=) annotations)
    (simply-annotate-save-database db)))

(defun simply-annotate-create-overlay (start end text)
  "Create annotation overlay.
Argument START .
Argument END .
Argument TEXT ."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'simply-annotation text)
    (overlay-put overlay 'face simply-annotate-highlight-face)
    (overlay-put overlay 'help-echo text)
    (overlay-put overlay 'mouse-face 'highlight)
    (push overlay simply-annotate-overlays)
    overlay))

(defun simply-annotate-remove-overlay (overlay)
  "Remove annotation OVERLAY."
  (setq simply-annotate-overlays (delq overlay simply-annotate-overlays))
  (delete-overlay overlay))

(defun simply-annotate-clear-all-overlays ()
  "Remove all annotation overlays from buffer."
  (mapc #'delete-overlay simply-annotate-overlays)
  (setq simply-annotate-overlays nil))

(defun simply-annotate-overlay-at-point (&optional pos)
  "Get annotation overlay at point.
Optional argument POS ."
  (cl-find-if (lambda (ov) (overlay-get ov 'simply-annotation))
              (overlays-at (or pos (point)))))

(defun simply-annotate-serialize-annotations ()
  "Convert buffer annotations to serializable format."
  (mapcar (lambda (overlay)
            `((start . ,(overlay-start overlay))
              (end . ,(overlay-end overlay))
              (text . ,(overlay-get overlay 'simply-annotation))))
          simply-annotate-overlays))

(defun simply-annotate-deserialize-annotations (annotations)
  "Restore ANNOTATIONS from serialized format."
  (dolist (ann annotations)
    (let ((start (alist-get 'start ann))
          (end (alist-get 'end ann))
          (text (alist-get 'text ann)))
      (when (and start end text
                 (<= start (point-max))
                 (<= end (point-max))
                 (> end start))
        (simply-annotate-create-overlay start end text)))))

(defun simply-annotate-save-annotations ()
  "Save current buffer's annotations."
  (let ((file-key (simply-annotate-file-key)))
    (when file-key
      (simply-annotate-update-database
       file-key
       (simply-annotate-serialize-annotations)))))

(defun simply-annotate-load-annotations ()
  "Load annotations for current buffer."
  (let* ((file-key (simply-annotate-file-key))
         (db (simply-annotate-load-database))
         (annotations (when db (alist-get file-key db nil nil #'string=))))
    (when annotations
      (simply-annotate-deserialize-annotations annotations))))

;;; Annotation Buffer Functions

(defun simply-annotate-get-annotation-buffer ()
  "Get or create the annotation buffer."
  (let ((buffer (get-buffer-create simply-annotate-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'simply-annotate-annotation-mode)
        (simply-annotate-annotation-mode)
        ))
    buffer))

(defun simply-annotate-update-annotation-buffer (annotation-text overlay)
  "Update the annotation buffer with ANNOTATION-TEXT and OVERLAY info."
  (let ((buffer (simply-annotate-get-annotation-buffer))
        (source-buf (current-buffer))  ; Capture the source buffer here
        (line-num (line-number-at-pos (overlay-start overlay)))
        (total (length simply-annotate-overlays)))
    
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize 
                 (format "Annotation %d/%d in %s (Line %d)\n" 
                         (with-current-buffer source-buf
                           (simply-annotate-get-annotation-number overlay))
                         total (buffer-name source-buf) line-num)
                 'face 'bold))
        (insert (propertize 
                 "Press C-x C-q to edit, C-c C-c to save changes\n"
                 'face 'italic))
        (insert (make-string 50 ?-) "\n\n")
        (setq simply-annotate-header-end-pos (point))
        (insert annotation-text)
        (goto-char simply-annotate-header-end-pos)
        (setq simply-annotate-source-buffer source-buf
              simply-annotate-current-overlay overlay)))))

(defun simply-annotate-show-annotation-buffer ()
  "Show the annotation buffer in a window."
  (let ((buffer (simply-annotate-get-annotation-buffer))
        (window (get-buffer-window simply-annotate-buffer-name)))
    
    (unless window
      (let ((height (max 3 (round (* (frame-height) simply-annotate-buffer-height)))))
        (setq window (split-window-below (- height)))
        (set-window-buffer window buffer)
        (set-window-dedicated-p window t)))
    
    (set-window-buffer window buffer)
    window))

(defun simply-annotate-hide-annotation-buffer ()
  "Hide the annotation buffer."
  (let ((window (get-buffer-window simply-annotate-buffer-name)))
    (when window (delete-window window))))

;;; Header-line Functions
(defun simply-annotate-get-annotation-number (target-overlay)
  "Get the position number of TARGET-OVERLAY in the sorted list of annotations."
  (when target-overlay
    (with-current-buffer (overlay-buffer target-overlay)
      (let ((sorted-overlays (seq-sort-by #'overlay-start #'< simply-annotate-overlays))
            (position 1))
        (catch 'found
          (dolist (overlay sorted-overlays)
            (when (eq overlay target-overlay)
              (throw 'found position))
            (cl-incf position))
          1)))))

(defun simply-annotate-format-header ()
  "Format header-line content for annotations."
  (let ((count (length simply-annotate-overlays)))
    (when (> count 0)
      (concat
       (propertize
        (concat (format " %d ANNOTATION" count)
                (if (> count 1) "S " " "))
        'face '(:weight bold))
       (format "C-c a [a]dd [m]ulti [d]elete [e]dit [l]ist [t]oggle [n]ext [p]revious")))))

(defun simply-annotate-update-header ()
  "Update header-line with current annotation info."
  (let ((overlay (simply-annotate-overlay-at-point))
        (new-annotation nil))
    
    (when overlay
      (setq new-annotation (overlay-get overlay 'simply-annotation)))
    
    (unless (equal new-annotation simply-annotate-current-annotation)
      (setq simply-annotate-current-annotation new-annotation)
      
      (when (and simply-annotate-auto-show-buffer overlay)
        (simply-annotate-update-annotation-buffer new-annotation overlay)
        (simply-annotate-show-annotation-buffer))
      
      (when (and (not overlay) (get-buffer-window simply-annotate-buffer-name))
        (simply-annotate-hide-annotation-buffer))
      
      (setq header-line-format (simply-annotate-format-header))
      (force-mode-line-update))))

(defun simply-annotate-setup-header ()
  "Setup header-line for annotation display."
  (setq simply-annotate-original-header-line header-line-format
        header-line-format (simply-annotate-format-header))
  (add-hook 'post-command-hook #'simply-annotate-update-header nil t))

(defun simply-annotate-cleanup-header ()
  "Restore original header-line."
  (setq header-line-format simply-annotate-original-header-line
        simply-annotate-current-annotation nil)
  (remove-hook 'post-command-hook #'simply-annotate-update-header t))

;;; Navigation Functions
(defun simply-annotate-get-sorted-overlays ()
  "Get annotation overlays sorted by position."
  (sort (copy-sequence simply-annotate-overlays)
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun simply-annotate-find-annotation (forward &optional wrap)
  "Find next/previous annotation.  FORWARD t for next, nil for previous.
Optional argument WRAP ."
  (let* ((pos (point))
         (overlays (if forward
                       (simply-annotate-get-sorted-overlays)
                     (reverse (simply-annotate-get-sorted-overlays))))
         (test-fn (if forward
                      (lambda (ov) (> (overlay-start ov) pos))
                    (lambda (ov) (< (overlay-end ov) pos))))
         (found (cl-find-if test-fn overlays)))
    
    (when (and (not found) wrap overlays)
      (setq found (car overlays)))
    found))

;;; Interactive Commands
;;;###autoload
(defun simply-annotate-next ()
  "Navigate to next annotation."
  (interactive)
  (if-let ((next-overlay (simply-annotate-find-annotation t t)))
      (progn
        (goto-char (overlay-start next-overlay))
        (message "Moved to next annotation"))
    (message "No annotations in buffer")))

;;;###autoload
(defun simply-annotate-previous ()
  "Navigate to previous annotation."
  (interactive)
  (if-let ((prev-overlay (simply-annotate-find-annotation nil t)))
      (progn
        (goto-char (overlay-start prev-overlay))
        (message "Moved to previous annotation"))
    (message "No annotations in buffer")))

;;;###autoload
(defun simply-annotate-add ()
  "Add annotation to selected region or current line."
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (line-beginning-position)))
         (end (if (use-region-p) (region-end) (line-end-position)))
         (text (read-string "Annotation: ")))
    (unless (string-empty-p text)
      (simply-annotate-create-overlay start end text)
      (simply-annotate-save-annotations)
      (simply-annotate-update-header)
      (when (use-region-p) (deactivate-mark))
      (message "Annotation added"))))

(defun simply-annotate-read-multiline-text (&optional initial-text)
  "Read multiline text from user.
Optional argument INITIAL-TEXT ."
  (let ((buffer-name "*Annotation Input*")
        (result nil))
    
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert "Enter your annotation below. Press C-c C-c when done, C-c C-k to cancel.\n")
      (insert (make-string 60 ?-) "\n\n")
      (let ((content-start (point)))
        (when initial-text (insert initial-text))
        
        (use-local-map (make-sparse-keymap))
        (local-set-key (kbd "C-c C-c")
                       (lambda ()
                         (interactive)
                         (setq result (buffer-substring content-start (point-max)))
                         (exit-recursive-edit)))
        (local-set-key (kbd "C-c C-k")
                       (lambda ()
                         (interactive)
                         (setq result nil)
                         (exit-recursive-edit)))
        
        (switch-to-buffer-other-window buffer-name)
        (goto-char content-start)
        (message "Enter annotation text. C-c C-c to save, C-c C-k to cancel")
        
        (recursive-edit)
        (kill-buffer buffer-name)))
    result))

;;;###autoload
(defun simply-annotate-add-multiline ()
  "Add multiline annotation to selected region or current line."
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (line-beginning-position)))
         (end (if (use-region-p) (region-end) (line-end-position)))
         (text (simply-annotate-read-multiline-text)))
    
    (when (and text (not (string-empty-p (string-trim text))))
      (simply-annotate-create-overlay start end text)
      (simply-annotate-save-annotations)
      (simply-annotate-update-header)
      (when (use-region-p) (deactivate-mark))
      (message "Multiline annotation added"))))

;;;###autoload
(defun simply-annotate-remove ()
  "Remove annotation at point."
  (interactive)
  (if-let ((overlay (simply-annotate-overlay-at-point)))
      (progn
        (simply-annotate-remove-overlay overlay)
        (simply-annotate-save-annotations)
        (simply-annotate-update-header)
        (message "Annotation removed"))
    (message "No annotation at point")))

;;;###autoload
(defun simply-annotate-edit ()
  "Edit annotation at point."
  (interactive)
  (if-let ((overlay (simply-annotate-overlay-at-point)))
      (let ((current-text (overlay-get overlay 'simply-annotation)))
        (if (string-match-p "\n" current-text)
            (simply-annotate-edit-in-buffer overlay)
          (let ((new-text (read-string "Edit annotation: " current-text)))
            (if (string-empty-p new-text)
                (simply-annotate-remove)
              (overlay-put overlay 'simply-annotation new-text)
              (overlay-put overlay 'help-echo new-text)
              (simply-annotate-save-annotations)
              (simply-annotate-update-header)
              (message "Annotation updated")))))
    (message "No annotation at point")))

(defun simply-annotate-edit-in-buffer (overlay)
  "Edit OVERLAY's annotation in a dedicated buffer."
  (let ((text (simply-annotate-read-multiline-text
               (overlay-get overlay 'simply-annotation))))
    (if (and text (not (string-empty-p (string-trim text))))
        (progn
          (overlay-put overlay 'simply-annotation text)
          (overlay-put overlay 'help-echo text)
          (simply-annotate-save-annotations)
          (simply-annotate-update-header)
          (message "Annotation updated"))
      (when (and text (string-empty-p (string-trim text)))
        (simply-annotate-remove)))))

;;;###autoload
(defun simply-annotate-toggle ()
  "Toggle auto annotation window."
  (interactive)
  (setq simply-annotate-auto-show-buffer (not simply-annotate-auto-show-buffer))
  (message "Auto-show annotation buffer: %s"
           (if simply-annotate-auto-show-buffer "enabled" "disabled")))

;;;###autoload
(defun simply-annotate-list ()
  "List all annotations in current buffer using `org-mode' format."
  (interactive)
  (if simply-annotate-overlays
      (let* ((source-buffer (current-buffer))
             (source-name (buffer-name))
             (buffer-name "*Annotations List*")
             (annotations (simply-annotate-get-sorted-overlays)))
        
        (with-current-buffer (get-buffer-create buffer-name)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (org-mode)
            
            (insert (format "#+TITLE: Annotations for %s\n" source-name) "\n")
            (insert (format "* %s\n" source-name))
            
            (dolist (overlay annotations)
              (let* ((start-pos (overlay-start overlay))
                     (line-num (with-current-buffer source-buffer
                                 (line-number-at-pos start-pos)))
                     (text (overlay-get overlay 'simply-annotation)))
                
                (insert (format "** [[elisp:(simply-annotate-jump-to-annotation %d \"%s\")][Line %d]]\n"
                                start-pos source-name line-num))
                (insert text "\n")))
            
            (goto-char (point-min))
            (when (fboundp 'org-show-all) (org-show-all)))
          (setq buffer-read-only t)
          (display-buffer buffer-name)))
    (message "No annotations in buffer")))

(defun simply-annotate-jump-to-annotation (position source-buffer-name)
  "Jump to annotation at POSITION in buffer named SOURCE-BUFFER-NAME."
  (if-let ((source-buffer (get-buffer source-buffer-name)))
      (progn
        (switch-to-buffer-other-window source-buffer)
        (goto-char position)
        (recenter)
        (when (fboundp 'pulse-momentary-highlight-one-line)
          (pulse-momentary-highlight-one-line (point)))
        (message "Jumped to annotation"))
    (message "Source buffer '%s' no longer exists" source-buffer-name)))

;;; Annotation Buffer Mode Functions
(defun simply-annotate-save-annotation-buffer ()
  "Save the annotation content from the buffer back to the overlay."
  (interactive)
  (when (and simply-annotate-current-overlay
             simply-annotate-source-buffer
             (buffer-live-p simply-annotate-source-buffer))
    (let ((content (string-trim
                    (buffer-substring simply-annotate-header-end-pos (point-max))))
          (overlay simply-annotate-current-overlay))
      
      (if (string-empty-p content)
          (progn
            (with-current-buffer simply-annotate-source-buffer
              (simply-annotate-remove-overlay overlay)
              (simply-annotate-save-annotations)
              (simply-annotate-update-header))
            (simply-annotate-hide-annotation-buffer)
            (message "Empty annotation removed"))
        (overlay-put overlay 'simply-annotation content)
        (overlay-put overlay 'help-echo content)
        (with-current-buffer simply-annotate-source-buffer
          (simply-annotate-save-annotations)
          (simply-annotate-update-header))
        (setq buffer-read-only t)
        (message "Annotation updated")))))

(defun simply-annotate-cancel-edit ()
  "Cancel editing and restore read-only mode."
  (interactive)
  (when (y-or-n-p "Cancel editing? (unsaved changes will be lost) ?")
    (setq buffer-read-only t)
    (when simply-annotate-current-overlay
      (simply-annotate-update-annotation-buffer
       (overlay-get simply-annotate-current-overlay 'simply-annotation)
       simply-annotate-current-overlay))
    (message "Edit cancelled")))

(defvar simply-annotate-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'simply-annotate-save-annotation-buffer)
    (define-key map (kbd "C-c C-k") #'simply-annotate-cancel-edit)
    (define-key map (kbd "q") (lambda () (interactive)
                                (when buffer-read-only
                                  (simply-annotate-hide-annotation-buffer))))

    (define-key map (kbd "n") (lambda () (interactive)
                                (when buffer-read-only
                                  (if simply-annotate-current-overlay
                                      (let* ((source-buf (overlay-buffer simply-annotate-current-overlay))
                                             (annotation-buf (current-buffer)))
                                        (when (buffer-live-p source-buf)
                                          (pop-to-buffer source-buf)
                                          (simply-annotate-next)
                                          (when (fboundp 'pulse-momentary-highlight-one-line)
                                            (pulse-momentary-highlight-one-line (point)))
                                          ;; Give the hooks a chance to run
                                          (run-hooks 'post-command-hook)
                                          ;; Pop back to annotation buffer
                                          (pop-to-buffer annotation-buf)))))))

    (define-key map (kbd "p") (lambda () (interactive)
                                (when buffer-read-only
                                  (if simply-annotate-current-overlay
                                      (let* ((source-buf (overlay-buffer simply-annotate-current-overlay))
                                             (annotation-buf (current-buffer)))
                                        (when (buffer-live-p source-buf)
                                          (pop-to-buffer source-buf)
                                          (simply-annotate-previous)
                                          (when (fboundp 'pulse-momentary-highlight-one-line)
                                            (pulse-momentary-highlight-one-line (point)))
                                          ;; Give the hooks a chance to run
                                          (run-hooks 'post-command-hook)
                                          ;; Pop back to annotation buffer
                                          (pop-to-buffer annotation-buf)))))))
    map)
  "Keymap for simply-annotate annotation buffer.")

(define-derived-mode simply-annotate-annotation-mode fundamental-mode "Annotation"
  "Mode for displaying and editing annotations."
  (setq buffer-read-only t)
  (visual-line-mode 1))

;;; Minor Mode
(defvar simply-annotate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a a") #'simply-annotate-add)
    (define-key map (kbd "C-c a m") #'simply-annotate-add-multiline)
    (define-key map (kbd "C-c a d") #'simply-annotate-remove)
    (define-key map (kbd "C-c a e") #'simply-annotate-edit)
    (define-key map (kbd "C-c a l") #'simply-annotate-list)
    (define-key map (kbd "C-c a t") #'simply-annotate-toggle)
    (define-key map (kbd "C-c a n") #'simply-annotate-next)
    (define-key map (kbd "C-c a p") #'simply-annotate-previous)
    map)
  "Keymap for `simply-annotate-mode'.")

;;;###autoload
(define-minor-mode simply-annotate-mode
  "Simply annotation mode."
  :lighter " SA"
  :keymap simply-annotate-mode-map
  (if simply-annotate-mode
      (progn
        (simply-annotate-clear-all-overlays)
        (simply-annotate-load-annotations)
        (simply-annotate-setup-header)
        (simply-annotate-update-header)
        (add-hook 'before-save-hook #'simply-annotate-save-annotations nil t)
        (add-hook 'kill-buffer-hook #'simply-annotate-save-annotations nil t)
        (add-hook 'kill-buffer-hook #'simply-annotate-hide-annotation-buffer nil t)
        (message "Simply-annotate mode enabled. Loaded %d annotations."
                 (length simply-annotate-overlays)))
    (simply-annotate-clear-all-overlays)
    (simply-annotate-cleanup-header)
    (simply-annotate-hide-annotation-buffer)
    (remove-hook 'before-save-hook #'simply-annotate-save-annotations t)
    (remove-hook 'kill-buffer-hook #'simply-annotate-save-annotations t)
    (remove-hook 'kill-buffer-hook #'simply-annotate-hide-annotation-buffer t)))

(provide 'simply-annotate)
;;; simply-annotate.el ends here
