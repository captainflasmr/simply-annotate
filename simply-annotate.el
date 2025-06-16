;;; simply-annotate.el --- Simple annotation system -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.0.2
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
;; 1. Open any file
;; 2. Enable annotation mode: =C-c A=
;; 3. Select text and press =M-s SPC= to create your first annotation
;; 4. Create some more annotations
;; 5. Navigate with =M-n= (next) and =M-p= (previous)
;;
;; Usage:
;;
;; * Editing
;; - Place cursor on annotated text
;; - Press =M-s SPC= to open the annotation buffer
;; - Make your changes
;; - Save with =C-c C-c=
;;
;; * Deleting
;; - Place cursor on annotated text
;; - Press =M-s -= to remove the annotation
;;
;; * Listing All Annotations
;; - Press =M-s l= to open a grep-mode buffer showing all annotations in the current file
;; - Click on line numbers or press =Enter= to jump directly to annotations
;; - Perfect for getting an overview of all your notes
;;
;; * Cross-file Overview
;; - Press =M-s 0= to browse annotations across all files
;; - Select a file from the completion list
;; - View all annotations for that file in grep-mode format
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

(defcustom simply-annotate-buffer-height 0.3
  "Height of annotation buffer as fraction of frame height."
  :type 'float
  :group 'simply-annotate)

;;; Variables

(defvar simply-annotate-mode nil
  "Non-nil if Simply-Annotate mode is enabled.")

(defvar-local simply-annotate-draft-overlay nil
  "Overlay for draft annotation being created.")

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
;;;###autoload
(defun simply-annotate-show-all ()
  "Show annotations from all files via `completing-read' selection."
  (interactive)
  (let* ((db (simply-annotate-load-database)))
    (if (not db)
        (message "No annotations database found")
      (let* ((files-with-annotations (mapcar #'car db))
             (file-display-alist
              (mapcar (lambda (file-key)
                        (let* ((annotations (alist-get file-key db nil nil #'string=))
                               (count (length annotations))
                               (display-name (format "%s (%d annotation%s)"
                                                     (file-name-nondirectory file-key)
                                                     count
                                                     (if (= count 1) "" "s"))))
                          (cons display-name file-key)))
                      files-with-annotations)))
        (if (not files-with-annotations)
            (message "No annotations found in database")
          (let* ((selected-display (completing-read "Select file with annotations: "
                                                    file-display-alist nil t))
                 (selected-file (cdr (assoc selected-display file-display-alist)))
                 (annotations (alist-get selected-file db nil nil #'string=)))
            (simply-annotate-display-file-annotations selected-file annotations)))))))

(defun simply-annotate-display-file-annotations (file-key annotations)
  "Display annotations for FILE-KEY in a buffer similar to `simply-annotate-list'.
Also opens the source file and focuses on the annotation list.
Argument ANNOTATIONS list of annotations for the file."
  (let* ((buffer-name "*Annotations*")
         (source-buffer (if (file-exists-p file-key)
                            (find-file-other-window file-key)
                          (find-file-noselect file-key 'nowarn)))
         (annotation-buffer (simply-annotate-format-annotations-for-buffer
                             file-key annotations source-buffer buffer-name)))
    
    ;; Display both buffers with focus on annotation buffer
    (if (file-exists-p file-key)
        (progn
          ;; Source file is already open in other window from find-file-other-window above
          ;; Now display annotation buffer in current window and focus on it
          (pop-to-buffer annotation-buffer)
          (goto-char (point-min))
          (message "Source file opened in other window. Navigate with Enter key."))
      ;; File doesn't exist, just show annotation buffer
      (progn
        (display-buffer annotation-buffer)
        (pop-to-buffer annotation-buffer)
        (goto-char (point-min))
        (message "Source file not found, showing annotations only.")))))

;;;###autoload
(defun simply-annotate-show ()
  "Show annotation at point, or jump to next annotation if none at point."
  (interactive)
  (if-let ((overlay (simply-annotate-overlay-at-point)))
      (let ((annotation-text (overlay-get overlay 'simply-annotation)))
        (simply-annotate-update-annotation-buffer annotation-text overlay)
        (simply-annotate-show-annotation-buffer))))

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
Only writes if there are actual annotations or if removing existing entries.
Argument FILE-KEY .
Argument ANNOTATIONS ."
  ;; Only proceed if we have annotations or need to remove existing ones
  (when (or annotations
            (and (simply-annotate-load-database)
                 (alist-get (if (symbolp file-key) (symbol-name file-key) file-key)
                            (simply-annotate-load-database) nil nil #'string=)))
    (let ((db (or (simply-annotate-load-database) '())))
      (if annotations
          ;; Add/update annotations
          (setf (alist-get (if (symbolp file-key) (symbol-name file-key) file-key)
                           db nil nil #'string=) annotations)
        ;; Remove empty annotation entries
        (setq db (cl-remove-if (lambda (entry)
                                 (string= (car entry)
                                          (if (symbolp file-key) (symbol-name file-key) file-key)))
                               db)))
      ;; Only save if we have a non-empty database or need to clean up
      (if db
          (simply-annotate-save-database db)
        ;; If database is now empty, optionally remove the file
        (when (file-exists-p simply-annotate-file)
          (delete-file simply-annotate-file))))))

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
    (when (and file-key simply-annotate-mode)  ; Only save if annotation mode is active
      (let ((annotations (simply-annotate-serialize-annotations)))
        ;; Only update database if we actually have annotations
        (when (or annotations
                  ;; Or if we had annotations before (to handle deletions)
                  (let ((db (simply-annotate-load-database)))
                    (and db (alist-get file-key db nil nil #'string=))))
          (simply-annotate-update-database file-key annotations))))))

(defun simply-annotate-should-save-p ()
  "Check if we should save annotations for the current buffer."
  (and simply-annotate-mode  ; Mode must be active
       (simply-annotate-file-key)  ; Must have a valid file key
       (or simply-annotate-overlays  ; Either have current overlays
           ;; Or had stored annotations (for handling deletions)
           (let ((db (simply-annotate-load-database)))
             (and db (alist-get (simply-annotate-file-key) db nil nil #'string=))))))

(defun simply-annotate-save-annotations-safe ()
  "Save current buffer's annotations only if appropriate."
  (when (simply-annotate-should-save-p)
    (let ((file-key (simply-annotate-file-key))
          (annotations (simply-annotate-serialize-annotations)))
      (simply-annotate-update-database file-key annotations))))

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
      (unless (derived-mode-p 'simply-annotate-annotation-mode)
        (simply-annotate-annotation-mode)))
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
        (insert (make-string 50 ?-) "\n")
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
  (interactive)
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
       (format "Prev %s | Next %s | Act %s | Del %s | List %s | Browse %s"

               (propertize "M-p" 'face '(:weight bold))
               (propertize "M-n" 'face '(:weight bold))
               
               (propertize "M-s SPC" 'face '(:weight bold))

               (propertize "M-s -" 'face '(:weight bold))
               (propertize "M-s l" 'face '(:weight bold))
               (propertize "M-s 0" 'face '(:weight bold)))))))

(defun simply-annotate-update-header ()
  "Update header-line with current annotation info."
  (let ((overlay (simply-annotate-overlay-at-point))
        (new-annotation nil))
    
    (when overlay
      (setq new-annotation (overlay-get overlay 'simply-annotation)))
    
    (unless (equal new-annotation simply-annotate-current-annotation)
      (setq simply-annotate-current-annotation new-annotation)
      
      (when (and (not overlay) (get-buffer-window simply-annotate-buffer-name))
        (simply-annotate-hide-annotation-buffer)))
    
    ;; Always update the header format and force refresh
    (setq header-line-format (simply-annotate-format-header))
    (force-mode-line-update t)))

(defun simply-annotate-setup-header ()
  "Setup header-line for annotation display."
  (setq simply-annotate-original-header-line header-line-format
        header-line-format (simply-annotate-format-header)))

(defun simply-annotate-cleanup-header ()
  "Restore original header-line."
  (setq header-line-format simply-annotate-original-header-line
        simply-annotate-current-annotation nil))

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
        ;; Update annotation buffer if it's visible
        (let ((annotation-text (overlay-get next-overlay 'simply-annotation)))
          (if (get-buffer-window simply-annotate-buffer-name)
              (progn
                (simply-annotate-update-annotation-buffer annotation-text next-overlay)
                (simply-annotate-show-annotation-buffer))
            (message annotation-text))))
    (message "No annotations in buffer")))

;;;###autoload
(defun simply-annotate-previous ()
  "Navigate to previous annotation."
  (interactive)
  (if-let ((prev-overlay (simply-annotate-find-annotation nil t)))
      (progn
        (goto-char (overlay-start prev-overlay))
        ;; Update annotation buffer if it's visible
        (let ((annotation-text (overlay-get prev-overlay 'simply-annotation)))
          (if (get-buffer-window simply-annotate-buffer-name)
              (progn
                (simply-annotate-update-annotation-buffer annotation-text prev-overlay)
                (simply-annotate-show-annotation-buffer))
            (message annotation-text))))
    (message "No annotations in buffer")))

;;;###autoload
(defun simply-annotate-add ()
  "Add annotation to selected region or current line."
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (line-beginning-position)))
         (end (if (use-region-p) (region-end) (line-end-position))))
    
    ;; Check if there's already an annotation at this location
    (if-let ((existing-overlay (simply-annotate-overlay-at-point start)))
        (progn
          ;; Edit existing annotation
          (goto-char (overlay-start existing-overlay))
          (simply-annotate-show))
      ;; Create new annotation in draft mode
      (let ((draft-overlay (make-overlay start end)))
        (overlay-put draft-overlay 'simply-annotation "")
        (overlay-put draft-overlay 'face simply-annotate-highlight-face)
        (overlay-put draft-overlay 'simply-annotation-draft t)
        (setq simply-annotate-draft-overlay draft-overlay)
        
        ;; Show annotation buffer for editing
        (simply-annotate-update-annotation-buffer "" draft-overlay)
        (simply-annotate-show-annotation-buffer)
        (pop-to-buffer (simply-annotate-get-annotation-buffer))
        
        ;; Make buffer editable immediately for new annotations
        (setq buffer-read-only nil)
        (goto-char simply-annotate-header-end-pos)
        
        (when (use-region-p) (deactivate-mark))
        (message "Enter annotation text (C-c C-c to save, C-c C-k to cancel)")))))

;;;###autoload
(defun simply-annotate-remove ()
  "Remove annotation at point."
  (interactive)
  (if-let ((overlay (simply-annotate-overlay-at-point)))
      (progn
        (simply-annotate-remove-overlay overlay)
        (simply-annotate-save-annotations)
        ;; Ensure header is updated after removal
        (simply-annotate-update-header)
        (message "Annotation removed"))
    (message "No annotation at point")))

;;;###autoload
(defcustom simply-annotate-annotation-separator "┌─────"
  "Regex matching the annotation separator line."
  :type 'regexp
  :group 'simply-annotate-list)

(defcustom simply-annotate-text-separator "└─────"
  "Regex matching the text separator line."
  :type 'regexp
  :group 'simply-annotate-list)

(defcustom simply-annotate-header-regexp "^Annotations for \\(.*\\)"
  "Regex matching the buffer header."
  :type 'regexp
  :group 'simply-annotate-list)

(defcustom simply-annotate-annotation-block-regexp
  "┌─────\\s-*\\(\\(.\\|\n\\)*?\\)\\s-*└─────"
  "Regexp matching the annotation block (between ANNOTATION and TEXT)."
  :type 'regexp
  :group 'simply-annotate-list)

(defun simply-annotate-format-annotations-for-buffer (file-key annotations source-buffer buffer-name)
  "Format ANNOTATIONS for FILE-KEY into BUFFER-NAME using SOURCE-BUFFER."
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Add a header comment
      (insert (format "Annotations for %s:\n\n" file-key))
      
      ;; Sort annotations by line position
      (let ((sorted-annotations
             (sort annotations
                   (lambda (a b) (< (alist-get 'start a) (alist-get 'start b))))))
        
        ;; Format each annotation in grep-mode style
        (dolist (ann sorted-annotations)
          (let* ((start-pos (alist-get 'start ann))
                 (end-pos (alist-get 'end ann))
                 (text (alist-get 'text ann))
                 (line-num (if source-buffer
                               (with-current-buffer source-buffer
                                 (save-excursion
                                   (goto-char (min start-pos (point-max)))
                                   (line-number-at-pos)))
                             1))
                 (col-num (if source-buffer
                              (with-current-buffer source-buffer
                                (save-excursion
                                  (goto-char (min start-pos (point-max)))
                                  (current-column)))
                            0))
                 (line-content (if source-buffer
                                   (with-current-buffer source-buffer
                                     (save-excursion
                                       (goto-char (min start-pos (point-max)))
                                       (let ((line-end (min end-pos
                                                            (line-end-position)
                                                            (point-max))))
                                         (buffer-substring-no-properties
                                          (min start-pos (point-max)) line-end))))
                                 "Content not available")))
            
            ;; Format: filename:line:column:content
            (insert (format "%s:%d:%d\n"
                            file-key line-num (1+ col-num)))
            
            (let* ((lines (split-string (string-trim text) "\n"))
                   (indented-text (mapconcat (lambda (line) (concat "│ " line)) lines "\n")))
              (insert (format "%s\n%s\n%s\n"
                              simply-annotate-annotation-separator
                              indented-text
                              simply-annotate-text-separator)))
            (insert (format "%s\n\n"
                            (string-trim line-content))))))
      
      ;; Enable grep-mode for navigation
      (grep-mode)
      ;; Add custom font-lock rules
      (font-lock-add-keywords nil
                              `((,simply-annotate-annotation-block-regexp 1 '(:slant italic))
                                (,simply-annotate-header-regexp 1 '(:weight bold))
                                (,simply-annotate-annotation-separator 0 '(:underline nil))
                                (,simply-annotate-text-separator 0 '(:underline nil)))
                              'append)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (current-buffer)))

;;;###autoload
(defun simply-annotate-list ()
  "Toggle annotation list buffer visibility.
If the annotation list buffer is visible, hide it.
Otherwise, list all annotations in current buffer using `grep-mode' format."
  (interactive)
  (let ((buffer-name "*Annotations*")
        (existing-window (get-buffer-window "*Annotations*")))
    (if existing-window
        ;; Buffer is visible - hide it
        (progn
          (delete-window existing-window)
          (message "Annotation list hidden"))
      ;; Buffer not visible - show annotations
      (if simply-annotate-overlays
          (let* ((source-buffer (current-buffer))
                 (source-file (or (buffer-file-name) (buffer-name)))
                 (annotations (simply-annotate-serialize-annotations))
                 (annotation-buffer (simply-annotate-format-annotations-for-buffer
                                     source-file annotations source-buffer buffer-name)))
            (pop-to-buffer annotation-buffer)
            (goto-char (point-min)))
        (message "No annotations in buffer")))))

;;; Annotation Buffer Mode Functions
(defun simply-annotate-save-annotation-buffer ()
  "Save the annotation content from the buffer back to the overlay."
  (interactive)
  (when (and simply-annotate-current-overlay
             simply-annotate-source-buffer
             (buffer-live-p simply-annotate-source-buffer))
    (let ((content (string-trim
                    (buffer-substring simply-annotate-header-end-pos (point-max))))
          (overlay simply-annotate-current-overlay)
          (is-draft (overlay-get simply-annotate-current-overlay 'simply-annotation-draft)))
      
      (if (string-empty-p content)
          (progn
            ;; Remove empty annotation
            (with-current-buffer simply-annotate-source-buffer
              (if is-draft
                  ;; Clean up draft overlay
                  (progn
                    (delete-overlay simply-annotate-draft-overlay)
                    (setq simply-annotate-draft-overlay nil))
                ;; Remove existing annotation
                (progn
                  (simply-annotate-remove-overlay overlay)
                  (simply-annotate-save-annotations)))
              ;; Update header after removal
              (simply-annotate-update-header))
            (simply-annotate-hide-annotation-buffer)
            (message "Annotation cancelled/removed"))
        (progn
          ;; Save non-empty annotation
          (overlay-put overlay 'simply-annotation content)
          (overlay-put overlay 'help-echo content)
          
          (with-current-buffer simply-annotate-source-buffer
            (if is-draft
                ;; Convert draft to real annotation
                (progn
                  (overlay-put overlay 'simply-annotation-draft nil)
                  (push overlay simply-annotate-overlays)
                  (setq simply-annotate-draft-overlay nil))
              ;; Update existing annotation
              nil)
            (simply-annotate-save-annotations)
            ;; Update header after addition/modification
            (simply-annotate-update-header)
            (simply-annotate-hide-annotation-buffer)
            (when (use-region-p) (deactivate-mark)))))
      
      (setq buffer-read-only t)
      (message "Annotation saved"))))

(defun simply-annotate-cancel-edit ()
  "Cancel editing and restore read-only mode."
  (interactive)
  (when (y-or-n-p "Cancel editing? (unsaved changes will be lost)?")
    (when (and simply-annotate-current-overlay
               simply-annotate-source-buffer
               (buffer-live-p simply-annotate-source-buffer))
      (let ((is-draft (overlay-get simply-annotate-current-overlay 'simply-annotation-draft)))
        (with-current-buffer simply-annotate-source-buffer
          (if is-draft
              ;; Clean up draft overlay
              (progn
                (delete-overlay simply-annotate-draft-overlay)
                (setq simply-annotate-draft-overlay nil))
            ;; Restore original annotation content
            (simply-annotate-update-annotation-buffer
             (overlay-get simply-annotate-current-overlay 'simply-annotation)
             simply-annotate-current-overlay)))))
    
    (setq buffer-read-only t)
    (simply-annotate-hide-annotation-buffer)
    (message "Edit cancelled")))

(defun simply-annotate-cleanup-draft ()
  "Clean up any draft overlays when disabling mode."
  (when simply-annotate-draft-overlay
    (delete-overlay simply-annotate-draft-overlay)
    (setq simply-annotate-draft-overlay nil)))

(defun simply-annotate-smart-action ()
  "Multipurpose annotation command:
1. With region selected: Create/edit annotation and enter edit mode
2. On overlay without region: Toggle annotation buffer visibility
3. Elsewhere: Hide annotation buffer if visible"
  (interactive)
  (cond
   ;; Case 1: Region is selected - always create/edit annotation and enter edit mode
   ((use-region-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (existing-overlay (simply-annotate-overlay-at-point start)))
      
      (if existing-overlay
          ;; Edit existing annotation
          (progn
            (goto-char (overlay-start existing-overlay))
            (let ((annotation-text (overlay-get existing-overlay 'simply-annotation)))
              (simply-annotate-update-annotation-buffer annotation-text existing-overlay)
              (simply-annotate-show-annotation-buffer)
              (pop-to-buffer (simply-annotate-get-annotation-buffer))
              ;; Enter edit mode immediately
              (setq buffer-read-only nil)
              (goto-char simply-annotate-header-end-pos)
              (message "Editing existing annotation (C-c C-c to save, C-c C-k to cancel)")))
        
        ;; Create new annotation
        (let ((draft-overlay (make-overlay start end)))
          (overlay-put draft-overlay 'simply-annotation "")
          (overlay-put draft-overlay 'face simply-annotate-highlight-face)
          (overlay-put draft-overlay 'simply-annotation-draft t)
          (setq simply-annotate-draft-overlay draft-overlay)
          
          ;; Show annotation buffer for editing
          (simply-annotate-update-annotation-buffer "" draft-overlay)
          (simply-annotate-show-annotation-buffer)
          (pop-to-buffer (simply-annotate-get-annotation-buffer))
          
          ;; Enter edit mode immediately
          (setq buffer-read-only nil)
          (goto-char simply-annotate-header-end-pos)
          (message "Enter annotation text (C-c C-c to save, C-c C-k to cancel)")))
      
      (deactivate-mark)))
   
   ;; Case 2: No region, but cursor is on an overlay - toggle annotation buffer
   ((simply-annotate-overlay-at-point)
    (let* ((overlay (simply-annotate-overlay-at-point))
           (annotation-text (overlay-get overlay 'simply-annotation))
           (buffer-window (get-buffer-window simply-annotate-buffer-name)))
      
      (if buffer-window
          ;; Buffer is visible - hide it (toggle off)
          (progn
            (simply-annotate-hide-annotation-buffer))
        ;; Buffer is not visible - show it (toggle on)
        (progn
          (simply-annotate-update-annotation-buffer annotation-text overlay)
          (simply-annotate-show-annotation-buffer)))))

   ;; Case 3: No region, not on overlay - hide annotation buffer if visible
   (t
    (let ((buffer-window (get-buffer-window simply-annotate-buffer-name)))
      (if buffer-window
          (progn
            (simply-annotate-hide-annotation-buffer))
        (message "No annotation at point"))))))

(defun simply-annotate-edit ()
  "Edit the buffer, just a wrapper around normal edit."
  (interactive)
  (setq buffer-read-only nil)
  (goto-char simply-annotate-header-end-pos)
  (message "Editing existing annotation (C-c C-c to save, C-c C-k to cancel)"))

(defvar simply-annotate-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'simply-annotate-save-annotation-buffer)
    (define-key map (kbd "C-c C-k") #'simply-annotate-cancel-edit)
    (define-key map (kbd "C-x C-q") #'simply-annotate-edit)
    map)
  "Keymap for simply-annotate annotation buffer.")

(define-derived-mode simply-annotate-annotation-mode fundamental-mode "Annotation"
  "Mode for displaying and editing annotations."
  (setq buffer-read-only t)
  (visual-line-mode 1))

;;; Minor Mode
(defvar simply-annotate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-s -") #'simply-annotate-remove)
    (define-key map (kbd "M-s l") #'simply-annotate-list)
    (define-key map (kbd "M-s 0") #'simply-annotate-show-all)
    (define-key map (kbd "M-s SPC") #'simply-annotate-smart-action)
    (define-key map (kbd "M-p") #'simply-annotate-previous)
    (define-key map (kbd "M-n") #'simply-annotate-next)
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
        (simply-annotate-cleanup-draft)
        (simply-annotate-load-annotations)
        (simply-annotate-setup-header)
        (simply-annotate-update-header)
        ;; Use the safer save function
        (add-hook 'before-save-hook #'simply-annotate-save-annotations-safe nil t)
        (add-hook 'kill-buffer-hook #'simply-annotate-save-annotations-safe nil t)
        (add-hook 'kill-buffer-hook #'simply-annotate-hide-annotation-buffer nil t)
        (message "Simply-annotate mode enabled. Loaded %d annotations."
                 (length simply-annotate-overlays)))
    (simply-annotate-clear-all-overlays)
    (simply-annotate-cleanup-header)
    (simply-annotate-hide-annotation-buffer)
    ;; Remove the safer save function hooks
    (remove-hook 'before-save-hook #'simply-annotate-save-annotations-safe t)
    (remove-hook 'kill-buffer-hook #'simply-annotate-save-annotations-safe t)
    (remove-hook 'kill-buffer-hook #'simply-annotate-hide-annotation-buffer t)))

(provide 'simply-annotate)
;;; simply-annotate.el ends here
