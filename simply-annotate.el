;; simply-annotate.el --- Enhanced annotation system with threading -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.5.0
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
;; original content. Enhanced with threading, collaboration, and org-mode integration.
;;
;; Quick Start:
;;
;; (use-package simply-annotate
;;  :bind ("C-c A" . simply-annotate-mode))
;;
;; 1. Open any file
;; 2. Enable annotation mode: =C-c A=
;; 3. Select text and press =M-s j= to create your first annotation
;; 4. Create some more annotations
;; 5. Navigate with =M-n= (next) and =M-p= (previous)
;;
;; Threading & Collaboration:
;;
;; * Replies
;; - Press =M-s r= to add a reply to any annotation
;; - Creates threaded conversations for code reviews
;;
;; * Status Management
;; - Press =M-s s= to set status (open, in-progress, resolved, closed)
;; - Press =M-s p= to set priority (low, normal, high, critical)
;; - Press =M-s t= to add tags for organization
;;
;; * Author Management
;; - Configure team members: (setq simply-annotate-author-list '("John" "Jane" "Bob"))
;; - Set prompting behavior: (setq simply-annotate-prompt-for-author 'threads-only)
;; - Press =M-s a= to change annotation author
;;
;; * Org-mode Integration
;; - Press =M-s o= to export annotations to org-mode files
;; - Each thread becomes a TODO item with replies as sub-entries
;;
;; Configuration Examples:
;;
;; ;; Single user (default)
;; (setq simply-annotate-prompt-for-author nil)
;;
;; ;; Team collaboration
;; (setq simply-annotate-author-list '("John Doe" "Jane Smith" "Bob Wilson"))
;; (setq simply-annotate-prompt-for-author 'threads-only)
;; (setq simply-annotate-remember-author-per-file t)
;;
;;; Code:

;;; Customization

(require 'cl-lib)

(defgroup simply-annotate nil
  "Simple annotation system with threading support."
  :group 'text)

(defcustom simply-annotate-file
  (expand-file-name "simply-annotations.el" user-emacs-directory)
  "File to store annotations."
  :type 'file
  :group 'simply-annotate)

(defcustom simply-annotate-highlight-face '(:inherit highlight)
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

(defcustom simply-annotate-display-style 'fringe
  "How to display annotated text.
- 'highlight: Highlight the annotated text (default behavior)
- 'fringe: Show indicators in the fringe
- 'both: Show both fringe indicators and text highlighting"
  :type '(choice (const :tag "Highlight text" highlight)
                 (const :tag "Fringe indicators" fringe)
                 (const :tag "Both fringe and highlight" both))
  :group 'simply-annotate)

(defcustom simply-annotate-fringe-indicator 'right-triangle
  "Symbol to use for fringe indicators."
  :type '(choice (const :tag "Left triangle" left-triangle)
                 (const :tag "Right triangle" right-triangle)
                 (const :tag "Filled rectangle" filled-rectangle)
                 (const :tag "Custom bitmap" custom)))

(defcustom simply-annotate-fringe-face 'simply-annotate-fringe-face
  "Face for fringe indicators."
  :type 'face
  :group 'simply-annotate)

;; Threading and Collaboration Customization

(defcustom simply-annotate-default-author 
  (or user-full-name user-login-name "Anonymous")
  "Default author name for annotations."
  :type 'string
  :group 'simply-annotate)

(defcustom simply-annotate-author-list
  (list (or user-full-name user-login-name "Anonymous"))
  "List of available authors for annotations.
The first entry is used as the default. Add team members here for collaborative workflows.
Example: '(\"John Doe\" \"Jane Smith\" \"Bob Wilson\" \"Alice Chen\")"
  :type '(repeat string)
  :group 'simply-annotate)

(defcustom simply-annotate-prompt-for-author nil
  "Whether to prompt for author selection when creating annotations.
- nil: Always use default author (single-user mode)
- 'first-only: Prompt only for the first annotation in a session, then remember choice
- 'always: Prompt for every annotation
- 'threads-only: Prompt only when adding replies to threads (useful for review workflows)"
  :type '(choice (const :tag "Never prompt (single-user)" nil)
                 (const :tag "Prompt once per session" first-only)
                 (const :tag "Always prompt" always)
                 (const :tag "Prompt only for thread replies" threads-only))
  :group 'simply-annotate)

(defcustom simply-annotate-remember-author-per-file nil
  "Whether to remember author choice per file.
When enabled, the package remembers the last author used for each file."
  :type 'boolean
  :group 'simply-annotate)

(defcustom simply-annotate-thread-statuses
  '("open" "in-progress" "resolved" "closed")
  "Available status values for annotation threads."
  :type '(repeat string)
  :group 'simply-annotate)

(defcustom simply-annotate-priority-levels
  '("low" "normal" "high" "critical")
  "Available priority levels for annotations."
  :type '(repeat string)
  :group 'simply-annotate)

(defcustom simply-annotate-annotation-separator "┌─────"
  "Regex matching the annotation separator line."
  :type 'regexp
  :group 'simply-annotate)

(defcustom simply-annotate-text-separator "└─────"
  "Regex matching the text separator line."
  :type 'regexp
  :group 'simply-annotate)

(defcustom simply-annotate-header-regexp "^Annotations for \\(.*\\)"
  "Regex matching the buffer header."
  :type 'regexp
  :group 'simply-annotate)

(defcustom simply-annotate-annotation-block-regexp
  "┌─────\\s-*\\(\\(.\\|\n\\)*?\\)\\s-*└─────"
  "Regexp matching the annotation block (between ANNOTATION and TEXT)."
  :type 'regexp
  :group 'simply-annotate)

;; Define faces for the indicators
(defface simply-annotate-fringe-face
  '((t (:foreground "orange" :background nil)))
  "Face for fringe annotation indicators."
  :group 'simply-annotate)

;; Custom fringe bitmap (optional)
(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'simply-annotate-fringe-bitmap
    [#b00000000
     #b00011000
     #b00111100
     #b01111110
     #b11111111
     #b01111110
     #b00111100
     #b00011000]
    nil nil 'center))

;;; Variables

(defvar simply-annotate-mode nil
  "Non-nil if Simply-Annotate mode is enabled.")

(defvar simply-annotate-draft-overlay nil
  "Overlay for draft annotation being created.")

(defvar simply-annotate-overlays nil
  "List of annotation overlays in current buffer.")

(defvar simply-annotate-original-header-line nil
  "Original `header-line-format' before annotation mode.")

(defvar simply-annotate-current-annotation nil
  "Current annotation text being displayed in header.")

(defvar simply-annotate-current-overlay nil
  "Current overlay being displayed/edited in annotation buffer.")

(defvar simply-annotate-source-buffer nil
  "Source buffer for annotation display.")

(defvar simply-annotate-header-end-pos nil
  "Position where the annotation content starts (after header).")

(defvar simply-annotate-editing-annotation-sexp nil
  "Non-nil if the *Annotation* buffer is currently displaying the raw sexp for editing.")

;; Threading Variables

(defvar simply-annotate-session-author nil
  "Author chosen for current session (when using 'first-only mode).")

(defvar simply-annotate-file-authors nil
  "Alist of (file-key . author) for per-file author memory.")

;;; Utility Functions

(defun simply-annotate-get-annotation-text (annotation-data)
  "Extract display text from annotation data (string or thread)."
  (if (stringp annotation-data)
      annotation-data  ; Legacy string annotation
    ;; Thread structure - get first comment text
    (let ((comments (alist-get 'comments annotation-data)))
      (when comments
        (alist-get 'text (car comments))))))

(defun simply-annotate-get-annotation-summary (annotation-data)
  "Get a summary for display (help-echo, header, etc.)."
  (if (stringp annotation-data)
      annotation-data  ; Legacy string annotation
    ;; Thread structure - use formatted summary
    (simply-annotate-format-thread-summary annotation-data)))

(defun simply-annotate-is-thread-p (annotation-data)
  "Check if annotation data is a thread structure."
  (and (listp annotation-data)
       (alist-get 'id annotation-data)
       (alist-get 'comments annotation-data)))

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

;;; Display and Fringe Functions

(defun simply-annotate-add-fringe-indicator (overlay)
  "Add fringe indicator to OVERLAY."
  (let* ((start (overlay-start overlay))
         (bitmap (pcase simply-annotate-fringe-indicator
                   ('left-triangle 'left-triangle)
                   ('right-triangle 'right-triangle)
                   ('filled-rectangle 'filled-rectangle)
                   ('custom 'simply-annotate-fringe-bitmap)
                   (_ 'left-triangle)))
         (fringe-spec `(left-fringe ,bitmap ,simply-annotate-fringe-face)))
    
    ;; Add the fringe indicator to the first character of the overlay
    (overlay-put overlay 'before-string 
                 (propertize " " 'display fringe-spec))))

(defun simply-annotate-update-display-style ()
  "Update display style for all existing annotations."
  (interactive)
  (dolist (overlay simply-annotate-overlays)
    ;; Clear existing display properties
    (overlay-put overlay 'face nil)
    (overlay-put overlay 'before-string nil)
    
    ;; Reapply based on current style
    (let ((text (overlay-get overlay 'simply-annotation)))
      (pcase simply-annotate-display-style
        ('highlight
         (overlay-put overlay 'face simply-annotate-highlight-face))
        
        ('fringe
         (simply-annotate-add-fringe-indicator overlay))
        
        ('both
         (overlay-put overlay 'face simply-annotate-highlight-face)
         (simply-annotate-add-fringe-indicator overlay)))))
  
  (message "Updated display style to: %s" simply-annotate-display-style))

(defun simply-annotate-cycle-display-style ()
  "Cycle through different annotation display styles."
  (interactive)
  (setq simply-annotate-display-style
        (pcase simply-annotate-display-style
          ('highlight 'fringe)
          ('fringe 'both)
          ('both 'highlight)
          (_ 'highlight)))
  (simply-annotate-update-display-style))

(defun simply-annotate-create-overlay (start end text)
  "Create annotation overlay with configurable display style.
Argument START .
Argument END .
Argument TEXT ."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'simply-annotation text)
    (overlay-put overlay 'help-echo (simply-annotate-get-annotation-summary text))
    (overlay-put overlay 'mouse-face 'highlight)
    
    ;; Apply display style based on customization
    (pcase simply-annotate-display-style
      ('highlight
       (overlay-put overlay 'face simply-annotate-highlight-face))
      
      ('fringe
       (simply-annotate-add-fringe-indicator overlay))
      
      ('both
       (overlay-put overlay 'face simply-annotate-highlight-face)
       (simply-annotate-add-fringe-indicator overlay)))
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
In fringe mode, searches the entire current line for overlays.
Optional argument POS specifies the position to check."
  (let ((check-pos (or pos (point))))
    (if (or (eq simply-annotate-display-style 'fringe))
        ;; In fringe mode, check the entire current line
        (simply-annotate-overlay-on-line check-pos)
      ;; In highlight mode, check only at the specific position
      (cl-find-if (lambda (ov) (overlay-get ov 'simply-annotation))
                  (overlays-at check-pos)))))

(defun simply-annotate-overlay-on-line (&optional pos)
  "Find annotation overlay anywhere on the current line.
Optional argument POS specifies the line to check (defaults to current point)."
  (save-excursion
    (when pos (goto-char pos))
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position))
          (found-overlay nil))
      ;; Check all overlays in the current buffer
      (dolist (overlay simply-annotate-overlays)
        (when (and (not found-overlay)
                   (overlay-get overlay 'simply-annotation)
                   ;; Check if overlay starts anywhere on the current line
                   (>= (overlay-start overlay) line-start)
                   (<= (overlay-start overlay) line-end))
          (setq found-overlay overlay)))
      found-overlay)))

;;; Threading Functions

(defun simply-annotate-create-thread (text &optional author priority tags)
  "Create a new annotation thread."
  (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))
        (id (format "thread-%d" (random 1000000))))
    `((id . ,id)
      (created . ,timestamp)
      (status . "open")
      (priority . ,(or priority "normal"))
      (tags . ,(or tags '()))
      (comments . (((author . ,(or author simply-annotate-default-author))
                    (timestamp . ,timestamp)
                    (text . ,text)
                    (type . "comment")))))))

(defun simply-annotate-create-thread-with-author (text &optional author priority tags)
  "Create a new annotation thread with proper author handling."
  (let* ((chosen-author (or author (simply-annotate-get-author-for-context 'annotation)))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))
         (id (format "thread-%d" (random 1000000))))
    
    ;; Remember the author choice
    (simply-annotate-remember-author chosen-author)
    
    `((id . ,id)
      (created . ,timestamp)
      (status . "open")
      (priority . ,(or priority "normal"))
      (tags . ,(or tags '()))
      (comments . (((author . ,chosen-author)
                    (timestamp . ,timestamp)
                    (text . ,text)
                    (type . "comment")))))))

(defun simply-annotate-add-reply (thread reply-text &optional author)
  "Add a reply to an existing thread."
  (let* ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))
         (reply `((author . ,(or author simply-annotate-default-author))
                  (timestamp . ,timestamp)
                  (text . ,reply-text)
                  (type . "reply")))
         (comments (alist-get 'comments thread)))
    (setf (alist-get 'comments thread) (append comments (list reply)))
    thread))

(defun simply-annotate-add-reply-with-author (thread reply-text &optional author)
  "Add a reply to an existing thread with proper author handling."
  (let* ((chosen-author (or author (simply-annotate-get-author-for-context 'reply)))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))
         (reply `((author . ,chosen-author)
                  (timestamp . ,timestamp)
                  (text . ,reply-text)
                  (type . "reply")))
         (comments (alist-get 'comments thread)))
    
    ;; Remember the author choice
    (simply-annotate-remember-author chosen-author)
    
    (setf (alist-get 'comments thread) (append comments (list reply)))
    thread))

(defun simply-annotate-set-thread-status (thread status)
  "Set the status of a thread."
  (when (member status simply-annotate-thread-statuses)
    (setf (alist-get 'status thread) status)
    thread))

(defun simply-annotate-set-thread-priority (thread priority)
  "Set the priority of a thread."
  (when (member priority simply-annotate-priority-levels)
    (setf (alist-get 'priority thread) priority)
    thread))

(defun simply-annotate-add-thread-tag (thread tag)
  "Add a tag to a thread."
  (let ((tags (alist-get 'tags thread)))
    (unless (member tag tags)
      (setf (alist-get 'tags thread) (append tags (list tag))))
    thread))

(defun simply-annotate-format-thread-summary (thread)
  "Format a brief summary of the thread for display."
  (let* ((status (alist-get 'status thread))
         (priority (alist-get 'priority thread))
         (comments (alist-get 'comments thread))
         (comment-count (length comments))
         (first-comment (car comments))
         (preview (truncate-string-to-width 
                   (alist-get 'text first-comment) 40 nil nil "...")))
    (format "[%s/%s] %s (%d comment%s)"
            (upcase status)
            (upcase priority)
            preview
            comment-count
            (if (= comment-count 1) "" "s"))))

(defun simply-annotate-format-thread-full (thread)
  "Format complete thread for display in annotation buffer."
  (let* ((id (alist-get 'id thread))
         (status (alist-get 'status thread))
         (priority (alist-get 'priority thread))
         (tags (alist-get 'tags thread))
         (comments (alist-get 'comments thread))
         (header (format "┌─ Thread: %s [%s/%s]" id status priority))
         (authors (delete-dups (mapcar (lambda (c) (alist-get 'author c)) comments)))
         (author-count (length authors)))
    
    (when tags
      (setq header (concat header (format " Tags: %s" (string-join tags ", ")))))
    
    (when (> author-count 1)
      (setq header (concat header (format " (%d authors)" author-count))))
    
    (concat
     header "\n"
     (mapconcat
      (lambda (comment)
        (let ((author (alist-get 'author comment))
              (timestamp (alist-get 'timestamp comment))
              (text (alist-get 'text comment))
              (type (alist-get 'type comment)))
          (format "│ %s %s (%s):\n│ %s"
                  (if (string= type "comment") "💬" "↳")
                  (propertize author 'face 'bold)
                  (format-time-string "%m/%d %H:%M" (date-to-time timestamp))
                  (replace-regexp-in-string "\n" "\n│ " text))))
      comments
      "\n├─────\n")
     "\n└─────")))

;;; Author Management

(defun simply-annotate-get-author-for-context (context &optional current-author)
  "Get appropriate author based on context and settings.
CONTEXT can be 'annotation, 'reply, or 'edit.
CURRENT-AUTHOR is the existing author when editing."
  (let* ((file-key (simply-annotate-file-key))
         (default-author (or simply-annotate-default-author 
                             (car simply-annotate-author-list)))
         (remembered-author (when simply-annotate-remember-author-per-file
                              (alist-get file-key simply-annotate-file-authors nil nil #'string=)))
         (session-author simply-annotate-session-author)
         (initial-author (or remembered-author session-author default-author)))
    
    (cond
     ;; Never prompt - use default/remembered author
     ((null simply-annotate-prompt-for-author)
      initial-author)
     
     ;; Always prompt
     ((eq simply-annotate-prompt-for-author 'always)
      (simply-annotate-select-author initial-author current-author))
     
     ;; Prompt once per session
     ((eq simply-annotate-prompt-for-author 'first-only)
      (if simply-annotate-session-author
          simply-annotate-session-author
        (let ((chosen (simply-annotate-select-author initial-author current-author)))
          (setq simply-annotate-session-author chosen)
          chosen)))
     
     ;; Prompt only for thread replies
     ((eq simply-annotate-prompt-for-author 'threads-only)
      (if (eq context 'reply)
          (simply-annotate-select-author initial-author current-author)
        initial-author))
     
     ;; Default fallback
     (t initial-author))))

(defun simply-annotate-select-author (&optional default-author current-author)
  "Prompt user to select an author from the configured list.
DEFAULT-AUTHOR is pre-selected. CURRENT-AUTHOR is shown when editing."
  (let* ((authors (if (> (length simply-annotate-author-list) 1)
                      simply-annotate-author-list
                    (append simply-annotate-author-list 
                            (list "Other..."))))
         (prompt (if current-author
                     (format "Author (current: %s): " current-author)
                   "Author: "))
         (default (or default-author (car authors)))
         (choice (completing-read prompt authors nil nil nil nil default)))
    
    ;; Handle "Other..." selection
    (if (string= choice "Other...")
        (let ((custom-author (read-string "Enter author name: " default)))
          (when (and custom-author (not (string-empty-p custom-author)))
            ;; Add to author list for future use
            (unless (member custom-author simply-annotate-author-list)
              (customize-save-variable 'simply-annotate-author-list
                                       (append simply-annotate-author-list (list custom-author))))
            custom-author))
      choice)))

(defun simply-annotate-remember-author (author)
  "Remember the chosen author for the current file if enabled."
  (when (and simply-annotate-remember-author-per-file author)
    (let ((file-key (simply-annotate-file-key)))
      (setf (alist-get file-key simply-annotate-file-authors nil nil #'string=) author))))

;;; Serialization Functions

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
        (push (simply-annotate-create-overlay start end text) simply-annotate-overlays)))))

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
      (unless (eq major-mode 'simply-annotate-annotation-mode)
        (simply-annotate-annotation-mode)))
    buffer))

(defun simply-annotate-update-annotation-buffer (annotation-data overlay &optional edit-sexp)
  "Update annotation buffer with compatibility for threads and strings.
  If EDIT-SEXP is non-nil, display the raw sexp for editing."
  (let ((buffer (simply-annotate-get-annotation-buffer))
        (source-buf (current-buffer)))
    
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local simply-annotate-editing-annotation-sexp edit-sexp)
        
        (if edit-sexp
            (progn
              (emacs-lisp-mode)
              (visual-line-mode 1)
              (setq-local buffer-read-only nil)
              (buffer-disable-undo)
              (buffer-enable-undo)
              
              ;; Convert string annotation or empty draft to a new thread structure for pp
              (let* ((data-to-print (if (simply-annotate-is-thread-p annotation-data)
                                        annotation-data
                                      (simply-annotate-create-thread-with-author
                                       (simply-annotate-get-annotation-text annotation-data))))
                     (print-level nil) (print-length nil))
                (pp data-to-print (current-buffer)))
              
              ;; In sexp edit mode, the content starts at point-min.
              (setq simply-annotate-header-end-pos (point-min)) 
              (setq header-line-format "Edit annotation sexp (C-c C-c to save, C-c C-k to cancel)"))
          
          (fundamental-mode)
          (visual-line-mode 1)
          (setq-local buffer-read-only t)
          (insert (make-string (1- (length "Commands: C-c C-c (save) C-c C-k (cancel), C-g to quit\n")) ?-) "\n")
          (setq simply-annotate-header-end-pos (point))
          (if (simply-annotate-is-thread-p annotation-data)
              (insert (simply-annotate-format-thread-full annotation-data))
            (insert (simply-annotate-get-annotation-text annotation-data)))
          (setq header-line-format nil)))
      
      (goto-char simply-annotate-header-end-pos)
      (setq simply-annotate-source-buffer source-buf
            simply-annotate-current-overlay overlay))))

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

(defun simply-annotate-save-annotation-buffer ()
  "Enhanced save function that handles both strings and threads with author support,
  and raw sexp editing."
  (interactive)
  
  (let* ((overlay (if (string= (buffer-name) simply-annotate-buffer-name)
                      simply-annotate-current-overlay
                    (simply-annotate-overlay-at-point)))
         (is-draft (overlay-get overlay 'simply-annotation-draft))
         (current-data (overlay-get overlay 'simply-annotation))
         (final-data nil))
    
    (if simply-annotate-editing-annotation-sexp
        ;; In raw sexp edit mode
        (condition-case err
            (progn
              (setq final-data (read (buffer-string)))
              ;; Basic validation for thread structure
              (unless (simply-annotate-is-thread-p final-data)
                (user-error "Invalid annotation format: must be a thread alist. Sexp read: %S" final-data))
              
              ;; If it was a draft, ensure it gets a proper ID and first comment if missing
              (when is-draft
                (unless (alist-get 'id final-data)
                  (setf (alist-get 'id final-data) (format "thread-%d" (random 1000000))))
                (unless (alist-get 'comments final-data)
                  ;; Add a placeholder comment if none exists
                  (setf (alist-get 'comments final-data) 
                        (list `((author . ,(simply-annotate-get-author-for-context 'annotation))
                                (timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S"))
                                (text . "Initial comment text (edited)")
                                (type . "comment")))))))
          (error
           (message "Error saving history: %s" (error-message-string err))
           (cl-return-from simply-annotate-save-annotation-buffer)))
      ;; Not in raw sexp edit mode, proceed with text-based editing (first comment)
      (let ((content (string-trim
                      (buffer-substring simply-annotate-header-end-pos (point-max)))))
        (if (string-empty-p content)
            ;; If content is empty, treat as cancellation/removal for text mode
            (progn
              (with-current-buffer simply-annotate-source-buffer
                (if is-draft
                    (progn
                      (delete-overlay simply-annotate-draft-overlay)
                      (setq simply-annotate-draft-overlay nil))
                  (progn
                    (simply-annotate-remove-overlay overlay)
                    (simply-annotate-save-annotations)))
                (simply-annotate-update-header))
              (simply-annotate-hide-annotation-buffer)
              (message "Annotation cancelled/removed")
              (cl-return-from simply-annotate-save-annotation-buffer))
          ;; Update existing thread's first comment or create new thread
          (setq final-data (if (simply-annotate-is-thread-p current-data)
                               (let ((thread (copy-alist current-data)))
                                 (let* ((comments (alist-get 'comments thread))
                                        (first-comment (copy-alist (car comments)))
                                        (rest-comments (cdr comments)))
                                   (setf (alist-get 'text first-comment) content)
                                   (setf (alist-get 'comments thread) 
                                         (cons first-comment rest-comments)))
                                 thread)
                             (simply-annotate-create-thread-with-author content))))))
    
    (overlay-put overlay 'simply-annotation final-data)
    (overlay-put overlay 'help-echo (simply-annotate-get-annotation-summary final-data))
    
    (with-current-buffer simply-annotate-source-buffer
      (if is-draft
          (progn
            (overlay-put overlay 'simply-annotation-draft nil)
            (push overlay simply-annotate-overlays)
            (setq simply-annotate-draft-overlay nil))
        nil)
      (simply-annotate-save-annotations)
      (simply-annotate-update-header "SAVED")
      (simply-annotate-hide-annotation-buffer)
      (when (use-region-p) (deactivate-mark)))
    
    (let ((author (if (simply-annotate-is-thread-p final-data)
                      (alist-get 'author (car (alist-get 'comments final-data)))
                    simply-annotate-default-author)))
      (message "Annotation saved by %s" author))))

(defun simply-annotate-cancel-edit ()
  "Cancel editing and restore read-only mode or clean up draft."
  (interactive)
  ;; (prin1 simply-annotate-current-overlay)
  ;; (prin1 simply-annotate-source-buffer)
  ;; (revert-buffer)
  (when (and simply-annotate-current-overlay
             simply-annotate-source-buffer
             (buffer-live-p simply-annotate-source-buffer))
    (let ((is-draft (overlay-get simply-annotate-current-overlay 'simply-annotation-draft)))
      (with-current-buffer simply-annotate-source-buffer
        (if is-draft
            (progn
              (when simply-annotate-draft-overlay
                (delete-overlay simply-annotate-draft-overlay))
              (setq simply-annotate-draft-overlay nil))
          ;; If not a draft, revert the annotation buffer to display mode
          (when (and simply-annotate-current-overlay
                     (overlay-buffer simply-annotate-current-overlay))
            (simply-annotate-update-annotation-buffer
             (overlay-get simply-annotate-current-overlay 'simply-annotation)
             simply-annotate-current-overlay))))))
  (simply-annotate-hide-annotation-buffer)
  (simply-annotate-update-header "QUIT")
  (message "Edit cancelled"))

(defun simply-annotate-cleanup-draft ()
  "Clean up any draft overlays when disabling mode."
  (when simply-annotate-draft-overlay
    (delete-overlay simply-annotate-draft-overlay)
    (setq simply-annotate-draft-overlay nil)))

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

(defun simply-annotate-format-header (&optional text)
  "Enhanced header format that shows thread information."
  (let ((count (length simply-annotate-overlays)))
    (when (> count 0)
      (concat
       ;; Annotation count: e.g., " [1/5] "
       (propertize (format " ANNOTATION %s/%d"
                           (if-let ((overlay (simply-annotate-overlay-at-point)))
                               (simply-annotate-get-annotation-number overlay)
                             "")
                           count)
                   'face '(bold :height 0.9 :box nil))
       " " ; Separator

       ;; Thread status info (conditional): e.g., "[O/N:3] " (Open/Normal:3 comments)
       (if-let* ((overlay (simply-annotate-overlay-at-point))
                 (annotation-data (overlay-get overlay 'simply-annotation))
                 (thread (and (simply-annotate-is-thread-p annotation-data) annotation-data)))
           (let ((status (alist-get 'status thread))
                 (priority (alist-get 'priority thread))
                 (comment-count (length (alist-get 'comments thread))))
             (propertize
              (format "[%s/%s:%d] " (upcase status) (upcase priority) comment-count)
              'face '(:height 0.9))
             )
         "") ; Empty string if no thread status

       ;; Optional custom text (e.g., "EDITING")
       (if text (concat text " ") "")

       ;; Keybinding hints: "n p" for M-n/M-p, then "|" separator, then "j r s - a l p t o e ]" for M-s commands
       ;; This is the most compact representation using just the key characters.
       (propertize " M- " 'face '(bold :height 0.9 :box t))
       (propertize " n p " 'face '(:height 0.9)) ; Navigation: M-n (Next), M-p (Previous)
       (propertize " M-s " 'face '(bold :height 0.9 :box t))
       (propertize " j r s - a l p t o e ]" 'face '(:height 0.9 :box nil))))))

(defun simply-annotate-update-header (&optional text)
  "Enhanced header update that handles threading information."
  (let ((overlay (simply-annotate-overlay-at-point))
        (new-annotation nil))
    
    (when overlay
      (let ((annotation-data (overlay-get overlay 'simply-annotation)))
        (setq new-annotation (if (simply-annotate-is-thread-p annotation-data)
                                 (simply-annotate-get-annotation-summary annotation-data)
                               annotation-data))))
    
    (unless (equal new-annotation simply-annotate-current-annotation)
      (setq simply-annotate-current-annotation new-annotation)
      
      (when (and (not overlay) (get-buffer-window simply-annotate-buffer-name))
        (simply-annotate-hide-annotation-buffer)))
    
    ;; Always update the header format and force refresh
    (setq header-line-format (simply-annotate-format-header text))
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

(defun simply-annotate--navigate-to-overlay (overlay)
  "Enhanced navigation that handles both string and thread annotations."
  (when overlay
    (goto-char (overlay-start overlay))
    (pulse-momentary-highlight-region (overlay-start overlay)
                                      (overlay-end overlay))
    
    ;; Get annotation data
    (let ((annotation-data (overlay-get overlay 'simply-annotation)))
      ;; Update annotation buffer if it's visible
      (if (get-buffer-window simply-annotate-buffer-name)
          (progn
            (simply-annotate-update-annotation-buffer annotation-data overlay)
            (simply-annotate-show-annotation-buffer))
        ;; Show appropriate message based on annotation type
        (if (simply-annotate-is-thread-p annotation-data)
            ;; Thread annotation - show summary
            (let* ((thread annotation-data)
                   (status (alist-get 'status thread))
                   (priority (alist-get 'priority thread))
                   (comments (alist-get 'comments thread))
                   (comment-count (length comments))
                   (first-comment (car comments))
                   (first-text (alist-get 'text first-comment))
                   (preview (truncate-string-to-width first-text 60 nil nil "..."))
                   (summary (format "[%s/%s] %s (%d comment%s)"
                                    (upcase status)
                                    (upcase priority) 
                                    preview
                                    comment-count
                                    (if (= comment-count 1) "" "s"))))
              (message "%s" summary))
          ;; Simple string annotation - show text
          (message "%s" (simply-annotate-get-annotation-text annotation-data)))))
    
    (simply-annotate-update-header)))

;;;###autoload
(defun simply-annotate-show ()
  "Enhanced version of simply-annotate-show that handles threading."
  (interactive)
  (if-let ((overlay (simply-annotate-overlay-at-point)))
      (let ((annotation-data (overlay-get overlay 'simply-annotation)))
        (simply-annotate-update-annotation-buffer annotation-data overlay)
        (simply-annotate-show-annotation-buffer))))

;;;###autoload
(defun simply-annotate-next ()
  "Navigate to next annotation."
  (interactive)
  (let ((source-buffer (if (string= (buffer-name) simply-annotate-buffer-name)
                           simply-annotate-source-buffer
                         (current-buffer))))
    (if (and source-buffer (buffer-live-p source-buffer))
        (with-current-buffer source-buffer
          (if-let ((next-overlay (simply-annotate-find-annotation t t)))
              (simply-annotate--navigate-to-overlay next-overlay)
            (message "No more annotations in buffer")))
      (message "Source buffer not available"))))

;;;###autoload
(defun simply-annotate-previous ()
  "Navigate to previous annotation."
  (interactive)
  (let ((source-buffer (if (string= (buffer-name) simply-annotate-buffer-name)
                           simply-annotate-source-buffer
                         (current-buffer))))
    (if (and source-buffer (buffer-live-p source-buffer))
        (with-current-buffer source-buffer
          (if-let ((prev-overlay (simply-annotate-find-annotation nil t)))
              (simply-annotate--navigate-to-overlay prev-overlay)
            (message "No more annotations in buffer")))
      (message "Source buffer not available"))))

;;; Interactive Commands

(defun simply-annotate-smart-action ()
  "Enhanced smart action with author support."
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
            (let ((annotation-data (overlay-get existing-overlay 'simply-annotation)))
              (simply-annotate-update-header "EDITING")
              (simply-annotate-update-annotation-buffer annotation-data existing-overlay)
              (simply-annotate-show-annotation-buffer)
              (pop-to-buffer (simply-annotate-get-annotation-buffer))
              ;; Enter edit mode immediately
              (goto-char simply-annotate-header-end-pos)
              (message "Editing existing annotation (C-c C-c to save, C-c C-k to cancel, C-g to quit)")))
        
        ;; Create new annotation with author selection
        (let ((draft-overlay (simply-annotate-create-overlay start end "")))
          (simply-annotate-update-header "EDITING")
          (overlay-put draft-overlay 'simply-annotation-draft t)
          (setq simply-annotate-draft-overlay draft-overlay)
          
          ;; Show annotation buffer for editing
          (simply-annotate-update-annotation-buffer "" draft-overlay)
          (simply-annotate-show-annotation-buffer)
          (pop-to-buffer (simply-annotate-get-annotation-buffer))
          
          ;; Enter edit mode immediately
          (goto-char simply-annotate-header-end-pos)
          (message "Enter annotation text (C-c C-c to save, C-c C-k to cancel, C-g to quit)")))
      
      (deactivate-mark)))
   
   ;; Case 2: No region, but cursor is on an overlay - toggle annotation buffer
   ((simply-annotate-overlay-at-point)
    (let* ((current-prefix-arg-val (prefix-numeric-value current-prefix-arg))
           (existing-overlay (simply-annotate-overlay-at-point))
           (annotation-data (overlay-get existing-overlay 'simply-annotation))
           (buffer-window (get-buffer-window simply-annotate-buffer-name)))

      (if (= current-prefix-arg-val 4)
          ;; Edit existing annotation
          (progn
            (goto-char (overlay-start existing-overlay))
            (simply-annotate-update-header "EDITING")
            (simply-annotate-update-annotation-buffer annotation-data existing-overlay)
            (simply-annotate-show-annotation-buffer)
            (pop-to-buffer (simply-annotate-get-annotation-buffer))
            ;; Enter edit mode immediately
            (goto-char simply-annotate-header-end-pos)
            (message "Editing existing annotation (C-c C-c to save, C-c C-k to cancel)"))
        (if buffer-window
            ;; Buffer is visible - hide it (toggle off)
            (progn
              (simply-annotate-hide-annotation-buffer))
          ;; Buffer is not visible - show it (toggle on)
          (progn
            (simply-annotate-update-annotation-buffer annotation-data existing-overlay)
            (simply-annotate-show-annotation-buffer))))))

   ;; Case 3: No region, not on overlay - create new annotation on current line
   (t
    (let ((buffer-window (get-buffer-window simply-annotate-buffer-name)))
      (if buffer-window
          (progn
            (simply-annotate-hide-annotation-buffer))
        (progn
          (let* ((start (save-excursion
                          (beginning-of-line)
                          (point)))
                 (end (save-excursion
                        (end-of-line)
                        (point)))
                 (draft-overlay (simply-annotate-create-overlay start end "")))
            (overlay-put draft-overlay 'simply-annotation-draft t)
            (setq simply-annotate-draft-overlay draft-overlay)
            
            ;; Show annotation buffer for editing
            (simply-annotate-update-annotation-buffer "" draft-overlay)
            (simply-annotate-show-annotation-buffer)
            (pop-to-buffer (simply-annotate-get-annotation-buffer))
            
            ;; Enter edit mode immediately
            (goto-char simply-annotate-header-end-pos)
            (message "Enter annotation text (C-c C-c to save, C-c C-k to cancel)"))))))))

;;;###autoload
(defun simply-annotate-edit-sexp ()
  "Edit the current annotation as a raw Elisp sexp.
This allows full control over the annotation's internal data structure,
including comments, replies, status, priority, and tags."
  (interactive)
  (let ((overlay (if (string= (buffer-name) simply-annotate-buffer-name)
                     simply-annotate-current-overlay
                   (simply-annotate-overlay-at-point))))
    (if overlay
        (progn
          ;; Switch to source buffer to update its header line
          (with-current-buffer (overlay-buffer overlay)
            (goto-char (overlay-start overlay)) ; Go to the source buffer's point
            (simply-annotate-update-header "EDITING SEXP"))
          
          ;; Update the annotation buffer to display raw sexp
          (simply-annotate-update-annotation-buffer (overlay-get overlay 'simply-annotation) overlay t)
          
          ;; Ensure annotation buffer is visible and current
          (simply-annotate-show-annotation-buffer)
          (pop-to-buffer (simply-annotate-get-annotation-buffer))
          
          ;; Move point to the beginning of the editable content
          ;; In sexp edit mode, the entire buffer is the content, so point-min is correct.
          (goto-char (point-min))
          
          (message "Editing annotation sexp. C-c C-c to save, C-c C-k to cancel."))
      (message "No annotation at point to edit."))))

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

;;; Threading Commands

(defun simply-annotate-reply-to-annotation ()
  "Enhanced reply function with author selection."
  (interactive)
  (let ((overlay (if (string= (buffer-name) simply-annotate-buffer-name)
                     simply-annotate-current-overlay
                   (simply-annotate-overlay-at-point))))
    (if overlay
        (let* ((current-data (overlay-get overlay 'simply-annotation))
               (thread (if (simply-annotate-is-thread-p current-data) 
                           current-data  ; Already a thread
                         (simply-annotate-create-thread-with-author current-data)))
               (reply-text (read-string "Reply: " nil nil nil t)))
          (when (and reply-text (not (string-empty-p reply-text)))
            (simply-annotate-add-reply-with-author thread reply-text)
            (overlay-put overlay 'simply-annotation thread)
            (overlay-put overlay 'help-echo (simply-annotate-get-annotation-summary thread))
            (simply-annotate-save-annotations)
            (simply-annotate-update-header "REPLY ADDED")
            
            ;; If annotation buffer is open, refresh it
            (when (get-buffer-window simply-annotate-buffer-name)
              (simply-annotate-update-annotation-buffer thread overlay)
              (simply-annotate-show-annotation-buffer))
            
            (let ((author (alist-get 'author (car (last (alist-get 'comments thread))))))
              (message "Reply added by %s" author))))
      (message "No annotation at point"))))

(defun simply-annotate-set-annotation-status ()
  "Set the status of annotation at point."
  (interactive)
  (let ((overlay (if (string= (buffer-name) simply-annotate-buffer-name)
                     simply-annotate-current-overlay
                   (simply-annotate-overlay-at-point))))
    (if overlay
        (let* ((current-data (overlay-get overlay 'simply-annotation))
               (thread (if (simply-annotate-is-thread-p current-data)
                           current-data
                         (simply-annotate-create-thread-with-author current-data)))
               (status (completing-read "Status: " simply-annotate-thread-statuses)))
          (simply-annotate-set-thread-status thread status)
          (overlay-put overlay 'simply-annotation thread)
          (overlay-put overlay 'help-echo (simply-annotate-get-annotation-summary thread))
          (simply-annotate-save-annotations)
          (simply-annotate-update-header)
          
          ;; If annotation buffer is open, refresh it
          (when (get-buffer-window simply-annotate-buffer-name)
            (simply-annotate-update-annotation-buffer thread overlay)
            (simply-annotate-show-annotation-buffer))

          (message "Status set to: %s" status))
      (message "No annotation at point"))))

(defun simply-annotate-set-annotation-priority ()
  "Set the priority of annotation at point."
  (interactive)
  (let ((overlay (if (string= (buffer-name) simply-annotate-buffer-name)
                     simply-annotate-current-overlay
                   (simply-annotate-overlay-at-point))))
    (if overlay
        (let* ((current-data (overlay-get overlay 'simply-annotation))
               (thread (if (simply-annotate-is-thread-p current-data)
                           current-data
                         (simply-annotate-create-thread-with-author current-data)))
               (priority (completing-read "Priority: " simply-annotate-priority-levels)))
          (simply-annotate-set-thread-priority thread priority)
          (overlay-put overlay 'simply-annotation thread)
          (overlay-put overlay 'help-echo (simply-annotate-get-annotation-summary thread))
          (simply-annotate-save-annotations)
          (simply-annotate-update-header)

          ;; If annotation buffer is open, refresh it
          (when (get-buffer-window simply-annotate-buffer-name)
            (simply-annotate-update-annotation-buffer thread overlay)
            (simply-annotate-show-annotation-buffer))

          (message "Priority set to: %s" priority))
      (message "No annotation at point"))))

(defun simply-annotate-add-annotation-tag ()
  "Add a tag to annotation at point."
  (interactive)
  (let ((overlay (if (string= (buffer-name) simply-annotate-buffer-name)
                     simply-annotate-current-overlay
                   (simply-annotate-overlay-at-point))))
    (if overlay
        (let* ((current-data (overlay-get overlay 'simply-annotation))
               (thread (if (simply-annotate-is-thread-p current-data)
                           current-data
                         (simply-annotate-create-thread-with-author current-data)))
               (tag (read-string "Tag: ")))
          (when (not (string-empty-p tag))
            (simply-annotate-add-thread-tag thread tag)
            (overlay-put overlay 'simply-annotation thread)
            (overlay-put overlay 'help-echo (simply-annotate-get-annotation-summary thread))
            (simply-annotate-save-annotations)
            (simply-annotate-update-header)

            ;; If annotation buffer is open, refresh it
            (when (get-buffer-window simply-annotate-buffer-name)
              (simply-annotate-update-annotation-buffer thread overlay)
              (simply-annotate-show-annotation-buffer))
            
            (message "Tag '%s' added" tag)))
      (message "No annotation at point"))))

(defun simply-annotate-change-annotation-author ()
  "Change the author of the current annotation or specific comment in a thread."
  (interactive)
  (if-let ((overlay (simply-annotate-overlay-at-point)))
      (let* ((current-data (overlay-get overlay 'simply-annotation)))
        (if (simply-annotate-is-thread-p current-data)
            ;; Handle thread - allow changing specific comment authors
            (let* ((thread current-data)
                   (comments (alist-get 'comments thread))
                   (comment-choices (mapcar 
                                     (lambda (comment)
                                       (let ((author (alist-get 'author comment))
                                             (text (alist-get 'text comment))
                                             (type (alist-get 'type comment)))
                                         (format "%s %s: %s" 
                                                 (if (string= type "comment") "ὊC" "↳")
                                                 author
                                                 (truncate-string-to-width text 40 nil nil "..."))))
                                     comments))
                   (selected (completing-read "Change author for: " comment-choices))
                   (comment-index (cl-position selected comment-choices :test #'string=))
                   (selected-comment (nth comment-index comments))
                   (current-author (alist-get 'author selected-comment))
                   (new-author (simply-annotate-select-author current-author current-author)))
              
              (when new-author
                (setf (alist-get 'author selected-comment) new-author)
                (overlay-put overlay 'simply-annotation thread)
                (overlay-put overlay 'help-echo (simply-annotate-get-annotation-summary thread))
                (simply-annotate-save-annotations)
                (message "Author changed from %s to %s" current-author new-author)))
          
          ;; Handle simple string annotation - convert to thread with new author
          (let* ((current-text (simply-annotate-get-annotation-text current-data))
                 (new-author (simply-annotate-select-author))
                 (new-thread (simply-annotate-create-thread-with-author current-text new-author)))
            (when new-author
              (overlay-put overlay 'simply-annotation new-thread)
              (overlay-put overlay 'help-echo (simply-annotate-get-annotation-summary new-thread))
              (simply-annotate-save-annotations)
              (message "Annotation converted to thread with author: %s" new-author)))))
    (message "No annotation at point")))

;;; List and Display Functions

(defun simply-annotate-format-annotations-for-buffer (file-key annotations source-buffer buffer-name)
  "Enhanced version of format-annotations-for-buffer that handles threading."
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Add a header comment with summary stats
      (let* ((total-annotations (length annotations))
             (thread-count 0)
             (string-count 0)
             (open-count 0)
             (resolved-count 0))
        
        ;; Count different types
        (dolist (ann annotations)
          (let ((data (alist-get 'text ann)))
            (if (simply-annotate-is-thread-p data)
                (progn
                  (setq thread-count (1+ thread-count))
                  (let ((status (alist-get 'status data)))
                    (cond
                     ((string= status "resolved") (setq resolved-count (1+ resolved-count)))
                     ((string= status "closed") (setq resolved-count (1+ resolved-count)))
                     (t (setq open-count (1+ open-count))))))
              (setq string-count (1+ string-count)
                    open-count (1+ open-count)))))
        
        (insert (format "Annotations for %s:\n" file-key))
        (insert (format "Total: %d | Threads: %d | Simple: %d | Open: %d | Resolved: %d\n\n"
                        total-annotations thread-count string-count open-count resolved-count)))
      
      ;; Sort annotations by line position, then by status (open items first)
      (let ((sorted-annotations
             (sort annotations
                   (lambda (a b)
                     (let ((start-a (alist-get 'start a))
                           (start-b (alist-get 'start b))
                           (data-a (alist-get 'text a))
                           (data-b (alist-get 'text b)))
                       (if (= start-a start-b)
                           ;; Same position - prioritize by status (open first)
                           (let ((status-a (if (simply-annotate-is-thread-p data-a)
                                               (alist-get 'status data-a)
                                             "open"))
                                 (status-b (if (simply-annotate-is-thread-p data-b)
                                               (alist-get 'status data-b)
                                             "open")))
                             (cond
                              ((and (member status-a '("open" "in-progress"))
                                    (member status-b '("resolved" "closed"))) t)
                              ((and (member status-a '("resolved" "closed"))
                                    (member status-b '("open" "in-progress"))) nil)
                              (t (string< status-a status-b))))
                         ;; Different positions - sort by line number
                         (< start-a start-b)))))))
        
        ;; Format each annotation
        (dolist (ann sorted-annotations)
          (let* ((start-pos (alist-get 'start ann))
                 (end-pos (alist-get 'end ann))
                 (annotation-data (alist-get 'text ann))
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
            
            (if (simply-annotate-is-thread-p annotation-data)
                ;; Format threaded annotation
                (let* ((thread annotation-data)
                       (id (alist-get 'id thread))
                       (status (alist-get 'status thread))
                       (priority (alist-get 'priority thread))
                       (tags (alist-get 'tags thread))
                       (comments (alist-get 'comments thread))
                       (comment-count (length comments))
                       (first-comment (car comments))
                       (first-text (alist-get 'text first-comment))
                       (author (alist-get 'author first-comment))
                       (timestamp (alist-get 'timestamp first-comment)))
                  
                  ;; Location line with status indicators
                  (insert (format "%s:%d:%d [%s/%s]"
                                  file-key line-num (1+ col-num)
                                  (upcase status) (upcase priority)))
                  (when tags
                    (insert (format " #%s" (string-join tags " #"))))
                  (insert (format " (%d comment%s)\n" 
                                  comment-count 
                                  (if (= comment-count 1) "" "s")))
                  
                  ;; Thread content
                  (insert "┌─ THREAD ─┐\n")
                  (dolist (comment comments)
                    (let* ((comment-author (alist-get 'author comment))
                           (comment-timestamp (alist-get 'timestamp comment))
                           (comment-text (alist-get 'text comment))
                           (comment-type (alist-get 'type comment))
                           (prefix (if (string= comment-type "comment") "💬" "↳"))
                           (formatted-time (format-time-string "%m/%d %H:%M" 
                                                               (date-to-time comment-timestamp))))
                      (insert (format "│ %s %s (%s):\n" prefix comment-author formatted-time))
                      ;; Indent comment text
                      (let ((text-lines (split-string comment-text "\n")))
                        (dolist (line text-lines)
                          (insert (format "│   %s\n" line))))))
                  (insert "└───────────┘\n")
                  
                  ;; Source line
                  (insert (format "│ Source: %s\n\n" (string-trim line-content))))
              
              ;; Format simple string annotation (legacy format)
              (insert (format "%s:%d:%d\n" file-key line-num (1+ col-num)))
              (insert (format "%s\n" simply-annotate-annotation-separator))
              (let* ((lines (split-string (string-trim annotation-data) "\n"))
                     (indented-text (mapconcat (lambda (line) (concat "│ " line)) lines "\n")))
                (insert (format "%s\n" indented-text)))
              (insert (format "%s\n" simply-annotate-text-separator))
              (insert (format "%s\n\n" (string-trim line-content)))))))
      
      ;; Enable grep-mode for navigation
      (grep-mode)
      ;; Enhanced font-lock rules for threading
      (font-lock-add-keywords nil
                              `((,simply-annotate-annotation-block-regexp 1 '(:slant italic))
                                (,simply-annotate-header-regexp 1 '(:weight bold))
                                (,simply-annotate-annotation-separator 0 '(:underline nil))
                                (,simply-annotate-text-separator 0 '(:underline nil))
                                ;; Thread-specific highlighting
                                ("┌─ THREAD ─┐\\|└───────────┘" 0 '(:weight bold))
                                ("│.*💬.*:" 0 '(:weight bold))
                                ("│.*↳.*:" 0 '(:slant italic))
                                ("\\[OPEN/[^]]*\\]\\|\\[IN-PROGRESS/[^]]*\\]" 0 '(:weight bold))
                                ("\\[RESOLVED/[^]]*\\]\\|\\[CLOSED/[^]]*\\]" 0 '(:weight bold))
                                ("#[a-zA-Z0-9_-]+" 0 '(:weight bold)))
                              'append)
      (goto-char (point-min)))
    (current-buffer)))

;;;###autoload
(defun simply-annotate-list ()
  "Enhanced version of simply-annotate-list that handles threading."
  (interactive)
  (let ((buffer-name "*Annotations*")
        (existing-window (get-buffer-window "*Annotations*")))
    ;; Buffer not visible - show annotations
    (if simply-annotate-overlays
        (let* ((source-buffer (current-buffer))
               (source-file (or (buffer-file-name) (buffer-name)))
               (annotations (simply-annotate-serialize-annotations))
               (annotation-buffer (simply-annotate-format-annotations-for-buffer
                                   source-file annotations source-buffer buffer-name)))
          (pop-to-buffer annotation-buffer)
          (goto-char (point-min)))
      (message "No annotations in buffer"))))

(defun simply-annotate-display-file-annotations (file-key annotations)
  "Enhanced version that handles threading in cross-file display."
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

;;; Org-mode Integration

(defun simply-annotate-thread-to-org (thread)
  "Convert a thread to org-mode format."
  (let* ((id (alist-get 'id thread))
         (status (alist-get 'status thread))
         (priority (alist-get 'priority thread))
         (tags (alist-get 'tags thread))
         (comments (alist-get 'comments thread))
         (first-comment (car comments))
         (first-text (alist-get 'text first-comment))
         (replies (cdr comments)))
    
    (concat
     (format "* TODO %s\n" (car (split-string first-text "\n")))
     (format ":PROPERTIES:\n")
     (format ":ID: %s\n" id)
     (format ":STATUS: %s\n" status)
     (format ":PRIORITY: %s\n" priority)
     (when tags (format ":TAGS: %s\n" (string-join tags " ")))
     (format ":CREATED: %s\n" (alist-get 'timestamp first-comment))
     (format ":AUTHOR: %s\n" (alist-get 'author first-comment))
     (format ":END:\n\n")
     (when (> (length first-text) (length (car (split-string first-text "\n"))))
       (concat (string-join (cdr (split-string first-text "\n")) "\n") "\n\n"))
     
     ;; Add replies as sub-entries
     (when replies
       (mapconcat
        (lambda (reply)
          (format "** Reply by %s (%s)\n%s\n\n"
                  (alist-get 'author reply)
                  (alist-get 'timestamp reply)
                  (alist-get 'text reply)))
        replies
        "")))))

(defun simply-annotate-export-to-org-file (filename)
  "Export all annotations in current buffer to an org file."
  (interactive "FExport annotations to org file: ")
  (let* ((file-key (simply-annotate-file-key))
         (db (simply-annotate-load-database))
         (annotations (when db (alist-get file-key db nil nil #'string=))))
    
    (if (not annotations)
        (message "No annotations to export")
      (with-temp-buffer
        (org-mode)
        (insert (format "#+TITLE: Annotations for %s\n" file-key))
        (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d")))
        
        ;; Convert annotations to threads if they aren't already
        (dolist (ann annotations)
          (let* ((text (alist-get 'text ann))
                 (thread (if (simply-annotate-is-thread-p text)
                             text  ; Already a thread
                           (simply-annotate-create-thread (simply-annotate-get-annotation-text text)))))
            (insert (simply-annotate-thread-to-org thread))
            (insert "\n")))
        
        (write-file filename)
        (message "Annotations exported to %s" filename)))))

;;; Annotation Buffer Mode

(defvar simply-annotate-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'simply-annotate-save-annotation-buffer)
    (define-key map (kbd "C-c C-k") #'simply-annotate-cancel-edit)
    (define-key map (kbd "C-g") #'simply-annotate-cancel-edit)
    (define-key map (kbd "M-s r") #'simply-annotate-reply-to-annotation)
    (define-key map (kbd "M-s s") #'simply-annotate-set-annotation-status)
    (define-key map (kbd "M-s p") #'simply-annotate-set-annotation-priority)
    (define-key map (kbd "M-s t") #'simply-annotate-add-annotation-tag)
    (define-key map (kbd "M-s o") #'simply-annotate-export-to-org-file)
    (define-key map (kbd "M-s e") #'simply-annotate-edit-sexp) ; New keybinding
    (define-key map (kbd "M-p") #'simply-annotate-previous)
    (define-key map (kbd "M-n") #'simply-annotate-next)
    map)
  "Keymap for simply-annotate annotation buffer.")

(define-derived-mode simply-annotate-annotation-mode fundamental-mode "Annotation"
  "Mode for displaying and editing annotations."
  (visual-line-mode 1))

;;; Minor Mode

(defvar simply-annotate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-s j") #'simply-annotate-smart-action)
    (define-key map (kbd "M-s r") #'simply-annotate-reply-to-annotation)
    (define-key map (kbd "M-s s") #'simply-annotate-set-annotation-status)
    (define-key map (kbd "M-s -") #'simply-annotate-remove)
    (define-key map (kbd "M-s a") #'simply-annotate-change-annotation-author)
    (define-key map (kbd "M-s l") #'simply-annotate-list)
    
    (define-key map (kbd "M-s 0") #'simply-annotate-show-all)
    
    (define-key map (kbd "M-s p") #'simply-annotate-set-annotation-priority)
    (define-key map (kbd "M-s t") #'simply-annotate-add-annotation-tag)
    (define-key map (kbd "M-s o") #'simply-annotate-export-to-org-file)
    (define-key map (kbd "M-s e") #'simply-annotate-edit-sexp) ; New keybinding
    (define-key map (kbd "M-s ]") #'simply-annotate-cycle-display-style)
    (define-key map (kbd "M-p") #'simply-annotate-previous)
    (define-key map (kbd "M-n") #'simply-annotate-next)
    map)
  "Keymap for `simply-annotate-mode'.")

;;;###autoload
(define-minor-mode simply-annotate-mode
  "Enhanced annotation mode with threading support."
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
