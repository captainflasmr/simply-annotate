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

(defcustom simply-annotate-display-style 'both
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

;; Threading Variables

(defvar simply-annotate-session-author nil
  "Author chosen for current session (when using 'first-only mode).")

(defvar simply-annotate-file-authors nil
  "Alist of (file-key . author) for per-file author memory.")

;;; Utility Functions

(defun simply-annotate-get-annotation-text (annotation-data)
  "Extract display text from annotation data (string or thread)."
  (if (stringp annotation-data)
      annotation-data
    (when-let ((comments (alist-get 'comments annotation-data)))
      (alist-get 'text (car comments)))))

(defun simply-annotate-get-annotation-summary (annotation-data)
  "Get a summary for display (help-echo, header, etc.)."
  (if (stringp annotation-data)
      annotation-data
    (simply-annotate-format-thread-summary annotation-data)))

(defun simply-annotate-is-thread-p (annotation-data)
  "Check if annotation data is a thread structure."
  (and (listp annotation-data)
       (alist-get 'id annotation-data)
       (alist-get 'comments annotation-data)))

(defun simply-annotate--symbol-name-or-string (val)
  "Convert VAL to string if it's a symbol, otherwise return as is.
Used for file-keys which can be strings or symbols (e.g. nil converted to \"nil\")."
  (if (symbolp val) (symbol-name val) val))

;;; Core Functions

(defun simply-annotate-file-key ()
  "Get unique key for current file."
  (or (buffer-file-name) (buffer-name)))

(defun simply-annotate-load-database ()
  "Load annotations from database file."
  (when (file-exists-p simply-annotate-file)
    (with-temp-buffer
      (insert-file-contents simply-annotate-file)
      (ignore-errors (car (read-from-string (string-trim (buffer-string)))))))) ; Trim before read

(defun simply-annotate-save-database (db)
  "Save database to file. Argument DB database."
  (with-temp-file simply-annotate-file
    (insert ";;; Simply Annotate Database\n;;; This file is auto-generated. Do not edit manually.\n\n")
    (prin1 db (current-buffer))
    (insert "\n")))

(defun simply-annotate-update-database (file-key annotations)
  "Update database with ANNOTATIONS for file.
Only writes if there are actual annotations or if removing existing entries."
  (let* ((db (or (simply-annotate-load-database) '()))
         (actual-file-key (simply-annotate--symbol-name-or-string file-key)))
    (if annotations
        (setf (alist-get actual-file-key db nil nil #'string=) annotations)
      (setq db (cl-remove-if (lambda (entry) (string= (car entry) actual-file-key)) db)))
    (if db
        (simply-annotate-save-database db)
      (when (file-exists-p simply-annotate-file)
        (delete-file simply-annotate-file))))) ; Delete file if DB becomes empty

;;; Display and Fringe Functions

(defun simply-annotate-add-fringe-indicator (overlay)
  "Add fringe indicator to OVERLAY."
  (let* ((bitmap (pcase simply-annotate-fringe-indicator
                   ('left-triangle 'left-triangle)
                   ('right-triangle 'right-triangle)
                   ('filled-rectangle 'filled-rectangle)
                   ('custom 'simply-annotate-fringe-bitmap)
                   (_ 'left-triangle)))
         (fringe-spec `(left-fringe ,bitmap ,simply-annotate-fringe-face)))
    (overlay-put overlay 'before-string (propertize " " 'display fringe-spec))))

(defun simply-annotate--apply-display-style (overlay annotation-data)
  "Apply display style to OVERLAY based on `simply-annotate-display-style'."
  ;; Clear existing display properties
  (overlay-put overlay 'face nil)
  (overlay-put overlay 'before-string nil)
  ;; Reapply based on current style
  (pcase simply-annotate-display-style
    ('highlight (overlay-put overlay 'face simply-annotate-highlight-face))
    ('fringe (simply-annotate-add-fringe-indicator overlay))
    ('both (overlay-put overlay 'face simply-annotate-highlight-face)
           (simply-annotate-add-fringe-indicator overlay))))

(defun simply-annotate-update-display-style ()
  "Update display style for all existing annotations."
  (interactive)
  (dolist (overlay simply-annotate-overlays)
    (simply-annotate--apply-display-style overlay (overlay-get overlay 'simply-annotation)))
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
  "Create annotation overlay with configurable display style."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'simply-annotation text)
    (overlay-put overlay 'help-echo (simply-annotate-get-annotation-summary text))
    (overlay-put overlay 'mouse-face 'highlight)
    (simply-annotate--apply-display-style overlay text) ; Use helper
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
    (if (eq simply-annotate-display-style 'fringe)
        ;; In fringe mode, check the entire current line
        (save-excursion
          (when check-pos (goto-char check-pos))
          (let ((line-start (line-beginning-position))
                (line-end (line-end-position)))
            (cl-find-if (lambda (ov)
                          (and (overlay-get ov 'simply-annotation)
                               (>= (overlay-start ov) line-start)
                               (<= (overlay-start ov) line-end)))
                        simply-annotate-overlays)))
      ;; In highlight mode, check only at the specific position
      (cl-find-if (lambda (ov) (overlay-get ov 'simply-annotation))
                  (overlays-at check-pos)))))

;;; Threading Functions

(defun simply-annotate-create-thread (text &optional author priority tags)
  "Create a new annotation thread. Handles author selection internally."
  (let* ((chosen-author (or author (simply-annotate-get-author-for-context 'annotation)))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))
         (id (format "thread-%d" (random 1000000))))
    (simply-annotate-remember-author chosen-author) ; Remember the author choice
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
  "Add a reply to an existing thread. Handles author selection internally."
  (let* ((chosen-author (or author (simply-annotate-get-author-for-context 'reply)))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))
         (reply `((author . ,chosen-author)
                  (timestamp . ,timestamp)
                  (text . ,reply-text)
                  (type . "reply")))
         (comments (alist-get 'comments thread)))
    (simply-annotate-remember-author chosen-author) ; Remember the author choice
    (setf (alist-get 'comments thread) (append comments (list reply)))
    thread))

(defun simply-annotate-set-thread-status (thread status)
  "Set the status of a thread."
  (when (member status simply-annotate-thread-statuses)
    (setf (alist-get 'status thread) status))
  thread)

(defun simply-annotate-set-thread-priority (thread priority)
  "Set the priority of a thread."
  (when (member priority simply-annotate-priority-levels)
    (setf (alist-get 'priority thread) priority))
  thread)

(defun simply-annotate-add-thread-tag (thread tag)
  "Add a tag to a thread."
  (setf (alist-get 'tags thread) (cl-pushnew tag (alist-get 'tags thread) :test #'string=))
  thread)

(defun simply-annotate-format-thread-summary (thread)
  "Format a brief summary of the thread for display."
  (let* ((status (alist-get 'status thread))
         (priority (alist-get 'priority thread))
         (comments (alist-get 'comments thread))
         (comment-count (length comments))
         (first-comment (car comments))
         (preview (truncate-string-to-width (alist-get 'text first-comment) 40 nil nil "...")))
    (format "[%s/%s] %s (%d comment%s)"
            (upcase status) (upcase priority) preview
            comment-count (if (= comment-count 1) "" "s"))))

(defun simply-annotate-format-thread-full (thread)
  "Format complete thread for display in annotation buffer."
  (let* ((id (alist-get 'id thread))
         (status (alist-get 'status thread))
         (priority (alist-get 'priority thread))
         (tags (alist-get 'tags thread))
         (comments (alist-get 'comments thread))
         (header (format "┌─ Thread: %s [%s/%s]" id status priority))
         (authors (delete-dups (mapcar (lambda (c) (alist-get 'author c)) comments)))
         (author-info (when (> (length authors) 1) (format " (%d authors)" (length authors)))))
    (concat
     header (when tags (format " Tags: %s" (string-join tags ", "))) author-info "\n"
     (mapconcat
      (lambda (comment)
        (let ((author (alist-get 'author comment))
              (timestamp (alist-get 'timestamp comment))
              (text (alist-get 'text comment))
              (type (alist-get 'type comment)))
          (format "│ %s %s (%s):\n│ %s"
                  (if (string= type "comment") "ὊC" "↳")
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
         (default-author (or simply-annotate-default-author (car simply-annotate-author-list)))
         (remembered-author (when simply-annotate-remember-author-per-file
                              (alist-get file-key simply-annotate-file-authors nil nil #'string=)))
         (initial-author (or remembered-author simply-annotate-session-author default-author)))
    (cond
     ((null simply-annotate-prompt-for-author) initial-author)
     ((eq simply-annotate-prompt-for-author 'always) (simply-annotate-select-author initial-author current-author))
     ((eq simply-annotate-prompt-for-author 'first-only)
      (or simply-annotate-session-author
          (setq simply-annotate-session-author (simply-annotate-select-author initial-author current-author))))
     ((eq simply-annotate-prompt-for-author 'threads-only)
      (if (eq context 'reply) (simply-annotate-select-author initial-author current-author) initial-author))
     (t initial-author))))

(defun simply-annotate-select-author (&optional default-author current-author)
  "Prompt user to select an author from the configured list.
DEFAULT-AUTHOR is pre-selected. CURRENT-AUTHOR is shown when editing."
  (let* ((authors (if (> (length simply-annotate-author-list) 1)
                      simply-annotate-author-list
                    (append simply-annotate-author-list (list "Other..."))))
         (prompt (format "Author%s: " (if current-author (format " (current: %s)" current-author) "")))
         (choice (completing-read prompt authors nil nil nil nil (or default-author (car authors)))))
    (if (string= choice "Other...")
        (let ((custom-author (read-string "Enter author name: " (or default-author ""))))
          (when (and custom-author (not (string-empty-p custom-author)))
            (unless (member custom-author simply-annotate-author-list)
              (customize-save-variable 'simply-annotate-author-list
                                       (append simply-annotate-author-list (list custom-author))))
            custom-author))
      choice)))

(defun simply-annotate-remember-author (author)
  "Remember the chosen author for the current file if enabled."
  (when (and simply-annotate-remember-author-per-file author)
    (setf (alist-get (simply-annotate-file-key) simply-annotate-file-authors nil nil #'string=) author)))

(defun simply-annotate-reset-session-author ()
  "Reset the session author (useful when switching contexts)."
  (interactive)
  (setq simply-annotate-session-author nil)
  (message "Session author reset. Next annotation will prompt for author."))

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
      (when (and start end text (<= start (point-max)) (<= end (point-max)) (> end start))
        (push (simply-annotate-create-overlay start end text) simply-annotate-overlays)))))

(defun simply-annotate-save-annotations ()
  "Save current buffer's annotations."
  (let ((file-key (simply-annotate-file-key)))
    (when (and file-key simply-annotate-mode)
      (simply-annotate-update-database file-key (simply-annotate-serialize-annotations)))))

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

(defun simply-annotate-update-annotation-buffer (annotation-data overlay)
  "Update annotation buffer with compatibility for threads and strings."
  (let ((buffer (simply-annotate-get-annotation-buffer))
        (source-buf (current-buffer))
        (commands "Commands: C-c C-c (save) C-c C-k (cancel), C-g to quit\n"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize commands 'face 'italic))
        (insert (make-string (1- (length commands)) ?-) "\n")
        (setq simply-annotate-header-end-pos (point))
        (insert (if (simply-annotate-is-thread-p annotation-data)
                    (simply-annotate-format-thread-full annotation-data)
                  (simply-annotate-get-annotation-text annotation-data)))
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
  (when-let ((window (get-buffer-window simply-annotate-buffer-name)))
    (delete-window window)))

(defun simply-annotate-save-annotation-buffer ()
  "Enhanced save function that handles both strings and threads with author support."
  (interactive)
  (when (and simply-annotate-current-overlay simply-annotate-source-buffer (buffer-live-p simply-annotate-source-buffer))
    (let* ((content (string-trim (buffer-substring simply-annotate-header-end-pos (point-max))))
           (overlay simply-annotate-current-overlay)
           (is-draft (overlay-get overlay 'simply-annotation-draft))
           (current-data (overlay-get overlay 'simply-annotation)))
      (if (string-empty-p content)
          (progn
            (with-current-buffer simply-annotate-source-buffer
              (when is-draft (delete-overlay simply-annotate-draft-overlay) (setq simply-annotate-draft-overlay nil))
              (unless is-draft (simply-annotate-remove-overlay overlay)) ; Only remove if not a draft
              (simply-annotate-save-annotations)
              (simply-annotate-update-header))
            (simply-annotate-hide-annotation-buffer)
            (message "Annotation cancelled/removed"))
        (let ((final-data
               (if (simply-annotate-is-thread-p current-data)
                   ;; Update existing thread's first comment
                   (let ((thread (copy-alist current-data)))
                     (setf (alist-get 'text (car (alist-get 'comments thread))) content)
                     thread)
                 ;; Create new thread or keep as string based on user preference
                 (if is-draft
                     (simply-annotate-create-thread content) ; Use consolidated create-thread
                   content))))
          (overlay-put overlay 'simply-annotation final-data)
          (overlay-put overlay 'help-echo (simply-annotate-get-annotation-summary final-data))
          (with-current-buffer simply-annotate-source-buffer
            (when is-draft
              (overlay-put overlay 'simply-annotation-draft nil)
              (push overlay simply-annotate-overlays)
              (setq simply-annotate-draft-overlay nil))
            (simply-annotate-save-annotations)
            (simply-annotate-update-header "SAVED")
            (simply-annotate-hide-annotation-buffer)
            (when (use-region-p) (deactivate-mark)))
          (message "Annotation saved by %s"
                   (if (simply-annotate-is-thread-p final-data)
                       (alist-get 'author (car (alist-get 'comments final-data)))
                     simply-annotate-default-author)))))))

(defun simply-annotate-cancel-edit ()
  "Cancel editing and restore read-only mode."
  (interactive)
  (when (and simply-annotate-current-overlay simply-annotate-source-buffer (buffer-live-p simply-annotate-source-buffer))
    (with-current-buffer simply-annotate-source-buffer
      (when simply-annotate-draft-overlay
        (delete-overlay simply-annotate-draft-overlay)
        (setq simply-annotate-draft-overlay nil))
      (when (and simply-annotate-current-overlay (overlay-buffer simply-annotate-current-overlay))
        (simply-annotate-update-annotation-buffer
         (overlay-get simply-annotate-current-overlay 'simply-annotation)
         simply-annotate-current-overlay))))
  (simply-annotate-hide-annotation-buffer)
  (simply-annotate-update-header "QUIT")
  (message "Edit cancelled"))

;;; Header-line Functions

(defun simply-annotate-get-annotation-number (target-overlay)
  "Get the position number of TARGET-OVERLAY in the sorted list of annotations."
  (when target-overlay
    (with-current-buffer (overlay-buffer target-overlay)
      (cl-position target-overlay (seq-sort-by #'overlay-start #'< simply-annotate-overlays) :test #'eq :start 0 :key #'identity)
      (1+ (cl-position target-overlay (seq-sort-by #'overlay-start #'< simply-annotate-overlays) :test #'eq)))))

(defun simply-annotate-format-header (&optional text)
  "Enhanced header format that shows thread information."
  (let ((count (length simply-annotate-overlays)))
    (when (> count 0)
      (let* ((overlay (simply-annotate-overlay-at-point))
             (annotation-data (when overlay (overlay-get overlay 'simply-annotation)))
             (status-info (when (and overlay (simply-annotate-is-thread-p annotation-data))
                            (let* ((thread annotation-data)
                                   (status (alist-get 'status thread))
                                   (priority (alist-get 'priority thread))
                                   (comment-count (length (alist-get 'comments thread))))
                              (format " [%s/%s:%d] "
                                      (upcase status) (upcase priority) comment-count)))))
        (concat
         (propertize (format " %s/%d " (if overlay (simply-annotate-get-annotation-number overlay) "") count)
                     'face '(:box t))
         (or status-info " ")
         (if text (concat text " ") " ")
         (format "%s %s %s %s"
                 (propertize " M- " 'face '(:box t))
                 (propertize "Prev(p) Next(n)" 'face 'bold)
                 (propertize " M-s " 'face '(:box t))
                 (propertize "Action(j) Reply(r) Status(s) Delete(-) List(l)" 'face 'bold)))))))

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
  (seq-sort-by #'overlay-start #'< simply-annotate-overlays))

(defun simply-annotate-find-annotation (forward &optional wrap)
  "Find next/previous annotation. FORWARD t for next, nil for previous.
Optional argument WRAP ."
  (let* ((pos (point))
         (overlays (if forward
                       (simply-annotate-get-sorted-overlays)
                     (reverse (simply-annotate-get-sorted-overlays))))
         (test-fn (if forward
                      (lambda (ov) (> (overlay-start ov) pos))
                    (lambda (ov) (< (overlay-end ov) pos))))
         (found (cl-find-if test-fn overlays)))
    (or found (and wrap overlays (car overlays)))))

(defun simply-annotate--navigate-to-overlay (overlay)
  "Enhanced navigation that handles both string and thread annotations."
  (when overlay
    (goto-char (overlay-start overlay))
    (pulse-momentary-highlight-region (overlay-start overlay) (overlay-end overlay))
    (let ((annotation-data (overlay-get overlay 'simply-annotation)))
      (if (get-buffer-window simply-annotate-buffer-name)
          (simply-annotate-update-annotation-buffer annotation-data overlay)
        (message "%s" (simply-annotate-get-annotation-summary annotation-data)))) ; Use summary for message
    (simply-annotate-update-header)))

;;;###autoload
(defun simply-annotate-show ()
  "Enhanced version of simply-annotate-show that handles threading."
  (interactive)
  (if-let ((overlay (simply-annotate-overlay-at-point)))
      (progn
        (simply-annotate-update-annotation-buffer (overlay-get overlay 'simply-annotation) overlay)
        (simply-annotate-show-annotation-buffer))))

;;;###autoload
(defun simply-annotate-next ()
  "Navigate to next annotation."
  (interactive)
  (let ((source-buffer (if (string= (buffer-name) simply-annotate-buffer-name)
                           simply-annotate-source-buffer (current-buffer))))
    (when source-buffer
      (with-current-buffer source-buffer
        (if-let ((next-overlay (simply-annotate-find-annotation t t)))
            (simply-annotate--navigate-to-overlay next-overlay)
          (message "No more annotations in buffer"))))))

;;;###autoload
(defun simply-annotate-previous ()
  "Navigate to previous annotation."
  (interactive)
  (let ((source-buffer (if (string= (buffer-name) simply-annotate-buffer-name)
                           simply-annotate-source-buffer (current-buffer))))
    (when source-buffer
      (with-current-buffer source-buffer
        (if-let ((prev-overlay (simply-annotate-find-annotation nil t)))
            (simply-annotate--navigate-to-overlay prev-overlay)
          (message "No more annotations in buffer"))))))

;;; Interactive Commands

(defun simply-annotate--edit-annotation-in-buffer (annotation-data overlay &optional is-draft)
  "Helper to open annotation buffer for editing."
  (simply-annotate-update-header "EDITING")
  (overlay-put overlay 'simply-annotation-draft is-draft)
  (setq simply-annotate-draft-overlay overlay)
  (simply-annotate-update-annotation-buffer annotation-data overlay)
  (simply-annotate-show-annotation-buffer)
  (pop-to-buffer (simply-annotate-get-annotation-buffer))
  (goto-char simply-annotate-header-end-pos)
  (message "Enter annotation text (C-c C-c to save, C-c C-k to cancel)"))

(defun simply-annotate-smart-action ()
  "Enhanced smart action with author support."
  (interactive)
  (cond
   ((use-region-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (existing-overlay (simply-annotate-overlay-at-point start)))
      (deactivate-mark)
      (if existing-overlay
          (simply-annotate--edit-annotation-in-buffer
           (overlay-get existing-overlay 'simply-annotation) existing-overlay)
        (simply-annotate--edit-annotation-in-buffer "" (simply-annotate-create-overlay start end "") t))))
   ((simply-annotate-overlay-at-point)
    (let* ((current-prefix-arg-val (prefix-numeric-value current-prefix-arg))
           (existing-overlay (simply-annotate-overlay-at-point))
           (annotation-data (overlay-get existing-overlay 'simply-annotation))
           (buffer-window (get-buffer-window simply-annotate-buffer-name)))
      (if (= current-prefix-arg-val 4) ; C-u prefix
          (simply-annotate--edit-annotation-in-buffer annotation-data existing-overlay)
        (if buffer-window
            (simply-annotate-hide-annotation-buffer)
          (progn
            (simply-annotate-update-annotation-buffer annotation-data existing-overlay)
            (simply-annotate-show-annotation-buffer))))))
   (t ; No region, not on overlay - create new annotation on current line
    (let ((buffer-window (get-buffer-window simply-annotate-buffer-name)))
      (if buffer-window
          (simply-annotate-hide-annotation-buffer)
        (let* ((start (line-beginning-position))
               (end (line-end-position)))
          (simply-annotate--edit-annotation-in-buffer "" (simply-annotate-create-overlay start end "") t)))))))

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

;;; Threading Commands

(defmacro simply-annotate--with-thread-action (action-form message-format &rest message-args)
  "Macro to simplify common thread modification pattern.
ACTION-FORM should be a form that evaluates to the modified thread.
MESSAGE-FORMAT and MESSAGE-ARGS are for `message'."
  `(let ((overlay (if (string= (buffer-name) simply-annotate-buffer-name)
                      simply-annotate-current-overlay
                    (simply-annotate-overlay-at-point))))
     (if overlay
         (let* ((current-data (overlay-get overlay 'simply-annotation))
                (thread (if (simply-annotate-is-thread-p current-data)
                            current-data
                          (simply-annotate-create-thread ; Convert to thread if not already
                           (simply-annotate-get-annotation-text current-data)))))
           (setq thread ,action-form) ; Perform the action, updating 'thread'
           (overlay-put overlay 'simply-annotation thread)
           (overlay-put overlay 'help-echo (simply-annotate-get-annotation-summary thread))
           (simply-annotate-save-annotations)
           (simply-annotate-update-header)
           (when (get-buffer-window simply-annotate-buffer-name) ; Refresh annotation buffer if open
             (simply-annotate-update-annotation-buffer thread overlay)
             (simply-annotate-show-annotation-buffer))
           (message message-format ,@message-args))
       (message "No annotation at point"))))

(defun simply-annotate-reply-to-annotation ()
  "Enhanced reply function with author selection."
  (interactive)
  (simply-annotate--with-thread-action
   (let ((reply-text (read-string "Reply: " nil nil nil t)))
     (when (and reply-text (not (string-empty-p reply-text)))
       (simply-annotate-add-reply thread reply-text))
     thread)
   "Reply added by %s" (alist-get 'author (car (last (alist-get 'comments thread))))))

(defun simply-annotate-set-annotation-status ()
  "Set the status of annotation at point."
  (interactive)
  (simply-annotate--with-thread-action
   (let ((status (completing-read "Status: " simply-annotate-thread-statuses)))
     (simply-annotate-set-thread-status thread status)
     status) ; Return status for message
   "Status set to: %s" status))

(defun simply-annotate-set-annotation-priority ()
  "Set the priority of annotation at point."
  (interactive)
  (simply-annotate--with-thread-action
   (let ((priority (completing-read "Priority: " simply-annotate-priority-levels)))
     (simply-annotate-set-thread-priority thread priority)
     priority) ; Return priority for message
   "Priority set to: %s" priority))

(defun simply-annotate-add-annotation-tag ()
  "Add a tag to annotation at point."
  (interactive)
  (simply-annotate--with-thread-action
   (let ((tag (read-string "Tag: ")))
     (when (not (string-empty-p tag))
       (simply-annotate-add-thread-tag thread tag))
     tag) ; Return tag for message
   "Tag '%s' added" tag))

(defun simply-annotate-change-annotation-author ()
  "Change the author of the current annotation or specific comment in a thread."
  (interactive)
  (let ((overlay (simply-annotate-overlay-at-point)))
    (if overlay
        (let* ((current-data (overlay-get overlay 'simply-annotation))
               (thread (if (simply-annotate-is-thread-p current-data)
                           current-data
                         (simply-annotate-create-thread (simply-annotate-get-annotation-text current-data)))))
          (if (simply-annotate-is-thread-p thread)
              (let* ((comments (alist-get 'comments thread))
                     (comment-choices (mapcar (lambda (comment)
                                                (format "%s %s: %s"
                                                        (if (string= (alist-get 'type comment) "comment") "ὊC" "↳")
                                                        (alist-get 'author comment)
                                                        (truncate-string-to-width (alist-get 'text comment) 40 nil nil "...")))
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
            ;; Should not happen with the 'thread' conversion above, but as a fallback
            (message "Annotation could not be converted to thread for author change.")))
      (message "No annotation at point"))))

;;; List and Display Functions

(defun simply-annotate-format-annotations-for-buffer (file-key annotations source-buffer buffer-name)
  "Enhanced version of format-annotations-for-buffer that handles threading."
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Add a header comment with summary stats
      (cl-loop with total = (length annotations)
               with threads = 0
               with simple = 0
               with open = 0
               with resolved = 0
               for ann in annotations do
               (let ((data (alist-get 'text ann)))
                 (if (simply-annotate-is-thread-p data)
                     (progn
                       (cl-incf threads)
                       (pcase (alist-get 'status data)
                         ((or "resolved" "closed") (cl-incf resolved))
                         (_ (cl-incf open))))
                   (cl-incf simple)
                   (cl-incf open)))
               finally
               (insert (format "Annotations for %s:\n" file-key))
               (insert (format "Total: %d | Threads: %d | Simple: %d | Open: %d | Resolved: %d\n\n"
                               total threads simple open resolved)))

      ;; Sort annotations by line position, then by status (open items first)
      (let ((sorted-annotations
             (seq-sort annotations
                       (lambda (a b)
                         (let* ((start-a (alist-get 'start a))
                                (start-b (alist-get 'start b))
                                (data-a (alist-get 'text a))
                                (data-b (alist-get 'text b))
                                (status-a (if (simply-annotate-is-thread-p data-a) (alist-get 'status data-a) "open"))
                                (status-b (if (simply-annotate-is-thread-p data-b) (alist-get 'status data-b) "open")))
                           (cond
                            ((= start-a start-b)
                             (cl-case status-a
                               ((or "open" "in-progress") (cl-case status-b
                                                            ((or "resolved" "closed") t)
                                                            (_ (string< status-a status-b))))
                               (_ (cl-case status-b
                                    ((or "open" "in-progress") nil)
                                    (_ (string< status-a status-b))))))
                            (t (< start-a start-b))))))))

        ;; Format each annotation
        (dolist (ann sorted-annotations)
          (let* ((start-pos (alist-get 'start ann))
                 (end-pos (alist-get 'end ann))
                 (annotation-data (alist-get 'text ann))
                 (line-num (when source-buffer (with-current-buffer source-buffer (save-excursion (goto-char (min start-pos (point-max))) (line-number-at-pos)))))
                 (col-num (when source-buffer (with-current-buffer source-buffer (save-excursion (goto-char (min start-pos (point-max))) (current-column)))))
                 (line-content (when source-buffer
                                 (with-current-buffer source-buffer
                                   (save-excursion
                                     (goto-char (min start-pos (point-max)))
                                     (buffer-substring-no-properties
                                      (min start-pos (point-max)) (min end-pos (line-end-position) (point-max))))))))

            (if (simply-annotate-is-thread-p annotation-data)
                (let* ((thread annotation-data)
                       (id (alist-get 'id thread))
                       (status (alist-get 'status thread))
                       (priority (alist-get 'priority thread))
                       (tags (alist-get 'tags thread))
                       (comments (alist-get 'comments thread))
                       (comment-count (length comments)))
                  (insert (format "%s:%d:%d [%s/%s]%s (%d comment%s)\n"
                                  file-key line-num (1+ col-num)
                                  (upcase status) (upcase priority)
                                  (if tags (format " #%s" (string-join tags " ")) "")
                                  comment-count (if (= comment-count 1) "" "s")))
                  (insert "┌─ THREAD ─┐\n")
                  (dolist (comment comments)
                    (insert (format "│ %s %s (%s):\n"
                                    (if (string= (alist-get 'type comment) "comment") "ὊC" "↳")
                                    (alist-get 'author comment)
                                    (format-time-string "%m/%d %H:%M" (date-to-time (alist-get 'timestamp comment))))
                            (mapconcat (lambda (line) (concat "│   " line))
                                       (split-string (alist-get 'text comment) "\n") "\n")
                            "\n"))
                  (insert "└───────────┘\n")
                  (insert (format "│ Source: %s\n\n" (string-trim line-content))))
              (insert (format "%s:%d:%d\n%s\n%s\n%s\n%s\n\n"
                              file-key line-num (1+ col-num)
                              simply-annotate-annotation-separator
                              (mapconcat (lambda (line) (concat "│ " line))
                                         (split-string (string-trim annotation-data) "\n") "\n")
                              simply-annotate-text-separator
                              (string-trim line-content)))))))
      (grep-mode)
      (font-lock-add-keywords nil
                              `((,simply-annotate-annotation-block-regexp 1 '(:slant italic))
                                (,simply-annotate-header-regexp 1 '(:weight bold))
                                (,simply-annotate-annotation-separator 0 '(:underline nil))
                                (,simply-annotate-text-separator 0 '(:underline nil))
                                ("┌─ THREAD ─┐\\|└───────────┘" 0 '(:weight bold))
                                ("│.*ὊC.*:" 0 '(:weight bold))
                                ("│.*↳.*:" 0 '(:slant italic))
                                ("\\[OPEN/[^]]*\\]\\|\\[IN-PROGRESS/[^]]*\\]" 0 '(:weight bold))
                                ("\\[RESOLVED/[^]]*\\]\\|\\[CLOSED/[^]]*\\]" 0 '(:weight bold))
                                ("#[a-zA-Z0-9_-]+" 0 '(:weight bold)))
                              'append)
      (goto-char (point-min)))))

;;;###autoload
(defun simply-annotate-list ()
  "Enhanced version of simply-annotate-list that handles threading."
  (interactive)
  (let ((buffer-name "*Annotations*"))
    (if simply-annotate-overlays
        (let* ((source-file (or (buffer-file-name) (buffer-name)))
               (annotations (simply-annotate-serialize-annotations)))
          (pop-to-buffer (simply-annotate-format-annotations-for-buffer
                          source-file annotations (current-buffer) buffer-name))
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
    (pop-to-buffer annotation-buffer)
    (goto-char (point-min))
    (message "Source file opened in other window. Navigate with Enter key.")))

;;;###autoload
(defun simply-annotate-show-all ()
  "Show annotations from all files via `completing-read' selection."
  (interactive)
  (if-let* ((db (simply-annotate-load-database))
            (files-with-annotations (mapcar #'car db)))
      (let* ((file-display-alist (mapcar (lambda (file-key)
                                           (let* ((annotations (alist-get file-key db nil nil #'string=))
                                                  (count (length annotations)))
                                             (cons (format "%s (%d annotation%s)"
                                                           (file-name-nondirectory file-key) count (if (= count 1) "" "s"))
                                                   file-key)))
                                         files-with-annotations))
             (selected-file (cdr (assoc (completing-read "Select file with annotations: " file-display-alist nil t)
                                        file-display-alist)))
             (annotations (alist-get selected-file db nil nil #'string=)))
        (simply-annotate-display-file-annotations selected-file annotations))
    (message "No annotations database found")))

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
         (first-line (car (split-string first-text "\n"))))
    (concat
     (format "* TODO %s\n" first-line)
     (format ":PROPERTIES:\n:ID: %s\n:STATUS: %s\n:PRIORITY: %s\n" id status priority)
     (when tags (format ":TAGS: %s\n" (string-join tags " ")))
     (format ":CREATED: %s\n:AUTHOR: %s\n:END:\n\n"
             (alist-get 'timestamp first-comment) (alist-get 'author first-comment))
     (when (> (length first-text) (length first-line))
       (concat (string-join (cdr (split-string first-text "\n")) "\n") "\n\n"))
     (when-let ((replies (cdr comments)))
       (mapconcat
        (lambda (reply)
          (format "** Reply by %s (%s)\n%s\n\n"
                  (alist-get 'author reply) (alist-get 'timestamp reply) (alist-get 'text reply)))
        replies "")))))

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
        (insert (format "#+TITLE: Annotations for %s\n#+DATE: %s\n\n"
                        file-key (format-time-string "%Y-%m-%d")))
        (dolist (ann annotations)
          (let* ((text (alist-get 'text ann))
                 (thread (if (simply-annotate-is-thread-p text) text (simply-annotate-create-thread text))))
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
    (define-key map (kbd "M-s -") #'simply-annotate-remove)
    (define-key map (kbd "M-s l") #'simply-annotate-list)
    (define-key map (kbd "M-s 0") #'simply-annotate-show-all)
    (define-key map (kbd "M-s j") #'simply-annotate-smart-action)
    (define-key map (kbd "M-s r") #'simply-annotate-reply-to-annotation)
    (define-key map (kbd "M-s s") #'simply-annotate-set-annotation-status)
    (define-key map (kbd "M-s p") #'simply-annotate-set-annotation-priority)
    (define-key map (kbd "M-s t") #'simply-annotate-add-annotation-tag)
    (define-key map (kbd "M-s a") #'simply-annotate-change-annotation-author)
    (define-key map (kbd "M-s A") #'simply-annotate-reset-session-author)
    (define-key map (kbd "M-s o") #'simply-annotate-export-to-org-file)
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
        (simply-annotate-load-annotations)
        (simply-annotate-setup-header)
        (simply-annotate-update-header)
        ;; Register save hook directly, `simply-annotate-save-annotations` is now robust
        (add-hook 'before-save-hook #'simply-annotate-save-annotations nil t)
        (add-hook 'kill-buffer-hook #'simply-annotate-save-annotations nil t)
        (add-hook 'kill-buffer-hook #'simply-annotate-hide-annotation-buffer nil t)
        (message "Simply-annotate mode enabled. Loaded %d annotations."
                 (length simply-annotate-overlays)))
    (simply-annotate-clear-all-overlays)
    (simply-annotate-cleanup-header)
    (simply-annotate-hide-annotation-buffer)
    ;; Ensure draft overlay is cleaned up on mode disable
    (when simply-annotate-draft-overlay
      (delete-overlay simply-annotate-draft-overlay)
      (setq simply-annotate-draft-overlay nil))
    ;; Remove hooks
    (remove-hook 'before-save-hook #'simply-annotate-save-annotations t)
    (remove-hook 'kill-buffer-hook #'simply-annotate-save-annotations t)
    (remove-hook 'kill-buffer-hook #'simply-annotate-hide-annotation-buffer t)))

(provide 'simply-annotate)
;;; simply-annotate.el ends here
