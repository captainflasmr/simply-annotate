;;; simply-annotate.el --- Enhanced annotation system with threading -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 0.9.4
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
;; A lightweight annotation system for Emacs that allows you to add
;; persistent notes to any text file without modifying the original
;; content.  Enhanced with threading, collaboration, and org-mode
;; integration.
;;
;; Quick Start:
;;
;;   (use-package simply-annotate
;;     :bind-keymap ("C-c a" . simply-annotate-command-map)
;;     :hook (find-file-hook . simply-annotate-mode))
;;
;;   (with-eval-after-load 'simply-annotate
;;     (add-hook 'dired-mode-hook #'simply-annotate-dired-mode))
;;
;; All commands live in `simply-annotate-command-map', which you bind
;; to a prefix key of your choice.  With C-c a as the prefix:
;;
;; 1. Open any file
;; 2. Select text and press C-c a j to create your first annotation
;; 3. Navigate with M-n (next) and M-p (previous)
;;
;; Keymap Configuration:
;;
;; `simply-annotate-mode-map' is intentionally minimal (only M-n and
;; M-p for navigation).  All other commands are in
;; `simply-annotate-command-map':
;;
;;   ;; Recommended: C-c a prefix (defers loading until first use)
;;   :bind-keymap ("C-c a" . simply-annotate-command-map)
;;
;;   ;; Alternative: M-s prefix (replaces Emacs search-map)
;;   ;; Requires :demand t with global-set-key in :config
;;   ;; Do NOT use :bind-keymap with M-s (see docstring for details)
;;   :demand t
;;   :config (global-set-key (kbd "M-s") simply-annotate-command-map)
;;
;; Threading & Collaboration:
;;
;; All keybindings below use <prefix> to denote your chosen prefix
;; (e.g. C-c a j means press C-c a then j).
;;
;; * Replies
;; - Press <prefix> r to add a reply to any annotation
;; - Creates threaded conversations for code reviews
;;
;; * Status Management
;; - Press <prefix> s to set status (open, in-progress, resolved, closed)
;; - Press <prefix> p to set priority (low, normal, high, critical)
;; - Press <prefix> t to add tags for organization
;;
;; * Author Management
;; - Configure team members: (setq simply-annotate-author-list '("John" "Jane" "Bob"))
;; - Set prompting behavior: (setq simply-annotate-prompt-for-author 'threads-only)
;; - Press <prefix> a to change annotation author
;;
;; * Editing
;; - Press <prefix> e to edit the current annotation
;; - Edit in a sexp form and then C-c C-c to save
;; - Any data field can be edited
;;
;; * Org-mode Integration
;; - Press <prefix> o to export annotations to org-mode files
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
(require 'color)

(declare-function org-mode "org")
(declare-function outline-hide-sublevels "outline")
(declare-function dired-get-filename "dired")

(defgroup simply-annotate nil
  "Simple annotation system with threading support."
  :group 'text)

(defcustom simply-annotate-file
  (expand-file-name "simply-annotations.el" user-emacs-directory)
  "File to store annotations."
  :type 'file
  :group 'simply-annotate)

(defface simply-annotate-highlight-face
  '((t (:inherit highlight)))
  "Face for highlighted annotated text."
  :group 'simply-annotate)

(defface simply-annotate-fringe-bracket-face
  '((t (:inherit simply-annotate-highlight-face :background unspecified)))
  "Face for fringe bracket indicators.
Uses the foreground of the highlight face with no background,
so the bracket renders flat without a filled region in the fringe."
  :group 'simply-annotate)

(defcustom simply-annotate-highlight-face 'simply-annotate-highlight-face
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
A single symbol or a list of symbols processed in order.
Styles are applied sequentially so later face-based styles
override earlier ones, while fringe styles are independent.

Available styles:
- highlight: Highlight the annotated text
- tint: Subtle background tint derived from the current background
- fringe: Show indicators in the fringe
- fringe-bracket: Show bracket indicators spanning the annotated region
- subtle: Show a thin left border on annotated text

Examples:
  \\='fringe                        ; single style
  \\='(highlight fringe)            ; combined highlight and fringe
  \\='(tint fringe-bracket)         ; tint background with fringe brackets"
  :type '(choice (const :tag "Highlight text" highlight)
                 (const :tag "Background tint" tint)
                 (const :tag "Fringe indicators" fringe)
                 (const :tag "Fringe bracket" fringe-bracket)
                 (const :tag "Subtle left border" subtle)
                 (repeat :tag "Combined styles"
                         (choice (const highlight)
                                 (const tint)
                                 (const fringe)
                                 (const fringe-bracket)
                                 (const subtle))))
  :group 'simply-annotate)

(defcustom simply-annotate-tint-amount 20
  "Percentage to lighten the background color for the tint display style."
  :type 'integer
  :group 'simply-annotate)

(defcustom simply-annotate-fringe-indicator 'right-triangle
  "Symbol to use for fringe indicators."
  :type '(choice (const :tag "Left triangle" left-triangle)
                 (const :tag "Right triangle" right-triangle)
                 (const :tag "Filled rectangle" filled-rectangle)
                 (const :tag "Custom bitmap" custom))
  :group 'simply-annotate)

(defcustom simply-annotate-fringe-face 'simply-annotate-fringe-face
  "Face for fringe indicators."
  :type 'face
  :group 'simply-annotate)

(defface simply-annotate-subtle-face
  '((((background dark))
     (:overline "gray40" :underline (:style line :color "gray40")))
    (((background light))
     (:overline "gray75" :underline (:style line :color "gray75")))
    (t (:overline "gray" :underline (:style line :color "gray"))))
  "Face for subtle annotation display.
Uses overline and underline to bracket the annotated region."
  :group 'simply-annotate)

(defface simply-annotate-inline-face
  '((t (:inherit font-lock-doc-face :extend t)))
  "Face for inline annotation text body."
  :group 'simply-annotate)

(defface simply-annotate-inline-border-face
  '((t (:inherit shadow)))
  "Face for inline annotation border characters."
  :group 'simply-annotate)

(defcustom simply-annotate-inline-position 'after
  "Where to display inline annotation blocks relative to the annotated region.
- after: Below the annotated text (as an after-string)
- above: Above the annotated text (as a before-string)"
  :type '(choice (const :tag "After annotated region" after)
                 (const :tag "Above annotated region" above))
  :group 'simply-annotate)

(defcustom simply-annotate-use-org-editing t
  "When non-nil, use `org-mode' in the annotation editing buffer.
This enables org syntax highlighting (bold, italics, links, etc.)
while writing annotations.  The stored text retains raw org markup.
Org-mode keybindings like C-c C-c are overridden to save the
annotation instead.  Set to nil to use `fundamental-mode'."
  :type 'boolean
  :group 'simply-annotate)

(defcustom simply-annotate-inline-max-lines 20
  "Maximum number of lines shown for inline annotations.
Lines beyond this limit are truncated with a count.
Set to nil to show all lines."
  :type '(choice integer (const :tag "No limit" nil))
  :group 'simply-annotate)

(defcustom simply-annotate-inline-fill-column 72
  "Column at which inline annotation text is word-wrapped.
Set to nil to disable wrapping."
  :type '(choice integer (const :tag "No wrapping" nil))
  :group 'simply-annotate)

(defcustom simply-annotate-inline-default nil
  "When non-nil, enable inline annotation display when the mode starts."
  :type 'boolean
  :group 'simply-annotate)

;; Threading and Collaboration Customization

(defcustom simply-annotate-default-author
  (or user-full-name user-login-name "Anonymous")
  "Default author name for annotations."
  :type 'string
  :group 'simply-annotate)

(defcustom simply-annotate-author-list
  (list (or user-full-name user-login-name "Anonymous"))
  "List of available authors for annotations."
  :type '(repeat string)
  :group 'simply-annotate)

(defcustom simply-annotate-prompt-for-author nil
  "Whether to prompt for author selection when creating annotations.
- nil: Always use default author (single-user mode)
- first-only: Prompt only for the first annotation in a session
- always: Prompt for every annotation
- threads-only: Prompt only when adding replies to threads"
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

(defvar-local simply-annotate-file-key-function nil
  "Buffer-local function to generate the unique key for annotations.
When non-nil, this function is called with no arguments and its
return value (a string) is used as the key for the current buffer.
This overrides `simply-annotate-file-key-functions` and the default logic.")

(defcustom simply-annotate-file-key-functions
  '((Info-mode . simply-annotate-info-file-key))
  "Alist mapping major modes to functions that generate annotation keys.
Each element is of the form (MAJOR-MODE . FUNCTION).
These functions are called with no arguments and should return a string key.
This allows node-specific annotations in modes like Info or nov.el."
  :type '(alist :key-type symbol :value-type function)
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

;; Annotation Levels

(defcustom simply-annotate-levels '(file defun line)
  "Available annotation levels, ordered from broadest to narrowest."
  :type '(repeat symbol)
  :group 'simply-annotate)

(defcustom simply-annotate-default-level 'all
  "Default annotation level for display when the mode starts."
  :type '(choice (const :tag "File-level" file)
                 (const :tag "Defun-level" defun)
                 (const :tag "Line-level" line)
                 (const :tag "All levels" all))
  :group 'simply-annotate)

;; Separators and Regexps

(defcustom simply-annotate-annotation-separator "┌─────"
  "String used as the annotation separator line.
Also used as a regexp in font-lock; avoid special regexp characters."
  :type 'string
  :group 'simply-annotate)

(defcustom simply-annotate-text-separator "└─────"
  "String used as the text separator line.
Also used as a regexp in font-lock; avoid special regexp characters."
  :type 'string
  :group 'simply-annotate)

(defcustom simply-annotate-header-regexp "^Annotations for \\(.*\\)"
  "Regexp matching the buffer header."
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
    nil nil 'center)
  ;; Bracket bitmaps: tall (40 rows) so they fill the character cell.
  ;; Alignment ensures the horizontal bars sit at the cell edge and
  ;; the vertical bars extend fully, with no gaps between lines.
  (define-fringe-bitmap 'simply-annotate-fringe-bracket-top
    (vconcat (make-vector 3 #b11111100)
             (make-vector 37 #b10000000))
    nil nil 'top)
  (define-fringe-bitmap 'simply-annotate-fringe-bracket-mid
    (make-vector 40 #b10000000)
    nil nil 'center)
  (define-fringe-bitmap 'simply-annotate-fringe-bracket-bot
    (vconcat (make-vector 37 #b10000000)
             (make-vector 3 #b11111100))
    nil nil 'bottom)
  (define-fringe-bitmap 'simply-annotate-fringe-bracket-single
    (vconcat (make-vector 3 #b11111100)
             (make-vector 10 #b10000000)
             (make-vector 3 #b11111100))
    nil nil 'center))

;;; Variables

(defvar simply-annotate-mode) ; forward declaration, defined by `define-minor-mode'

(defvar-local simply-annotate-draft-overlay nil
  "Overlay for draft annotation being created.")

(defvar-local simply-annotate-overlays nil
  "List of annotation overlays in current buffer.")

(defvar-local simply-annotate-original-header-line nil
  "Original `header-line-format' before annotation mode.")

(defvar-local simply-annotate-current-annotation nil
  "Current annotation text being displayed in header.")

(defvar simply-annotate-current-overlay nil
  "Current overlay being displayed/edited in annotation buffer.
This is global because the singleton *Annotation* buffer undergoes
mode changes that would kill buffer-local values.")

(defvar simply-annotate-source-buffer nil
  "Source buffer for annotation display.")

(defvar simply-annotate-header-end-pos nil
  "Position where the annotation content starts (after header).")

(defvar simply-annotate-editing-annotation-sexp nil
  "Non-nil if the *Annotation* buffer is currently displaying the raw sexp.")

(defvar-local simply-annotate-current-level 'defun
  "Currently displayed annotation level in this buffer.")

(defvar-local simply-annotate-inline nil
  "Non-nil when inline annotation display is active in this buffer.")

(defvar-local simply-annotate-listing-source nil
  "Source buffer that generated this listing buffer.")

(defvar simply-annotate-reply-mode nil
  "Non-nil when the *Annotation* buffer is in reply mode.")

(defvar simply-annotate-reply-parent-id nil
  "Comment ID to which the current reply is addressed.")

(defvar simply-annotate-editing-comment-id nil
  "Comment ID currently being edited, or nil to edit the first comment.")

;; Threading Variables

(defvar simply-annotate-session-author nil
  "Author chosen for current session (when using first-only mode).")

(defvar simply-annotate-file-authors nil
  "Alist of (file-key . author) for per-file author memory.")

;;; Helper Functions

(defun simply-annotate--timestamp ()
  "Generate current timestamp string."
  (format-time-string "%Y-%m-%dT%H:%M:%S"))

(defvar Info-current-file)
(defvar Info-current-node)
(declare-function Info-find-node "info" (filename nodename &optional no-going-back strict-case))

(defun simply-annotate-info-file-key ()
  "Get unique key for Info-mode buffers.
Returns a string identifying both the Info file and the current node."
  (when (derived-mode-p 'Info-mode)
    (require 'info)
    (let ((file (file-name-nondirectory Info-current-file))
          (node Info-current-node))
      (format "(%s) %s"
              (replace-regexp-in-string "\\.info\\(\\.[^.]+\\)?$" "" file)
              node))))

(defun simply-annotate--thread-p (annotation-data)
  "Check if ANNOTATION-DATA is a thread structure."
  (and (listp annotation-data)
       (alist-get 'id annotation-data)
       (alist-get 'comments annotation-data)))

(defun simply-annotate--annotation-text (annotation-data)
  "Extract display text from ANNOTATION-DATA (string or thread)."
  (if (stringp annotation-data)
      annotation-data
    (let ((comments (alist-get 'comments annotation-data)))
      (when comments
        (alist-get 'text (car comments))))))

(defun simply-annotate--strip-boilerplate (text)
  "Strip box-drawing and thread boilerplate from TEXT.
This helps fix corrupted annotations from older versions."
  (if (or (null text) (not (stringp text)))
      ""
    (let ((lines (split-string text "\n")))
      ;; 1. Remove obvious header/footer/metadata lines
      (setq lines (cl-remove-if
                   (lambda (line)
                     (or (string-match-p "^[┌└─]" line) ; Thread/Box header or footer
                         (string-match-p "^│?\\s-*\\(comment\\|reply\\):" line) ; Comment indicator
                         (string-match-p "^│?\\s-*Source:" line) ; Source info
                         (string-match-p "^│?\\s-*Thread:" line))) ; Thread info
                   lines))
      ;; 2. For remaining lines, strip leading '│' and whitespace
      (setq lines (mapcar (lambda (l) (replace-regexp-in-string "^│\\s-*" "" l)) lines))
      ;; 3. Join and trim
      (string-trim (mapconcat #'identity lines "\n")))))

(defun simply-annotate--annotation-summary (annotation-data)
  "Get a summary for display (help-echo, header, etc.) from ANNOTATION-DATA."
  (if (stringp annotation-data)
      annotation-data
    (simply-annotate--format-thread-summary annotation-data)))

(defun simply-annotate--file-key ()
  "Get unique key for current buffer/file.
Checks `simply-annotate-file-key-function` (buffer-local) and
`simply-annotate-file-key-functions` before falling back to
file path or buffer name."
  (let ((custom-func (or simply-annotate-file-key-function
                         (cl-some (lambda (entry)
                                    (when (derived-mode-p (car entry))
                                      (cdr entry)))
                                  simply-annotate-file-key-functions))))
    (if (and custom-func (functionp custom-func))
        (funcall custom-func)
      (or (buffer-file-name) (buffer-name)))))

(defun simply-annotate--key-info-p (key)
  "Return non-nil if KEY is an Info node key."
  (and (stringp key) (string-prefix-p "(" key) (string-match-p ")" key)))

(defun simply-annotate--key-directory (key)
  "Get directory component for KEY.
For files, this is the directory path.  For Info nodes, it is the manual name."
  (cond
   ((simply-annotate--key-info-p key)
    (substring key 0 (1+ (string-match-p ")" key))))
   (t
    (or (file-name-directory (abbreviate-file-name key)) "./"))))

(defun simply-annotate--key-name (key)
  "Get filename component for KEY.
For files, this is the filename.  For Info nodes, it is the node name."
  (cond
   ((simply-annotate--key-info-p key)
    (string-trim (substring key (1+ (string-match-p ")" key)))))
   (t
    (file-name-nondirectory (abbreviate-file-name key)))))

(defun simply-annotate--format-jump-name (key)
  "Format KEY for `completing-read` display to ensure uniqueness.
For files, includes the directory. For Info nodes, returns the full key."
  (if (simply-annotate--key-info-p key)
      key
    (let ((name (file-name-nondirectory (abbreviate-file-name key)))
          (dir (file-name-directory (abbreviate-file-name key))))
      (if dir
          (format "%s  [%s]" name dir)
        name))))

(defun simply-annotate--get-key-buffer (key &optional select)
  "Get (and optionally SELECT) the buffer associated with KEY.
Returns the buffer if successful, or nil if KEY cannot be resolved."
  (cond
   ((simply-annotate--key-info-p key)
    (let ((manual (substring key 1 (string-match-p ")" key)))
          (node (string-trim (substring key (1+ (string-match-p ")" key))))))
      (require 'info)
      (condition-case err
          (if select
              (progn
                (info (format "(%s)%s" manual node))
                (current-buffer))
            (let ((info-buffer (get-buffer-create "*info*")))
              (with-current-buffer info-buffer
                (unless (derived-mode-p 'Info-mode)
                  (Info-mode))
                (Info-find-node manual node)
                (current-buffer))))
        (error
         (message "Simply-annotate: failed to open Info node %s: %s"
                  key (error-message-string err))
         nil))))
   ((file-exists-p key)
    (if select
        (find-file key)
      (find-file-noselect key)))
   (t
    (when-let ((buf (get-buffer key)))
      (when select (switch-to-buffer buf))
      buf))))

;;; Database Operations

(defun simply-annotate--load-database ()
  "Load annotations from database file."
  (when (file-exists-p simply-annotate-file)
    (with-temp-buffer
      (insert-file-contents simply-annotate-file)
      (let ((content (string-trim (buffer-string))))
        (unless (string-empty-p content)
          (condition-case err
              (car (read-from-string content))
            (error
             (message "Simply-annotate: failed to read database %s: %s"
                      simply-annotate-file (error-message-string err))
             nil)))))))

(defun simply-annotate--save-database (db)
  "Save DB to file."
  (with-temp-file simply-annotate-file
    (insert ";;; Simply Annotate Database\n")
    (insert ";;; This file is auto-generated. Do not edit manually.\n\n")
    (prin1 db (current-buffer))
    (insert "\n")))

(defun simply-annotate--update-database (file-key annotations)
  "Update database with ANNOTATIONS for FILE-KEY."
  (let ((db (or (simply-annotate--load-database) '())))
    (when (or annotations (alist-get file-key db nil nil #'string=))
      (if annotations
          (setf (alist-get file-key db nil nil #'string=) annotations)
        (setq db (cl-remove-if (lambda (entry)
                                 (string= (car entry) file-key))
                               db)))
      (if db
          (simply-annotate--save-database db)
        (when (file-exists-p simply-annotate-file)
          (delete-file simply-annotate-file))))))

;;; Display Management

(defun simply-annotate--wrap-line (line width)
  "Word-wrap LINE to WIDTH, returning a list of lines."
  (if (or (not width) (<= (length line) width))
      (list line)
    (with-temp-buffer
      (insert line)
      (let ((fill-column width))
        (fill-region (point-min) (point-max))
        (split-string (buffer-string) "\n")))))

(defun simply-annotate--comment-node-lines (node depth &optional continuations last-p wrap-width)
  "Format comment tree NODE at DEPTH into a flat list of lines.
CONTINUATIONS is a list of booleans tracking which ancestor depths
have more siblings.  LAST-P is non-nil if this node is the last
sibling.  WRAP-WIDTH, when non-nil, word-wraps text lines.
Returns lines in display order (not reversed)."
  (let* ((comment (car node))
         (children (cdr node))
         (text (alist-get 'text comment))
         (author (alist-get 'author comment))
         (timestamp (alist-get 'timestamp comment))
         (is-reply (> depth 0))
         (indent (simply-annotate--tree-indent continuations))
         (branch (cond ((not is-reply) "")
                       (t "↳ ")))
         (child-conts (if is-reply
                         (append continuations (list (not last-p)))
                       continuations))
         (text-prefix (concat (simply-annotate--tree-indent child-conts) "  "))
         (lines '()))
    ;; Author/meta line
    (when (and author timestamp)
      (push (format "%s%s%s (%s)"
                    indent branch
                    (propertize author 'face 'bold)
                    (format-time-string "%m/%d %H:%M" (date-to-time timestamp)))
            lines))
    ;; Text body lines (with optional word-wrap)
    (dolist (line (split-string text "\n"))
      (dolist (wrapped (simply-annotate--wrap-line line wrap-width))
        (push (concat text-prefix wrapped) lines)))
    ;; Children
    (let ((last-idx (1- (length children)))
          (idx 0))
      (dolist (child children)
        (dolist (child-line (simply-annotate--comment-node-lines
                             child (1+ depth) child-conts (= idx last-idx) wrap-width))
          (push child-line lines))
        (cl-incf idx)))
    (nreverse lines)))

(defun simply-annotate--inline-text (overlay)
  "Return the inline display string for OVERLAY as a box-drawn block."
  (let* ((data (overlay-get overlay 'simply-annotation))
         (is-thread (simply-annotate--thread-p data))
         (tags (when is-thread (alist-get 'tags data)))
         (meta (when is-thread
                 (let ((status (alist-get 'status data))
                       (priority (alist-get 'priority data)))
                   (when (or status priority)
                     (concat
                      (format " [%s]"
                              (string-join
                               (delq nil (list (when status (upcase status))
                                               (when priority (upcase priority))))
                               "/"))
                      (when tags
                        (format " %s" (string-join tags ", "))))))))
         (comments (if is-thread
                       (progn
                         (simply-annotate--ensure-comment-ids data)
                         (alist-get 'comments data))
                     (list `((text . ,data) (type . "comment") (author . "System")))))
         (formatted-lines '())
         (label (concat "✎" (or meta "")))
         (rule-len (max 20 (+ 4 (string-width label))))
         (wrap-width (when simply-annotate-inline-fill-column
                       (- simply-annotate-inline-fill-column 4))))

    ;; Build tree and format using common renderer
    (if is-thread
        (let* ((tree (simply-annotate--build-comment-tree comments))
               (last-idx (1- (length tree)))
               (idx 0))
          (dolist (root-node tree)
            (setq formatted-lines
                  (append formatted-lines
                          (simply-annotate--comment-node-lines
                           root-node 0 nil (= idx last-idx) wrap-width)))
            (cl-incf idx)))
      ;; Non-thread: use common renderer with a synthetic node
      (setq formatted-lines
            (simply-annotate--comment-node-lines
             (cons (car comments) nil) 0 nil t wrap-width)))

    ;; Handle max lines if necessary
    (when (and simply-annotate-inline-max-lines
               (> (length formatted-lines) simply-annotate-inline-max-lines))
      (setq formatted-lines
            (append (seq-take formatted-lines simply-annotate-inline-max-lines)
                    (list (format "… +%d lines"
                                  (- (length formatted-lines)
                                     simply-annotate-inline-max-lines))))))

    (let ((header (propertize (concat "┌─ " label " "
                                     (make-string
                                      (max 1 (- rule-len 4 (string-width label)))
                                      ?─))
                             'face 'simply-annotate-inline-border-face))
          (body (mapconcat
                 (lambda (line)
                   (let ((styled-line (copy-sequence line)))
                     (add-face-text-property
                      0 (length styled-line)
                      'simply-annotate-inline-face t styled-line)
                     (concat (propertize "│ " 'face 'simply-annotate-inline-border-face)
                             styled-line)))
                 formatted-lines "\n"))
          (footer (propertize (concat "└" (make-string (max 1 rule-len) ?─) "┘")
                              'face 'simply-annotate-inline-border-face)))
      (let ((result (concat "\n" header "\n" body "\n" footer "\n")))
      ;; Collapse runs of multiple blank lines to at most one
      (replace-regexp-in-string "\n\\(\n\\)\\(\n\\)+" "\n\n" result)))))

(defun simply-annotate--add-inline-text (overlay)
  "Add inline annotation text to OVERLAY.
Position is controlled by `simply-annotate-inline-position'."
  (let ((text (simply-annotate--inline-text overlay)))
    (if (eq simply-annotate-inline-position 'above)
        (let ((fringe (when (cl-intersection (simply-annotate--display-styles)
                                              '(fringe fringe-bracket))
                        (overlay-get overlay 'before-string)))
              (prev-char (and (> (overlay-start overlay) (point-min))
                              (char-before (overlay-start overlay)))))
          ;; Skip leading newline when the preceding character is already a newline
          (when (and prev-char (= prev-char ?\n)
                     (string-prefix-p "\n" text))
            (setq text (substring text 1)))
          (overlay-put overlay 'before-string
                       (concat text (or fringe ""))))
      (overlay-put overlay 'after-string text))))

(defun simply-annotate--refresh-overlay-display (overlay)
  "Refresh display properties of OVERLAY after a data change.
Does a full display re-apply to update inline text."
  (with-current-buffer (overlay-buffer overlay)
    (simply-annotate--cleanup-bracket-overlays overlay)
    (overlay-put overlay 'face nil)
    (overlay-put overlay 'before-string nil)
    (overlay-put overlay 'after-string nil)
    (simply-annotate--apply-display-style overlay)))

(defun simply-annotate--display-styles ()
  "Return the current display style as a list of symbols.
Normalises a single symbol to a one-element list."
  (let ((style simply-annotate-display-style))
    (if (listp style) style (list style))))

(defun simply-annotate--apply-display-style (overlay)
  "Apply current display style to OVERLAY.
Overlays not matching the active level are hidden.
File-level annotations at the `all' pseudo-level skip visual styles
but still show inline text when enabled."
  (let ((file-at-all (and (eq simply-annotate-current-level 'all)
                          (eq (overlay-get overlay 'simply-annotation-level) 'file))))
    (if (simply-annotate--level-match-p overlay)
        (progn
          (unless file-at-all
            (dolist (style (simply-annotate--display-styles))
              (pcase style
                ('highlight
                 (overlay-put overlay 'face simply-annotate-highlight-face))
                ('tint
                 (let ((bg (simply-annotate--tint-background)))
                   (when bg
                     (overlay-put overlay 'face `(:background ,bg :extend t)))))
                ('fringe
                 (simply-annotate--add-fringe-indicator overlay))
                ('fringe-bracket
                 (simply-annotate--add-fringe-bracket overlay))
                ('subtle
                 (overlay-put overlay 'face 'simply-annotate-subtle-face)))))
          (when simply-annotate-inline
            (simply-annotate--add-inline-text overlay)))
      (simply-annotate--cleanup-bracket-overlays overlay)
      (overlay-put overlay 'face nil)
      (overlay-put overlay 'before-string nil)
      (overlay-put overlay 'after-string nil))))

(defun simply-annotate--tint-background ()
  "Return a background color slightly lighter than the default."
  (let ((bg (face-background 'default nil t)))
    (when (and bg (color-defined-p bg))
      (color-lighten-name bg simply-annotate-tint-amount))))

(defun simply-annotate--add-fringe-indicator (overlay)
  "Add fringe indicator to OVERLAY."
  (let* ((bitmap (pcase simply-annotate-fringe-indicator
                   ('left-triangle 'left-triangle)
                   ('right-triangle 'right-triangle)
                   ('filled-rectangle 'filled-rectangle)
                   ('custom 'simply-annotate-fringe-bitmap)
                   (_ 'left-triangle)))
         (fringe-spec `(left-fringe ,bitmap ,simply-annotate-fringe-face)))
    (overlay-put overlay 'before-string
                 (propertize " " 'display fringe-spec))))

(defun simply-annotate--cleanup-bracket-overlays (overlay)
  "Remove auxiliary bracket fringe overlays associated with OVERLAY."
  (dolist (aux (overlay-get overlay 'simply-annotate-bracket-overlays))
    (when (overlayp aux)
      (delete-overlay aux)))
  (overlay-put overlay 'simply-annotate-bracket-overlays nil))

(defun simply-annotate--add-fringe-bracket (overlay)
  "Add fringe bracket indicators spanning the full extent of OVERLAY.
Uses the configured fringe indicator on the first and last lines,
with vertical bars on intermediate lines."
  (simply-annotate--cleanup-bracket-overlays overlay)
  (save-excursion
    (let* ((start (overlay-start overlay))
           (end (overlay-end overlay))
           (start-line (line-number-at-pos start t))
           (end-line (line-number-at-pos end t))
           (total-lines (1+ (- end-line start-line)))
           (cap-bitmap (pcase simply-annotate-fringe-indicator
                         ('left-triangle 'left-triangle)
                         ('right-triangle 'right-triangle)
                         ('filled-rectangle 'filled-rectangle)
                         ('custom 'simply-annotate-fringe-bitmap)
                         (_ 'left-triangle)))
           (aux-overlays nil))
      (goto-char start)
      (beginning-of-line)
      (dotimes (i total-lines)
        (let* ((bitmap (cond
                        ((= total-lines 1) cap-bitmap)
                        ((= i 0) cap-bitmap)
                        ((= i (1- total-lines)) 'simply-annotate-fringe-bracket-bot)
                        (t 'simply-annotate-fringe-bracket-mid)))
               (fringe-spec `(left-fringe ,bitmap simply-annotate-fringe-bracket-face))
               (indicator (propertize " " 'display fringe-spec)))
          (if (= i 0)
              (overlay-put overlay 'before-string indicator)
            (let ((aux (make-overlay (point) (point))))
              (overlay-put aux 'before-string indicator)
              (overlay-put aux 'simply-annotate-bracket t)
              (push aux aux-overlays))))
        (forward-line 1))
      (overlay-put overlay 'simply-annotate-bracket-overlays aux-overlays))))

(defun simply-annotate-update-display-style ()
  "Update display style for all existing annotations."
  (interactive)
  (dolist (overlay simply-annotate-overlays)
    ;; Clear existing display properties
    (simply-annotate--cleanup-bracket-overlays overlay)
    (overlay-put overlay 'face nil)
    (overlay-put overlay 'before-string nil)
    (overlay-put overlay 'after-string nil)
    ;; Reapply based on current style
    (simply-annotate--apply-display-style overlay))
  (message "Updated display style to: %s" simply-annotate-display-style))

(defun simply-annotate-cycle-display-style ()
  "Cycle through individual display styles.
For combined styles, use `simply-annotate-display-style' directly."
  (interactive)
  (let ((styles (simply-annotate--display-styles)))
    (setq simply-annotate-display-style
          (if (= (length styles) 1)
              (pcase (car styles)
                ('highlight 'tint)
                ('tint 'fringe)
                ('fringe 'fringe-bracket)
                ('fringe-bracket 'subtle)
                (_ 'highlight))
            'highlight)))
  (simply-annotate-update-display-style))

;;;###autoload
(defun simply-annotate-toggle-inline ()
  "Toggle inline annotation display.
When active, annotation text is shown as box-drawn blocks
relative to the annotated region, filtered to the current
annotation level.  This layers on top of the current display style."
  (interactive)
  (setq simply-annotate-inline (not simply-annotate-inline))
  (simply-annotate-update-display-style)
  (message "Inline annotations %s" (if simply-annotate-inline "enabled" "disabled")))

;;; Level Management

(defun simply-annotate--level-match-p (overlay)
  "Return non-nil if OVERLAY matches the current annotation level.
When the current level is `all', every overlay matches."
  (or (eq simply-annotate-current-level 'all)
      (eq (or (overlay-get overlay 'simply-annotation-level) 'defun)
          simply-annotate-current-level)))

(defun simply-annotate--active-overlays ()
  "Return list of overlays matching the current annotation level."
  (cl-remove-if-not #'simply-annotate--level-match-p simply-annotate-overlays))

(defun simply-annotate--apply-level-filter ()
  "Update display after a level change.
Only overlays matching the active level are shown."
  (when (and simply-annotate-current-overlay
             (not (simply-annotate--level-match-p simply-annotate-current-overlay))
             (get-buffer-window simply-annotate-buffer-name))
    (simply-annotate-hide-annotation-buffer))
  (dolist (ov simply-annotate-overlays)
    (simply-annotate--cleanup-bracket-overlays ov)
    (overlay-put ov 'face nil)
    (overlay-put ov 'before-string nil)
    (overlay-put ov 'after-string nil)
    (simply-annotate--apply-display-style ov))
  (simply-annotate--update-header))

;;;###autoload
(defun simply-annotate-cycle-level-forward ()
  "Cycle forward through annotation levels.
The cycle includes `all' after the last defined level."
  (interactive)
  (let* ((levels (append simply-annotate-levels '(all)))
         (current-pos (cl-position simply-annotate-current-level levels))
         (next-pos (if current-pos
                       (mod (1+ current-pos) (length levels))
                     0)))
    (setq simply-annotate-current-level (nth next-pos levels))
    (simply-annotate--apply-level-filter)
    (message "Annotation level: %s" simply-annotate-current-level)))

;;;###autoload
(defun simply-annotate-cycle-level-backward ()
  "Cycle backward through annotation levels.
The cycle includes `all' after the last defined level."
  (interactive)
  (let* ((levels (append simply-annotate-levels '(all)))
         (current-pos (cl-position simply-annotate-current-level levels))
         (next-pos (if current-pos
                       (mod (1- current-pos) (length levels))
                     0)))
    (setq simply-annotate-current-level (nth next-pos levels))
    (simply-annotate--apply-level-filter)
    (message "Annotation level: %s" simply-annotate-current-level)))

;;;###autoload
(defun simply-annotate-set-level (level)
  "Set annotation view to LEVEL."
  (interactive
   (list (intern (completing-read "Level: "
                                  (mapcar #'symbol-name
                                          (append simply-annotate-levels '(all)))
                                  nil t))))
  (setq simply-annotate-current-level level)
  (simply-annotate--apply-level-filter)
  (message "Annotation level: %s" level))

;;; Overlay Management

(defun simply-annotate--create-overlay (start end text)
  "Create annotation overlay with configurable display style.
START and END define the region, TEXT is the annotation content."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'simply-annotation text)
    (overlay-put overlay 'help-echo (simply-annotate--annotation-summary text))
    (overlay-put overlay 'mouse-face 'highlight)
    (simply-annotate--apply-display-style overlay)
    overlay))

(defun simply-annotate--remove-overlay (overlay)
  "Remove annotation OVERLAY."
  (simply-annotate--cleanup-bracket-overlays overlay)
  (setq simply-annotate-overlays (delq overlay simply-annotate-overlays))
  (delete-overlay overlay))

(defun simply-annotate--clear-all-overlays ()
  "Remove all annotation overlays from buffer."
  (mapc #'simply-annotate--cleanup-bracket-overlays simply-annotate-overlays)
  (mapc #'delete-overlay simply-annotate-overlays)
  (setq simply-annotate-overlays nil))

(defun simply-annotate--most-specific-overlay (candidates)
  "Return the most specific overlay from CANDIDATES.
Prefers the smallest region; when at the `all' level, deprioritises
file-level overlays so more granular annotations take precedence."
  (when candidates
    (car (sort candidates
                (lambda (a b)
                  (let ((a-file (eq (overlay-get a 'simply-annotation-level) 'file))
                        (b-file (eq (overlay-get b 'simply-annotation-level) 'file)))
                    (cond
                     ;; At 'all level, push file-level to the back
                     ((and (eq simply-annotate-current-level 'all)
                           a-file (not b-file))
                      nil)
                     ((and (eq simply-annotate-current-level 'all)
                           b-file (not a-file))
                      t)
                     ;; Otherwise prefer smallest region
                     (t (< (- (overlay-end a) (overlay-start a))
                            (- (overlay-end b) (overlay-start b)))))))))))

(defun simply-annotate--overlay-at-point (&optional pos)
  "Get annotation overlay at POS (defaults to point) matching current level.
In fringe mode, searches the entire current line for overlays.
When multiple overlays match, returns the most specific one.
File-level overlays are excluded at the `all' pseudo-level so that
`smart-action' can create granular annotations anywhere in the buffer."
  (let ((check-pos (or pos (point))))
    (if (cl-intersection (simply-annotate--display-styles) '(fringe fringe-bracket))
        (simply-annotate--overlay-on-line check-pos)
      (simply-annotate--most-specific-overlay
       (cl-remove-if-not (lambda (ov)
                           (and (overlay-get ov 'simply-annotation)
                                (simply-annotate--level-match-p ov)
                                (not (and (eq simply-annotate-current-level 'all)
                                          (eq (overlay-get ov 'simply-annotation-level) 'file)))))
                         (overlays-at check-pos))))))

(defun simply-annotate--overlay-on-line (&optional pos)
  "Find annotation overlay matching current level on the line containing POS.
Matches any overlay whose region intersects the current line.
When multiple overlays match, returns the most specific one.
File-level overlays are excluded at the `all' pseudo-level."
  (save-excursion
    (when pos (goto-char pos))
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position)))
      (simply-annotate--most-specific-overlay
       (cl-remove-if-not (lambda (overlay)
                           (and (overlay-get overlay 'simply-annotation)
                                (simply-annotate--level-match-p overlay)
                                (not (and (eq simply-annotate-current-level 'all)
                                          (eq (overlay-get overlay 'simply-annotation-level) 'file)))
                                (<= (overlay-start overlay) line-end)
                                (>= (overlay-end overlay) line-start)))
                         simply-annotate-overlays)))))

;;; Thread Management

(defun simply-annotate--comment-id ()
  "Generate a unique comment ID."
  (format "c-%s-%06d" (format-time-string "%s") (random 1000000)))

(defun simply-annotate--ensure-comment-ids (thread)
  "Ensure all comments in THREAD have id and parent-id fields.
For legacy comments without these fields, infers the hierarchy from
the type field: comments with type \"reply\" become children of the
first root comment.  Returns THREAD."
  (let* ((comments (alist-get 'comments thread))
         (needs-migration (cl-some (lambda (c) (not (alist-get 'id c))) comments))
         (root-id nil))
    (when needs-migration
      (setf (alist-get 'comments thread)
            (mapcar (lambda (comment)
                      (unless (alist-get 'id comment)
                        (let ((new-id (simply-annotate--comment-id)))
                          (push (cons 'id new-id) comment)
                          ;; Track the first root comment's id
                          (when (and (not root-id)
                                     (string= (or (alist-get 'type comment) "comment")
                                              "comment"))
                            (setq root-id new-id))))
                      (unless (assq 'parent-id comment)
                        ;; Replies become children of the first root comment
                        (let ((type (or (alist-get 'type comment) "comment")))
                          (push (cons 'parent-id
                                      (if (string= type "reply") root-id nil))
                                comment)))
                      comment)
                    comments))))
  thread)

(defun simply-annotate--create-thread (text &optional author priority tags)
  "Create a new annotation thread with TEXT, AUTHOR, PRIORITY, and TAGS."
  (let ((timestamp (simply-annotate--timestamp))
        (id (format "thread-%s-%06d" (format-time-string "%s") (random 1000000))))
    `((id . ,id)
      (created . ,timestamp)
      (status . "open")
      (priority . ,(or priority "normal"))
      (tags . ,(or tags '()))
      (comments . (((id . ,(simply-annotate--comment-id))
                    (parent-id . nil)
                    (author . ,(or author (simply-annotate--author-for-context 'annotation)))
                    (timestamp . ,timestamp)
                    (text . ,text)
                    (type . "comment")))))))

(defun simply-annotate--add-reply (thread reply-text &optional author parent-comment-id)
  "Add REPLY-TEXT to an existing THREAD with optional AUTHOR.
When PARENT-COMMENT-ID is non-nil, the reply is nested under that comment.
Otherwise it replies to the first (root) comment."
  (simply-annotate--ensure-comment-ids thread)
  (let* ((timestamp (simply-annotate--timestamp))
         (comments (alist-get 'comments thread))
         (effective-parent (or parent-comment-id
                               (alist-get 'id (car comments))))
         (reply `((id . ,(simply-annotate--comment-id))
                  (parent-id . ,effective-parent)
                  (author . ,(or author (simply-annotate--author-for-context 'reply)))
                  (timestamp . ,timestamp)
                  (text . ,reply-text)
                  (type . "reply"))))
    (simply-annotate--remember-author (alist-get 'author reply))
    (setf (alist-get 'comments thread) (append comments (list reply)))
    thread))

(defun simply-annotate--build-comment-tree (comments)
  "Build a tree from flat COMMENTS list using parent-id references.
Returns a list of root nodes, where each node is (comment . children)
and children is a recursive list of nodes."
  (let ((by-parent (make-hash-table :test 'equal)))
    ;; Group comments by parent-id
    (dolist (comment comments)
      (let ((pid (alist-get 'parent-id comment)))
        (push comment (gethash pid by-parent))))
    ;; Reverse each bucket to restore insertion order
    (maphash (lambda (key val)
               (puthash key (nreverse val) by-parent))
             by-parent)
    ;; Recursively build tree from a given parent id
    (simply-annotate--build-subtree by-parent nil)))

(defun simply-annotate--build-subtree (by-parent parent-id)
  "Build subtree from BY-PARENT hash for PARENT-ID."
  (mapcar (lambda (c)
            (cons c (simply-annotate--build-subtree by-parent (alist-get 'id c))))
          (gethash parent-id by-parent)))

(defun simply-annotate--set-thread-property (thread property value valid-values)
  "Set PROPERTY of THREAD to VALUE if it's in VALID-VALUES."
  (when (member value valid-values)
    (setf (alist-get property thread) value)
    thread))

(defun simply-annotate--add-thread-tag (thread tag)
  "Add a TAG to a THREAD."
  (let ((tags (alist-get 'tags thread)))
    (unless (member tag tags)
      (setf (alist-get 'tags thread) (append tags (list tag))))
    thread))

(defun simply-annotate--format-thread-summary (thread)
  "Format a brief summary of the THREAD for display."
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

(defun simply-annotate--tree-indent (continuations)
  "Build an indent string from CONTINUATIONS, a list of booleans.
Each t means there are more siblings at that depth (draw │),
nil means last sibling at that depth (draw space)."
  (mapconcat (lambda (has-more) (if has-more "│ " "  "))
             continuations ""))

(defun simply-annotate--format-comment-node (node depth _rule-len &optional continuations last-p)
  "Format a comment tree NODE at DEPTH for the annotation buffer.
_RULE-LEN is unused but kept for caller compatibility.
CONTINUATIONS is a list of booleans tracking which ancestor depths
have more siblings.  LAST-P is non-nil if this node is the last sibling."
  (mapconcat (lambda (line) (concat "│ " line))
             (simply-annotate--comment-node-lines
              node depth continuations last-p nil)
             "\n"))

(defun simply-annotate--format-thread-full (thread)
  "Format complete THREAD for display in annotation buffer."
  (simply-annotate--ensure-comment-ids thread)
  (let* ((status (alist-get 'status thread))
         (priority (alist-get 'priority thread))
         (tags (alist-get 'tags thread))
         (comments (alist-get 'comments thread))
         (tree (simply-annotate--build-comment-tree comments))
         (label (concat "✎ [" (upcase status) "/" (upcase priority) "]"))
         (rule-len (max 20 (+ 4 (string-width label)))))

    (when tags
      (setq label (concat label " " (string-join tags ", "))))

    (let ((result
           (concat
            (format "┌─ %s %s" label (make-string (max 1 (- rule-len 4 (string-width label))) ?─))
            "\n"
            (let ((last-idx (1- (length tree)))
                  (idx 0))
              (mapconcat
               (lambda (node)
                 (prog1
                     (simply-annotate--format-comment-node
                      node 0 rule-len nil (= idx last-idx))
                   (cl-incf idx)))
               tree
               "\n│\n"))
            "\n└" (make-string (max 1 rule-len) ?─) "┘")))
      ;; Collapse runs of multiple blank lines to at most one
      (replace-regexp-in-string "\n\\(\n\\)\\(\n\\)+" "\n\n" result))))

;;; Author Management

(defun simply-annotate--author-for-context (context &optional current-author)
  "Get appropriate author based on CONTEXT and CURRENT-AUTHOR."
  (let* ((file-key (simply-annotate--file-key))
         (default-author (or simply-annotate-default-author
                             (car simply-annotate-author-list)))
         (remembered-author (when simply-annotate-remember-author-per-file
                              (alist-get file-key simply-annotate-file-authors nil nil #'string=)))
         (initial-author (or remembered-author simply-annotate-session-author default-author)))
    
    (cond
     ((null simply-annotate-prompt-for-author) initial-author)
     ((eq simply-annotate-prompt-for-author 'always)
      (simply-annotate--select-author initial-author current-author))
     ((eq simply-annotate-prompt-for-author 'first-only)
      (or simply-annotate-session-author
          (setq simply-annotate-session-author
                (simply-annotate--select-author initial-author current-author))))
     ((eq simply-annotate-prompt-for-author 'threads-only)
      (if (eq context 'reply)
          (simply-annotate--select-author initial-author current-author)
        initial-author))
     (t initial-author))))

(defun simply-annotate--select-author (&optional default-author current-author)
  "Prompt user to select an author from the configured list.
DEFAULT-AUTHOR is pre-selected. CURRENT-AUTHOR is shown when editing."
  (let* ((authors (if (> (length simply-annotate-author-list) 1)
                      simply-annotate-author-list
                    (append simply-annotate-author-list (list "Other..."))))
         (prompt (if current-author
                     (format "Author (current: %s): " current-author)
                   "Author: "))
         (default (or default-author (car authors)))
         (choice (completing-read prompt authors nil nil nil nil default)))
    
    (if (string= choice "Other...")
        (let ((custom-author (read-string "Enter author name: " default)))
          (when (and custom-author (not (string-empty-p custom-author)))
            (unless (member custom-author simply-annotate-author-list)
              (customize-save-variable 'simply-annotate-author-list
                                       (append simply-annotate-author-list (list custom-author))))
            custom-author))
      choice)))

(defun simply-annotate--remember-author (author)
  "Remember the chosen AUTHOR for the current file if enabled."
  (when (and simply-annotate-remember-author-per-file author)
    (let ((file-key (simply-annotate--file-key)))
      (setf (alist-get file-key simply-annotate-file-authors nil nil #'string=) author))))

;;; Serialization

(defun simply-annotate--serialize-annotations ()
  "Convert buffer annotations to serializable format."
  (mapcar (lambda (overlay)
            `((start . ,(overlay-start overlay))
              (end . ,(overlay-end overlay))
              (text . ,(overlay-get overlay 'simply-annotation))
              (level . ,(or (overlay-get overlay 'simply-annotation-level)
                            simply-annotate-default-level))))
          simply-annotate-overlays))

(defun simply-annotate--deserialize-annotations (annotations)
  "Restore ANNOTATIONS from serialized format."
  (dolist (ann annotations)
    (let ((start (alist-get 'start ann))
          (end (alist-get 'end ann))
          (text (alist-get 'text ann))
          (level (or (alist-get 'level ann) 'defun)))
      (when (and start end text
                 (<= start (point-max))
                 (<= end (point-max))
                 (> end start))
        (let ((ov (simply-annotate--create-overlay start end text)))
          (overlay-put ov 'simply-annotation-level level)
          (push ov simply-annotate-overlays))))))

(defun simply-annotate--save-annotations ()
  "Save current buffer's annotations."
  (when simply-annotate-mode
    (let ((file-key (simply-annotate--file-key)))
      (when file-key
        (let ((annotations (simply-annotate--serialize-annotations)))
          (when (or annotations
                    (let ((db (simply-annotate--load-database)))
                      (and db (alist-get file-key db nil nil #'string=))))
            (simply-annotate--update-database file-key annotations)))))))

(defun simply-annotate--load-annotations ()
  "Load annotations for current buffer."
  (let* ((file-key (simply-annotate--file-key))
         (db (simply-annotate--load-database))
         (annotations (when db (alist-get file-key db nil nil #'string=))))
    (when annotations
      (simply-annotate--deserialize-annotations annotations))))

;;; Annotation Buffer Management

(defvar simply-annotate-annotation-mode-map)

(defun simply-annotate--apply-annotation-keymap ()
  "Apply annotation buffer keybindings on top of the current major mode keymap.
This uses `make-composed-keymap' so our bindings (C-c C-c, C-c C-k, etc.)
take priority while the underlying mode's bindings remain accessible."
  (use-local-map (make-composed-keymap simply-annotate-annotation-mode-map
                                       (current-local-map))))

(defun simply-annotate--editing-major-mode ()
  "Activate the appropriate major mode for annotation editing.
Uses `org-mode' when `simply-annotate-use-org-editing' is non-nil,
otherwise `fundamental-mode'."
  (if simply-annotate-use-org-editing
      (org-mode)
    (fundamental-mode))
  (simply-annotate--apply-annotation-keymap))

(defun simply-annotate--get-annotation-buffer ()
  "Get or create the annotation buffer."
  (get-buffer-create simply-annotate-buffer-name))

(defun simply-annotate--update-annotation-buffer (annotation-data overlay &optional mode edit-text)
  "Update annotation buffer with ANNOTATION-DATA and OVERLAY.
MODE can be \\='view, \\='edit, or \\='sexp (default is \\='view).
EDIT-TEXT, when non-nil, is the specific comment text to edit."
  (let ((buffer (simply-annotate--get-annotation-buffer))
        (source-buf (current-buffer))
        (mode (or mode 'view)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq simply-annotate-editing-annotation-sexp (eq mode 'sexp))
        (setq simply-annotate-reply-mode (eq mode 'reply))
        
        (cond
         ((eq mode 'sexp)
          (emacs-lisp-mode)
          (simply-annotate--apply-annotation-keymap)
          (visual-line-mode 1)
          (setq-local buffer-read-only nil)
          (buffer-disable-undo)
          (buffer-enable-undo)
          (let* ((data-to-print (if (simply-annotate--thread-p annotation-data)
                                    annotation-data
                                  (simply-annotate--create-thread
                                   (simply-annotate--annotation-text annotation-data))))
                 (print-level nil) (print-length nil))
            (pp data-to-print (current-buffer)))
          (setq simply-annotate-header-end-pos (point-min))
          (setq header-line-format (propertize " SEXP EDITING: C-c C-c to save, C-c C-k / C-g to cancel. " 'face 'highlight)))

         ((eq mode 'edit)
          (simply-annotate--editing-major-mode)
          (visual-line-mode 1)
          (setq-local buffer-read-only nil)
          (buffer-enable-undo)
          (let ((text (or edit-text (simply-annotate--annotation-text annotation-data))))
            (insert (simply-annotate--strip-boilerplate text)))
          (setq simply-annotate-header-end-pos (point-min))
          (setq header-line-format (propertize " EDITING: C-c C-c to save, C-c C-k / C-g to cancel. " 'face 'highlight)))

         ((eq mode 'reply)
          (simply-annotate--editing-major-mode)
          (visual-line-mode 1)
          (setq-local buffer-read-only nil)
          (buffer-enable-undo)
          ;; Reply buffer starts empty
          (setq simply-annotate-header-end-pos (point-min))
          (setq header-line-format (propertize " REPLYING: C-c C-c to save, C-c C-k / C-g to cancel. " 'face 'highlight)))

         (t ;; 'view mode
          (simply-annotate--editing-major-mode)
          (visual-line-mode 1)
          (setq-local buffer-read-only t)
          (if (simply-annotate--thread-p annotation-data)
              (insert (simply-annotate--format-thread-full annotation-data))
            (insert (simply-annotate--annotation-text annotation-data)))
          (setq simply-annotate-header-end-pos (point-min))
          (setq header-line-format (propertize " VIEW MODE " 'face 'highlight))))
      
      (goto-char (point-min))
      (setq simply-annotate-source-buffer source-buf
            simply-annotate-current-overlay overlay)))))

(defun simply-annotate--show-annotation-buffer (&optional select)
  "Show the annotation buffer in a window.
When SELECT is non-nil, move point to the annotation buffer."
  (let ((buffer (simply-annotate--get-annotation-buffer))
        (window (get-buffer-window simply-annotate-buffer-name)))

    (unless window
      (let ((height (max 3 (round (* (frame-height) simply-annotate-buffer-height)))))
        (setq window (split-window-below (- height)))
        (set-window-buffer window buffer)
        (set-window-dedicated-p window t)))

    (set-window-buffer window buffer)
    (when select (select-window window))
    window))

(defun simply-annotate-hide-annotation-buffer ()
  "Hide the annotation buffer."
  (interactive)
  (let ((window (get-buffer-window simply-annotate-buffer-name)))
    (when window (delete-window window))))

(defun simply-annotate-save-annotation-buffer ()
  "Enhanced save function that handles both strings and threads."
  (interactive)
  
  (let* ((overlay (if (string= (buffer-name) simply-annotate-buffer-name)
                      simply-annotate-current-overlay
                    (simply-annotate--overlay-at-point)))
         (is-draft (overlay-get overlay 'simply-annotation-draft))
         (current-data (overlay-get overlay 'simply-annotation))
         (final-data nil))
    
    (if simply-annotate-editing-annotation-sexp
        ;; In raw sexp edit mode
        (condition-case err
            (progn
              (setq final-data (read (buffer-string)))
              (unless (simply-annotate--thread-p final-data)
                (user-error "Invalid annotation format: must be a thread alist"))
              
              (when is-draft
                (unless (alist-get 'id final-data)
                  (setf (alist-get 'id final-data) (format "thread-%s-%06d" (format-time-string "%s") (random 1000000))))
                (unless (alist-get 'comments final-data)
                  (setf (alist-get 'comments final-data)
                        (list `((author . ,(simply-annotate--author-for-context 'annotation))
                                (timestamp . ,(simply-annotate--timestamp))
                                (text . "Initial comment text (edited)")
                                (type . "comment")))))))
          (error
           (message "Error saving annotation: %s" (error-message-string err))
           (cl-return-from simply-annotate-save-annotation-buffer)))
      ;; Text-based editing mode
      (let ((content (simply-annotate--strip-boilerplate
                      (buffer-substring simply-annotate-header-end-pos (point-max)))))
        (if (string-empty-p content)
            (progn
              (simply-annotate--cancel-annotation overlay is-draft)
              (cl-return-from simply-annotate-save-annotation-buffer))
          (setq final-data (cond
                            (simply-annotate-reply-mode
                             (let ((new-thread (simply-annotate--add-reply
                                                (if (simply-annotate--thread-p current-data)
                                                    current-data
                                                  (simply-annotate--create-thread current-data))
                                                content
                                                nil
                                                simply-annotate-reply-parent-id)))
                               (setq simply-annotate-reply-mode nil)
                               (setq simply-annotate-reply-parent-id nil)
                               new-thread))
                            ((simply-annotate--thread-p current-data)
                             (prog1
                                 (simply-annotate--update-thread-comment
                                  current-data content simply-annotate-editing-comment-id)
                               (setq simply-annotate-editing-comment-id nil)))
                            (t (simply-annotate--create-thread content)))))))
    
    (simply-annotate--finalize-annotation overlay final-data is-draft)))

(defun simply-annotate--cancel-annotation (overlay is-draft)
  "Cancel annotation editing for OVERLAY, handling IS-DRAFT state."
  (with-current-buffer simply-annotate-source-buffer
    (if is-draft
        (progn
          (delete-overlay simply-annotate-draft-overlay)
          (setq simply-annotate-draft-overlay nil))
      (progn
        (simply-annotate--remove-overlay overlay)
        (simply-annotate--save-annotations)))
    (simply-annotate--update-header))
  (simply-annotate-hide-annotation-buffer)
  (message "Annotation cancelled/removed"))

(defun simply-annotate--update-thread-comment (thread content &optional comment-id)
  "Update a comment in THREAD with CONTENT.
When COMMENT-ID is non-nil, update the comment with that id.
Otherwise update the first comment."
  (let ((thread-copy (copy-alist thread)))
    (setf (alist-get 'comments thread-copy)
          (mapcar (lambda (comment)
                    (let ((c (copy-alist comment)))
                      (when (if comment-id
                                (equal (alist-get 'id c) comment-id)
                              (eq comment (car (alist-get 'comments thread))))
                        (setf (alist-get 'text c) content))
                      c))
                  (alist-get 'comments thread-copy)))
    thread-copy))

(defun simply-annotate--update-thread-first-comment (thread content)
  "Update the first comment of THREAD with CONTENT."
  (simply-annotate--update-thread-comment thread content nil))

(defun simply-annotate--finalize-annotation (overlay final-data is-draft)
  "Finalize annotation for OVERLAY with FINAL-DATA, handling IS-DRAFT state."
  (overlay-put overlay 'simply-annotation final-data)
  (overlay-put overlay 'help-echo (simply-annotate--annotation-summary final-data))
  (simply-annotate--refresh-overlay-display overlay)

  (with-current-buffer simply-annotate-source-buffer
    (when is-draft
      (overlay-put overlay 'simply-annotation-draft nil)
      (push overlay simply-annotate-overlays)
      (setq simply-annotate-draft-overlay nil))
    (simply-annotate--save-annotations)
    (simply-annotate--update-header "SAVED")
    (simply-annotate-hide-annotation-buffer)
    (when (use-region-p) (deactivate-mark)))
  
  (simply-annotate--invalidate-listing)
  (let ((author (if (simply-annotate--thread-p final-data)
                    (alist-get 'author (car (alist-get 'comments final-data)))
                  simply-annotate-default-author)))
    (message "Annotation saved by %s" author)))

(defun simply-annotate-cancel-edit ()
  "Cancel editing and restore read-only mode or clean up draft."
  (interactive)
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
          (when (and simply-annotate-current-overlay
                     (overlay-buffer simply-annotate-current-overlay))
            (simply-annotate--update-annotation-buffer
             (overlay-get simply-annotate-current-overlay 'simply-annotation)
             simply-annotate-current-overlay))))))
  (simply-annotate-hide-annotation-buffer)
  (simply-annotate--update-header "QUIT")
  (message "Edit cancelled"))

(defun simply-annotate--cleanup-draft ()
  "Clean up any draft overlays when disabling mode."
  (when simply-annotate-draft-overlay
    (delete-overlay simply-annotate-draft-overlay)
    (setq simply-annotate-draft-overlay nil)))

;;; Header Management

(defun simply-annotate--annotation-number (target-overlay)
  "Get the position number of TARGET-OVERLAY among active-level annotations."
  (when target-overlay
    (with-current-buffer (overlay-buffer target-overlay)
      (let ((sorted-active (seq-sort-by #'overlay-start #'<
                                        (simply-annotate--active-overlays))))
        (1+ (or (cl-position target-overlay sorted-active) 0))))))

(defun simply-annotate--level-counts ()
  "Return a formatted string showing annotation counts per level.
The active level is shown in bold."
  (let* ((levels (append simply-annotate-levels '(all)))
         (current-overlay (simply-annotate--overlay-at-point))
         (current-num (when current-overlay
                        (simply-annotate--annotation-number current-overlay))))
    (mapconcat
     (lambda (level)
       (let* ((name (upcase (symbol-name level)))
              (n (if (eq level 'all)
                     (length simply-annotate-overlays)
                   (cl-count-if
                    (lambda (ov)
                      (eq (or (overlay-get ov 'simply-annotation-level) 'defun) level))
                    simply-annotate-overlays)))
              (label (if (and (eq level simply-annotate-current-level) current-num)
                         (format "%s: %d/%d" name current-num n)
                       (format "%s:%d" name n))))
         (if (eq level simply-annotate-current-level)
             (propertize label 'face '(bold :height 0.9))
           (propertize label 'face '(:height 0.9)))))
     levels
     (propertize " | " 'face '(:height 0.9)))))

(defun simply-annotate--format-header (&optional text)
  "Header format showing level counts with embedded position.
Optional TEXT is appended (e.g. status messages)."
  (let ((total (length simply-annotate-overlays)))
    (when (> total 0)
      (concat
       " "
       (simply-annotate--level-counts)
       " "
       (if-let* ((overlay (simply-annotate--overlay-at-point))
                 (annotation-data (overlay-get overlay 'simply-annotation))
                 (thread (and (simply-annotate--thread-p annotation-data) annotation-data)))
           (let ((status (alist-get 'status thread))
                 (priority (alist-get 'priority thread))
                 (comment-count (length (alist-get 'comments thread))))
             (propertize
              (format "[%s/%s:%d] " (upcase status) (upcase priority) comment-count)
              'face '(:height 0.9)))
         "")
       (if text (concat text " ") "")))))

(defun simply-annotate--update-header (&optional text)
  "Enhanced header update that handles threading information with optional TEXT."
  (let ((overlay (simply-annotate--overlay-at-point))
        (new-annotation nil))
    
    (when overlay
      (let ((annotation-data (overlay-get overlay 'simply-annotation)))
        (setq new-annotation (if (simply-annotate--thread-p annotation-data)
                                 (simply-annotate--annotation-summary annotation-data)
                               annotation-data))))
    
    (unless (equal new-annotation simply-annotate-current-annotation)
      (setq simply-annotate-current-annotation new-annotation))
    
    (setq header-line-format (simply-annotate--format-header text))
    (force-mode-line-update t)))

(defun simply-annotate--setup-header ()
  "Setup header-line for annotation display."
  (setq simply-annotate-original-header-line header-line-format
        header-line-format (simply-annotate--format-header)))

(defun simply-annotate--cleanup-header ()
  "Restore original header-line."
  (setq header-line-format simply-annotate-original-header-line
        simply-annotate-current-annotation nil))

;;; Navigation

(defun simply-annotate--sorted-overlays ()
  "Get annotation overlays sorted by position."
  (sort (copy-sequence simply-annotate-overlays)
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun simply-annotate--find-annotation (forward &optional wrap)
  "Find next/previous visible annotation.
FORWARD t for next, nil for previous.
If WRAP is non-nil, wrap around to the beginning/end."
  (let* ((pos (point))
         (active (cl-remove-if-not
                  #'simply-annotate--level-match-p
                  (if forward
                      (simply-annotate--sorted-overlays)
                    (reverse (simply-annotate--sorted-overlays)))))
         (test-fn (if forward
                      (lambda (ov) (> (overlay-start ov) pos))
                    (lambda (ov) (< (overlay-end ov) pos))))
         (found (cl-find-if test-fn active)))

    (when (and (not found) wrap active)
      (setq found (car active)))
    found))

(defun simply-annotate--navigate-to-overlay (overlay)
  "Enhanced navigation that handles both string and thread annotations for OVERLAY."
  (when overlay
    (goto-char (overlay-start overlay))
    (pulse-momentary-highlight-region (overlay-start overlay)
                                      (overlay-end overlay))
    
    (let ((annotation-data (overlay-get overlay 'simply-annotation)))
      (when (get-buffer-window simply-annotate-buffer-name)
        (simply-annotate--update-annotation-buffer annotation-data overlay 'view)
        (simply-annotate--show-annotation-buffer)))
    
    (simply-annotate--update-header)))

;;;###autoload
(defun simply-annotate-show ()
  "Enhanced version that handles threading."
  (interactive)
  (if-let ((overlay (simply-annotate--overlay-at-point)))
      (let ((annotation-data (overlay-get overlay 'simply-annotation)))
        (simply-annotate--update-annotation-buffer annotation-data overlay 'view)
        (simply-annotate--show-annotation-buffer))))

;;;###autoload
(defun simply-annotate-next ()
  "Navigate to next annotation."
  (interactive)
  (let ((source-buffer (if (string= (buffer-name) simply-annotate-buffer-name)
                           simply-annotate-source-buffer
                         (current-buffer))))
    (if (and source-buffer (buffer-live-p source-buffer))
        (with-current-buffer source-buffer
          (if-let ((next-overlay (simply-annotate--find-annotation t t)))
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
          (if-let ((prev-overlay (simply-annotate--find-annotation nil t)))
              (simply-annotate--navigate-to-overlay prev-overlay)
            (message "No more annotations in buffer")))
      (message "Source buffer not available"))))

;;; Interactive Commands

(defun simply-annotate-smart-action ()
  "Enhanced smart action with author support."
  (interactive)
  (cond
   ((use-region-p)
    (simply-annotate--handle-region-action))
   ((simply-annotate--overlay-at-point)
    (simply-annotate--handle-overlay-action))
   (t
    (simply-annotate--handle-line-action))))

(defun simply-annotate--handle-region-action ()
  "Handle smart action when region is selected."
  (let* ((start (region-beginning))
         (end (region-end))
         (existing-overlay (simply-annotate--overlay-at-point start)))
    
    (if existing-overlay
        (simply-annotate--edit-annotation existing-overlay)
      (simply-annotate--create-new-annotation start end))
    (deactivate-mark)))

(defun simply-annotate--handle-overlay-action ()
  "Handle smart action when cursor is on an overlay."
  (let* ((existing-overlay (simply-annotate--overlay-at-point))
         (annotation-data (overlay-get existing-overlay 'simply-annotation))
         (buffer-window (get-buffer-window simply-annotate-buffer-name)))

    (if (= (prefix-numeric-value current-prefix-arg) 4)
        (simply-annotate--edit-annotation existing-overlay)
      (if buffer-window
          (simply-annotate-hide-annotation-buffer)
        (progn
          (simply-annotate--update-annotation-buffer annotation-data existing-overlay 'view)
          (simply-annotate--show-annotation-buffer))))))

(defun simply-annotate--handle-line-action ()
  "Handle smart action when no region or overlay."
  (let ((buffer-window (get-buffer-window simply-annotate-buffer-name)))
    (if buffer-window
        (simply-annotate-hide-annotation-buffer)
      (let* ((start (line-beginning-position))
             (end (line-end-position)))
        (simply-annotate--create-new-annotation start end)))))

(defun simply-annotate--select-edit-target (thread)
  "Prompt user to select which comment in THREAD to edit.
Returns a cons of (comment-id . comment-text)."
  (simply-annotate--ensure-comment-ids thread)
  (let* ((comments (alist-get 'comments thread))
         (tree (simply-annotate--build-comment-tree comments))
         (flat (simply-annotate--flatten-comment-tree tree))
         (choices (mapcar
                   (lambda (pair)
                     (let* ((comment (car pair))
                            (depth (cdr pair))
                            (indent (make-string (* 2 depth) ?\s))
                            (author (alist-get 'author comment))
                            (text (alist-get 'text comment))
                            (preview (truncate-string-to-width
                                      (car (split-string text "\n")) 50 nil nil "...")))
                       (cons (format "%s%s%s: %s"
                                     indent
                                     (if (> depth 0) "↳ " "")
                                     (or author "Unknown")
                                     preview)
                             comment)))
                   flat))
         (selected (completing-read "Edit comment: " (mapcar #'car choices) nil t))
         (comment (cdr (assoc selected choices))))
    (cons (alist-get 'id comment) (alist-get 'text comment))))

(defun simply-annotate--edit-annotation (overlay)
  "Edit existing annotation OVERLAY.
For threads with multiple comments, prompts which comment to edit."
  (goto-char (overlay-start overlay))
  (let* ((annotation-data (overlay-get overlay 'simply-annotation))
         (is-thread (simply-annotate--thread-p annotation-data))
         (comments (when is-thread (alist-get 'comments annotation-data)))
         (target (cond
                  ((not is-thread) nil)
                  ((<= (length comments) 1) nil)
                  (t (simply-annotate--select-edit-target annotation-data))))
         (edit-id (car target))
         (edit-text (or (cdr target)
                        (simply-annotate--annotation-text annotation-data))))
    (setq simply-annotate-editing-comment-id edit-id)
    (simply-annotate--update-header "EDITING")
    (simply-annotate--update-annotation-buffer annotation-data overlay 'edit edit-text)
    (simply-annotate--show-annotation-buffer t)
    (goto-char simply-annotate-header-end-pos)
))

(defun simply-annotate--create-new-annotation (start end)
  "Create new annotation from START to END."
  (let ((draft-overlay (simply-annotate--create-overlay start end "")))
    (simply-annotate--update-header "EDITING")
    (overlay-put draft-overlay 'simply-annotation-draft t)
    (overlay-put draft-overlay 'simply-annotation-level simply-annotate-current-level)
    (setq simply-annotate-draft-overlay draft-overlay)

    (simply-annotate--update-annotation-buffer "" draft-overlay 'edit)
    (simply-annotate--show-annotation-buffer t)

    (goto-char simply-annotate-header-end-pos)))

;;;###autoload
(defun simply-annotate-edit-sexp ()
  "Edit the current annotation as a raw Elisp sexp."
  (interactive)
  (let ((overlay (if (string= (buffer-name) simply-annotate-buffer-name)
                     simply-annotate-current-overlay
                   (simply-annotate--overlay-at-point))))
    (if overlay
        (progn
          (goto-char (overlay-start overlay))
          (simply-annotate--update-header "EDITING SEXP")

          (simply-annotate--update-annotation-buffer (overlay-get overlay 'simply-annotation) overlay 'sexp)
          (simply-annotate--show-annotation-buffer t)
          (goto-char (point-min)))
      (message "No annotation at point to edit."))))

;;;###autoload
(defun simply-annotate-remove ()
  "Remove annotation at point."
  (interactive)
  (if-let ((overlay (simply-annotate--overlay-at-point)))
      (when (y-or-n-p "Really remove annotation? ")
        (simply-annotate--remove-overlay overlay)
        (simply-annotate--save-annotations)
        (simply-annotate--update-header)
        (simply-annotate--invalidate-listing)
        (message "Annotation removed"))
    (message "No annotation at point")))

;;; Threading Commands

(defun simply-annotate--flatten-comment-tree (tree &optional depth)
  "Flatten comment TREE into an ordered list of (comment . depth) pairs.
DEPTH defaults to 0 for root nodes."
  (let ((d (or depth 0))
        (result '()))
    (dolist (node tree)
      (push (cons (car node) d) result)
      (when (cdr node)
        (setq result (append (nreverse
                              (simply-annotate--flatten-comment-tree (cdr node) (1+ d)))
                             result))))
    (nreverse result)))

(defun simply-annotate--select-reply-target (thread)
  "Prompt user to select which comment in THREAD to reply to.
Returns the comment ID of the selected comment."
  (simply-annotate--ensure-comment-ids thread)
  (let* ((comments (alist-get 'comments thread))
         (tree (simply-annotate--build-comment-tree comments))
         (flat (simply-annotate--flatten-comment-tree tree))
         (choices (mapcar
                   (lambda (pair)
                     (let* ((comment (car pair))
                            (depth (cdr pair))
                            (indent (make-string (* 2 depth) ?\s))
                            (author (alist-get 'author comment))
                            (text (alist-get 'text comment))
                            (preview (truncate-string-to-width
                                      (car (split-string text "\n")) 50 nil nil "...")))
                       (cons (format "%s%s%s: %s"
                                     indent
                                     (if (> depth 0) "↳ " "")
                                     (or author "Unknown")
                                     preview)
                             (alist-get 'id comment))))
                   flat))
         (selected (completing-read "Reply to: " (mapcar #'car choices) nil t))
         (comment-id (cdr (assoc selected choices))))
    comment-id))

(defun simply-annotate-reply-to-annotation ()
  "Enhanced reply function using the *Annotation* buffer.
When the thread has multiple comments, prompts to select which
comment to reply to for nested threading."
  (interactive)
  (let ((overlay (if (string= (buffer-name) simply-annotate-buffer-name)
                     simply-annotate-current-overlay
                   (simply-annotate--overlay-at-point))))
    (if overlay
        (let* ((current-data (overlay-get overlay 'simply-annotation))
               (is-thread (simply-annotate--thread-p current-data))
               (comments (when is-thread (alist-get 'comments current-data)))
               (parent-id (cond
                           ;; Not a thread yet - no parent needed
                           ((not is-thread) nil)
                           ;; Single comment - reply to it directly
                           ((<= (length comments) 1)
                            (alist-get 'id (car comments)))
                           ;; Multiple comments - prompt user
                           (t (simply-annotate--select-reply-target current-data)))))
          (setq simply-annotate-reply-parent-id parent-id)
          (simply-annotate--update-annotation-buffer current-data overlay 'reply)
          (simply-annotate--show-annotation-buffer t))
      (message "No annotation at point"))))

(defun simply-annotate-migrate-threading ()
  "Migrate all annotations in the database to nested threading format.
Assigns comment IDs and infers parent-child relationships from the
type field: replies become children of the first root comment in
each thread.  Safe to run multiple times."
  (interactive)
  (let* ((db (simply-annotate--load-database))
         (migrated 0))
    (if (not db)
        (message "No annotation database found")
      (dolist (file-entry db)
        (dolist (ann (cdr file-entry))
          (let ((data (alist-get 'text ann)))
            (when (simply-annotate--thread-p data)
              (let ((comments (alist-get 'comments data)))
                (when (cl-some (lambda (c) (not (alist-get 'id c))) comments)
                  (simply-annotate--ensure-comment-ids data)
                  (cl-incf migrated)))))))
      (simply-annotate--save-database db)
      (message "Migration complete: %d thread%s updated"
               migrated (if (= migrated 1) "" "s")))))

(defun simply-annotate-set-annotation-status ()
  "Set the status of annotation at point."
  (interactive)
  (simply-annotate--set-annotation-property 'status simply-annotate-thread-statuses "Status"))

(defun simply-annotate-set-annotation-priority ()
  "Set the priority of annotation at point."
  (interactive)
  (simply-annotate--set-annotation-property 'priority simply-annotate-priority-levels "Priority"))

(defun simply-annotate--set-annotation-property (property valid-values prompt)
  "Set PROPERTY of annotation at point from VALID-VALUES with PROMPT."
  (let ((overlay (if (string= (buffer-name) simply-annotate-buffer-name)
                     simply-annotate-current-overlay
                   (simply-annotate--overlay-at-point))))
    (if overlay
        (let* ((current-data (overlay-get overlay 'simply-annotation))
               (thread (if (simply-annotate--thread-p current-data)
                           current-data
                         (simply-annotate--create-thread current-data)))
               (value (completing-read (concat prompt ": ") valid-values)))
          (simply-annotate--set-thread-property thread property value valid-values)
          (overlay-put overlay 'simply-annotation thread)
          (overlay-put overlay 'help-echo (simply-annotate--annotation-summary thread))
          (simply-annotate--refresh-overlay-display overlay)
          (simply-annotate--save-annotations)
          (simply-annotate--update-header)
          
          (when (get-buffer-window simply-annotate-buffer-name)
            (simply-annotate--update-annotation-buffer thread overlay 'view)
            (simply-annotate--show-annotation-buffer))

          (simply-annotate--invalidate-listing)
          (message "%s set to: %s" prompt value))
      (message "No annotation at point"))))

(defun simply-annotate-add-annotation-tag ()
  "Add a tag to annotation at point."
  (interactive)
  (let ((overlay (if (string= (buffer-name) simply-annotate-buffer-name)
                     simply-annotate-current-overlay
                   (simply-annotate--overlay-at-point))))
    (if overlay
        (let* ((current-data (overlay-get overlay 'simply-annotation))
               (thread (if (simply-annotate--thread-p current-data)
                           current-data
                         (simply-annotate--create-thread current-data)))
               (tag (read-string "Tag: ")))
          (when (not (string-empty-p tag))
            (simply-annotate--add-thread-tag thread tag)
            (overlay-put overlay 'simply-annotation thread)
            (overlay-put overlay 'help-echo (simply-annotate--annotation-summary thread))
            (simply-annotate--refresh-overlay-display overlay)
            (simply-annotate--save-annotations)
            (simply-annotate--update-header)

            (when (get-buffer-window simply-annotate-buffer-name)
              (simply-annotate--update-annotation-buffer thread overlay 'view)
              (simply-annotate--show-annotation-buffer))

            (simply-annotate--invalidate-listing)
            (message "Tag '%s' added" tag)))
      (message "No annotation at point"))))

(defun simply-annotate-change-annotation-author ()
  "Change the author of the current annotation or specific comment in a thread."
  (interactive)
  (if-let ((overlay (simply-annotate--overlay-at-point)))
      (let* ((current-data (overlay-get overlay 'simply-annotation)))
        (if (simply-annotate--thread-p current-data)
            (simply-annotate--change-thread-author overlay current-data)
          (simply-annotate--convert-to-thread-with-author overlay current-data)))
    (message "No annotation at point")))

(defun simply-annotate--change-thread-author (overlay thread)
  "Change author for THREAD in OVERLAY."
  (simply-annotate--ensure-comment-ids thread)
  (let* ((comments (alist-get 'comments thread))
         (tree (simply-annotate--build-comment-tree comments))
         (flat (simply-annotate--flatten-comment-tree tree))
         (comment-choices (mapcar
                           (lambda (pair)
                             (let* ((comment (car pair))
                                    (depth (cdr pair))
                                    (indent (make-string (* 2 depth) ?\s))
                                    (author (alist-get 'author comment))
                                    (text (alist-get 'text comment)))
                               (format "%s%s%s: %s"
                                       indent
                                       (if (> depth 0) "↳ " "")
                                       author
                                       (truncate-string-to-width
                                        (car (split-string text "\n")) 40 nil nil "..."))))
                           flat))
         (selected (completing-read "Change author for: " comment-choices nil t))
         (choice-index (cl-position selected comment-choices :test #'string=))
         (selected-comment (car (nth choice-index flat)))
         (current-author (alist-get 'author selected-comment))
         (new-author (simply-annotate--select-author current-author current-author)))
    
    (when new-author
      (setf (alist-get 'author selected-comment) new-author)
      (overlay-put overlay 'simply-annotation thread)
      (overlay-put overlay 'help-echo (simply-annotate--annotation-summary thread))
      (simply-annotate--refresh-overlay-display overlay)
      (simply-annotate--save-annotations)
      (simply-annotate--invalidate-listing)
      (message "Author changed from %s to %s" current-author new-author))))

(defun simply-annotate--convert-to-thread-with-author (overlay current-data)
  "Convert simple annotation in OVERLAY with CURRENT-DATA."
  (let* ((current-text (simply-annotate--annotation-text current-data))
         (new-author (simply-annotate--select-author))
         (new-thread (simply-annotate--create-thread current-text new-author)))
    (when new-author
      (overlay-put overlay 'simply-annotation new-thread)
      (overlay-put overlay 'help-echo (simply-annotate--annotation-summary new-thread))
      (simply-annotate--refresh-overlay-display overlay)
      (simply-annotate--save-annotations)
      (simply-annotate--invalidate-listing)
      (message "Annotation converted to thread with author: %s" new-author))))

;;; Export and Listing

(defun simply-annotate--format-annotations-for-buffer (file-key annotations source-buffer buffer-name)
  "Format ANNOTATIONS for FILE-KEY from SOURCE-BUFFER into BUFFER-NAME.
Uses org-mode for folding with a custom minor mode for navigation."
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (simply-annotate--insert-header file-key annotations)
      (simply-annotate--insert-formatted-annotations file-key annotations source-buffer)
      (simply-annotate--finish-annotation-list-mode)
      (setq simply-annotate-listing-source source-buffer)
      (goto-char (point-min)))
    (current-buffer)))

(defun simply-annotate--insert-refresh-button ()
  "Insert a clickable [Refresh] button at the top of the listing buffer."
  (let ((start (point)))
    (insert "[Refresh]")
    (make-text-button start (point)
                      'action (lambda (_button)
                                (simply-annotate-listing-refresh))
                      'follow-link t
                      'help-echo "Rebuild the annotation listing")
    (insert "\n")))

(defun simply-annotate--insert-header (file-key annotations)
  "Insert header with summary stats for FILE-KEY and ANNOTATIONS."
  (let* ((total (length annotations))
         (open-count 0)
         (resolved-count 0))

    (dolist (ann annotations)
      (let ((data (alist-get 'text ann)))
        (if (simply-annotate--thread-p data)
            (let ((status (alist-get 'status data)))
              (if (member status '("resolved" "closed"))
                  (setq resolved-count (1+ resolved-count))
                (setq open-count (1+ open-count))))
          (setq open-count (1+ open-count)))))

    (simply-annotate--insert-refresh-button)
    (let ((start (point)))
      (insert (format "#+TITLE: Annotations for %s\n" (simply-annotate--key-name file-key)))
      (put-text-property start (point) 'simply-annotate-nav (list :file file-key)))
    (insert (format "#+STARTUP: show2levels\n"))
    (insert (format "Total: %d" total))
    (dolist (level simply-annotate-levels)
      (let ((n (cl-count-if
                (lambda (ann) (eq (or (alist-get 'level ann) 'defun) level))
                annotations)))
        (insert (format " | %s: %d" (upcase (symbol-name level)) n))))
    (insert (format " | Open: %d | Resolved: %d\n\n"
                    open-count resolved-count))))

(defun simply-annotate--insert-formatted-annotations (file-key annotations source-buffer &optional depth)
  "Insert formatted ANNOTATIONS for FILE-KEY from SOURCE-BUFFER grouped by level.
DEPTH controls heading level offset (default 0)."
  (let ((d (or depth 0))
        (line-table (simply-annotate--batch-line-info annotations source-buffer)))
    (dolist (level simply-annotate-levels)
      (let ((level-annotations (cl-remove-if-not
                                (lambda (ann) (eq (or (alist-get 'level ann) 'defun) level))
                                annotations)))
        (when level-annotations
          (let* ((name (upcase (symbol-name level)))
                 (count (length level-annotations))
                 (stars (make-string (+ 1 d) ?*)))
            (insert (format "%s %s (%d)\n" stars name count)))

          (let ((sorted-annotations (simply-annotate--sort-annotations level-annotations)))
            (dolist (ann sorted-annotations)
              (let* ((start-pos (alist-get 'start ann))
                     (annotation-data (alist-get 'text ann))
                     (line-info (gethash start-pos line-table
                                         (list 1 0 "Content not available"))))

                (if (simply-annotate--thread-p annotation-data)
                    (simply-annotate--insert-thread-annotation annotation-data line-info file-key level d)
                  (simply-annotate--insert-simple-annotation annotation-data line-info file-key level d))))))))))

(defun simply-annotate--sort-annotations (annotations)
  "Sort ANNOTATIONS by line position, then by status (open items first)."
  (sort annotations
        (lambda (a b)
          (let ((start-a (alist-get 'start a))
                (start-b (alist-get 'start b))
                (data-a (alist-get 'text a))
                (data-b (alist-get 'text b)))
            (if (= start-a start-b)
                (let ((status-a (if (simply-annotate--thread-p data-a)
                                    (alist-get 'status data-a)
                                  "open"))
                      (status-b (if (simply-annotate--thread-p data-b)
                                    (alist-get 'status data-b)
                                  "open")))
                  (cond
                   ((and (member status-a '("open" "in-progress"))
                         (member status-b '("resolved" "closed"))) t)
                   ((and (member status-a '("resolved" "closed"))
                         (member status-b '("open" "in-progress"))) nil)
                   (t (string< status-a status-b))))
              (< start-a start-b))))))

(defun simply-annotate--batch-line-info (annotations source-buffer)
  "Pre-compute line info for all ANNOTATIONS from SOURCE-BUFFER in one pass.
Returns a hash table mapping start positions to (line-number column content)."
  (let ((table (make-hash-table :test 'eql)))
    (if (not (and source-buffer (buffer-live-p source-buffer)))
        (dolist (ann annotations)
          (puthash (alist-get 'start ann) (list 1 0 "Content not available") table))
      (with-current-buffer source-buffer
        (save-excursion
          (let* ((pmin (point-min))
                 (pmax (point-max))
                 (sorted (sort (copy-sequence annotations)
                               (lambda (a b) (< (alist-get 'start a) (alist-get 'start b)))))
                 (cur-line 1)
                 (cur-pos pmin))
            (dolist (ann sorted)
              (let* ((start-pos (alist-get 'start ann))
                     (end-pos (alist-get 'end ann))
                     (start (max pmin (min start-pos pmax)))
                     (end (max pmin (min end-pos pmax))))
                ;; Count newlines from cur-pos to start (incremental)
                (setq cur-line (+ cur-line (count-lines cur-pos start)))
                (setq cur-pos start)
                (goto-char start)
                (puthash start-pos
                         (list cur-line
                               (current-column)
                               (buffer-substring-no-properties start end))
                         table)))))))
    table))

(defun simply-annotate--insert-thread-annotation (thread line-info file-key level &optional depth)
  "Insert formatted THREAD annotation with LINE-INFO for FILE-KEY at LEVEL.
DEPTH controls heading level offset (default 0)."
  (let* ((d (or depth 0))
         (h2 (make-string (+ 2 d) ?*))
         (h3 (make-string (+ 3 d) ?*))
         (status (alist-get 'status thread))
         (priority (alist-get 'priority thread))
         (tags (alist-get 'tags thread))
         (comments (alist-get 'comments thread))
         (comment-count (length comments))
         (line-num (car line-info))
         (col-num (cadr line-info))
         (line-content (caddr line-info))
         (heading-start (point)))

    ;; Annotation heading
    (insert (format "%s [%s/%s] Line %d" h2 (upcase status) (upcase priority) line-num))
    (when tags
      (insert (format " #%s" (string-join tags " #"))))
    (insert (format " (%d comment%s)\n"
                    comment-count
                    (if (= comment-count 1) "" "s")))
    ;; Store navigation properties on the heading
    (put-text-property heading-start (point)
                       'simply-annotate-nav
                       (list :file file-key :line line-num
                             :col (1+ col-num) :level level))

    ;; Annotated region under its own heading
    (insert (format "%s Region\n" h3))
    (dolist (line (split-string (string-trim line-content) "\n"))
      (insert (format "~%s~\n" line)))

    ;; Comments as nested headings reflecting thread hierarchy
    (simply-annotate--ensure-comment-ids thread)
    (let ((tree (simply-annotate--build-comment-tree
                 (alist-get 'comments thread))))
      (simply-annotate--insert-comment-tree tree (+ 3 d)))))

(defun simply-annotate--short-timestamp (timestamp)
  "Extract a short MM/DD HH:MM display from ISO TIMESTAMP string.
Avoids the overhead of `date-to-time' + `format-time-string'."
  (if (and timestamp (>= (length timestamp) 16))
      (let ((month (substring timestamp 5 7))
            (day (substring timestamp 8 10))
            (hour (substring timestamp 11 13))
            (min (substring timestamp 14 16)))
        (format "%s/%s %s:%s" month day hour min))
    (or timestamp "")))

(defun simply-annotate--insert-comment-tree (tree heading-level)
  "Insert comment TREE nodes as org headings at HEADING-LEVEL.
Recursively inserts children at deeper heading levels."
  (dolist (node tree)
    (let* ((comment (car node))
           (children (cdr node))
           (stars (make-string heading-level ?*))
           (author (alist-get 'author comment))
           (timestamp (alist-get 'timestamp comment))
           (text (alist-get 'text comment))
           (type (alist-get 'type comment))
           (type-label (if (string= type "comment") "Comment" "Reply"))
           (formatted-time (simply-annotate--short-timestamp timestamp)))
      (insert (format "%s %s - %s (%s)\n" stars type-label author formatted-time))
      (dolist (line (split-string text "\n"))
        (insert (format "%s\n" line)))
      (when children
        (simply-annotate--insert-comment-tree children (1+ heading-level))))))

(defun simply-annotate--insert-simple-annotation (annotation-data line-info file-key level &optional depth)
  "Insert formatted simple ANNOTATION-DATA with LINE-INFO for FILE-KEY at LEVEL.
DEPTH controls heading level offset (default 0)."
  (let* ((d (or depth 0))
         (h2 (make-string (+ 2 d) ?*))
         (h3 (make-string (+ 3 d) ?*))
         (line-num (car line-info))
         (col-num (cadr line-info))
         (line-content (caddr line-info))
         (heading-start (point)))
    ;; Annotation heading
    (insert (format "%s Line %d\n" h2 line-num))
    ;; Store navigation properties on the heading
    (put-text-property heading-start (point)
                       'simply-annotate-nav
                       (list :file file-key :line line-num
                             :col (1+ col-num) :level level))
    ;; Annotated region under its own heading
    (insert (format "%s Region\n" h3))
    (dolist (line (split-string (string-trim line-content) "\n"))
      (insert (format "~%s~\n" line)))
    ;; Annotation text under its own heading
    (insert (format "%s Note\n" h3))
    (dolist (line (split-string (string-trim annotation-data) "\n"))
      (insert (format "%s\n" line)))))

(defun simply-annotate--listing-nav-props ()
  "Return the navigation plist for the annotation at or before point."
  (or (get-text-property (point) 'simply-annotate-nav)
      (save-excursion
        (let ((prev (previous-single-property-change (point) 'simply-annotate-nav)))
          (when prev
            (get-text-property (1- prev) 'simply-annotate-nav))))))

(defun simply-annotate--listing-goto-source (&optional nav)
  "Jump to the source location described by NAV plist.
If NAV is nil, derive it from point."
  (let ((props (or nav (simply-annotate--listing-nav-props))))
    (when props
      (let ((file (plist-get props :file))
            (line (plist-get props :line))
            (col  (plist-get props :col))
            (level (plist-get props :level)))
        (if-let ((source-buf (simply-annotate--get-key-buffer file)))
            (let ((win (display-buffer source-buf
                                       '(display-buffer-use-some-window
                                         (inhibit-same-window . t)))))
              (with-selected-window win
                (unless simply-annotate-mode
                  (simply-annotate-mode 1))
                (goto-char (point-min))
                (forward-line (1- line))
                (forward-char (max 0 (1- col)))
                (when (and (boundp 'simply-annotate-current-level)
                           level
                           (not (eq simply-annotate-current-level level)))
                  (setq simply-annotate-current-level level)
                  (simply-annotate--apply-level-filter)))))))))

(defun simply-annotate-listing-next ()
  "Move to the next annotation heading and jump to its source."
  (interactive)
  (let ((next (next-single-property-change (point) 'simply-annotate-nav)))
    (when next
      ;; If we landed at the end of a nav region, find the start of the next one
      (unless (get-text-property next 'simply-annotate-nav)
        (setq next (next-single-property-change next 'simply-annotate-nav)))
      (when next
        (goto-char next)
        (beginning-of-line)
        (simply-annotate--listing-goto-source)))))

(defun simply-annotate-listing-prev ()
  "Move to the previous annotation heading and jump to its source."
  (interactive)
  (let ((prev (previous-single-property-change (point) 'simply-annotate-nav)))
    (when prev
      (if (get-text-property (1- prev) 'simply-annotate-nav)
          (goto-char (1- prev))
        (let ((prev2 (previous-single-property-change prev 'simply-annotate-nav)))
          (when prev2
            (goto-char (1- prev2)))))
      (beginning-of-line)
      (simply-annotate--listing-goto-source))))

(defun simply-annotate--listing-file-at-point ()
  "Get the file key at point in a listing buffer."
  (let ((props (simply-annotate--listing-nav-props)))
    (plist-get props :file)))

(defun simply-annotate-listing-jump ()
  "Jump to the source location of the annotation at point.
On a file-level heading, open the file.  On an annotation heading,
jump to the source location."
  (interactive)
  (let ((file-key (simply-annotate--listing-file-at-point)))
    (if file-key
        (when-let ((buf (simply-annotate--get-key-buffer file-key t)))
          (let ((win (get-buffer-window buf)))
            (select-window win)
            (unless simply-annotate-mode
              (simply-annotate-mode 1))))
      (let ((props (get-text-property (point) 'simply-annotate-nav)))
        (when props
          (simply-annotate--listing-goto-source props))))))

(defun simply-annotate-listing-quit ()
  "Quit the annotation listing buffer."
  (interactive)
  (quit-window))

(defvar simply-annotate-listing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'simply-annotate-listing-next)
    (define-key map (kbd "p") #'simply-annotate-listing-prev)
    (define-key map (kbd "RET") #'simply-annotate-listing-jump)
    (define-key map (kbd "g") #'simply-annotate-listing-refresh)
    (define-key map (kbd "q") #'simply-annotate-listing-quit)
    map)
  "Keymap for simply-annotate-listing-mode.")

(define-minor-mode simply-annotate-listing-mode
  "Minor mode for navigating the annotation listing buffer.
\\<simply-annotate-listing-mode-map>
\\[simply-annotate-listing-next] - next annotation
\\[simply-annotate-listing-prev] - previous annotation
\\[simply-annotate-listing-jump] - jump to source
\\[simply-annotate-listing-quit] - quit"
  :lighter " SA-List"
  :keymap simply-annotate-listing-mode-map)

(defun simply-annotate--invalidate-listing ()
  "Kill the cached listing buffer so the next open rebuilds it."
  (when-let ((buf (get-buffer "*Annotations*")))
    (kill-buffer buf)))

(defun simply-annotate--finish-annotation-list-mode ()
  "Finish setting up the annotation list buffer.
Uses `outline-mode' to fold headings cheaply, then activates
`org-mode' which only fontifies the visible collapsed headings."
  (outline-mode)
  (outline-hide-sublevels 2)
  (org-mode)
  (visual-line-mode -1)
  (setq truncate-lines t)
  (simply-annotate-listing-mode 1)
  (setq buffer-read-only t))

(defun simply-annotate-listing-refresh ()
  "Rebuild the annotation listing buffer from scratch."
  (interactive)
  (let* ((buf-name (buffer-name))
         (is-all (string= buf-name "*All Annotations*"))
         (listing-buf (current-buffer))
         (source (buffer-local-value 'simply-annotate-listing-source listing-buf)))
    (kill-buffer listing-buf)
    (if is-all
        (simply-annotate-show-all)
      (when (and source (buffer-live-p source))
        (with-current-buffer source
          (simply-annotate-list))))))

;;;###autoload
(defun simply-annotate-list ()
  "Show the annotation listing for the current buffer.
Reuses an existing listing buffer if one exists; press the
Refresh button or \\[simply-annotate-listing-refresh] to rebuild."
  (interactive)
  (let* ((buffer-name "*Annotations*")
         (existing (get-buffer buffer-name)))
    (if existing
        (pop-to-buffer existing)
      (if simply-annotate-overlays
          (let* ((source-buffer (current-buffer))
                 (source-file (or (buffer-file-name) (buffer-name)))
                 (annotations (simply-annotate--serialize-annotations))
                 (annotation-buffer (simply-annotate--format-annotations-for-buffer
                                     source-file annotations source-buffer buffer-name)))
            (pop-to-buffer annotation-buffer)
            (goto-char (point-min)))
        (message "No annotations in buffer")))))

;;;###autoload
(defun simply-annotate-show-all ()
  "Show all annotations across all files in an org-mode buffer.
Reuses an existing buffer if one exists; press the Refresh button
or \\[simply-annotate-listing-refresh] to rebuild.
Files are grouped by directory.  Directories are top-level headings,
files are second-level, and annotation levels below that."
  (interactive)
  (let ((buffer-name "*All Annotations*"))
    (if-let ((existing (get-buffer buffer-name)))
        (pop-to-buffer existing)
      (let* ((db (simply-annotate--load-database)))
        (if (not db)
            (message "No annotations database found")
          (let ((files-with-annotations (mapcar #'car db))
                (total 0))
            (if (not files-with-annotations)
                (message "No annotations found in database")
              (with-current-buffer (get-buffer-create buffer-name)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  ;; Count totals
                  (dolist (file-key files-with-annotations)
                    (setq total (+ total (length (alist-get file-key db nil nil #'string=)))))
                  (simply-annotate--insert-refresh-button)
                  (insert (format "#+TITLE: All Annotations\n"))
                  (insert (format "#+STARTUP: show2levels\n"))
                  (insert (format "Total: %d annotations across %d files\n\n"
                                  total (length files-with-annotations)))
                  ;; Group files by directory
                  (let ((dir-alist nil))
                    (dolist (file-key files-with-annotations)
                      (let* ((dir (simply-annotate--key-directory file-key))
                             (name (simply-annotate--key-name file-key)))
                        (push (cons name file-key)
                              (alist-get dir dir-alist nil nil #'string=))))
                    ;; Sort directories and insert
                    (dolist (dir-entry (sort dir-alist
                                            (lambda (a b) (string< (car a) (car b)))))
                      (let* ((dir (car dir-entry))
                             (files (nreverse (cdr dir-entry)))
                             (dir-count (cl-reduce #'+ files
                                                   :key (lambda (f)
                                                          (length (alist-get (cdr f) db nil nil #'string=))))))
                        (insert (format "* %s (%d)\n" dir dir-count))
                        (dolist (file-entry files)
                          (let* ((name (car file-entry))
                                 (file-key (cdr file-entry))
                                 (annotations (alist-get file-key db nil nil #'string=))
                                 (count (length annotations))
                                 (source-buffer (simply-annotate--get-key-buffer file-key))
                                 (start (point)))
                            (insert (format "** %s (%d)\n" name count))
                            (put-text-property start (point) 'simply-annotate-nav (list :file file-key))
                            (simply-annotate--insert-formatted-annotations
                             file-key annotations source-buffer 2))))))
                  ;; Finish mode setup
                  (simply-annotate--finish-annotation-list-mode)
                  (goto-char (point-min))))
              (pop-to-buffer buffer-name))))))))

;;;###autoload
(defun simply-annotate-jump-to-file ()
  "Jump to an annotated file via completing-read.
Opens the selected file and enables `simply-annotate-mode'."
  (interactive)
  (let* ((db (simply-annotate--load-database)))
    (if (not db)
        (message "No annotations database found")
      (let* ((files-with-annotations (mapcar #'car db))
             (file-display-alist
              (mapcar (lambda (file-key)
                        (let* ((annotations (alist-get file-key db nil nil #'string=))
                               (count (length annotations))
                               (display-name (format "%s (%d annotation%s)"
                                                     (simply-annotate--format-jump-name file-key)
                                                     count
                                                     (if (= count 1) "" "s"))))
                          (cons display-name file-key)))
                      files-with-annotations)))
        (if (not files-with-annotations)
            (message "No annotations found in database")
          (let* ((selected-display (completing-read "Annotated file: "
                                                    file-display-alist nil t))
                 (selected-key (cdr (assoc selected-display file-display-alist))))
            (when (and selected-key (simply-annotate--get-key-buffer selected-key t))
              (unless simply-annotate-mode
                (simply-annotate-mode 1)))))))))

;;; Org Export

(defun simply-annotate--comment-tree-to-org (tree depth)
  "Convert comment TREE to org headings at DEPTH (number of stars)."
  (mapconcat
   (lambda (node)
     (let* ((comment (car node))
            (children (cdr node))
            (stars (make-string depth ?*))
            (author (alist-get 'author comment))
            (timestamp (alist-get 'timestamp comment))
            (text (alist-get 'text comment)))
       (concat
        (format "%s Reply by %s (%s)\n%s\n\n" stars author timestamp text)
        (when children
          (simply-annotate--comment-tree-to-org children (1+ depth))))))
   tree ""))

(defun simply-annotate--thread-to-org (thread)
  "Convert a THREAD to org-mode format."
  (simply-annotate--ensure-comment-ids thread)
  (let* ((id (alist-get 'id thread))
         (status (alist-get 'status thread))
         (priority (alist-get 'priority thread))
         (tags (alist-get 'tags thread))
         (comments (alist-get 'comments thread))
         (tree (simply-annotate--build-comment-tree comments))
         (first-node (car tree))
         (first-comment (car first-node))
         (first-children (cdr first-node))
         (first-text (alist-get 'text first-comment))
         (remaining-roots (cdr tree)))

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
     ;; Children of the first comment at ** level
     (when first-children
       (simply-annotate--comment-tree-to-org first-children 2))
     ;; Any additional root-level comments (shouldn't normally happen)
     (when remaining-roots
       (simply-annotate--comment-tree-to-org remaining-roots 2)))))

(defun simply-annotate-export-to-org-file (filename)
  "Export all annotations in current buffer to an org file FILENAME."
  (interactive "FExport annotations to org file: ")
  (let* ((file-key (simply-annotate--file-key))
         (db (simply-annotate--load-database))
         (annotations (when db (alist-get file-key db nil nil #'string=))))
    
    (if (not annotations)
        (message "No annotations to export")
      (with-temp-buffer
        (org-mode)
        (insert (format "#+TITLE: Annotations for %s\n" (simply-annotate--key-name file-key)))
        (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d")))
        
        (dolist (ann annotations)
          (let* ((text (alist-get 'text ann))
                 (thread (if (simply-annotate--thread-p text)
                             text
                           (simply-annotate--create-thread (simply-annotate--annotation-text text)))))
            (insert (simply-annotate--thread-to-org thread))
            (insert "\n")))
        
        (write-file filename)
        (message "Annotations exported to %s" filename)))))

;;; Mode definitions

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
    (define-key map (kbd "M-s e") #'simply-annotate-edit-sexp)
    (define-key map (kbd "M-s [") #'simply-annotate-cycle-level-backward)
    (define-key map (kbd "M-s ]") #'simply-annotate-cycle-level-forward)
    (define-key map (kbd "M-s '") #'simply-annotate-cycle-display-style)
    (define-key map (kbd "M-p") #'simply-annotate-previous)
    (define-key map (kbd "M-n") #'simply-annotate-next)
    map)
  "Keymap for simply-annotate annotation buffer.")

(define-derived-mode simply-annotate-annotation-mode fundamental-mode "Annotation"
  "Mode for displaying and editing annotations."
  (visual-line-mode 1))

;;;###autoload
(defvar simply-annotate-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "j") #'simply-annotate-smart-action)
    (define-key map (kbd "r") #'simply-annotate-reply-to-annotation)
    (define-key map (kbd "s") #'simply-annotate-set-annotation-status)
    (define-key map (kbd "-") #'simply-annotate-remove)
    (define-key map (kbd "a") #'simply-annotate-change-annotation-author)
    (define-key map (kbd "l") #'simply-annotate-list)
    (define-key map (kbd "L") #'simply-annotate-show-all)
    (define-key map (kbd "f") #'simply-annotate-jump-to-file)
    (define-key map (kbd "p") #'simply-annotate-set-annotation-priority)
    (define-key map (kbd "t") #'simply-annotate-add-annotation-tag)
    (define-key map (kbd "o") #'simply-annotate-export-to-org-file)
    (define-key map (kbd "e") #'simply-annotate-edit-sexp)
    (define-key map (kbd "[") #'simply-annotate-cycle-level-backward)
    (define-key map (kbd "]") #'simply-annotate-cycle-level-forward)
    (define-key map (kbd "'") #'simply-annotate-cycle-display-style)
    (define-key map (kbd "/") #'simply-annotate-toggle-inline)
    (define-key map (kbd "n") #'simply-annotate-next)
    (define-key map (kbd "v") #'simply-annotate-previous)
    map)
  "Command map for Simply Annotate.
This keymap contains all annotation commands with short single-key
bindings.  Bind it to a prefix key of your choice.  The recommended
prefix is M-s:

  (global-set-key (kbd \"M-s\") simply-annotate-command-map)

This gives you M-s j (smart action), M-s r (reply), M-s l (list), etc.

Note: use `global-set-key' in :config for the M-s prefix.
:bind-keymap defers loading by creating an autoload proxy -- a
temporary non-prefix command that loads the package on first
keypress.  This works for free keys like C-c a, but M-s is
already Emacs's `search-map' prefix.  The proxy replaces
search-map with a non-prefix command, breaking any code that
tries to bind under M-s.  `global-set-key' in :config binds
directly to the real keymap after the package has loaded.

  ;; Recommended: M-s prefix via :demand t + :config
  ;; :demand t loads immediately so M-s works from startup
  (use-package simply-annotate
    :demand t
    :hook (find-file-hook . simply-annotate-mode)
    :config
    (global-set-key (kbd \"M-s\") simply-annotate-command-map))

  ;; Alternative: C-c a prefix via :bind-keymap
  ;; Works well here because C-c a is a free key with no
  ;; existing prefix map, and defers loading until first use
  (use-package simply-annotate
    :bind-keymap (\"C-c a\" . simply-annotate-command-map)
    :hook (find-file-hook . simply-annotate-mode))

Available keys:

  j  smart-action      r  reply           s  status
  -  remove            a  change author   l  list
  L  show all          f  jump to file    p  priority
  t  tag               o  org export      e  edit sexp
  [  level backward    ]  level forward   \\='  cycle style
  /  toggle inline     n  next            v  previous")

(defvar simply-annotate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-p") #'simply-annotate-previous)
    (define-key map (kbd "M-n") #'simply-annotate-next)
    map)
  "Keymap for `simply-annotate-mode'.
This keymap is intentionally minimal -- it only binds M-n and M-p
for quick navigation between annotations.

All other commands live in `simply-annotate-command-map', which you
should bind to a prefix key of your choice (M-s is recommended).")

;;;###autoload
(define-minor-mode simply-annotate-mode
  "Enhanced annotation mode with threading support."
  :lighter " SA"
  :keymap simply-annotate-mode-map
  (if simply-annotate-mode
      (progn
        ;; Persist any overlays created while the mode was off,
        ;; so they aren't lost when we clear and reload from the database.
        (when simply-annotate-overlays
          (let ((file-key (simply-annotate--file-key)))
            (when file-key
              (let* ((new-annotations (simply-annotate--serialize-annotations))
                     (db (simply-annotate--load-database))
                     (existing (when db (alist-get file-key db nil nil #'string=)))
                     (merged (append existing new-annotations)))
                (simply-annotate--update-database file-key merged)))))
        (simply-annotate--clear-all-overlays)
        (simply-annotate--cleanup-draft)
        (simply-annotate--load-annotations)
        (setq simply-annotate-current-level simply-annotate-default-level)
        (setq simply-annotate-inline simply-annotate-inline-default)
        (simply-annotate--apply-level-filter)
        (simply-annotate--setup-header)
        (simply-annotate--update-header)
        (add-hook 'before-save-hook #'simply-annotate--save-annotations nil t)
        (add-hook 'kill-buffer-hook #'simply-annotate--save-annotations nil t)
        (add-hook 'kill-buffer-hook #'simply-annotate-hide-annotation-buffer nil t)
        (when (> (length simply-annotate-overlays) 0)
          (message "Simply-annotate: loaded %d annotations."
                   (length simply-annotate-overlays))))
    (simply-annotate--clear-all-overlays)
    (simply-annotate--cleanup-header)
    (simply-annotate-hide-annotation-buffer)
    (remove-hook 'before-save-hook #'simply-annotate--save-annotations t)
    (remove-hook 'kill-buffer-hook #'simply-annotate--save-annotations t)
    (remove-hook 'kill-buffer-hook #'simply-annotate-hide-annotation-buffer t)))

;;; Info-mode Integration

(defun simply-annotate--info-selection-hook ()
  "Refresh annotations when a new Info node is selected."
  (when simply-annotate-mode
    (simply-annotate--clear-all-overlays)
    (simply-annotate--cleanup-draft)
    (simply-annotate--load-annotations)
    (simply-annotate--apply-level-filter)
    (simply-annotate--update-header)))

(add-hook 'Info-selection-hook #'simply-annotate--info-selection-hook)

;;; Dired Integration

(defvar-local simply-annotate-dired-overlays nil
  "List of fringe overlays added by `simply-annotate-dired-mode'.")

(defun simply-annotate--dired-annotated-files ()
  "Return a set of absolute file paths that have annotations in the database."
  (let ((db (simply-annotate--load-database)))
    (when db
      (mapcar #'car db))))

(defun simply-annotate--dired-mark-annotated ()
  "Add fringe indicators to dired lines for files with annotations."
  (simply-annotate--dired-clear-marks)
  (let ((annotated-files (simply-annotate--dired-annotated-files)))
    (when annotated-files
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when-let* ((file (ignore-errors (dired-get-filename nil t))))
            (when (member file annotated-files)
              (let* ((ov (make-overlay (line-beginning-position)
                                       (line-end-position)))
                     (fringe-spec `(left-fringe ,simply-annotate-fringe-indicator
                                                ,simply-annotate-fringe-face)))
                (overlay-put ov 'before-string (propertize " " 'display fringe-spec))
                (overlay-put ov 'simply-annotate-dired t)
                (push ov simply-annotate-dired-overlays))))
          (forward-line 1))))))

(defun simply-annotate--dired-clear-marks ()
  "Remove all simply-annotate fringe overlays from the current dired buffer."
  (mapc #'delete-overlay simply-annotate-dired-overlays)
  (setq simply-annotate-dired-overlays nil))

;;;###autoload
(define-minor-mode simply-annotate-dired-mode
  "Show fringe indicators in dired for files that have annotations."
  :lighter " SA-Dir"
  (if simply-annotate-dired-mode
      (progn
        (simply-annotate--dired-mark-annotated)
        (add-hook 'dired-after-readin-hook
                  #'simply-annotate--dired-mark-annotated nil t))
    (simply-annotate--dired-clear-marks)
    (remove-hook 'dired-after-readin-hook
                 #'simply-annotate--dired-mark-annotated t)))

(provide 'simply-annotate)
;;; simply-annotate.el ends here
