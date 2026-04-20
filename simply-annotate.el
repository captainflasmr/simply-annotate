;;; simply-annotate.el --- Enhanced annotation system with threading -*- lexical-binding: t; -*-
;;
;; Author: James Dyer <captainflasmr@gmail.com>
;; Version: 2.1.1
;; Package-Requires: ((emacs "27.2"))
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
;; integration.  Requires Emacs 27.2+, no external dependencies.
;;
;; Quick Start (use-package):
;;
;;   (use-package simply-annotate
;;     :bind-keymap ("C-c a" . simply-annotate-command-map)
;;     :hook (find-file-hook . simply-annotate-mode))
;;
;;   (with-eval-after-load 'simply-annotate
;;     (add-hook 'dired-mode-hook #'simply-annotate-dired-mode))
;;
;; Quick Start (require):
;;
;;   (require 'simply-annotate)
;;   (global-set-key (kbd "C-c a") simply-annotate-command-map)
;;   (add-hook 'find-file-hook #'simply-annotate-mode)
;;   (add-hook 'dired-mode-hook #'simply-annotate-dired-mode)
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
;;   ;; Call `simply-annotate-inherit-search-map' to keep the default
;;   ;; M-s bindings (occur, isearch-forward-symbol-at-point, etc.)
;;   ;; working via keymap inheritance.
;;   :demand t
;;   :config
;;   (global-set-key (kbd "M-s") simply-annotate-command-map)
;;   (simply-annotate-inherit-search-map)
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
;; Inline Pointer Styles:
;;
;; When inline display is enabled, a pointer connects the annotation
;; box to the annotated text.  Customise via
;; `simply-annotate-inline-pointer-after' (box below text) and
;; `simply-annotate-inline-pointer-above' (box above text).  Strings
;; can be multiline (use \n).  Set to nil to disable.
;;
;; ;; Simple arrows (default)
;; (setq simply-annotate-inline-pointer-after "▴")
;; (setq simply-annotate-inline-pointer-above "▾")
;;
;; ;; Larger triangles
;; (setq simply-annotate-inline-pointer-after "▲")
;; (setq simply-annotate-inline-pointer-above "▼")
;;
;; ;; Tall stems
;; (setq simply-annotate-inline-pointer-after "  ║")
;; (setq simply-annotate-inline-pointer-above "  ║")
;;
;; ;; Speech bubble tails
;; (setq simply-annotate-inline-pointer-after " ╰┐")
;; (setq simply-annotate-inline-pointer-above " ╰┐")
;;
;; ;; L-bracket connectors
;; (setq simply-annotate-inline-pointer-after "│\n╰─")
;; (setq simply-annotate-inline-pointer-above "╭─\n│")
;;
;; (setq simply-annotate-inline-pointer-after "│\n╰──▸")
;; (setq simply-annotate-inline-pointer-above "╭──▸\n│")
;;
;; ;; Heavy L-bracket
;; (setq simply-annotate-inline-pointer-after "┃\n┗━▶")
;; (setq simply-annotate-inline-pointer-above "┏━▶\n┃")
;;
;; ;; Decorative single char
;; (setq simply-annotate-inline-pointer-after "●")
;; (setq simply-annotate-inline-pointer-above "●")
;;
;; (setq simply-annotate-inline-pointer-after "◆")
;; (setq simply-annotate-inline-pointer-above "◆")
;;
;; (setq simply-annotate-inline-pointer-after "✦")
;; (setq simply-annotate-inline-pointer-above "✦")
;;
;; (setq simply-annotate-inline-pointer-after "⟡")
;; (setq simply-annotate-inline-pointer-above "⟡")
;;
;; ;; Minimal / subtle
;; (setq simply-annotate-inline-pointer-after "·")
;; (setq simply-annotate-inline-pointer-above "·")
;;
;; (setq simply-annotate-inline-pointer-after "┊")
;; (setq simply-annotate-inline-pointer-above "┊")
;;
;; (setq simply-annotate-inline-pointer-after "╷")
;; (setq simply-annotate-inline-pointer-above "╵")
;;
;; ;; Bracket pairs
;; (setq simply-annotate-inline-pointer-after "╰─┤")
;; (setq simply-annotate-inline-pointer-above "╭─┤")
;;
;; ;; Fancy multiline
;; (setq simply-annotate-inline-pointer-after "│\n│\n╰──▸")
;; (setq simply-annotate-inline-pointer-above "╭──▸\n│\n│")
;;
;; (setq simply-annotate-inline-pointer-after "┃\n┃\n┗━━▶")
;; (setq simply-annotate-inline-pointer-above "┏━━▶\n┃\n┃")
;;
;; Project-Aware Annotations:
;;
;; By default, annotations are stored in a single global database.
;; Set `simply-annotate-database-strategy' to use per-project databases
;; that can be committed alongside your code:
;;
;; ;; Store annotations at the project root (.simply-annotations.el)
;; (setq simply-annotate-database-strategy 'project)
;;
;; ;; Or merge project and global databases (project wins on conflicts)
;; (setq simply-annotate-database-strategy 'both)
;;
;; Project-scoped commands (require project.el):
;; - <prefix> P   show annotations for the current project (org listing)
;; - <prefix> C-t show annotations for the current project (sortable table)
;; - <prefix> f   jump to annotated file
;; - <prefix> A   cross-project overview of every project with annotations
;;                (sortable table with per-status counts).  Discovers both
;;                global-db entries and per-project `.simply-annotations.el'
;;                files via `project-known-project-roots'.  From the table,
;;                RET/L/K drill into the project's table/org/kanban view.
;;
;; Dired-aware narrowing: when the four project commands above are
;; invoked from a `dired' buffer that lives under a project
;; subdirectory, they automatically narrow to that subdirectory --
;; useful for slicing large projects without any explicit selection
;; UI.  Prefix arguments escape this:
;;
;;   no prefix  dired-aware project view (auto-narrow if applicable)
;;   C-u        whole project, ignoring any dired auto-narrow
;;   C-u C-u    every annotated file in the database (jump-to-file
;;              and `simply-annotate-show-project'); for the table and
;;              kanban this behaves the same as C-u
;;
;; In the kanban buffer, a narrowed board can be widened with `d'
;; (clear directory filter) without leaving the buffer.  The currently
;; active directory is shown at the front of the kanban header line.
;;
;; To migrate existing global annotations into a project database:
;;   M-x simply-annotate-migrate-to-project
;;
;;; Code:

;;; Customization

(require 'cl-lib)
(require 'color)

(declare-function org-mode "org")
(declare-function outline-hide-sublevels "outline")
(declare-function dired-get-filename "dired")
(declare-function project-current "project")
(declare-function project-root "project")
(declare-function project-roots "project")
(declare-function project-known-project-roots "project")

(defun simply-annotate--project-root (project)
  "Return the root directory of PROJECT.
Uses `project-root' when available (Emacs 28+), falls back to
`project-roots' for Emacs 27."
  (if (fboundp 'project-root)
      (project-root project)
    (car (with-no-warnings (project-roots project)))))

(defgroup simply-annotate nil
  "Simple annotation system with threading support."
  :group 'text)

(defcustom simply-annotate-file
  (expand-file-name "simply-annotations.el" user-emacs-directory)
  "File to store annotations in global mode."
  :type 'file
  :group 'simply-annotate)

(defcustom simply-annotate-database-strategy 'global
  "Where to store annotations.
`global'  -- single file at `simply-annotate-file' (default)
`project' -- per-project file at project root, falling back to global
`both'    -- write to project database when in a project, read from
             both project and global (union)"
  :type '(choice (const :tag "Global only" global)
                 (const :tag "Project first, global fallback" project)
                 (const :tag "Both (union reads)" both))
  :group 'simply-annotate)

(defcustom simply-annotate-project-file ".simply-annotations.el"
  "Filename for per-project annotation databases.
Placed at the project root detected by `project-current'."
  :type 'string
  :group 'simply-annotate)

(defface simply-annotate-highlight-face
  '((t (:inherit highlight)))
  "Face for highlighted annotated text."
  :group 'simply-annotate)

(defface simply-annotate-fringe-bracket-face
  '((t (:inherit simply-annotate-fringe-face)))
  "Face for fringe bracket indicators."
  :group 'simply-annotate)

(defcustom simply-annotate-buffer-name "*Annotation*"
  "Name of the buffer to display annotations."
  :type 'string
  :group 'simply-annotate)

(defcustom simply-annotate-buffer-height 0.3
  "Height of annotation buffer as fraction of frame height."
  :type 'float
  :group 'simply-annotate)

(defcustom simply-annotate-display-style 'bracket
  "How to display annotated text.
A single symbol or a list of symbols processed in order.
Styles are applied sequentially so later face-based styles
override earlier ones, while fringe styles are independent.

Available styles:
- highlight: Highlight the annotated text
- tint: Subtle background tint derived from the current background
- fringe: Show indicators in the fringe
- fringe-bracket: Show bracket indicators spanning the annotated region
- bar: Show a vertical bar on the left of the annotated region
- bracket: Show a bracket outlining the annotated region
- subtle: Show a thin left border on annotated text

Examples:
  \\='fringe                        ; single style
  \\='(highlight fringe)            ; combined highlight and fringe
  \\='(tint fringe-bracket)         ; tint background with fringe brackets"
  :type '(choice (const :tag "Highlight text" highlight)
                 (const :tag "Background tint" tint)
                 (const :tag "Fringe indicators" fringe)
                 (const :tag "Fringe bracket" fringe-bracket)
                 (const :tag "Vertical bar" bar)
                 (const :tag "Bracket outline" bracket)
                 (const :tag "Subtle left border" subtle)
                 (repeat :tag "Combined styles"
                         (choice (const highlight)
                                 (const tint)
                                 (const fringe)
                                 (const fringe-bracket)
                                 (const bar)
                                 (const bracket)
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

(defface simply-annotate-subtle-face
  '((((background dark))
     (:overline "gray40" :underline (:style line :color "gray40")))
    (((background light))
     (:overline "gray75" :underline (:style line :color "gray75")))
    (t (:overline "gray" :underline (:style line :color "gray"))))
  "Face for subtle annotation display.
Uses overline and underline to bracket the annotated region."
  :group 'simply-annotate)

(defface simply-annotate-bar-face
  '((((background dark)) (:foreground "gray40" :weight bold))
    (((background light)) (:foreground "gray75" :weight bold))
    (t (:inherit font-lock-comment-face :weight bold)))
  "Face for the bar display style."
  :group 'simply-annotate)

(defface simply-annotate-inline-face
  '((t (:inherit font-lock-doc-face :extend t)))
  "Face for inline annotation text body."
  :group 'simply-annotate)

(defface simply-annotate-inline-border-face
  '((t (:inherit shadow)))
  "Face for inline annotation border characters."
  :group 'simply-annotate)

(defface simply-annotate-stale-face
  '((t (:foreground "tomato" :slant italic)))
  "Face for the stale indicator on annotations whose text has changed."
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

(defcustom simply-annotate-inline-pointer-after "▲"
  "Pointer string shown between annotated text and the inline box below.
Each line is indented to the annotation start column.  Can be multiline
for a more prominent indicator.  Set to nil or empty string to disable."
  :type '(choice string (const :tag "None" nil))
  :group 'simply-annotate)

(defcustom simply-annotate-inline-pointer-above "▼"
  "Pointer string shown between the inline box above and annotated text.
Each line is indented to the annotation start column.  Can be multiline
for a more prominent indicator.  Set to nil or empty string to disable."
  :type '(choice string (const :tag "None" nil))
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

;; Tag Filtering

(defcustom simply-annotate-default-tag-filter nil
  "Default active tag filter when the mode starts.
Nil means show annotations regardless of tag.  Any non-nil value is the
name of a single tag; only annotations carrying that tag will be shown."
  :type '(choice (const :tag "All tags" nil) string)
  :group 'simply-annotate)

(defcustom simply-annotate-background-threshold 20
  "Minimum line span for an annotation to be treated as a background overlay.
Background overlays skip per-line visual styles (highlight, fringe,
etc.) and lose hit-test ties to smaller overlays so that large
overview-style annotations do not drown out finer-grained ones.  Inline
text display is unaffected.  Set to nil to disable background
detection entirely."
  :type '(choice (const :tag "Disabled" nil) integer)
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

(defvar-local simply-annotate-current-tag-filter nil
  "Currently active tag filter in this buffer.
Nil means show every annotation; a string restricts display to
annotations whose thread `tags' list contains that string.")

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
Checks `simply-annotate-file-key-function' (buffer-local) and
`simply-annotate-file-key-functions' before falling back to file
path or buffer name.  When `simply-annotate-database-strategy' is
not `global' and the buffer visits a file inside a recognised
project, returns a project-relative path so the annotation
database is portable."
  (let ((custom-func (or simply-annotate-file-key-function
                         (cl-some (lambda (entry)
                                    (when (derived-mode-p (car entry))
                                      (cdr entry)))
                                  simply-annotate-file-key-functions))))
    (if (and custom-func (functionp custom-func))
        (funcall custom-func)
      (let ((raw-key (or (buffer-file-name) (buffer-name))))
        (if (and (not (eq simply-annotate-database-strategy 'global))
                 (buffer-file-name))
            (if-let* ((proj (project-current nil))
                      (root (simply-annotate--project-root proj)))
                (file-relative-name raw-key root)
              raw-key)
          raw-key)))))

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

(defun simply-annotate--resolve-key-path (key)
  "Resolve KEY to an absolute file path.
Relative keys are resolved against the current project root.
Returns KEY unchanged if it is absolute, an Info key, or a buffer name."
  (if (or (simply-annotate--key-info-p key)
          (file-name-absolute-p key))
      key
    (if-let* ((proj (project-current nil))
              (root (simply-annotate--project-root proj)))
        (expand-file-name key root)
      key)))

(defun simply-annotate--get-key-buffer (key &optional select)
  "Get (and optionally SELECT) the buffer associated with KEY.
Returns the buffer if successful, or nil if KEY cannot be resolved.
Relative keys are resolved against the current project root."
  (let ((resolved (simply-annotate--resolve-key-path key)))
    (cond
     ((simply-annotate--key-info-p resolved)
      (let ((manual (substring resolved 1 (string-match-p ")" resolved)))
            (node (string-trim (substring resolved (1+ (string-match-p ")" resolved))))))
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
                    resolved (error-message-string err))
           nil))))
     ((file-exists-p resolved)
      (if select
          (find-file resolved)
        (find-file-noselect resolved)))
     (t
      (when-let ((buf (get-buffer resolved)))
        (when select (switch-to-buffer buf))
        buf)))))

;;; Database Operations

(defun simply-annotate--database-path ()
  "Return the database file path for the current context.
When `simply-annotate-database-strategy' is `global', always returns
`simply-annotate-file'.  When `project' or `both', returns the
project-local path if inside a recognised project, otherwise falls
back to `simply-annotate-file'."
  (if (eq simply-annotate-database-strategy 'global)
      simply-annotate-file
    (if-let* ((proj (condition-case nil
                        (project-current nil)
                      (error nil)))
              (root (simply-annotate--project-root proj)))
        (expand-file-name simply-annotate-project-file root)
      simply-annotate-file)))

(defun simply-annotate--read-db (path)
  "Read and return the annotation alist from PATH, or nil."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (condition-case err
          (read (current-buffer))
        (end-of-file nil)
        (error
         (message "Simply-annotate: failed to read database %s: %s"
                  path (error-message-string err))
         nil)))))

(defun simply-annotate--merge-databases (primary secondary)
  "Merge PRIMARY and SECONDARY database alists.
PRIMARY entries win when both contain the same file-key."
  (let ((merged (copy-alist (or primary '()))))
    (dolist (entry (or secondary '()))
      (unless (alist-get (car entry) merged nil nil #'string=)
        (push entry merged)))
    merged))

(defun simply-annotate--load-database ()
  "Load annotations from the appropriate database file(s).
Respects `simply-annotate-database-strategy':
`global'  -- reads `simply-annotate-file'
`project' -- reads project-local file, falling back to global
`both'    -- merges project and global, project wins on conflicts"
  (if (eq simply-annotate-database-strategy 'both)
      (let ((project-db (simply-annotate--read-db (simply-annotate--database-path)))
            (global-db (simply-annotate--read-db simply-annotate-file)))
        (simply-annotate--merge-databases project-db global-db))
    (simply-annotate--read-db (simply-annotate--database-path))))

(defun simply-annotate--save-database (db)
  "Save DB to the appropriate database file."
  (let ((path (simply-annotate--database-path)))
    (with-temp-file path
      (insert ";;; Simply Annotate Database\n")
      (insert ";;; This file is auto-generated. Do not edit manually.\n\n")
      (prin1 db (current-buffer))
      (insert "\n"))))

(defun simply-annotate--update-database (file-key annotations)
  "Update database with ANNOTATIONS for FILE-KEY."
  (let ((db (or (simply-annotate--load-database) '())))
    (when (or annotations (alist-get file-key db nil nil #'string=))
      (if annotations
          (setf (alist-get file-key db nil nil #'string=) annotations)
        (setq db (cl-remove-if (lambda (entry)
                                 (string= (car entry) file-key))
                               db)))
      (let ((path (simply-annotate--database-path)))
        (if db
            (simply-annotate--save-database db)
          (when (file-exists-p path)
            (delete-file path)))))))

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
         (stale-p (eq (overlay-get overlay 'simply-annotate-stale) t))
         (relocated-p (overlay-get overlay 'simply-annotate-relocated))
         (label (concat "✎" (or meta "")
                        (when stale-p
                          (propertize " [STALE]" 'face 'simply-annotate-stale-face))
                        (when relocated-p
                          (propertize " [MOVED]" 'face 'font-lock-warning-face))))
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
Position is controlled by `simply-annotate-inline-position'.
Pointer is controlled by `simply-annotate-inline-pointer-after'
and `simply-annotate-inline-pointer-above'."
  (let* ((position simply-annotate-inline-position)
         (pointer-str (if (eq position 'above)
                          simply-annotate-inline-pointer-above
                        simply-annotate-inline-pointer-after))
         (text (simply-annotate--inline-text overlay)))
    ;; Insert pointer when configured
    (when (and pointer-str (not (string-empty-p pointer-str)))
      (let* ((col (save-excursion
                    (goto-char (overlay-start overlay))
                    (current-column)))
             (indent (make-string col ?\s))
             (pointer-block
              (propertize
               (mapconcat (lambda (line) (concat indent line))
                          (split-string pointer-str "\n")
                          "\n")
               'face 'simply-annotate-inline-border-face)))
        (if (eq position 'above)
            ;; Pointer at bottom of box, pointing down to text
            (setq text (concat text pointer-block "\n"))
          ;; Pointer at top of box, pointing up to text
          ;; text starts with \n; inject pointer after it
          (setq text (concat (substring text 0 1)
                             pointer-block "\n"
                             (substring text 1))))))
    ;; Apply to overlay
    (if (eq position 'above)
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
Overlays not matching the active tag filter are hidden.
When no tag filter is active, background overlays (see
`simply-annotate-background-threshold') skip per-line visual styles
but still show inline text when enabled, so they do not drown out
finer-grained annotations."
  (let ((subdue-background (and (null simply-annotate-current-tag-filter)
                                (simply-annotate--background-overlay-p overlay))))
    (if (simply-annotate--tag-match-p overlay)
        (progn
          (unless subdue-background
            (dolist (style (simply-annotate--display-styles))
              (pcase style
                ('highlight
                 (overlay-put overlay 'face 'simply-annotate-highlight-face))
                ('tint
                 (let ((bg (simply-annotate--tint-background)))
                   (when bg
                     (overlay-put overlay 'face `(:background ,bg :extend t)))))
                ('fringe
                 (simply-annotate--add-fringe-indicator overlay))
                ('fringe-bracket
                 (simply-annotate--add-fringe-bracket overlay))
                ('bar
                 (simply-annotate--add-bar-indicator overlay))
                ('bracket
                 (simply-annotate--add-bracket-indicator overlay))
                ('subtle
                 (overlay-put overlay 'face 'simply-annotate-subtle-face)))))
          (when simply-annotate-inline
            (simply-annotate--add-inline-text overlay)))
      (simply-annotate--cleanup-bracket-overlays overlay)
      (simply-annotate--cleanup-bar-overlays overlay)
      (simply-annotate--cleanup-bracket-text-overlays overlay)
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
         (fringe-spec `(left-fringe ,bitmap simply-annotate-fringe-face)))
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
           (end-line (line-number-at-pos
                      (if (and (> end start)
                               (= (char-before end) ?\n))
                          (1- end)
                        end)
                      t))
           (total-lines (1+ (- end-line start-line)))
           (aux-overlays nil))
      (goto-char start)
      (beginning-of-line)
      (dotimes (i total-lines)
        (let* ((bitmap (cond
                        ((= total-lines 1) 'simply-annotate-fringe-bracket-single)
                        ((= i 0) 'simply-annotate-fringe-bracket-top)
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

(defun simply-annotate--cleanup-bar-overlays (overlay)
  "Remove auxiliary bar overlays associated with OVERLAY."
  (dolist (aux (overlay-get overlay 'simply-annotate-bar-overlays))
    (when (overlayp aux)
      (delete-overlay aux)))
  (overlay-put overlay 'simply-annotate-bar-overlays nil))

(defun simply-annotate--add-bar-indicator (overlay)
  "Add a vertical bar indicator spanning the full extent of OVERLAY.
Uses a vertical bar character on every line of the annotated region."
  (simply-annotate--cleanup-bar-overlays overlay)
  (save-excursion
    (let* ((start (overlay-start overlay))
           (end (overlay-end overlay))
           (start-line (line-number-at-pos start t))
           (end-line (line-number-at-pos
                      (if (and (> end start)
                               (= (char-before end) ?\n))
                          (1- end)
                        end)
                      t))
           (total-lines (1+ (- end-line start-line)))
           (aux-overlays nil)
           (bar-char (propertize "┃ " 'face 'simply-annotate-bar-face)))
      (goto-char start)
      (beginning-of-line)
      (dotimes (_ total-lines)
        (let ((aux (make-overlay (point) (point))))
          (overlay-put aux 'before-string bar-char)
          (overlay-put aux 'simply-annotate-bar t)
          (push aux aux-overlays))
        (forward-line 1))
      (overlay-put overlay 'simply-annotate-bar-overlays aux-overlays))))

(defun simply-annotate--cleanup-bracket-text-overlays (overlay)
  "Remove auxiliary bracket text overlays associated with OVERLAY."
  (dolist (aux (overlay-get overlay 'simply-annotate-bracket-text-overlays))
    (when (overlayp aux)
      (delete-overlay aux)))
  (overlay-put overlay 'simply-annotate-bracket-text-overlays nil))

(defun simply-annotate--add-bracket-indicator (overlay)
  "Add a bracket indicator spanning the full extent of OVERLAY.
Uses box-drawing characters (┏, ┃, ┗) on the left of the annotated region."
  (simply-annotate--cleanup-bracket-text-overlays overlay)
  (save-excursion
    (let* ((start (overlay-start overlay))
           (end (overlay-end overlay))
           (start-line (line-number-at-pos start t))
           (end-line (line-number-at-pos
                      (if (and (> end start)
                               (= (char-before end) ?\n))
                          (1- end)
                        end)
                      t))
           (total-lines (1+ (- end-line start-line)))
           (aux-overlays nil)
           (face 'simply-annotate-bar-face))
      (goto-char start)
      (beginning-of-line)
      (dotimes (i total-lines)
        (let* ((char (cond
                      ((= total-lines 1) "┠ ")
                      ((= i 0) "┏ ")
                      ((= i (1- total-lines)) "┗ ")
                      (t "┃ ")))
               (indicator (propertize char 'face face))
               (aux (make-overlay (point) (point))))
          (overlay-put aux 'before-string indicator)
          (overlay-put aux 'simply-annotate-bracket-text t)
          (push aux aux-overlays))
        (forward-line 1))
      (overlay-put overlay 'simply-annotate-bracket-text-overlays aux-overlays))))

(defun simply-annotate-update-display-style ()
  "Update display style for all existing annotations."
  (interactive)
  (dolist (overlay simply-annotate-overlays)
    ;; Clear existing display properties
    (simply-annotate--cleanup-bracket-overlays overlay)
    (simply-annotate--cleanup-bar-overlays overlay)
    (simply-annotate--cleanup-bracket-text-overlays overlay)
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
                ('fringe-bracket 'bar)
                ('bar 'bracket)
                ('bracket 'subtle)
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

;;; Overlay Classification

(defun simply-annotate--background-overlay-p (overlay)
  "Return non-nil if OVERLAY should be treated as a background overlay.
See `simply-annotate-background-threshold' for the heuristic."
  (and simply-annotate-background-threshold
       (let ((buf (overlay-buffer overlay)))
         (and buf (buffer-live-p buf)
              (with-current-buffer buf
                (>= (count-lines (overlay-start overlay)
                                 (overlay-end overlay))
                    simply-annotate-background-threshold))))))

;;; Tag Filtering

(defun simply-annotate--overlay-tags (overlay)
  "Return the tag list stored on OVERLAY's annotation thread, or nil."
  (let ((data (overlay-get overlay 'simply-annotation)))
    (and (listp data) (alist-get 'tags data))))

(defun simply-annotate--tag-match-p (overlay)
  "Return non-nil if OVERLAY matches `simply-annotate-current-tag-filter'.
A nil filter matches every overlay."
  (or (null simply-annotate-current-tag-filter)
      (member simply-annotate-current-tag-filter
              (simply-annotate--overlay-tags overlay))))

(defun simply-annotate--active-overlays ()
  "Return overlays matching the current tag filter."
  (cl-remove-if-not #'simply-annotate--tag-match-p simply-annotate-overlays))

(defun simply-annotate--buffer-tags ()
  "Return a sorted list of unique tag strings across buffer overlays."
  (let ((tags nil))
    (dolist (ov simply-annotate-overlays)
      (dolist (tag (simply-annotate--overlay-tags ov))
        (unless (member tag tags)
          (push tag tags))))
    (sort tags #'string<)))

(defun simply-annotate--apply-tag-filter ()
  "Update display after a tag-filter change.
Hides the *Annotation* buffer if its current overlay no longer matches,
then re-applies display styles so filtered overlays hide/show."
  (when (and simply-annotate-current-overlay
             (not (simply-annotate--tag-match-p simply-annotate-current-overlay))
             (get-buffer-window simply-annotate-buffer-name))
    (simply-annotate-hide-annotation-buffer))
  (dolist (ov simply-annotate-overlays)
    (simply-annotate--cleanup-bracket-overlays ov)
    (simply-annotate--cleanup-bar-overlays ov)
    (simply-annotate--cleanup-bracket-text-overlays ov)
    (overlay-put ov 'face nil)
    (overlay-put ov 'before-string nil)
    (overlay-put ov 'after-string nil)
    (simply-annotate--apply-display-style ov))
  (simply-annotate--update-header))

(defun simply-annotate--tag-cycle-choices ()
  "Return the cycle ring: nil (all) followed by buffer tags."
  (cons nil (simply-annotate--buffer-tags)))

(defun simply-annotate--cycle-tag (direction)
  "Cycle `simply-annotate-current-tag-filter' by DIRECTION (+1 or -1)."
  (let* ((choices (simply-annotate--tag-cycle-choices))
         (len (length choices)))
    (if (<= len 1)
        (message "No tags in this buffer")
      (let* ((pos (or (cl-position simply-annotate-current-tag-filter choices
                                   :test #'equal)
                      0))
             (next (mod (+ pos direction) len)))
        (setq simply-annotate-current-tag-filter (nth next choices))
        (simply-annotate--apply-tag-filter)
        (message "Tag filter: %s"
                 (or simply-annotate-current-tag-filter "[all]"))))))

;;;###autoload
(defun simply-annotate-cycle-tag-forward ()
  "Cycle forward through tag filters present in this buffer."
  (interactive)
  (simply-annotate--cycle-tag 1))

;;;###autoload
(defun simply-annotate-cycle-tag-backward ()
  "Cycle backward through tag filters present in this buffer."
  (interactive)
  (simply-annotate--cycle-tag -1))

;;;###autoload
(defun simply-annotate-set-tag-filter (tag)
  "Set the active tag filter to TAG.
Interactively, choose from tags present in the current buffer.
A blank selection clears the filter."
  (interactive
   (list (let* ((choices (cons "[all]" (simply-annotate--buffer-tags)))
                (choice (completing-read "Tag filter: " choices nil t)))
           (if (or (string-empty-p choice) (string= choice "[all]"))
               nil
             choice))))
  (setq simply-annotate-current-tag-filter tag)
  (simply-annotate--apply-tag-filter)
  (message "Tag filter: %s" (or tag "[all]")))

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
  (simply-annotate--cleanup-bar-overlays overlay)
  (simply-annotate--cleanup-bracket-text-overlays overlay)
  (setq simply-annotate-overlays (delq overlay simply-annotate-overlays))
  (delete-overlay overlay))

(defun simply-annotate--clear-all-overlays ()
  "Remove all annotation overlays from buffer."
  (mapc #'simply-annotate--cleanup-bracket-overlays simply-annotate-overlays)
  (mapc #'simply-annotate--cleanup-bar-overlays simply-annotate-overlays)
  (mapc #'simply-annotate--cleanup-bracket-text-overlays simply-annotate-overlays)
  (mapc #'delete-overlay simply-annotate-overlays)
  (setq simply-annotate-overlays nil))

(defun simply-annotate--most-specific-overlay (candidates)
  "Return the most specific overlay from CANDIDATES.
Prefers the smallest region; when viewing everything, deprioritises
background overlays so finer-grained annotations take precedence."
  (when candidates
    (let ((unfiltered (null simply-annotate-current-tag-filter)))
      (car (sort candidates
                 (lambda (a b)
                   (let ((a-bg (and unfiltered
                                    (simply-annotate--background-overlay-p a)))
                         (b-bg (and unfiltered
                                    (simply-annotate--background-overlay-p b))))
                     (cond
                      ((and a-bg (not b-bg)) nil)
                      ((and b-bg (not a-bg)) t)
                      (t (< (- (overlay-end a) (overlay-start a))
                            (- (overlay-end b) (overlay-start b))))))))))))

(defun simply-annotate--overlay-at-point (&optional pos)
  "Get annotation overlay at POS (defaults to point) matching current level.
In fringe mode, searches the entire current line for overlays.
When multiple overlays match, returns the most specific one.
Background overlays are excluded when viewing everything, so that
`smart-action' can create granular annotations anywhere in the buffer."
  (let ((check-pos (or pos (point)))
        (unfiltered (null simply-annotate-current-tag-filter)))
    (if (cl-intersection (simply-annotate--display-styles) '(fringe fringe-bracket))
        (simply-annotate--overlay-on-line check-pos)
      (simply-annotate--most-specific-overlay
       (cl-remove-if-not (lambda (ov)
                           (and (overlay-get ov 'simply-annotation)
                                (simply-annotate--tag-match-p ov)
                                (not (and unfiltered
                                          (simply-annotate--background-overlay-p ov)))))
                         (overlays-at check-pos))))))

(defun simply-annotate--overlay-on-line (&optional pos)
  "Find annotation overlay matching current level on the line containing POS.
Matches any overlay whose region intersects the current line.
When multiple overlays match, returns the most specific one.
Background overlays are excluded when viewing everything."
  (save-excursion
    (when pos (goto-char pos))
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position))
          (unfiltered (null simply-annotate-current-tag-filter)))
      (simply-annotate--most-specific-overlay
       (cl-remove-if-not (lambda (overlay)
                           (and (overlay-get overlay 'simply-annotation)
                                (simply-annotate--tag-match-p overlay)
                                (not (and unfiltered
                                          (simply-annotate--background-overlay-p overlay)))
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

(defun simply-annotate--remove-thread-tag (thread tag)
  "Remove TAG from THREAD.
TAG must match a string in the thread's tags list."
  (let ((tags (alist-get 'tags thread)))
    (when (member tag tags)
      (setf (alist-get 'tags thread) (delete tag tags)))
    thread))

(defun simply-annotate--collect-all-tags (db)
  "Return a sorted list of unique tag strings from DB."
  (let ((tags nil))
    (dolist (db-entry db)
      (dolist (ann (cdr db-entry))
        (let ((data (alist-get 'text ann)))
          (when (simply-annotate--thread-p data)
            (dolist (tag (alist-get 'tags data))
              (unless (member tag tags)
                (push tag tags)))))))
    (sort tags #'string<)))

(defun simply-annotate--filter-db-by-tag (db tag)
  "Return a filtered copy of DB keeping only annotations matching TAG.
Each file entry is kept only if it has at least one annotation whose
thread contains TAG.  Entries without the tag are removed entirely."
  (let ((result nil))
    (dolist (entry db)
      (let* ((file-key (car entry))
             (annotations (cdr entry))
             (matching (seq-filter
                        (lambda (ann)
                          (let ((data (alist-get 'text ann)))
                            (and (simply-annotate--thread-p data)
                                 (member tag (alist-get 'tags data)))))
                        annotations)))
        (when matching
          (push (cons file-key matching) result))))
    (nreverse result)))

(defun simply-annotate--kanban-collect-tags (db)
  "Return a sorted list of unique tag strings from DB for kanban filtering."
  (simply-annotate--collect-all-tags db))

(defun simply-annotate--annotation-tags-at-point ()
  "Return a list of tags for the annotation at point, or nil if none."
  (let ((overlay (or (when (string= (buffer-name) simply-annotate-buffer-name)
                       simply-annotate-current-overlay)
                     (simply-annotate--overlay-at-point))))
    (when overlay
      (let ((data (overlay-get overlay 'simply-annotation)))
        (when (simply-annotate--thread-p data)
          (alist-get 'tags data))))))

(defun simply-annotate--format-tag-annotation-entry (file-key ann)
  "Format a single annotation ANN from FILE-KEY for completing-read."
  (let* ((data (alist-get 'text ann))
         (start (alist-get 'start ann))
         (comments (alist-get 'comments data))
         (preview (when comments
                     (truncate-string-to-width
                      (alist-get 'text (car comments)) 50 nil nil "...")))
         (status (alist-get 'status data))
         (tags (alist-get 'tags data)))
    (format "%s:%d  [%s] %s  %s"
            (simply-annotate--key-name file-key)
            start
            (upcase (or status "open"))
            (or preview "")
            (string-join tags " "))))

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

(defun simply-annotate--format-comment-node (node depth &optional continuations last-p)
  "Format a comment tree NODE at DEPTH for the annotation buffer.
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
                      node 0 nil (= idx last-idx))
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

(defun simply-annotate--migrate-legacy-level (text level)
  "Fold a legacy LEVEL field into TEXT as a tag.
TEXT is the stored annotation payload (string or thread alist).  If
LEVEL is nil or the `all' pseudo-level, TEXT is returned unchanged.
Otherwise TEXT is promoted to a thread if it is still a bare string,
and the level symbol's name is appended to the thread's tags if not
already present.  The returned value is always suitable as the overlay
`simply-annotation' property."
  (if (or (null level) (eq level 'all))
      text
    (let* ((tag (symbol-name level))
           (thread (if (simply-annotate--thread-p text)
                       text
                     (simply-annotate--create-thread
                      (or (and (stringp text) text) "")))))
      (simply-annotate--add-thread-tag thread tag)
      thread)))

(defun simply-annotate--annotation-stale-p (ann source-buffer)
  "Return stale status for annotation ANN given SOURCE-BUFFER.
Returns t if stale, nil if fresh, or `unknown' if no hash stored."
  (let ((stored-hash (alist-get 'text-hash ann))
        (start (alist-get 'start ann))
        (end (alist-get 'end ann)))
    (cond
     ((null stored-hash) 'unknown)
     ((not (and source-buffer (buffer-live-p source-buffer))) 'unknown)
     (t (let* ((pmax (with-current-buffer source-buffer (point-max)))
               (s (min start pmax))
               (e (min end pmax))
               (current-hash (with-current-buffer source-buffer
                               (sxhash-equal
                                (buffer-substring-no-properties s e)))))
          (if (/= stored-hash current-hash) t nil))))))

(defun simply-annotate--serialize-annotations ()
  "Convert buffer annotations to serializable format.
Includes a `text-hash' and `text-context' for stale detection and
position recovery.  When an annotation is stale, the original hash
and context are preserved so the stale state persists across saves."
  (mapcar (lambda (overlay)
            (let* ((s (overlay-start overlay))
                   (e (overlay-end overlay))
                   (stale (overlay-get overlay 'simply-annotate-stale))
                   (hash (if (eq stale t)
                             (overlay-get overlay 'simply-annotate-stored-hash)
                           (sxhash-equal
                            (buffer-substring-no-properties s e))))
                   (context (if (eq stale t)
                                (overlay-get overlay 'simply-annotate-text-context)
                              (simply-annotate--make-context s e))))
              `((start . ,s)
                (end . ,e)
                (text . ,(overlay-get overlay 'simply-annotation))
                (text-hash . ,hash)
                ,@(when context `((text-context . ,context))))))
          simply-annotate-overlays))

(defun simply-annotate--make-context (start end)
  "Return the buffer text between START and END, truncated for storage.
Keeps up to 150 characters to allow reliable relocation."
  (let ((text (buffer-substring-no-properties start (min end (point-max)))))
    (if (> (length text) 150)
        (substring text 0 150)
      text)))

(defun simply-annotate--relocate-annotation (context original-start region-length)
  "Search for CONTEXT in the current buffer and return (START . END) or nil.
ORIGINAL-START is used to prefer the closest match when duplicates exist.
REGION-LENGTH is the original annotation length."
  (when (and context (> (length context) 0))
    (save-excursion
      (goto-char (point-min))
      (let ((matches nil)
            (search-text context))
        (while (search-forward search-text nil t)
          (push (match-beginning 0) matches))
        (when matches
          (let* ((best (car (sort matches
                                  (lambda (a b)
                                    (< (abs (- a original-start))
                                       (abs (- b original-start)))))))
                 (new-end (min (+ best region-length) (point-max))))
            (cons best new-end)))))))

(defun simply-annotate--deserialize-annotations (annotations)
  "Restore ANNOTATIONS from serialized format.
When the text hash mismatches at the stored position, attempts to
relocate the annotation by searching for the stored text-context.
If relocation succeeds, the overlay is placed at the new position.
If relocation fails, the annotation is marked stale."
  (dolist (ann annotations)
    (let* ((start (alist-get 'start ann))
           (end (alist-get 'end ann))
           (raw-text (alist-get 'text ann))
           (legacy-level (alist-get 'level ann))
           (text (simply-annotate--migrate-legacy-level raw-text legacy-level)))
      (when (and start end text
                 (<= start (point-max))
                 (<= end (point-max))
                 (> end start))
        (let* ((stored-hash (alist-get 'text-hash ann))
               (context (alist-get 'text-context ann))
               (current-hash (sxhash-equal
                              (buffer-substring-no-properties start end)))
               (hash-match (and stored-hash (= stored-hash current-hash)))
               (relocated nil)
               (actual-start start)
               (actual-end end)
               (stale nil))
          ;; Attempt relocation when hash mismatches
          (when (and stored-hash (not hash-match))
            (let ((new-pos (simply-annotate--relocate-annotation
                            context start (- end start))))
              (if new-pos
                  (let* ((ns (car new-pos))
                         (ne (cdr new-pos))
                         (new-hash (sxhash-equal
                                    (buffer-substring-no-properties ns ne))))
                    (if (= stored-hash new-hash)
                        (setq actual-start ns
                              actual-end ne
                              relocated t)
                      ;; Found the text but region hash differs (partial match)
                      (setq stale t)))
                ;; Context not found anywhere
                (setq stale t))))
          (when (null stored-hash)
            (setq stale 'unknown))
          (let ((ov (simply-annotate--create-overlay actual-start actual-end text)))
            (overlay-put ov 'simply-annotate-stale stale)
            (when stored-hash
              (overlay-put ov 'simply-annotate-stored-hash stored-hash))
            (when context
              (overlay-put ov 'simply-annotate-text-context context))
            (when relocated
              (overlay-put ov 'simply-annotate-relocated t))
            (when (eq stale t)
              (overlay-put ov 'help-echo
                           (concat "[STALE: underlying text changed] "
                                   (or (overlay-get ov 'help-echo) ""))))
            (when relocated
              (overlay-put ov 'help-echo
                           (concat "[RELOCATED: text moved] "
                                   (or (overlay-get ov 'help-echo) ""))))
            (push ov simply-annotate-overlays)))))))

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
           (message "Error saving annotation: %s" (error-message-string err))))
      ;; Text-based editing mode
      (let ((content (simply-annotate--strip-boilerplate
                      (buffer-substring simply-annotate-header-end-pos (point-max)))))
        (if (string-empty-p content)
            (simply-annotate--cancel-annotation overlay is-draft)
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

    (when final-data
      (simply-annotate--finalize-annotation overlay final-data is-draft))))

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
  "Finalize annotation for OVERLAY with FINAL-DATA, handling IS-DRAFT state.
When IS-DRAFT is non-nil and a tag filter is active, the active tag
is auto-applied to the new annotation so it remains visible under
the current filter."
  (with-current-buffer simply-annotate-source-buffer
    (when (and is-draft
               simply-annotate-current-tag-filter
               (simply-annotate--thread-p final-data))
      (simply-annotate--add-thread-tag final-data
                                       simply-annotate-current-tag-filter))
    (overlay-put overlay 'simply-annotation final-data)
    (overlay-put overlay 'help-echo (simply-annotate--annotation-summary final-data))
    
    (when is-draft
      (overlay-put overlay 'simply-annotation-draft nil)
      (push overlay simply-annotate-overlays)
      (setq simply-annotate-draft-overlay nil))
      
    (simply-annotate--refresh-overlay-display overlay)
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
  "Get the position number of TARGET-OVERLAY among tag-filtered annotations."
  (when target-overlay
    (with-current-buffer (overlay-buffer target-overlay)
      (let ((sorted-active (seq-sort-by #'overlay-start #'<
                                        (simply-annotate--active-overlays))))
        (1+ (or (cl-position target-overlay sorted-active) 0))))))

(defun simply-annotate--tag-counts ()
  "Return a formatted string showing annotation counts by tag.
The active tag filter is shown in bold; an [ALL] segment shows the
total count when no filter is active."
  (let* ((all-count (length simply-annotate-overlays))
         (buffer-tags (simply-annotate--buffer-tags))
         (current (simply-annotate--overlay-at-point))
         (current-num (when current
                        (simply-annotate--annotation-number current)))
         (active-filter simply-annotate-current-tag-filter)
         (all-label (if (and (null active-filter) current-num)
                        (format "ALL: %d/%d" current-num all-count)
                      (format "ALL:%d" all-count)))
         (all-segment (if (null active-filter)
                          (propertize all-label 'face '(bold :height 0.9))
                        (propertize all-label 'face '(:height 0.9))))
         (tag-segments
          (mapcar
           (lambda (tag)
             (let* ((n (cl-count-if
                        (lambda (ov)
                          (member tag (simply-annotate--overlay-tags ov)))
                        simply-annotate-overlays))
                    (active (equal tag active-filter))
                    (label (if (and active current-num)
                               (format "%s: %d/%d" tag current-num n)
                             (format "%s:%d" tag n))))
               (if active
                   (propertize label 'face '(bold :height 0.9))
                 (propertize label 'face '(:height 0.9)))))
           buffer-tags)))
    (mapconcat #'identity
               (cons all-segment tag-segments)
               (propertize " | " 'face '(:height 0.9)))))

(defun simply-annotate--format-header (&optional text)
  "Header format showing level counts with embedded position.
Optional TEXT is appended (e.g. status messages)."
  (let ((total (length simply-annotate-overlays)))
    (when (> total 0)
      (concat
       " "
       (simply-annotate--tag-counts)
       " "
       (if-let* ((overlay (simply-annotate--overlay-at-point))
                 (annotation-data (overlay-get overlay 'simply-annotation))
                 (thread (and (simply-annotate--thread-p annotation-data) annotation-data)))
           (let ((status (or (alist-get 'status thread) "OPEN"))
                 (priority (or (alist-get 'priority thread) "NORMAL"))
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
                  #'simply-annotate--tag-match-p
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

;;;###autoload
(defun simply-annotate-clear-buffer-annotations ()
  "Remove all annotations from the current buffer."
  (interactive)
  (if (not simply-annotate-overlays)
      (message "No annotations in this buffer to clear")
    (when (y-or-n-p "Really remove all annotations from this buffer? ")
      (simply-annotate--clear-all-overlays)
      (simply-annotate--save-annotations)
      (simply-annotate--update-header)
      (simply-annotate--invalidate-listing)
      (message "All buffer annotations removed"))))

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
               (existing-tags (simply-annotate--collect-all-tags
                               (simply-annotate--load-database)))
               (tag (completing-read "Tag: " existing-tags nil nil)))
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

;;;###autoload
(defun simply-annotate-remove-annotation-tag ()
  "Remove a tag from the annotation at point.
Select from the annotation's existing tags via `completing-read'."
  (interactive)
  (let ((overlay (if (string= (buffer-name) simply-annotate-buffer-name)
                     simply-annotate-current-overlay
                   (simply-annotate--overlay-at-point))))
    (if overlay
        (let* ((current-data (overlay-get overlay 'simply-annotation))
               (thread (when (simply-annotate--thread-p current-data) current-data))
               (tags (when thread (alist-get 'tags thread))))
          (if (not tags)
              (message "No tags on this annotation")
            (let ((tag (completing-read "Remove tag: " tags nil t)))
              (when (and tag (not (string-empty-p tag)))
                (simply-annotate--remove-thread-tag thread tag)
                (overlay-put overlay 'simply-annotation thread)
                (overlay-put overlay 'help-echo (simply-annotate--annotation-summary thread))
                (simply-annotate--refresh-overlay-display overlay)
                (simply-annotate--save-annotations)
                (simply-annotate--update-header)
                (when (get-buffer-window simply-annotate-buffer-name)
                  (simply-annotate--update-annotation-buffer thread overlay 'view)
                  (simply-annotate--show-annotation-buffer))
                (simply-annotate--invalidate-listing)
                (message "Tag '%s' removed" tag)))))
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
    (insert (format "Total: %d | Open: %d | Resolved: %d\n\n"
                    total open-count resolved-count))))

(defun simply-annotate--insert-formatted-annotations (file-key annotations source-buffer &optional depth)
  "Insert formatted ANNOTATIONS for FILE-KEY from SOURCE-BUFFER.
DEPTH controls heading level offset (default 0)."
  (let* ((d (or depth 0))
         (line-table (simply-annotate--batch-line-info annotations source-buffer))
         (sorted-annotations (simply-annotate--sort-annotations annotations)))
    (dolist (ann sorted-annotations)
      (let* ((start-pos (alist-get 'start ann))
             (annotation-data (alist-get 'text ann))
             (line-info (gethash start-pos line-table
                                 (list 1 0 "Content not available"))))
        (if (simply-annotate--thread-p annotation-data)
            (simply-annotate--insert-thread-annotation annotation-data line-info file-key d)
          (simply-annotate--insert-simple-annotation annotation-data line-info file-key d))))))

(defun simply-annotate--sort-annotations (annotations)
  "Sort ANNOTATIONS by line position, then by status (open items first).
Returns a new sorted list; ANNOTATIONS is not modified."
  (sort (copy-sequence annotations)
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

(defun simply-annotate--insert-thread-annotation (thread line-info file-key &optional depth)
  "Insert formatted THREAD annotation with LINE-INFO for FILE-KEY.
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
      (insert (format " %s" (string-join tags " "))))
    (insert (format " (%d comment%s)\n"
                    comment-count
                    (if (= comment-count 1) "" "s")))
    ;; Store navigation properties on the heading
    (put-text-property heading-start (point)
                       'simply-annotate-nav
                       (list :file file-key :line line-num
                             :col (1+ col-num)))

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

(defun simply-annotate--insert-simple-annotation (annotation-data line-info file-key &optional depth)
  "Insert formatted simple ANNOTATION-DATA with LINE-INFO for FILE-KEY.
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
                             :col (1+ col-num)))
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
  (let ((props (or nav (simply-annotate--listing-nav-props)))
        (view-tag-filter (cond ((derived-mode-p 'simply-annotate-kanban-mode)
                                (bound-and-true-p simply-annotate-kanban-filter-tag))
                               ((derived-mode-p 'simply-annotate-table-mode)
                                (bound-and-true-p simply-annotate-table-filter-tag))
                               ((derived-mode-p 'simply-annotate-project-table-mode)
                                (bound-and-true-p simply-annotate-table-filter-tag))
                               (t nil))))
    (when props
      (let ((file (plist-get props :file))
            (line (plist-get props :line))
            (col  (plist-get props :col)))
        (if-let ((source-buf (simply-annotate--get-key-buffer file)))
            (let ((win (display-buffer source-buf
                                       '(display-buffer-use-some-window
                                         (inhibit-same-window . t)))))
              (with-selected-window win
                (unless simply-annotate-mode
                  (simply-annotate-mode 1))
                (when view-tag-filter
                  (setq simply-annotate-current-tag-filter view-tag-filter)
                  (simply-annotate--apply-tag-filter))
                (goto-char (point-min))
                (forward-line (1- line))
                (forward-char (max 0 (1- col)))
                ;; Pulse the annotation overlay at this position
                (when-let ((ov (simply-annotate--overlay-at-point)))
                  (pulse-momentary-highlight-region
                   (overlay-start ov) (overlay-end ov))))))))))

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

;;; Tabulated annotation listing

(defvar simply-annotate-table-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'simply-annotate-table-goto-source)
    (define-key map (kbd "g") #'simply-annotate-table-refresh)
    (define-key map (kbd "t") #'simply-annotate-table-filter-by-tag)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `simply-annotate-table-mode'.")

(define-derived-mode simply-annotate-table-mode tabulated-list-mode "SA-Table"
  "Major mode for displaying annotations in a sortable table."
  (setq tabulated-list-format
        [("Stale" 5 t)
         ("Line" 6 (lambda (a b)
                     (< (string-to-number (aref (cadr a) 1))
                        (string-to-number (aref (cadr b) 1)))))
         ("Status" 12 t)
         ("Priority" 9 t)
         ("Cmt" 4 (lambda (a b)
                    (< (string-to-number (let ((s (aref (cadr a) 4))) (if (string= s "") "0" s)))
                       (string-to-number (let ((s (aref (cadr b) 4))) (if (string= s "") "0" s))))))
         ("Tags" 12 t)
         ("Author" 10 t)
         ("Comment" 0 t)])
  (setq tabulated-list-sort-key '("Line" . nil))
  (tabulated-list-init-header))

(defun simply-annotate--tabulated-entries (annotations source-buffer file-key)
  "Build `tabulated-list-entries' from ANNOTATIONS in SOURCE-BUFFER for FILE-KEY."
  (let ((line-table (simply-annotate--batch-line-info annotations source-buffer))
        (entries nil))
    (dolist (ann (simply-annotate--sort-annotations annotations))
      (let* ((start-pos (alist-get 'start ann))
             (data (alist-get 'text ann))
             (line-info (gethash start-pos line-table '(1 0 "")))
             (line-num (car line-info))
             (col-num (cadr line-info))
             (stale (simply-annotate--annotation-stale-p ann source-buffer))
             (stale-str (pcase stale
                          ('t (propertize "Yes" 'face 'simply-annotate-stale-face))
                          ('unknown "?")
                          (_ "")))
             (thread-p (simply-annotate--thread-p data))
             (status (if thread-p (upcase (or (alist-get 'status data) "open")) "OPEN"))
             (priority (if thread-p (upcase (or (alist-get 'priority data) "normal")) "NORMAL"))
             (tags (if thread-p
                       (string-join (or (alist-get 'tags data) nil) ",")
                     ""))
             (comments (if thread-p (alist-get 'comments data) nil))
             (count (if thread-p (length comments) 0))
             (author (if thread-p
                        (or (alist-get 'author (car comments)) "")
                      ""))
             (first-text (cond
                          (thread-p
                           (let ((c (car comments)))
                             (or (alist-get 'text c) "")))
                          ((stringp data) data)
                          (t "")))
             (summary (string-trim (car (split-string first-text "\n"))))
             (nav (list :file file-key :line line-num
                        :col (1+ col-num))))
        (let ((current-tag-filter (bound-and-true-p simply-annotate-table-filter-tag)))
          (when (or (null current-tag-filter)
                    (and thread-p (member current-tag-filter (alist-get 'tags data))))
            (push (list nav
                      (vector
                       stale-str
                     (number-to-string line-num)
                     (propertize status 'face
                                 (pcase status
                                   ("OPEN" 'warning)
                                   ("IN-PROGRESS" 'success)
                                   ("RESOLVED" 'shadow)
                                   ("CLOSED" 'shadow)
                                   (_ 'default)))
                     priority
                     (if (> count 0) (number-to-string count) "")
                     tags
                     author
                     summary))
              entries)))))
    (nreverse entries)))

(defun simply-annotate-table-goto-source ()
  "Jump to the source location of the annotation at point."
  (interactive)
  (when-let ((entry (tabulated-list-get-id)))
    (simply-annotate--listing-goto-source entry)))

(defvar-local simply-annotate-table-project-root nil
  "Project root for project-scoped table buffers.
Nil for single-buffer tables.")

(defvar-local simply-annotate-table-filter-directory nil
  "When non-nil, restrict project-table entries to this directory.
The value is a project-relative or absolute directory string;
files outside it are filtered out by
`simply-annotate--filter-db-by-directory'.")

(defvar-local simply-annotate-table-filter-tag nil
  "When non-nil, restrict table entries to this tag.")

(defun simply-annotate-table-filter-by-tag ()
  "Filter table entries by tag."
  (interactive)
  (let* ((root simply-annotate-table-project-root)
         (db (when root (simply-annotate--project-annotations root)))
         (tags (if root
                   (simply-annotate--kanban-collect-tags db)
                 (when (and simply-annotate-listing-source (buffer-live-p simply-annotate-listing-source))
                   (with-current-buffer simply-annotate-listing-source
                     (simply-annotate--buffer-tags)))))
         (choices (cons "[All]" tags))
         (selection (completing-read
                     (format "Filter by tag%s: "
                             (if simply-annotate-table-filter-tag
                                 (format " [current: %s]" simply-annotate-table-filter-tag)
                               ""))
                     choices nil t)))
    (setq simply-annotate-table-filter-tag
          (unless (string= selection "[All]") selection))
    (simply-annotate-table-refresh)
    (message "Tag filter: %s"
             (or simply-annotate-table-filter-tag "all tags"))))

(defun simply-annotate-table-refresh ()
  "Rebuild the annotation table in place, preserving the window."
  (interactive)
  (let ((source simply-annotate-listing-source)
        (project-root-val simply-annotate-table-project-root)
        (subdir simply-annotate-table-filter-directory)
        (pos (point)))
    (if project-root-val
        (let* ((db (simply-annotate--project-annotations project-root-val))
               (filtered (simply-annotate--filter-db-by-directory
                          db project-root-val subdir)))
          (setq tabulated-list-entries
                (if filtered
                    (simply-annotate--project-tabulated-entries filtered)
                  nil))
          (tabulated-list-print)
          (goto-char (min pos (point-max))))
      (when (and source (buffer-live-p source))
        (let* ((file-key (with-current-buffer source
                           (or (buffer-file-name) (buffer-name))))
               (annotations (with-current-buffer source
                              (simply-annotate--serialize-annotations))))
          (setq tabulated-list-entries
                (simply-annotate--tabulated-entries annotations source file-key))
          (tabulated-list-print)
          (goto-char (min pos (point-max))))))))

;;;###autoload
(defun simply-annotate-list-table ()
  "Show annotations for the current buffer in a sortable table.
Reuses an existing table buffer if one exists; press g to rebuild."
  (interactive)
  (let* ((buffer-name "*Annotations Table*")
         (source-buffer (current-buffer))
         (existing (get-buffer buffer-name)))
    (if (and existing
             (eq source-buffer
                 (buffer-local-value 'simply-annotate-listing-source existing)))
        (pop-to-buffer existing)
      (when existing (kill-buffer existing))
      (if simply-annotate-overlays
          (let* ((file-key (or (buffer-file-name) (buffer-name)))
                 (annotations (simply-annotate--serialize-annotations))
                 (tag-filter simply-annotate-current-tag-filter))
            (with-current-buffer (get-buffer-create buffer-name)
              (simply-annotate-table-mode)
              (setq simply-annotate-table-filter-tag tag-filter)
              (setq tabulated-list-entries
                    (simply-annotate--tabulated-entries annotations source-buffer file-key))
              (tabulated-list-print)
              (setq simply-annotate-listing-source source-buffer)
              (setq simply-annotate-table-project-root nil))
            (pop-to-buffer buffer-name))
        (message "No annotations in buffer")))))

(define-derived-mode simply-annotate-project-table-mode tabulated-list-mode "SA-ProjTable"
  "Major mode for displaying project-wide annotations in a sortable table."
  (setq tabulated-list-format
        [("File" 25 t)
         ("Stale" 5 t)
         ("Line" 6 (lambda (a b)
                     (< (string-to-number (aref (cadr a) 2))
                        (string-to-number (aref (cadr b) 2)))))
         ("Status" 12 t)
         ("Priority" 9 t)
         ("Cmt" 4 (lambda (a b)
                    (< (string-to-number (let ((s (aref (cadr a) 5))) (if (string= s "") "0" s)))
                       (string-to-number (let ((s (aref (cadr b) 5))) (if (string= s "") "0" s))))))
         ("Tags" 12 t)
         ("Author" 10 t)
         ("Comment" 0 t)])
  (setq tabulated-list-sort-key '("File" . nil))
  (let ((map simply-annotate-project-table-mode-map))
    (define-key map (kbd "RET") #'simply-annotate-table-goto-source)
    (define-key map (kbd "g") #'simply-annotate-table-refresh)
    (define-key map (kbd "t") #'simply-annotate-table-filter-by-tag)
    (define-key map (kbd "q") #'quit-window))
  (tabulated-list-init-header))

(defun simply-annotate--project-tabulated-entries (db)
  "Build `tabulated-list-entries' from a multi-file DB alist.
Each entry includes a File column."
  (let ((entries nil))
    (dolist (db-entry db)
      (let* ((file-key (car db-entry))
             (annotations (cdr db-entry))
             (source-buffer (simply-annotate--get-key-buffer file-key))
             (line-table (simply-annotate--batch-line-info annotations source-buffer))
             (short-name (simply-annotate--key-name file-key)))
        (dolist (ann (simply-annotate--sort-annotations annotations))
          (let* ((start-pos (alist-get 'start ann))
                 (data (alist-get 'text ann))
                 (line-info (gethash start-pos line-table '(1 0 "")))
                 (line-num (car line-info))
                 (col-num (cadr line-info))
                 (thread-p (simply-annotate--thread-p data))
                 (status (if thread-p (upcase (or (alist-get 'status data) "open")) "OPEN"))
                 (priority (if thread-p (upcase (or (alist-get 'priority data) "normal")) "NORMAL"))
                 (tags (if thread-p
                           (string-join (or (alist-get 'tags data) nil) ",")
                         ""))
                 (comments (if thread-p (alist-get 'comments data) nil))
                 (count (if thread-p (length comments) 0))
                 (author (if thread-p
                            (or (alist-get 'author (car comments)) "")
                          ""))
                 (first-text (cond
                              (thread-p
                               (let ((c (car comments)))
                                 (or (alist-get 'text c) "")))
                              ((stringp data) data)
                              (t "")))
                 (summary (string-trim (car (split-string first-text "\n"))))
                 (stale (simply-annotate--annotation-stale-p ann source-buffer))
                 (stale-str (pcase stale
                              ('t (propertize "Yes" 'face 'simply-annotate-stale-face))
                              ('unknown "?")
                              (_ "")))
                 (nav (list :file file-key :line line-num
                            :col (1+ col-num))))
            (let ((current-tag-filter (bound-and-true-p simply-annotate-table-filter-tag)))
              (when (or (null current-tag-filter)
                        (and thread-p (member current-tag-filter (alist-get 'tags data))))
                (push (list nav
                          (vector
                           short-name
                         stale-str
                         (number-to-string line-num)
                         (propertize status 'face
                                     (pcase status
                                       ("OPEN" 'warning)
                                       ("IN-PROGRESS" 'success)
                                       ("RESOLVED" 'shadow)
                                       ("CLOSED" 'shadow)
                                       (_ 'default)))
                         priority
                         (if (> count 0) (number-to-string count) "")
                         tags
                         author
                         summary))
                  entries)))))))
    (nreverse entries)))

(defun simply-annotate--show-project-table (root &optional subdir tag-filter)
  "Show a project-wide annotation table for project at ROOT.
If SUBDIR is non-nil, restrict the table to files under that
project-relative directory."
  (let* ((project-name (file-name-nondirectory (directory-file-name root)))
         (suffix (if subdir (format " [%s]" subdir) ""))
         (buffer-name (format "*Annotations Table: %s%s*" project-name suffix))
         (db (simply-annotate--project-annotations root))
         (filtered (simply-annotate--filter-db-by-directory db root subdir)))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (if (not filtered)
        (message "No annotations found for project %s%s" project-name suffix)
      (with-current-buffer (get-buffer-create buffer-name)
        (simply-annotate-project-table-mode)
        (setq simply-annotate-table-filter-tag tag-filter)
        (setq tabulated-list-entries
              (simply-annotate--project-tabulated-entries filtered))
        (tabulated-list-print)
        (setq simply-annotate-listing-source nil)
        (setq simply-annotate-table-project-root root)
        (setq simply-annotate-table-filter-directory subdir))
      (pop-to-buffer buffer-name))))

;;;###autoload
(defun simply-annotate-list-project-table (&optional arg)
  "Show a sortable table of all annotations in the current project.
Like `simply-annotate-list-table' but spanning all project files,
with an additional File column.

When called without a prefix argument from a `dired' buffer or
a file buffer that lives under a project subdirectory, the table
is automatically narrowed to that subdirectory.  Any prefix
argument (ARG) disables the auto-narrow and shows the whole
project."
  (interactive "P")
  (if-let* ((proj (project-current t))
            (root (simply-annotate--project-root proj)))
      (let* ((subdir (unless arg
                       (simply-annotate--buffer-default-subdir root)))
             (tag-filter simply-annotate-current-tag-filter))
        (simply-annotate--show-project-table root subdir tag-filter))
    (message "Not in a project")))

;;; Kanban Board View

(defvar simply-annotate-kanban-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'simply-annotate-kanban-goto-source)
    (define-key map (kbd "s") #'simply-annotate-kanban-set-status)
    (define-key map (kbd "g") #'simply-annotate-kanban-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "n") #'simply-annotate-kanban-next-card)
    (define-key map (kbd "p") #'simply-annotate-kanban-prev-card)
    (define-key map (kbd "f") #'simply-annotate-kanban-next-column)
    (define-key map (kbd "b") #'simply-annotate-kanban-prev-column)
    (define-key map (kbd "TAB") #'simply-annotate-kanban-next-column)
    (define-key map (kbd "<backtab>") #'simply-annotate-kanban-prev-column)
    (define-key map (kbd "}") #'simply-annotate-kanban-move-right)
    (define-key map (kbd "{") #'simply-annotate-kanban-move-left)
    (define-key map (kbd "F") #'simply-annotate-kanban-toggle-follow)
    (define-key map (kbd "e") #'simply-annotate-kanban-toggle-expand)
    (define-key map (kbd "E") #'simply-annotate-kanban-toggle-expand-all)
    (define-key map (kbd "a") #'simply-annotate-kanban-filter-by-author)
    (define-key map (kbd "t") #'simply-annotate-kanban-filter-by-tag)
    (define-key map (kbd "d") #'simply-annotate-kanban-clear-directory-filter)
    map)
  "Keymap for `simply-annotate-kanban-mode'.")

(defvar-local simply-annotate-kanban-expand-all t
  "When non-nil, all cards are rendered in expanded view.")

(defvar-local simply-annotate-kanban-expanded-cards nil
  "Hash table of card IDs that are individually expanded.")

(defvar-local simply-annotate-kanban-filter-author nil
  "When non-nil, only show cards by this author.")

(defvar-local simply-annotate-kanban-filter-directory nil
  "When non-nil, only show cards from files under this directory.
The value is a project-relative directory string ending with a
slash, as produced by `simply-annotate--read-annotated-directory'.")

(defvar-local simply-annotate-kanban-filter-tag nil
  "When non-nil, only show cards that have this tag string.")

(define-derived-mode simply-annotate-kanban-mode special-mode "SA-Kanban"
  "Major mode for displaying annotations as a kanban board.
Annotations are grouped into columns by their status."
  (setq truncate-lines t)
  (setq header-line-format
        '(" Kanban  "
          "dir "
          (:eval (if simply-annotate-kanban-filter-directory
                     (concat
                      (propertize simply-annotate-kanban-filter-directory
                                  'face 'success)
                      "  "
                      (propertize "d" 'face 'help-key-binding)
                      " clear")
                   (propertize "[all]" 'face 'shadow)))
          "  "
          (:eval (propertize "RET" 'face 'help-key-binding)) " goto  "
          (:eval (propertize "{/}" 'face 'help-key-binding)) " move  "
          (:eval (propertize "s" 'face 'help-key-binding)) " status  "
          (:eval (propertize "e/E" 'face 'help-key-binding)) " expand  "
          (:eval (propertize "a" 'face 'help-key-binding)) " author"
          (:eval (if simply-annotate-kanban-filter-author
                     (propertize (concat "[" simply-annotate-kanban-filter-author "]")
                                 'face 'success)
                   ""))
          "  "
          (:eval (propertize "t" 'face 'help-key-binding)) " tag"
          (:eval (if simply-annotate-kanban-filter-tag
                     (propertize (concat "[" simply-annotate-kanban-filter-tag "]")
                                 'face 'success)
                   ""))
          "  "
          (:eval (propertize "F" 'face 'help-key-binding)) " follow"
          (:eval (if simply-annotate-kanban-follow
                     (propertize "[ON]" 'face 'success)
                   ""))
          "  "
          (:eval (propertize "q" 'face 'help-key-binding)) " quit"))
  (setq simply-annotate-kanban-expanded-cards (make-hash-table))
  (add-hook 'post-command-hook #'simply-annotate-kanban--highlight-card nil t))

(defvar-local simply-annotate-kanban-project-root nil
  "Project root for the current kanban board.")

(defvar-local simply-annotate-kanban-cards nil
  "Alist mapping card IDs to navigation plists.")

(defvar-local simply-annotate-kanban-card-positions nil
  "Ordered list of (POSITION . CARD-ID) for card navigation.")

(defvar-local simply-annotate-kanban-card-grid nil
  "Vector of columns, each a list of card IDs in row order.")

(defvar-local simply-annotate-kanban-card-coords nil
  "Hash table mapping card ID to (COLUMN . ROW).")

(defvar-local simply-annotate-kanban-follow nil
  "When non-nil, navigating cards also jumps to the source annotation.")

(defvar-local simply-annotate-kanban--highlight-overlays nil
  "List of overlays used to highlight the current card.")

(defvar-local simply-annotate-kanban--current-card-id nil
  "Card ID currently highlighted.")

(defun simply-annotate-kanban--highlight-card ()
  "Highlight the card at point, clearing any previous highlight."
  (let ((cid (get-text-property (point) 'simply-annotate-kanban-card)))
    (unless (eql cid simply-annotate-kanban--current-card-id)
      (setq simply-annotate-kanban--current-card-id cid)
      (dolist (ov simply-annotate-kanban--highlight-overlays)
        (delete-overlay ov))
      (setq simply-annotate-kanban--highlight-overlays nil)
      (when cid
        ;; Create an overlay for each contiguous region of this card ID
        (save-excursion
          (goto-char (point-min))
          (while (< (point) (point-max))
            (if (eql (get-text-property (point) 'simply-annotate-kanban-card) cid)
                (let ((start (point))
                      (end (or (next-single-property-change
                                (point) 'simply-annotate-kanban-card)
                               (point-max))))
                  (let ((ov (make-overlay start end)))
                    (overlay-put ov 'face 'bold)
                    (overlay-put ov 'priority 100)
                    (push ov simply-annotate-kanban--highlight-overlays))
                  (goto-char end))
              (goto-char (or (next-single-property-change
                              (point) 'simply-annotate-kanban-card)
                             (point-max)))))))
        ;; Follow mode: show source in other window
        (when simply-annotate-kanban-follow
          (let ((nav (alist-get cid simply-annotate-kanban-cards)))
            (when nav
              (save-selected-window
                (simply-annotate--listing-goto-source nav))))))))

(defun simply-annotate-kanban-toggle-follow ()
  "Toggle follow mode -- automatically jump to source when navigating cards."
  (interactive)
  (setq simply-annotate-kanban-follow (not simply-annotate-kanban-follow))
  (force-mode-line-update)
  (message "Kanban follow %s" (if simply-annotate-kanban-follow "ON" "OFF"))
  ;; Immediately follow if turning on and on a card
  (when simply-annotate-kanban-follow
    (when-let* ((cid (get-text-property (point) 'simply-annotate-kanban-card))
                (nav (alist-get cid simply-annotate-kanban-cards)))
      (save-selected-window
        (simply-annotate--listing-goto-source nav)))))

(defun simply-annotate-kanban-toggle-expand ()
  "Toggle expanded view for the card at point."
  (interactive)
  (when-let ((cid (get-text-property (point) 'simply-annotate-kanban-card)))
    (unless simply-annotate-kanban-expanded-cards
      (setq simply-annotate-kanban-expanded-cards (make-hash-table)))
    (if (gethash cid simply-annotate-kanban-expanded-cards)
        (remhash cid simply-annotate-kanban-expanded-cards)
      (puthash cid t simply-annotate-kanban-expanded-cards))
    (simply-annotate-kanban--refresh-preserving-card cid)))

(defun simply-annotate-kanban-toggle-expand-all ()
  "Toggle expanded view for all cards."
  (interactive)
  (setq simply-annotate-kanban-expand-all
        (not simply-annotate-kanban-expand-all))
  (clrhash simply-annotate-kanban-expanded-cards)
  (let ((cid (get-text-property (point) 'simply-annotate-kanban-card)))
    (simply-annotate-kanban--refresh-preserving-card cid))
  (message "Expand all %s" (if simply-annotate-kanban-expand-all "ON" "OFF")))

(defun simply-annotate-kanban--refresh-preserving-card (target-cid)
  "Refresh the kanban board, relocating point to the card matching TARGET-CID.
The card is identified by its source file and position."
  (let* ((nav (when target-cid
                (alist-get target-cid simply-annotate-kanban-cards)))
         (file-key (when nav (plist-get nav :file)))
         (start-pos (when nav (plist-get nav :start)))
         (root simply-annotate-kanban-project-root)
         (subdir simply-annotate-kanban-filter-directory))
    (when root
      (let* ((db (simply-annotate--project-annotations root))
             (filtered (simply-annotate--filter-db-by-directory db root subdir)))
        (if filtered
            (simply-annotate--kanban-render filtered)
          (let ((inhibit-read-only t)) (erase-buffer)
               (insert "No annotations"))))
      ;; Relocate to the same card by source identity
      (when (and file-key start-pos)
        (let ((new-pos nil))
          (dolist (entry simply-annotate-kanban-cards)
            (let ((nav2 (cdr entry)))
              (when (and (string= (plist-get nav2 :file) file-key)
                         (= (plist-get nav2 :start) start-pos))
                (setq new-pos
                      (simply-annotate-kanban--card-position (car entry))))))
          (when new-pos (goto-char new-pos)))))))

(defun simply-annotate--kanban-collect-authors (db)
  "Return a sorted list of unique author names from DB."
  (let ((authors nil))
    (dolist (db-entry db)
      (dolist (ann (cdr db-entry))
        (let* ((data (alist-get 'text ann)))
          (when (simply-annotate--thread-p data)
            (let ((author (alist-get 'author (car (alist-get 'comments data)))))
              (when (and author (not (string-empty-p author))
                         (not (member author authors)))
                (push author authors)))))))
    (sort authors #'string<)))

(defun simply-annotate-kanban-filter-by-author ()
  "Filter kanban cards by author.  Select from available authors or clear filter."
  (interactive)
  (let* ((root simply-annotate-kanban-project-root)
         (db (when root (simply-annotate--project-annotations root)))
         (authors (when db (simply-annotate--kanban-collect-authors db)))
         (choices (cons "[All]" authors))
         (selection (completing-read
                     (format "Filter by author%s: "
                             (if simply-annotate-kanban-filter-author
                                 (format " [current: %s]"
                                         simply-annotate-kanban-filter-author)
                               ""))
                     choices nil t)))
    (setq simply-annotate-kanban-filter-author
          (unless (string= selection "[All]") selection))
    (simply-annotate-kanban-refresh)
    (message "Filter: %s"
             (or simply-annotate-kanban-filter-author "all authors"))))

(defun simply-annotate-kanban-filter-by-tag ()
  "Filter kanban cards by tag.  Select from available tags or clear filter."
  (interactive)
  (let* ((root simply-annotate-kanban-project-root)
         (db (when root (simply-annotate--project-annotations root)))
         (tags (when db (simply-annotate--kanban-collect-tags db)))
         (choices (cons "[All]" tags))
         (selection (completing-read
                     (format "Filter by tag%s: "
                             (if simply-annotate-kanban-filter-tag
                                 (format " [current: %s]"
                                         simply-annotate-kanban-filter-tag)
                               ""))
                     choices nil t)))
    (setq simply-annotate-kanban-filter-tag
          (unless (string= selection "[All]") selection))
    (simply-annotate-kanban-refresh)
    (message "Tag filter: %s"
             (or simply-annotate-kanban-filter-tag "all tags"))))

(defun simply-annotate-kanban-clear-directory-filter ()
  "Clear the directory filter on the current kanban board and refresh.
The directory filter is initially set when `simply-annotate-kanban'
is invoked from a `dired' buffer that lives under a project
subdirectory.  This command widens such a narrowed board back to
the whole project without leaving the kanban buffer."
  (interactive)
  (if (not simply-annotate-kanban-filter-directory)
      (message "Directory filter already cleared")
    (setq simply-annotate-kanban-filter-directory nil)
    (simply-annotate-kanban-refresh)
    (message "Directory filter cleared")))

;;;###autoload
(defun simply-annotate-kanban (&optional arg)
  "Show a kanban board of project annotations grouped by status.

When called without a prefix argument from a `dired' buffer or
a file buffer that lives under a project subdirectory, the board
is automatically narrowed to that subdirectory.  Any prefix
argument (ARG) disables the auto-narrow and shows the whole
project.  A narrowed board can be widened from inside the kanban
buffer with \\<simply-annotate-kanban-mode-map>\\[simply-annotate-kanban-clear-directory-filter]."
  (interactive "P")
  (if-let* ((proj (project-current t))
            (root (simply-annotate--project-root proj)))
      (let* ((subdir (unless arg
                       (simply-annotate--buffer-default-subdir root)))
             (tag-filter simply-annotate-current-tag-filter))
        (simply-annotate--show-kanban root subdir tag-filter))
    (message "Not in a project")))

(defun simply-annotate--show-kanban (root &optional subdir tag-filter)
  "Display kanban board for project at ROOT.
If SUBDIR is non-nil, restrict the board to files under that
project-relative directory."
  (let* ((project-name (file-name-nondirectory (directory-file-name root)))
         (suffix (if subdir (format " [%s]" subdir) ""))
         (buffer-name (format "*Kanban: %s%s*" project-name suffix))
         (db (simply-annotate--project-annotations root))
         (filtered (simply-annotate--filter-db-by-directory db root subdir)))
    (if (not filtered)
        (message "No annotations found for project %s%s" project-name suffix)
      (with-current-buffer (get-buffer-create buffer-name)
        (simply-annotate-kanban-mode)
        (setq simply-annotate-kanban-project-root root)
        (setq simply-annotate-kanban-filter-directory subdir)
        (setq simply-annotate-kanban-filter-tag tag-filter))
      ;; Display first so window-width is correct for the target window
      (pop-to-buffer buffer-name)
      (simply-annotate--kanban-render filtered)
      (goto-char (point-min))
      (simply-annotate-kanban-next-card))))

(defun simply-annotate--kanban-group-by-status (db statuses)
  "Group annotations from DB into lists keyed by STATUS.
Returns alist of (STATUS . cards) where each card is
\(NAV PRIORITY SUMMARY LOCATION DATA)."
  (let ((groups (mapcar (lambda (s) (cons s nil)) statuses)))
    (dolist (db-entry db)
      (let* ((file-key (car db-entry))
             (annotations (cdr db-entry))
             (source-buffer (simply-annotate--get-key-buffer file-key))
             (line-table (simply-annotate--batch-line-info annotations source-buffer))
             (short-name (simply-annotate--key-name file-key)))
        (dolist (ann annotations)
          (let* ((start-pos (alist-get 'start ann))
                 (data (alist-get 'text ann))
                 (thread-p (simply-annotate--thread-p data))
                 (status (if thread-p
                             (or (alist-get 'status data) "open")
                           "open"))
                 (priority (if thread-p
                               (upcase (or (alist-get 'priority data) "normal"))
                             "NORMAL"))
                 (comments (if thread-p (alist-get 'comments data) nil))
                 (first-text (cond
                               (thread-p (or (alist-get 'text (car comments)) ""))
                               ((stringp data) data)
                               (t "")))
                 (summary (string-trim (car (split-string first-text "\n"))))
                 (line-info (gethash start-pos line-table '(1 0 "")))
                 (line-num (car line-info))
                 (col-num (cadr line-info))
                 (location (format "%s:%d" short-name line-num))
                 (nav (list :file file-key :line line-num
                            :col (1+ col-num)
                            :start start-pos))
                 (author (if thread-p
                             (or (alist-get 'author (car comments)) "")
                           ""))
                 (card (list nav priority summary location data)))
            ;; Apply filters
            (when (and (or (null simply-annotate-kanban-filter-author)
                           (string-equal-ignore-case
                            author simply-annotate-kanban-filter-author))
                       (or (null simply-annotate-kanban-filter-tag)
                           (and thread-p
                                (member simply-annotate-kanban-filter-tag
                                        (alist-get 'tags data)))))
              (let ((group (assoc status groups #'string=)))
                (when group
                  (setcdr group (append (cdr group) (list card))))))))))
    groups))

(defun simply-annotate--kanban-status-face (status)
  "Return face for column header STATUS."
  (pcase (downcase status)
    ("open" 'warning)
    ("in-progress" 'success)
    ("resolved" 'shadow)
    ("closed" 'shadow)
    (_ nil)))

(defun simply-annotate--kanban-priority-face (priority)
  "Return face for PRIORITY badge."
  (pcase priority
    ("CRITICAL" 'error)
    ("HIGH" 'warning)
    ("LOW" 'shadow)
    (_ nil)))

(defun simply-annotate--kanban-truncate (str width)
  "Truncate STR to WIDTH display columns, adding ellipsis if too long."
  (if (> (string-width str) width)
      (concat (truncate-string-to-width str (max 0 (- width 1))) "…")
    str))

(defun simply-annotate--kanban-pad (str width)
  "Pad STR to exactly WIDTH display columns, truncating if needed."
  (let ((truncated (simply-annotate--kanban-truncate str width)))
    (concat truncated (make-string (max 0 (- width (string-width truncated))) ?\s))))

(defun simply-annotate--kanban-card-expanded-p (cid)
  "Return non-nil if card CID should be rendered expanded.
When `expand-all' is on, individual toggles act as opt-out (collapse).
When `expand-all' is off, individual toggles act as opt-in (expand)."
  (let ((toggled (and simply-annotate-kanban-expanded-cards
                  (gethash cid simply-annotate-kanban-expanded-cards))))
    (if simply-annotate-kanban-expand-all
        (not toggled)
      toggled)))

(defun simply-annotate--kanban-wrap-text (text width)
  "Wrap TEXT to WIDTH display columns, returning a list of strings."
  (let ((lines nil)
        (remaining (string-trim text)))
    (while (> (string-width remaining) 0)
      (if (<= (string-width remaining) width)
          (progn (push remaining lines)
                 (setq remaining ""))
        (let* ((trunc (truncate-string-to-width remaining width))
               (trunc-len (length trunc))
               (break-pos (or (let ((pos (string-match-p "\\s-" (reverse trunc))))
                                (when pos (- trunc-len pos)))
                              trunc-len)))
          (push (substring remaining 0 break-pos) lines)
          (setq remaining (string-trim-left (substring remaining break-pos))))))
    (nreverse lines)))

(defun simply-annotate--kanban-build-column (cards col-width card-id-start)
  "Build column card blocks from CARDS at COL-WIDTH.
CARD-ID-START is the first card ID to assign.
Returns (BLOCKS CARD-MAP NEXT-ID) where BLOCKS is a list of
card blocks, each block being a list of (STRING . CARD-ID),
CARD-MAP is an alist of (CARD-ID . NAV-PLIST), and NEXT-ID
is the next available ID."
  (let ((inner (- col-width 2))
        (blocks nil)
        (card-map nil)
        (cid card-id-start))
    (dolist (card cards)
      (let* ((nav (nth 0 card))
             (priority (nth 1 card))
             (summary (nth 2 card))
             (location (nth 3 card))
             (data (nth 4 card))
             (expanded (simply-annotate--kanban-card-expanded-p cid))
             (prio-face (simply-annotate--kanban-priority-face priority))
             (badge (format "[%s]" priority))
             (badge-len (length badge))
             (summary-width (max 1 (- inner badge-len 1)))
             (summary-trunc (simply-annotate--kanban-truncate summary summary-width))
             (badge-display (if prio-face
                                (propertize badge 'face prio-face)
                              badge))
             (line1-content (concat badge-display " " summary-trunc))
             (line1-visual-len (+ badge-len 1 (string-width summary-trunc)))
             (line1-pad (max 0 (- inner line1-visual-len)))
             (line1 (concat "│" line1-content
                            (make-string line1-pad ?\s) "│"))
             (loc-trunc (simply-annotate--kanban-truncate location (- inner 1)))
             (loc-display (propertize loc-trunc 'face 'font-lock-comment-face))
             (loc-pad (max 0 (- inner 1 (string-width loc-trunc))))
             (line2 (concat "│ " loc-display
                            (make-string loc-pad ?\s) "│"))
             (thread-p (simply-annotate--thread-p data))
             (comments (when thread-p (alist-get 'comments data)))
             (author (if thread-p
                         (or (alist-get 'author (car comments)) "")
                       ""))
             (author-str (when (and author (not (string-empty-p author)))
                           (simply-annotate--kanban-truncate
                            author (- inner 1))))
             (top (concat "┌" (make-string inner ?─) "┐"))
             (bot (concat "└" (make-string inner ?─) "┘"))
             (block nil))
        (push (cons cid nav) card-map)
        (push (cons top cid) block)
        (push (cons line1 cid) block)
        (push (cons line2 cid) block)
        ;; Author line (always shown)
        (when author-str
          (let* ((author-display (propertize author-str 'face 'font-lock-type-face))
                 (apad (max 0 (- inner 1 (string-width author-str)))))
            (push (cons (concat "│ " author-display
                                (make-string apad ?\s) "│")
                        cid)
                  block)))
        ;; Expanded: full text, tags, comments
        (when expanded
          (let* ((tags (when thread-p (alist-get 'tags data)))
                 (full-text (cond
                              (thread-p (or (alist-get 'text (car comments)) ""))
                              ((stringp data) data)
                              (t "")))
                 (sep (concat "├" (make-string inner ?─) "┤"))
                 (text-width (- inner 2)))
            ;; Separator
            (push (cons sep cid) block)
            ;; Full annotation text wrapped
            (let ((wrapped (simply-annotate--kanban-wrap-text full-text text-width)))
              (dolist (wline wrapped)
                (let* ((wtrunc (simply-annotate--kanban-truncate wline text-width))
                       (wpad (max 0 (- text-width (string-width wtrunc)))))
                  (push (cons (concat "│ " wtrunc
                                      (make-string wpad ?\s) " │")
                              cid)
                        block))))
            ;; Tags
            (when (and tags (> (length tags) 0))
              (let* ((tag-str (mapconcat (lambda (tg) (concat "#" tg)) tags " "))
                     (tag-trunc (simply-annotate--kanban-truncate tag-str (- inner 1)))
                     (tag-display (propertize tag-trunc 'face 'font-lock-constant-face))
                     (tpad (max 0 (- inner 1 (string-width tag-trunc)))))
                (push (cons (concat "│ " tag-display
                                    (make-string tpad ?\s) "│")
                            cid)
                      block)))
            ;; Reply count
            (when (and comments (> (length comments) 1))
              (let* ((reply-str (format "%d replies" (1- (length comments))))
                     (rpad (max 0 (- inner 1 (string-width reply-str)))))
                (push (cons (concat "│ "
                                    (propertize reply-str 'face 'font-lock-comment-face)
                                    (make-string rpad ?\s) "│")
                            cid)
                      block)))))
        (push (cons bot cid) block)
        (push (nreverse block) blocks)
        (setq cid (1+ cid))))
    (list (nreverse blocks) (nreverse card-map) cid)))

(defun simply-annotate--kanban-render (db)
  "Render the kanban board from DB into the current buffer."
  (let* ((inhibit-read-only t)
         (statuses simply-annotate-thread-statuses)
         (num-cols (length statuses))
         (col-width (max 22 (/ (- (window-width) (* (1- num-cols) 2))
                               num-cols)))
         (gap "  ")
         (grouped (simply-annotate--kanban-group-by-status db statuses))
         (card-id 0)
         (columns nil)
         (all-card-map nil)
         (seen-cards (make-hash-table))
         (card-positions nil))
    ;; Build columns
    (dolist (status statuses)
      (let* ((cards (alist-get status grouped nil nil #'string=))
             (result (simply-annotate--kanban-build-column
                      cards col-width card-id)))
        (push (nth 0 result) columns)
        (setq all-card-map (append all-card-map (nth 1 result)))
        (setq card-id (nth 2 result))))
    (setq columns (nreverse columns))
    (setq simply-annotate-kanban-cards all-card-map)
    ;; Build navigation grid: card IDs grouped by column with coords
    (let ((grid (make-vector num-cols nil))
          (coords (make-hash-table))
          (col-idx 0)
          (id-offset 0))
      (dolist (status statuses)
        (let ((num-cards (length (alist-get status grouped nil nil #'string=))))
          (let ((col-cards nil))
            (dotimes (row num-cards)
              (let ((cid (+ id-offset row)))
                (push cid col-cards)
                (puthash cid (cons col-idx row) coords)))
            (aset grid col-idx (nreverse col-cards)))
          (setq id-offset (+ id-offset num-cards))
          (setq col-idx (1+ col-idx))))
      (setq simply-annotate-kanban-card-grid grid)
      (setq simply-annotate-kanban-card-coords coords))
    (erase-buffer)
    ;; Header
    (dotimes (c num-cols)
      (when (> c 0) (insert gap))
      (let* ((status (nth c statuses))
             (count (length (alist-get status grouped nil nil #'string=)))
             (header (format " %s (%d)" (upcase status) count))
             (face (simply-annotate--kanban-status-face status)))
        (insert (propertize (simply-annotate--kanban-pad header col-width)
                            'face (if face
                                      (list :inherit face :weight 'bold)
                                    'bold)))))
    (insert "\n")
    ;; Separator
    (dotimes (c num-cols)
      (when (> c 0) (insert gap))
      (insert (propertize (make-string col-width ?─) 'face 'shadow)))
    (insert "\n")
    ;; Card rows -- iterate by card index, padding blocks to equal height
    (let* ((max-cards (apply #'max 0 (mapcar #'length columns)))
           (blank-line (cons (make-string col-width ?\s) nil)))
      (dotimes (card-idx max-cards)
        ;; Find the tallest block at this card index across all columns
        (let ((max-height 0))
          (dotimes (c num-cols)
            (let ((col-blocks (nth c columns)))
              (when (< card-idx (length col-blocks))
                (setq max-height
                      (max max-height (length (nth card-idx col-blocks)))))))
          ;; Render each line of this card row
          (dotimes (line-idx max-height)
            (dotimes (c num-cols)
              (when (> c 0) (insert gap))
              (let* ((col-blocks (nth c columns))
                     (block (when (< card-idx (length col-blocks))
                              (nth card-idx col-blocks)))
                     (cell (if (and block (< line-idx (length block)))
                               (nth line-idx block)
                             blank-line))
                     (text (car cell))
                     (cid (cdr cell))
                     (start (point)))
                (insert text)
                (when cid
                  (put-text-property start (point)
                                    'simply-annotate-kanban-card cid)
                  (unless (gethash cid seen-cards)
                    (puthash cid t seen-cards)
                    (push (cons start cid) card-positions)))))
            (insert "\n"))
          ;; Blank separator line between card rows
          (when (< card-idx (1- max-cards))
            (dotimes (c num-cols)
              (when (> c 0) (insert gap))
              (insert (make-string col-width ?\s)))
            (insert "\n")))))
    (setq simply-annotate-kanban-card-positions
          (nreverse card-positions))))

;; Kanban navigation

(defun simply-annotate-kanban--card-position (card-id)
  "Return the buffer position for CARD-ID, or nil."
  (car (rassq card-id simply-annotate-kanban-card-positions)))

(defun simply-annotate-kanban-next-card ()
  "Move point to the next card in the same column."
  (interactive)
  (let ((current-id (get-text-property (point) 'simply-annotate-kanban-card)))
    (if (null current-id)
        ;; Not on a card -- jump to first card
        (when-let ((first (car simply-annotate-kanban-card-positions)))
          (goto-char (car first)))
      (when-let* ((coord (gethash current-id simply-annotate-kanban-card-coords))
                  (col-idx (car coord))
                  (row (cdr coord))
                  (col-cards (aref simply-annotate-kanban-card-grid col-idx))
                  (next-row (1+ row)))
        (when (< next-row (length col-cards))
          (when-let ((pos (simply-annotate-kanban--card-position
                           (nth next-row col-cards))))
            (goto-char pos)))))))

(defun simply-annotate-kanban-prev-card ()
  "Move point to the previous card in the same column."
  (interactive)
  (let ((current-id (get-text-property (point) 'simply-annotate-kanban-card)))
    (when current-id
      (when-let* ((coord (gethash current-id simply-annotate-kanban-card-coords))
                  (col-idx (car coord))
                  (row (cdr coord))
                  (col-cards (aref simply-annotate-kanban-card-grid col-idx))
                  (prev-row (1- row)))
        (when (>= prev-row 0)
          (when-let ((pos (simply-annotate-kanban--card-position
                           (nth prev-row col-cards))))
            (goto-char pos)))))))

(defun simply-annotate-kanban-next-column ()
  "Move point to the same row in the next column, wrapping around."
  (interactive)
  (let ((current-id (get-text-property (point) 'simply-annotate-kanban-card))
        (grid simply-annotate-kanban-card-grid)
        (num-cols (length simply-annotate-kanban-card-grid)))
    (if (null current-id)
        (when-let ((first (car simply-annotate-kanban-card-positions)))
          (goto-char (car first)))
      (when-let* ((coord (gethash current-id simply-annotate-kanban-card-coords))
                  (col-idx (car coord))
                  (row (cdr coord)))
        ;; Search columns to the right, then wrap from the left
        (cl-loop for i from 1 below num-cols
                 for c = (mod (+ col-idx i) num-cols)
                 for col-cards = (aref grid c)
                 when (and col-cards (< row (length col-cards)))
                 do (when-let ((pos (simply-annotate-kanban--card-position
                                     (nth row col-cards))))
                      (goto-char pos))
                 and return nil)))))

(defun simply-annotate-kanban-prev-column ()
  "Move point to the same row in the previous column, wrapping around."
  (interactive)
  (let ((current-id (get-text-property (point) 'simply-annotate-kanban-card))
        (grid simply-annotate-kanban-card-grid)
        (num-cols (length simply-annotate-kanban-card-grid)))
    (if (null current-id)
        (when-let ((first (car simply-annotate-kanban-card-positions)))
          (goto-char (car first)))
      (when-let* ((coord (gethash current-id simply-annotate-kanban-card-coords))
                  (col-idx (car coord))
                  (row (cdr coord)))
        ;; Search columns to the left, then wrap from the right
        (cl-loop for i from 1 below num-cols
                 for c = (mod (- col-idx i) num-cols)
                 for col-cards = (aref grid c)
                 when (and col-cards (< row (length col-cards)))
                 do (when-let ((pos (simply-annotate-kanban--card-position
                                     (nth row col-cards))))
                      (goto-char pos))
                 and return nil)))))

;; Kanban actions

(defun simply-annotate-kanban-goto-source ()
  "Jump to the source location of the card at point."
  (interactive)
  (let ((card-id (get-text-property (point) 'simply-annotate-kanban-card)))
    (if (not card-id)
        (message "No card at point")
      (let ((nav (alist-get card-id simply-annotate-kanban-cards)))
        (when nav
          (simply-annotate--listing-goto-source nav))))))

(defun simply-annotate--kanban-update-status (new-status)
  "Set the card at point to NEW-STATUS, updating database and overlays.
Plain string annotations are auto-converted to threads."
  (let ((card-id (get-text-property (point) 'simply-annotate-kanban-card)))
    (if (not card-id)
        (message "No card at point")
      (let* ((nav (alist-get card-id simply-annotate-kanban-cards))
             (file-key (plist-get nav :file))
             (start-pos (plist-get nav :start))
             (default-directory (or simply-annotate-kanban-project-root
                                    default-directory))
             (db (simply-annotate--load-database))
             (file-anns (alist-get file-key db nil nil #'string=))
             (ann (when file-anns
                    (cl-find-if
                     (lambda (a) (= (alist-get 'start a) start-pos))
                     file-anns))))
        (when ann
          (let ((data (alist-get 'text ann)))
            ;; Auto-convert plain annotations to threads
            (unless (simply-annotate--thread-p data)
              (setq data (simply-annotate--create-thread
                          (if (stringp data) data "")))
              (setf (alist-get 'text ann) data))
            (setf (alist-get 'status data) new-status)
            (simply-annotate--save-database db)
            ;; Update overlay in source buffer if open
            (when-let* ((source (simply-annotate--get-key-buffer file-key)))
              (when (buffer-live-p source)
                (with-current-buffer source
                  (dolist (ov simply-annotate-overlays)
                    (when (= (overlay-start ov) start-pos)
                      (overlay-put ov 'simply-annotation data)))
                  (simply-annotate-update-display-style))))
            (simply-annotate-kanban-refresh)
            ;; Re-find the moved card by its source location
            (let ((target-pos nil))
              (dolist (entry simply-annotate-kanban-cards)
                (let ((nav2 (cdr entry)))
                  (when (and (string= (plist-get nav2 :file) file-key)
                             (= (plist-get nav2 :start) start-pos))
                    (setq target-pos
                          (simply-annotate-kanban--card-position (car entry))))))
              (when target-pos (goto-char target-pos)))
            (message "Status: %s" new-status)))))))

(defun simply-annotate-kanban-set-status ()
  "Change the status of the annotation card at point."
  (interactive)
  (simply-annotate--kanban-update-status
   (completing-read "Status: " simply-annotate-thread-statuses nil t)))

(defun simply-annotate--kanban-card-status ()
  "Return the current status of the card at point, or nil."
  (when-let* ((card-id (get-text-property (point) 'simply-annotate-kanban-card))
              (nav (alist-get card-id simply-annotate-kanban-cards))
              (file-key (plist-get nav :file))
              (start-pos (plist-get nav :start))
              (default-directory (or simply-annotate-kanban-project-root
                                     default-directory))
              (db (simply-annotate--load-database))
              (file-anns (alist-get file-key db nil nil #'string=))
              (ann (cl-find-if
                    (lambda (a) (= (alist-get 'start a) start-pos))
                    file-anns))
              (data (alist-get 'text ann)))
    (if (simply-annotate--thread-p data)
        (or (alist-get 'status data) "open")
      "open")))

(defun simply-annotate-kanban-move-right ()
  "Move the card at point one column to the right."
  (interactive)
  (let* ((current (simply-annotate--kanban-card-status))
         (statuses simply-annotate-thread-statuses)
         (idx (when current (cl-position current statuses :test #'string=))))
    (cond
     ((not current) (message "No card at point"))
     ((not idx) (message "Unknown status"))
     ((>= (1+ idx) (length statuses)) (message "Already in last column"))
     (t (simply-annotate--kanban-update-status (nth (1+ idx) statuses))))))

(defun simply-annotate-kanban-move-left ()
  "Move the card at point one column to the left."
  (interactive)
  (let* ((current (simply-annotate--kanban-card-status))
         (statuses simply-annotate-thread-statuses)
         (idx (when current (cl-position current statuses :test #'string=))))
    (cond
     ((not current) (message "No card at point"))
     ((not idx) (message "Unknown status"))
     ((<= idx 0) (message "Already in first column"))
     (t (simply-annotate--kanban-update-status (nth (1- idx) statuses))))))

(defun simply-annotate-kanban-refresh ()
  "Refresh the kanban board in place."
  (interactive)
  (let ((pos (point))
        (root simply-annotate-kanban-project-root)
        (subdir simply-annotate-kanban-filter-directory))
    (when root
      (let* ((db (simply-annotate--project-annotations root))
             (filtered (simply-annotate--filter-db-by-directory db root subdir)))
        (if filtered
            (progn
              (simply-annotate--kanban-render filtered)
              (goto-char (min pos (point-max))))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "No annotations found.")))))))

;;;###autoload
(defun simply-annotate-list ()
  "Show the annotation listing for the current buffer.
Reuses an existing listing buffer if one exists; press the
Refresh button or \\[simply-annotate-listing-refresh] to rebuild."
  (interactive)
  (let* ((buffer-name "*Annotations*")
         (source-buffer (current-buffer))
         (existing (get-buffer buffer-name)))
    (if (and existing
             (eq source-buffer
                 (buffer-local-value 'simply-annotate-listing-source existing)))
        (pop-to-buffer existing)
      (when existing (kill-buffer existing))
      (if simply-annotate-overlays
          (let* ((source-file (or (buffer-file-name) (buffer-name)))
                 (annotations (simply-annotate--serialize-annotations))
                 (annotation-buffer (simply-annotate--format-annotations-for-buffer
                                     source-file annotations source-buffer buffer-name)))
            (pop-to-buffer annotation-buffer)
            (goto-char (point-min)))
        (message "No annotations in buffer")))))

(defun simply-annotate--project-annotations (root)
  "Return database entries for files under ROOT.
Reads the per-project annotation file at ROOT (if any) and merges
it with global-database entries whose absolute keys fall under
ROOT.  Works regardless of the current buffer's project context,
so cross-project browsing from the projects overview reads the
target project's annotations, not the current one's."
  (let* ((expanded-root (file-name-as-directory (expand-file-name root)))
         (proj-file (expand-file-name simply-annotate-project-file expanded-root))
         (proj-db (when (file-exists-p proj-file)
                    (simply-annotate--read-db proj-file)))
         (global-db (simply-annotate--read-db simply-annotate-file))
         (global-under-root
          (cl-remove-if-not
           (lambda (entry)
             (let ((key (car entry)))
               (and (file-name-absolute-p key)
                    (string-prefix-p expanded-root (expand-file-name key)))))
           global-db)))
    (simply-annotate--merge-databases proj-db global-under-root)))

(defun simply-annotate--filter-db-by-directory (db root subdir)
  "Return entries from DB whose file key lives under SUBDIR.
ROOT is the project root used to resolve relative keys (project
strategy) into absolute paths.  SUBDIR is an absolute or
project-relative directory path; nil or empty means no filtering.
Info-node keys are excluded from filtered views."
  (if (or (null subdir) (string-empty-p subdir))
      db
    (let* ((expanded-root (expand-file-name (file-name-as-directory root)))
           (expanded-sub
            (file-name-as-directory
             (expand-file-name
              (if (file-name-absolute-p subdir)
                  subdir
                (expand-file-name subdir expanded-root))))))
      (cl-remove-if-not
       (lambda (entry)
         (let ((key (car entry)))
           (and (not (simply-annotate--key-info-p key))
                (let ((abs (expand-file-name
                            (if (file-name-absolute-p key)
                                key
                              (expand-file-name key expanded-root)))))
                  (string-prefix-p expanded-sub abs)))))
       db))))

(defun simply-annotate--buffer-default-subdir (root)
  "Return the project-relative directory of the current buffer.
ROOT is the project root.  Returns a slash-terminated relative
path when the current buffer's `default-directory' lies under
ROOT but is not ROOT itself.  Returns nil otherwise (root equals
project root, or outside the project).  Used to seed the
directory filter on project commands."
  (when default-directory
    (let* ((expanded-root (expand-file-name (file-name-as-directory root)))
           (current-dir (expand-file-name
                         (file-name-as-directory default-directory))))
      (when (and (string-prefix-p expanded-root current-dir)
                 (not (string= expanded-root current-dir)))
        (file-relative-name current-dir expanded-root)))))

(defun simply-annotate--show-filtered (buffer-name title db)
  "Display annotations from DB in a buffer called BUFFER-NAME.
TITLE is inserted as the org #+TITLE header.  Shared rendering
logic used by `simply-annotate-show-all' and `simply-annotate-show-project'."
  (if-let ((existing (get-buffer buffer-name)))
      (pop-to-buffer existing)
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
              (insert (format "#+TITLE: %s\n" title))
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
          (pop-to-buffer buffer-name))))))

;;; Cross-Project Overview

(defun simply-annotate--discover-projects ()
  "Discover all projects with annotations.
Scans two sources:

1. The global annotation database (`simply-annotate-file'): entries
   whose file-keys resolve to a project are grouped by that project.

2. Known project roots from `project.el' (`project-known-project-roots',
   the same list that backs `project-switch-project'): for each root
   that contains a per-project annotation file
   (`simply-annotate-project-file'), its entries are folded in.

Returns an alist of (PROJECT-ROOT . DB-ENTRIES) where each
DB-ENTRIES is a list of (file-key . annotations).  Global-db
entries whose keys cannot be resolved to a project are grouped
under a \"(no project)\" pseudo-root."
  (let ((result nil)
        (project-cache (make-hash-table :test #'equal)))
    ;; 1. Scan the global database.
    (when-let ((db (simply-annotate--read-db simply-annotate-file)))
      (dolist (entry db)
        (let* ((key (car entry))
               (root
                (cond
                 ;; Info-node keys have no project
                 ((simply-annotate--key-info-p key) nil)
                 ;; Absolute file paths: find project root
                 ((file-name-absolute-p key)
                  (let ((dir (file-name-directory key)))
                    (or (gethash dir project-cache)
                        (let ((found (when (and dir (file-directory-p dir))
                                       (let ((proj (ignore-errors
                                                     (project-current nil dir))))
                                         (when proj
                                           (simply-annotate--project-root proj))))))
                          (puthash dir (or found :none) project-cache)
                          (unless (eq found nil) found)))))
                 ;; Relative keys (project strategy): use current project
                 (t (when-let* ((proj (project-current nil))
                                (r (simply-annotate--project-root proj)))
                      r)))))
          (when (eq root :none) (setq root nil))
          (let ((group-key (if root
                               (file-name-as-directory (expand-file-name root))
                             "(no project)")))
            (push entry (alist-get group-key result nil nil #'string=))))))
    (dolist (group result)
      (setcdr group (nreverse (cdr group))))
    ;; 2. Scan known project roots for per-project annotation files.
    (when (fboundp 'project-known-project-roots)
      (dolist (raw-root (ignore-errors (project-known-project-roots)))
        (let* ((root (file-name-as-directory (expand-file-name raw-root)))
               (ann-file (expand-file-name simply-annotate-project-file root)))
          (when (and (file-directory-p root)
                     (file-exists-p ann-file))
            (when-let ((db (simply-annotate--read-db ann-file)))
              (let ((existing (alist-get root result nil nil #'string=))
                    (new-entries nil))
                (dolist (entry db)
                  (unless (cl-find (car entry) existing
                                   :key #'car :test #'equal)
                    (push entry new-entries)))
                (when new-entries
                  (setf (alist-get root result nil nil #'string=)
                        (append existing (nreverse new-entries))))))))))
    result))

(defvar simply-annotate-projects-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'simply-annotate-projects-goto-table)
    (define-key map (kbd "L") #'simply-annotate-projects-goto-org)
    (define-key map (kbd "K") #'simply-annotate-projects-goto-kanban)
    (define-key map (kbd "g") #'simply-annotate-projects-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `simply-annotate-projects-mode'.")

(defun simply-annotate--projects-numeric-sorter (col)
  "Return a tabulated-list sort predicate comparing column COL numerically."
  (lambda (a b)
    (< (string-to-number (aref (cadr a) col))
       (string-to-number (aref (cadr b) col)))))

(defun simply-annotate--abbrev-status (status)
  "Return a short header abbreviation for STATUS.
Takes the initial of each hyphen- or space-separated word, so
\"open\" -> \"O\", \"in-progress\" -> \"IP\", \"resolved\" ->
\"R\", \"closed\" -> \"C\"."
  (mapconcat (lambda (word) (upcase (substring word 0 1)))
             (split-string status "[-_ ]+" t)
             ""))

(define-derived-mode simply-annotate-projects-mode tabulated-list-mode "SA-Projects"
  "Major mode for listing all projects with annotations.

Status columns use initials: O=open, IP=in-progress, R=resolved,
C=closed (derived from `simply-annotate-thread-statuses').

Keymap:\n\n  RET  open project annotation table\n  L    open project org listing\n  K    open project kanban\n  g    refresh\n  q    quit"
  (let* ((statuses simply-annotate-thread-statuses)
         (status-cols
          (let ((col 3)
                (cols nil))
            (dolist (st statuses)
              (let ((header (simply-annotate--abbrev-status st)))
                (push (list header
                            (max 4 (+ 2 (length header)))
                            (simply-annotate--projects-numeric-sorter col))
                      cols))
              (setq col (1+ col)))
            (nreverse cols)))
         (format-list
          (append
           (list '("Project" 25 t)
                 (list "Files" 6 (simply-annotate--projects-numeric-sorter 1))
                 (list "Ann" 5 (simply-annotate--projects-numeric-sorter 2)))
           status-cols
           (list '("Path" 0 t)))))
    (setq tabulated-list-format (vconcat format-list)))
  (setq tabulated-list-sort-key '("Project" . nil))
  (tabulated-list-init-header))

(defun simply-annotate--count-statuses (db-entries statuses)
  "Return a hash table mapping each status in STATUSES to its count.
DB-ENTRIES is a list of (file-key . annotations) pairs.  Each
annotation's status defaults to \"open\"; legacy string
annotations (no thread) are also counted as \"open\".  Statuses
not in STATUSES are ignored."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (st statuses) (puthash st 0 table))
    (dolist (entry db-entries)
      (dolist (ann (cdr entry))
        (let* ((data (alist-get 'text ann))
               (thread-p (simply-annotate--thread-p data))
               (status (if thread-p
                           (or (alist-get 'status data) "open")
                         "open")))
          (when (gethash status table)
            (puthash status (1+ (gethash status table)) table)))))
    table))

(defun simply-annotate--projects-tabulated-entries (projects)
  "Build `tabulated-list-entries' from PROJECTS alist.
PROJECTS is an alist of (ROOT . DB-ENTRIES) as returned by
`simply-annotate--discover-projects'."
  (let ((entries nil)
        (statuses simply-annotate-thread-statuses))
    (dolist (group projects)
      (let* ((root (car group))
             (db-entries (cdr group))
             (project-name (if (string= root "(no project)")
                               root
                             (file-name-nondirectory
                              (directory-file-name root))))
             (file-count (length db-entries))
             (ann-count (cl-reduce #'+ db-entries
                                   :key (lambda (e) (length (cdr e)))))
             (status-counts (simply-annotate--count-statuses db-entries statuses))
             (status-cells (mapcar (lambda (st)
                                     (number-to-string (gethash st status-counts 0)))
                                   statuses))
             (display-path (if (string= root "(no project)")
                               ""
                             (abbreviate-file-name root))))
        (push (list root
                    (vconcat
                     (vector project-name
                             (number-to-string file-count)
                             (number-to-string ann-count))
                     (apply #'vector status-cells)
                     (vector display-path)))
              entries)))
    (nreverse entries)))

(defun simply-annotate--projects-root-at-point ()
  "Return the project root for the entry at point, or nil."
  (let ((id (tabulated-list-get-id)))
    (when (and id (not (string= id "(no project)")))
      id)))

(defun simply-annotate-projects-goto-table ()
  "Open the project annotation table for the project at point."
  (interactive)
  (if-let ((root (simply-annotate--projects-root-at-point)))
      (let ((default-directory root))
        (simply-annotate--show-project-table root nil nil))
    (message "No project at point")))

(defun simply-annotate-projects-goto-org ()
  "Open the project org listing for the project at point."
  (interactive)
  (if-let ((root (simply-annotate--projects-root-at-point)))
      (let* ((project-name (file-name-nondirectory
                            (directory-file-name root)))
             (db (simply-annotate--project-annotations root)))
        (simply-annotate--show-filtered
         (format "*Annotations: %s*" project-name)
         (format "Project Annotations: %s" project-name)
         db))
    (message "No project at point")))

(defun simply-annotate-projects-goto-kanban ()
  "Open the kanban board for the project at point."
  (interactive)
  (if-let ((root (simply-annotate--projects-root-at-point)))
      (let ((default-directory root))
        (simply-annotate-kanban 'project))
    (message "No project at point")))

(defun simply-annotate-projects-refresh ()
  "Refresh the projects overview buffer."
  (interactive)
  (let ((projects (simply-annotate--discover-projects)))
    (setq tabulated-list-entries
          (simply-annotate--projects-tabulated-entries projects))
    (tabulated-list-print t)))

;;;###autoload
(defun simply-annotate-list-projects ()
  "Show all projects that have annotations in a sortable table.
Scans the global annotation database and all project.el-known
project roots (`project-known-project-roots') for per-project
annotation files, displaying each project with file and
annotation counts.  From the table you can drill down into any
project's annotations via RET (table), L (org listing), or
K (kanban)."
  (interactive)
  (let* ((buffer-name "*Annotation Projects*")
         (existing (get-buffer buffer-name)))
    (when existing (kill-buffer existing))
    (let ((projects (simply-annotate--discover-projects)))
      (if (not projects)
          (message "No annotations found in global or known-project databases")
        (with-current-buffer (get-buffer-create buffer-name)
          (simply-annotate-projects-mode)
          (setq tabulated-list-entries
                (simply-annotate--projects-tabulated-entries projects))
          (tabulated-list-print))
        (pop-to-buffer buffer-name)))))

;;;###autoload
(defun simply-annotate-show-all ()
  "Show all annotations across all files in an org-mode buffer.
Reuses an existing buffer if one exists; press the Refresh button
or \\[simply-annotate-listing-refresh] to rebuild.
Files are grouped by directory.  Directories are top-level headings,
files are second-level, and annotation levels below that."
  (interactive)
  (simply-annotate--show-filtered
   "*All Annotations*"
   "All Annotations"
   (simply-annotate--load-database)))

;;;###autoload
(defun simply-annotate-show-project (&optional arg)
  "Show annotations for all files in the current project.
Like `simply-annotate-show-all' but filtered to the current project
as detected by `project-current'.

ARG is the raw prefix argument and selects scope:

  no prefix    project files; when called from a `dired' buffer or
               a file buffer that lives under a project subdirectory,
               the view is automatically narrowed to that subdirectory
  \\[universal-argument]            whole project, ignoring any auto-narrow
  \\[universal-argument] \\[universal-argument]        every annotated file in the database
                  (delegates to `simply-annotate-show-all')"
  (interactive "P")
  (cond
   ((equal arg '(16))
    (simply-annotate-show-all))
   (t
    (if-let* ((proj (project-current t))
              (root (simply-annotate--project-root proj)))
        (let* ((project-name (file-name-nondirectory (directory-file-name root)))
               (db (simply-annotate--project-annotations root))
               (subdir (unless arg
                         (simply-annotate--buffer-default-subdir root)))
               (filtered (simply-annotate--filter-db-by-directory db root subdir))
               (suffix (if subdir (format " [%s]" subdir) "")))
          (simply-annotate--show-filtered
           (format "*Annotations: %s%s*" project-name suffix)
           (format "Project Annotations: %s%s" project-name suffix)
           filtered))
      (message "Not in a project")))))

;;;###autoload
(defun simply-annotate-jump-to-file (&optional arg)
  "Jump to an annotated file via completing-read.
Opens the selected file and enables `simply-annotate-mode'.

ARG is the raw prefix argument and selects the candidate set:

  no prefix    project files only (or all files if not in a project);
               when called from a `dired' buffer or a file buffer
               that lives under a project subdirectory, the candidates
               are automatically narrowed to that subdirectory
  \\[universal-argument]            whole project, ignoring any auto-narrow
  \\[universal-argument] \\[universal-argument]        every annotated file in the database"
  (interactive "P")
  (let* ((all (equal arg '(16)))
         (in-project (and (not all)
                          (not (eq simply-annotate-database-strategy 'global))
                          (project-current nil)))
         (root (when in-project
                 (simply-annotate--project-root in-project)))
         (project-name (when root
                         (file-name-nondirectory (directory-file-name root))))
         (project-db (when root
                       (simply-annotate--project-annotations root)))
         (subdir (when (and root project-db (not arg))
                   (simply-annotate--buffer-default-subdir root)))
         (db (cond
              (all (simply-annotate--load-database))
              (project-db
               (simply-annotate--filter-db-by-directory project-db root subdir))
              (t (simply-annotate--load-database))))
         (scope-label (cond
                       (all "global")
                       (subdir (format "%s/%s" project-name subdir))
                       (project-name project-name)
                       (t "global"))))
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
            (message "No annotations found in %s" scope-label)
          (let* ((prompt (format "Annotated file [%s]: " scope-label))
                 (selected-display (completing-read prompt
                                                    file-display-alist nil t))
                 (selected-key (cdr (assoc selected-display file-display-alist))))
            (when (and selected-key (simply-annotate--get-key-buffer selected-key t))
              (unless simply-annotate-mode
                (simply-annotate-mode 1)))))))))

;;;###autoload
(defun simply-annotate-search-by-tag (&optional arg)
  "Search for annotations by tag and jump to the selected one.
First selects a tag via `completing-read', then selects from matching
annotations, then visits the file and navigates to the annotation.

ARG is the raw prefix argument and selects the candidate set:

  no prefix    project files only (or all files if not in a project);
               when called from a `dired' buffer or a file buffer
               that lives under a project subdirectory, the candidates
               are automatically narrowed to that subdirectory
  \\[universal-argument]            whole project, ignoring any auto-narrow
  \\[universal-argument] \\[universal-argument]        every annotated file in the database"
  (interactive "P")
  (let* ((all (equal arg '(16)))
         (in-project (and (not all)
                          (not (eq simply-annotate-database-strategy 'global))
                          (project-current nil)))
         (root (when in-project
                 (simply-annotate--project-root in-project)))
         (project-name (when root
                         (file-name-nondirectory (directory-file-name root))))
         (project-db (when root
                       (simply-annotate--project-annotations root)))
         (subdir (when (and root project-db (not arg))
                   (simply-annotate--buffer-default-subdir root)))
         (db (cond
              (all (simply-annotate--load-database))
              (project-db
               (simply-annotate--filter-db-by-directory project-db root subdir))
              (t (simply-annotate--load-database))))
         (scope-label (cond
                       (all "global")
                       (subdir (format "%s/%s" project-name subdir))
                       (project-name project-name)
                       (t "global")))
         (all-tags (when db (simply-annotate--collect-all-tags db))))
    (if (not all-tags)
        (message "No tags found in %s" scope-label)
      (let* ((tag (completing-read
                   (format "Tag [%s]: " scope-label)
                   all-tags nil t))
             (filtered (simply-annotate--filter-db-by-tag db tag)))
        (if (not filtered)
            (message "No annotations with tag '%s'" tag)
          (let* ((candidates nil))
            (dolist (entry filtered)
              (let ((file-key (car entry))
                    (annotations (cdr entry)))
                (dolist (ann annotations)
                  (let ((data (alist-get 'text ann)))
                    (when (simply-annotate--thread-p data)
                      (push (cons (simply-annotate--format-tag-annotation-entry file-key ann)
                                  (cons file-key ann))
                            candidates))))))
            (let* ((sorted (nreverse (sort candidates (lambda (a b) (string< (car a) (car b))))))
                   (selected-display (completing-read
                                      (format "Annotation with %s [%s]: " tag scope-label)
                                      (mapcar #'car sorted) nil t))
                   (selected (cdr (assoc selected-display sorted))))
              (when selected
                (let ((file-key (car selected))
                      (ann (cdr selected)))
                  (when (simply-annotate--get-key-buffer file-key t)
                    (unless simply-annotate-mode
                      (simply-annotate-mode 1))
                    (let ((start (alist-get 'start ann)))
                      (when start
                        (goto-char start)))))))))))))

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
    (define-key map (kbd "M-s C-t") #'simply-annotate-remove-annotation-tag)
    (define-key map (kbd "M-s o") #'simply-annotate-export-to-org-file)
    (define-key map (kbd "M-s e") #'simply-annotate-edit-sexp)
    (define-key map (kbd "M-s [") #'simply-annotate-cycle-tag-backward)
    (define-key map (kbd "M-s ]") #'simply-annotate-cycle-tag-forward)
    (define-key map (kbd "M-s T") #'simply-annotate-set-tag-filter)
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
    (define-key map (kbd "C") #'simply-annotate-clear-buffer-annotations)
    (define-key map (kbd "a") #'simply-annotate-change-annotation-author)
    (define-key map (kbd "l") #'simply-annotate-list)
    (define-key map (kbd "T") #'simply-annotate-list-table)
    (define-key map (kbd "L") #'simply-annotate-show-all)
    (define-key map (kbd "P") #'simply-annotate-show-project)
    (define-key map (kbd "C-t") #'simply-annotate-list-project-table)
    (define-key map (kbd "K") #'simply-annotate-kanban)
    (define-key map (kbd "A") #'simply-annotate-list-projects)
    (define-key map (kbd "f") #'simply-annotate-jump-to-file)
    (define-key map (kbd "p") #'simply-annotate-set-annotation-priority)
    (define-key map (kbd "t") #'simply-annotate-add-annotation-tag)
    (define-key map (kbd "C-d") #'simply-annotate-remove-annotation-tag)
    (define-key map (kbd "F") #'simply-annotate-search-by-tag)
    (define-key map (kbd "o") #'simply-annotate-export-to-org-file)
    (define-key map (kbd "e") #'simply-annotate-edit-sexp)
    (define-key map (kbd "[") #'simply-annotate-cycle-tag-backward)
    (define-key map (kbd "]") #'simply-annotate-cycle-tag-forward)
    (define-key map (kbd "'") #'simply-annotate-cycle-display-style)
    (define-key map (kbd "/") #'simply-annotate-toggle-inline)
    (define-key map (kbd "n") #'simply-annotate-next)
    (define-key map (kbd "v") #'simply-annotate-previous)
    (define-key map (kbd "g") #'simply-annotate-update-display-style)
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
  -  remove            C  clear buffer     a  change author   l  list
  T  table             L  show all        P  show project
  A  all projects      C-t  project table   f  jump to file    p  priority
  t  tag               o  org export      e  edit sexp
  [  level backward    ]  level forward   \\='  cycle style
  /  toggle inline     n  next            v  previous
  g  refresh

When binding to M-s, you will shadow Emacs's default `search-map'
prefix, so bindings like M-s o (occur) and M-s . (isearch-forward-
symbol-at-point) will stop working.  Call
`simply-annotate-inherit-search-map' after loading the package to
restore them via keymap inheritance:

  (with-eval-after-load \\='simply-annotate
    (simply-annotate-inherit-search-map))

Simply-annotate's own bindings still take precedence; anything not
defined in the command map falls through to the standard search-map.")

(defun simply-annotate-inherit-search-map ()
  "Make `simply-annotate-command-map' inherit from Emacs's `search-map'.
Recommended when binding the command map to M-s, which otherwise
shadows the default search-map prefix and breaks bindings like
\\[occur-dwim] (occur) and \\[isearch-forward-symbol-at-point]
\(isearch-forward-symbol-at-point\).

After calling this, keys not defined in `simply-annotate-command-map'
fall through to `search-map' via Emacs's keymap parent chain.
Simply-annotate's own bindings take precedence over any matching
search-map keys, so e.g. M-s n remains `simply-annotate-next'."
  (set-keymap-parent simply-annotate-command-map search-map))

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
        (setq simply-annotate-current-tag-filter simply-annotate-default-tag-filter)
        (setq simply-annotate-inline simply-annotate-inline-default)
        (simply-annotate--apply-tag-filter)
        (simply-annotate--setup-header)
        (simply-annotate--update-header)
        (add-hook 'before-save-hook #'simply-annotate--save-annotations nil t)
        (add-hook 'kill-buffer-hook #'simply-annotate--save-annotations nil t)
        (add-hook 'kill-buffer-hook #'simply-annotate-hide-annotation-buffer nil t)
        (add-hook 'before-revert-hook #'simply-annotate--before-revert nil t)
        (add-hook 'after-revert-hook #'simply-annotate--after-revert nil t)
        (add-hook 'Info-selection-hook #'simply-annotate--info-selection-hook)
        (when simply-annotate-overlays
          (message "Simply-annotate: loaded %d annotations."
                   (length simply-annotate-overlays))))
    (simply-annotate--clear-all-overlays)
    (simply-annotate--cleanup-header)
    (simply-annotate-hide-annotation-buffer)
    (remove-hook 'before-revert-hook #'simply-annotate--before-revert t)
    (remove-hook 'after-revert-hook #'simply-annotate--after-revert t)
    (remove-hook 'before-save-hook #'simply-annotate--save-annotations t)
    (remove-hook 'kill-buffer-hook #'simply-annotate--save-annotations t)
    (remove-hook 'kill-buffer-hook #'simply-annotate-hide-annotation-buffer t)
    (remove-hook 'Info-selection-hook #'simply-annotate--info-selection-hook)))

;;; Buffer Revert

(defun simply-annotate--before-revert ()
  "Save annotations and clear overlays before `revert-buffer'.
Clearing overlays here prevents the mode-enable persist logic from
re-merging them into the database if the mode is re-activated during revert."
  (simply-annotate--save-annotations)
  (simply-annotate--clear-all-overlays))

(defun simply-annotate--after-revert ()
  "Reload annotations after `revert-buffer'.
Clears any surviving overlays and reloads from the database."
  (simply-annotate--clear-all-overlays)
  (simply-annotate--cleanup-draft)
  (simply-annotate--load-annotations)
  (simply-annotate--apply-tag-filter)
  (simply-annotate--update-header))

;;; Info-mode Integration

(defun simply-annotate--info-selection-hook ()
  "Refresh annotations when a new Info node is selected."
  (when simply-annotate-mode
    (simply-annotate--clear-all-overlays)
    (simply-annotate--cleanup-draft)
    (simply-annotate--load-annotations)
    (simply-annotate--apply-tag-filter)
    (simply-annotate--update-header)))


;;; Project Migration

;;;###autoload
(defun simply-annotate-migrate-to-project ()
  "Migrate annotations for the current project from global to project database.
Copies annotations whose file-keys fall under the current project root
into a new project-local database with relative keys.  Migrated entries
are removed from the global database."
  (interactive)
  (let* ((proj (project-current t))
         (root (simply-annotate--project-root proj))
         (global-db (simply-annotate--read-db simply-annotate-file))
         (project-path (expand-file-name simply-annotate-project-file root))
         (project-db (simply-annotate--read-db project-path))
         (expanded-root (expand-file-name root))
         (migrated 0)
         (remaining nil))
    (unless global-db
      (user-error "No global annotations database found"))
    (dolist (entry global-db)
      (let ((key (car entry))
            (annotations (cdr entry)))
        (if (and (not (simply-annotate--key-info-p key))
                 (file-name-absolute-p key)
                 (string-prefix-p expanded-root (expand-file-name key)))
            (let ((rel-key (file-relative-name key root)))
              (setf (alist-get rel-key project-db nil nil #'string=) annotations)
              (setq migrated (1+ migrated)))
          (push entry remaining))))
    (if (zerop migrated)
        (message "No annotations found for project %s" root)
      (with-temp-file project-path
        (insert ";;; Simply Annotate Database\n")
        (insert ";;; This file is auto-generated. Do not edit manually.\n\n")
        (prin1 project-db (current-buffer))
        (insert "\n"))
      (if remaining
          (with-temp-file simply-annotate-file
            (insert ";;; Simply Annotate Database\n")
            (insert ";;; This file is auto-generated. Do not edit manually.\n\n")
            (prin1 (nreverse remaining) (current-buffer))
            (insert "\n"))
        (when (file-exists-p simply-annotate-file)
          (delete-file simply-annotate-file)))
      (message "Migrated %d annotation(s) to %s" migrated project-path))))


;;; Dired Integration

(defvar-local simply-annotate-dired-overlays nil
  "List of fringe overlays added by `simply-annotate-dired-mode'.")

(defun simply-annotate--dired-annotated-files ()
  "Return a set of absolute file paths that have annotations in the database.
Relative keys (used by the `project' database strategy) are
resolved against the current project root via
`simply-annotate--resolve-key-path', so dired matching works for
both absolute and project-relative entries.  Info-node keys, and
relative keys that cannot be resolved (no project context),
are skipped."
  (let ((db (simply-annotate--load-database)))
    (when db
      (delq nil
            (mapcar (lambda (entry)
                      (let ((key (car entry)))
                        (unless (simply-annotate--key-info-p key)
                          (let ((resolved (simply-annotate--resolve-key-path key)))
                            (when (file-name-absolute-p resolved)
                              (expand-file-name resolved))))))
                    db)))))

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
                                                simply-annotate-fringe-face)))
                (overlay-put ov 'before-string (propertize " " 'display fringe-spec))
                (overlay-put ov 'simply-annotate-dired t)
                (push ov simply-annotate-dired-overlays))))
          (forward-line 1))))))

(defun simply-annotate--dired-clear-marks ()
  "Remove all simply-annotate fringe overlays from the current dired buffer."
  (mapc #'delete-overlay simply-annotate-dired-overlays)
  (setq simply-annotate-dired-overlays nil))

(defvar simply-annotate-dired-mode-map
  (make-sparse-keymap)
  "Keymap for `simply-annotate-dired-mode'.
Populated at mode-enable time with the same prefix key that
`simply-annotate-command-map' is bound to globally, so that
annotation commands remain accessible in dired buffers where the
major-mode keymap may shadow the global binding (e.g. M-s).")

(defun simply-annotate--dired-setup-keymap ()
  "Mirror the global `simply-annotate-command-map' prefix in the dired keymap.
This ensures annotation commands are not shadowed by dired's own
local bindings on the same prefix."
  (setcdr simply-annotate-dired-mode-map nil)
  (when-let* ((keys (where-is-internal simply-annotate-command-map
                                       (list global-map))))
    (dolist (key keys)
      (define-key simply-annotate-dired-mode-map key simply-annotate-command-map))))

;;;###autoload
(define-minor-mode simply-annotate-dired-mode
  "Show fringe indicators in dired for files that have annotations."
  :lighter " SA-Dir"
  :keymap simply-annotate-dired-mode-map
  (if simply-annotate-dired-mode
      (progn
        (simply-annotate--dired-setup-keymap)
        (simply-annotate--dired-mark-annotated)
        (add-hook 'dired-after-readin-hook
                  #'simply-annotate--dired-mark-annotated nil t))
    (simply-annotate--dired-clear-marks)
    (remove-hook 'dired-after-readin-hook
                 #'simply-annotate--dired-mark-annotated t)))

(provide 'simply-annotate)
;;; simply-annotate.el ends here
