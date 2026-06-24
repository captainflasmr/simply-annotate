;;; simply-annotate-tests.el --- Tests for simply-annotate -*- lexical-binding: t; -*-

;; Author: James Dyer <captainflasmr@gmail.com>
;; URL: https://github.com/captainflasmr/simply-annotate

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;;; Commentary:

;; ERT test suite for `simply-annotate'.  Run in batch with:
;;
;;   make test
;;
;; or directly:
;;
;;   emacs -Q --batch -L . -l ert -l tests/simply-annotate-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Tests are batch-safe: nothing here requires a window system or user
;; interaction.  File-touching tests are isolated to temporary files (and
;; a temporary HOME for the tilde-expansion test), so they never read or
;; write the user's real annotation database.

;;; Code:

(require 'ert)
(require 'simply-annotate)

;;; Helpers

(ert-deftest sa-test-timestamp-format ()
  "Timestamps are ISO-8601 \"YYYY-MM-DDThh:mm:ss\"."
  (should (string-match-p
           "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\'"
           (simply-annotate--timestamp))))

(ert-deftest sa-test-thread-p ()
  "`--thread-p' distinguishes threads from strings and plain alists."
  (should-not (simply-annotate--thread-p "a plain string"))
  (should (simply-annotate--thread-p '((id . "x") (comments ((text . "c"))))))
  (should-not (simply-annotate--thread-p '((foo . "bar")))))

(ert-deftest sa-test-annotation-text ()
  "`--annotation-text' returns the string or the first comment text."
  (should (string= (simply-annotate--annotation-text "plain") "plain"))
  (should (string= (simply-annotate--annotation-text
                    '((id . "x") (comments ((text . "first comment")))))
                   "first comment")))

(ert-deftest sa-test-annotation-summary-string ()
  "`--annotation-summary' returns a bare string unchanged."
  (should (string= (simply-annotate--annotation-summary "hello") "hello")))

(ert-deftest sa-test-key-info-p ()
  "Info-node keys are recognised; file paths are not."
  (should (simply-annotate--key-info-p "(emacs) Glossary"))
  (should-not (simply-annotate--key-info-p "/home/user/file.el"))
  (should-not (simply-annotate--key-info-p "relative/file.el")))

(ert-deftest sa-test-strip-boilerplate ()
  "`--strip-boilerplate' removes box-drawing/header lines and gutter bars."
  (should (string= (simply-annotate--strip-boilerplate
                    "┌─────\n│ some text\n└─────")
                   "some text"))
  (should (string= (simply-annotate--strip-boilerplate nil) ""))
  (should (string= (simply-annotate--strip-boilerplate "plain") "plain")))

;;; Default status / priority (issue #9)

(ert-deftest sa-test-default-status-and-priority ()
  "Defaults come from the head of the configured lists."
  (let ((simply-annotate-thread-statuses '("inbox" "doing"))
        (simply-annotate-priority-levels '("normal" "high")))
    (should (string= (simply-annotate--default-status) "inbox"))
    (should (string= (simply-annotate--default-priority) "normal")))
  ;; Empty lists fall back to the historical literals.
  (let ((simply-annotate-thread-statuses nil)
        (simply-annotate-priority-levels nil))
    (should (string= (simply-annotate--default-status) "open"))
    (should (string= (simply-annotate--default-priority) "normal"))))

(ert-deftest sa-test-default-list-heads ()
  "Stock defaults keep open/normal as the first entries (issue #9 reorder)."
  (should (string= (car (default-value 'simply-annotate-thread-statuses)) "open"))
  (should (string= (car (default-value 'simply-annotate-priority-levels)) "normal")))

;;; Thread management

(ert-deftest sa-test-create-thread-defaults ()
  "A new thread carries an id, one comment, and configured defaults."
  (let ((simply-annotate-thread-statuses '("inbox" "doing" "done"))
        (simply-annotate-priority-levels '("normal" "low" "high"))
        (simply-annotate-default-author "Tester")
        (simply-annotate-prompt-for-author nil))
    (let ((th (simply-annotate--create-thread "hello")))
      (should (simply-annotate--thread-p th))
      (should (string= (alist-get 'status th) "inbox"))
      (should (string= (alist-get 'priority th) "normal"))
      (should (alist-get 'id th))
      (should (= 1 (length (alist-get 'comments th))))
      (should (string= (alist-get 'text (car (alist-get 'comments th))) "hello"))
      (should (string= (alist-get 'author (car (alist-get 'comments th))) "Tester")))))

(ert-deftest sa-test-create-thread-explicit-priority ()
  "An explicit priority argument overrides the default."
  (let ((simply-annotate-default-author "Tester")
        (simply-annotate-prompt-for-author nil))
    (let ((th (simply-annotate--create-thread "x" nil "critical")))
      (should (string= (alist-get 'priority th) "critical")))))

(ert-deftest sa-test-add-reply-roots-to-first-comment ()
  "A reply with no explicit parent attaches to the root comment."
  (let ((simply-annotate-default-author "Tester")
        (simply-annotate-prompt-for-author nil))
    (let* ((th (simply-annotate--create-thread "root"))
           (root-id (alist-get 'id (car (alist-get 'comments th)))))
      (simply-annotate--add-reply th "a reply" "Bob")
      (let* ((comments (alist-get 'comments th))
             (reply (cadr comments)))
        (should (= 2 (length comments)))
        (should (string= (alist-get 'text reply) "a reply"))
        (should (string= (alist-get 'author reply) "Bob"))
        (should (string= (alist-get 'parent-id reply) root-id))))))

(ert-deftest sa-test-add-reply-nested ()
  "An explicit parent id nests the reply under that comment."
  (let ((simply-annotate-default-author "Tester")
        (simply-annotate-prompt-for-author nil))
    (let* ((th (simply-annotate--create-thread "root"))
           (root-id (alist-get 'id (car (alist-get 'comments th)))))
      (simply-annotate--add-reply th "child" "Bob")
      (let ((child-id (alist-get 'id (cadr (alist-get 'comments th)))))
        (simply-annotate--add-reply th "grandchild" "Eve" child-id)
        (let ((grandchild (caddr (alist-get 'comments th))))
          (should (string= (alist-get 'parent-id grandchild) child-id))
          (should-not (string= child-id root-id)))))))

(ert-deftest sa-test-ensure-comment-ids-migrates-legacy ()
  "Legacy comments without ids gain ids and inferred parent links."
  ;; Built with `copy-tree' so the mutating `--ensure-comment-ids' does not
  ;; corrupt a shared quoted literal.
  (let ((thread (copy-tree
                 '((id . "t")
                   (comments ((author . "A") (text . "c") (type . "comment"))
                             ((author . "B") (text . "r") (type . "reply")))))))
    (simply-annotate--ensure-comment-ids thread)
    (let* ((comments (alist-get 'comments thread))
           (c1 (car comments))
           (c2 (cadr comments)))
      (should (alist-get 'id c1))
      (should (alist-get 'id c2))
      ;; The reply is reparented onto the first root comment.
      (should (string= (alist-get 'parent-id c2) (alist-get 'id c1))))))

(ert-deftest sa-test-build-comment-tree-nesting ()
  "`--build-comment-tree' reconstructs hierarchy from parent-id links."
  (let* ((comments '(((id . "1") (parent-id . nil) (text . "root"))
                     ((id . "2") (parent-id . "1") (text . "child"))
                     ((id . "3") (parent-id . "2") (text . "grandchild"))
                     ((id . "4") (parent-id . "1") (text . "child2"))))
         (tree (simply-annotate--build-comment-tree comments)))
    (should (= 1 (length tree)))
    (let* ((root (car tree))
           (root-children (cdr root)))
      (should (string= (alist-get 'text (car root)) "root"))
      (should (= 2 (length root-children)))
      (let ((child (car root-children)))
        (should (string= (alist-get 'text (car child)) "child"))
        (should (= 1 (length (cdr child))))
        (should (string= (alist-get 'text (caar (cdr child))) "grandchild"))))))

(ert-deftest sa-test-set-thread-property-validates ()
  "`--set-thread-property' only accepts values from the allowed set."
  (let ((th (list (cons 'status "open"))))
    (should (simply-annotate--set-thread-property
             th 'status "resolved" '("open" "resolved")))
    (should (string= (alist-get 'status th) "resolved"))
    (should-not (simply-annotate--set-thread-property
                 th 'status "bogus" '("open" "resolved")))
    (should (string= (alist-get 'status th) "resolved"))))

(ert-deftest sa-test-thread-tag-add-remove ()
  "Tags add idempotently and remove cleanly."
  (let ((th (list (cons 'tags nil))))
    (simply-annotate--add-thread-tag th "bug")
    (should (member "bug" (alist-get 'tags th)))
    (simply-annotate--add-thread-tag th "bug")
    (should (= 1 (length (alist-get 'tags th))))
    (simply-annotate--remove-thread-tag th "bug")
    (should-not (member "bug" (alist-get 'tags th)))))

(ert-deftest sa-test-collect-all-tags-sorted-unique ()
  "`--collect-all-tags' returns a sorted, de-duplicated list."
  (let ((db (list (cons "a.el"
                        (list (list (cons 'text '((id . "1")
                                                  (comments ((text . "x")))
                                                  (tags "zeta" "alpha"))))))
                  (cons "b.el"
                        (list (list (cons 'text '((id . "2")
                                                  (comments ((text . "y")))
                                                  (tags "alpha" "mu")))))))))
    (should (equal (simply-annotate--collect-all-tags db) '("alpha" "mu" "zeta")))))

(ert-deftest sa-test-filter-db-by-tag ()
  "`--filter-db-by-tag' keeps only entries with a matching tag."
  (let* ((db (list (cons "a.el"
                         (list (list (cons 'text '((id . "1")
                                                   (comments ((text . "x")))
                                                   (tags "bug"))))))
                   (cons "b.el"
                         (list (list (cons 'text '((id . "2")
                                                   (comments ((text . "y")))
                                                   (tags "feature"))))))))
         (filtered (simply-annotate--filter-db-by-tag db "bug")))
    (should (= 1 (length filtered)))
    (should (assoc "a.el" filtered))
    (should-not (assoc "b.el" filtered))))

;;; Database operations

(ert-deftest sa-test-database-save-read-roundtrip ()
  "Saving then reading the database returns an equal structure."
  (let* ((tmp (make-temp-file "sa-test-db" nil ".el"))
         (simply-annotate-file tmp)
         (simply-annotate-database-strategy 'global)
         (db '(("a.el" "note1") ("b.el" "note2"))))
    (unwind-protect
        (progn
          (simply-annotate--save-database db)
          (should (equal (simply-annotate--read-db tmp) db)))
      (delete-file tmp))))

(ert-deftest sa-test-update-database-add-and-remove ()
  "`--update-database' adds an entry and removes it when annotations are nil."
  (let* ((tmp (make-temp-file "sa-test-db" nil ".el"))
         (simply-annotate-file tmp)
         (simply-annotate-database-strategy 'global))
    (unwind-protect
        (progn
          (simply-annotate--update-database "a.el" '("note"))
          (should (equal (alist-get "a.el" (simply-annotate--load-database)
                                    nil nil #'string=)
                         '("note")))
          (simply-annotate--update-database "a.el" nil)
          (should-not (alist-get "a.el" (simply-annotate--load-database)
                                 nil nil #'string=)))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest sa-test-merge-databases-primary-wins ()
  "`--merge-databases' keeps primary entries on key conflicts."
  (let* ((primary '(("a" . 1) ("b" . 2)))
         (secondary '(("b" . 99) ("c" . 3)))
         (merged (simply-annotate--merge-databases primary secondary)))
    (should (equal (alist-get "a" merged nil nil #'string=) 1))
    (should (equal (alist-get "b" merged nil nil #'string=) 2))
    (should (equal (alist-get "c" merged nil nil #'string=) 3))))

(ert-deftest sa-test-database-tilde-path-expanded ()
  "Tilde paths in `simply-annotate-file' are expanded (issue #10).
Runs against a throwaway HOME so the real home directory is untouched."
  (let* ((home (make-temp-file "sa-test-home" t))
         (process-environment (cons (concat "HOME=" home) process-environment))
         (simply-annotate-file "~/annotations.el")
         (simply-annotate-database-strategy 'global))
    (unwind-protect
        (progn
          (should (string= (simply-annotate--database-path)
                           (expand-file-name "annotations.el" home)))
          (simply-annotate--save-database '(("x.el" "n")))
          (should (file-exists-p (expand-file-name "annotations.el" home)))
          (should (equal (simply-annotate--read-db "~/annotations.el")
                         '(("x.el" "n")))))
      (delete-directory home t))))

(ert-deftest sa-test-both-strategy-no-global-pollution ()
  "Saving under the `both' strategy must not copy global entries into
the project-local database (critical bug C1).  Uses a transient project
so the active store resolves to the project file."
  (require 'project)
  (let* ((root (file-name-as-directory (make-temp-file "sa-test-proj" t)))
         (default-directory root)
         (project-find-functions (list (lambda (_dir) (cons 'transient root))))
         (simply-annotate-database-strategy 'both)
         (simply-annotate-project-file ".simply-annotations.el")
         (global (make-temp-file "sa-test-global" nil ".el"))
         (simply-annotate-file global)
         (proj-path (expand-file-name ".simply-annotations.el" root)))
    (unwind-protect
        (progn
          ;; Global store holds an unrelated entry.
          (with-temp-file global
            (prin1 '(("/some/other/abs/file.el" "global note")) (current-buffer)))
          ;; Sanity: the active store for this context is the project file.
          (should (string= (simply-annotate--database-path) proj-path))
          ;; Save an annotation for a project-relative key.
          (simply-annotate--update-database "rel/file.el" '("project note"))
          (let ((proj-db (simply-annotate--read-db proj-path)))
            (should (equal (alist-get "rel/file.el" proj-db nil nil #'string=)
                           '("project note")))
            ;; The global entry must NOT have leaked into the project file.
            (should-not (assoc "/some/other/abs/file.el" proj-db))))
      (when (file-exists-p global) (delete-file global))
      (delete-directory root t))))

(ert-deftest sa-test-projects-database-file ()
  "`--projects-database-file' resolves the backing store for a table row."
  (let* ((root (file-name-as-directory (make-temp-file "sa-test-proj" t)))
         (simply-annotate-project-file ".simply-annotations.el")
         (global (make-temp-file "sa-test-global" nil ".el"))
         (simply-annotate-file global)
         (proj-file (expand-file-name ".simply-annotations.el" root)))
    (unwind-protect
        (progn
          ;; The pseudo-root and nil resolve to the global database.
          (should (string= (simply-annotate--projects-database-file "(no project)")
                           (expand-file-name global)))
          (should (string= (simply-annotate--projects-database-file nil)
                           (expand-file-name global)))
          ;; A project with no per-project file falls back to global.
          (should (string= (simply-annotate--projects-database-file root)
                           (expand-file-name global)))
          ;; A project with a per-project file resolves to that file.
          (with-temp-file proj-file (insert "()"))
          (should (string= (simply-annotate--projects-database-file root)
                           proj-file)))
      (when (file-exists-p global) (delete-file global))
      (delete-directory root t))))

(ert-deftest sa-test-save-annotations-survives-write-failure ()
  "A failed database write in `--save-annotations' must not signal (issue I1).
On `before-save-hook' a propagated error would abort the user's buffer save."
  (with-temp-buffer
    (insert "some content here")
    (setq-local simply-annotate-mode t)
    (let ((simply-annotate-display-style 'highlight)
          (simply-annotate-overlays nil)
          (simply-annotate-database-strategy 'global)
          ;; Unwritable: the parent directories do not exist.
          (simply-annotate-file (expand-file-name
                                 (format "sa-test-nodir-%d/sub/db.el" (random 1000000))
                                 temporary-file-directory)))
      (push (simply-annotate--create-overlay 1 5 "note") simply-annotate-overlays)
      ;; Returns normally despite the write failing (no signal escapes).
      (simply-annotate--save-annotations)
      ;; And the unwritable path was indeed not created.
      (should-not (file-exists-p simply-annotate-file)))))

;;; Overlay management

(ert-deftest sa-test-create-overlay-sets-properties ()
  "`--create-overlay' records the annotation text and span."
  (with-temp-buffer
    (insert "some content here")
    (let ((simply-annotate-display-style 'highlight))
      (let ((ov (simply-annotate--create-overlay 1 5 "my note")))
        (should (overlayp ov))
        (should (string= (overlay-get ov 'simply-annotation) "my note"))
        (should (= (overlay-start ov) 1))
        (should (= (overlay-end ov) 5))))))

(ert-deftest sa-test-clear-all-overlays ()
  "`--clear-all-overlays' empties the list and the buffer."
  (with-temp-buffer
    (insert "abcdefgh")
    (let ((simply-annotate-display-style 'highlight)
          (simply-annotate-overlays nil))
      (push (simply-annotate--create-overlay 1 3 "a") simply-annotate-overlays)
      (push (simply-annotate--create-overlay 4 6 "b") simply-annotate-overlays)
      (simply-annotate--clear-all-overlays)
      (should (null simply-annotate-overlays))
      (should (= 0 (length (overlays-in (point-min) (point-max))))))))

(ert-deftest sa-test-remove-overlay-cleans-aux ()
  "Removing a text-style overlay also deletes its auxiliary overlays (issue #7)."
  (with-temp-buffer
    (insert "line one\nline two\n")
    (let ((simply-annotate-display-style 'bracket)
          (simply-annotate-overlays nil))
      (let ((ov (simply-annotate--create-overlay 1 9 "note")))
        ;; The bracket style attaches indicator overlays as a property.
        (should (overlay-get ov 'simply-annotate-bracket-text-overlays))
        (simply-annotate--remove-overlay ov)
        ;; No orphaned indicator overlays remain.
        (should (= 0 (length (overlays-in (point-min) (point-max)))))))))

;;; Hide done/closed threads (issue #11)

(ert-deftest sa-test-overlay-status ()
  "`--overlay-status' returns the thread status or the configured default."
  (let ((simply-annotate-thread-statuses '("open" "resolved" "closed")))
    (with-temp-buffer
      (insert "some text")
      (let ((simply-annotate-display-style 'highlight)
            (simply-annotate-overlays nil))
        (let ((ov-thread (simply-annotate--create-overlay
                          1 5 '((id . "t")
                                (status . "closed")
                                (priority . "normal")
                                (comments ((text . "x"))))))
              (ov-bare (simply-annotate--create-overlay 1 5 "a plain note")))
          (should (string= (simply-annotate--overlay-status ov-thread) "closed"))
          ;; A bare-string annotation has no status; it falls back to the default.
          (should (string= (simply-annotate--overlay-status ov-bare) "open")))))))

(ert-deftest sa-test-overlay-hidden-by-status-p ()
  "`--overlay-hidden-by-status-p' honours the switch and the status list."
  (let ((simply-annotate-thread-statuses '("open" "resolved" "closed")))
    (with-temp-buffer
      (insert "some text")
      (let ((simply-annotate-display-style 'highlight)
            (simply-annotate-overlays nil)
            (simply-annotate-hide-done t)
            (simply-annotate-hide-done-statuses '("resolved" "closed")))
        (let ((ov-closed (simply-annotate--create-overlay
                          1 5 '((id . "t")
                                (status . "closed")
                                (priority . "normal")
                                (comments ((text . "x"))))))
              (ov-open (simply-annotate--create-overlay
                        1 5 '((id . "u")
                              (status . "open")
                              (priority . "normal")
                              (comments ((text . "y"))))))
              (ov-bare (simply-annotate--create-overlay 1 5 "plain")))
          (should (simply-annotate--overlay-hidden-by-status-p ov-closed))
          (should-not (simply-annotate--overlay-hidden-by-status-p ov-open))
          ;; Bare strings resolve to the default status "open", which is not hidden.
          (should-not (simply-annotate--overlay-hidden-by-status-p ov-bare))
          ;; Disabling the switch hides nothing.
          (let ((simply-annotate-hide-done nil))
            (should-not (simply-annotate--overlay-hidden-by-status-p ov-closed)))
          ;; Empty status list hides nothing.
          (let ((simply-annotate-hide-done-statuses nil))
            (should-not (simply-annotate--overlay-hidden-by-status-p ov-closed))))))))

(ert-deftest sa-test-overlay-visible-p-combines-filters ()
  "`--overlay-visible-p' requires both tag match and status visibility."
  (with-temp-buffer
    (insert "some text")
    (let ((simply-annotate-display-style 'highlight)
          (simply-annotate-overlays nil)
          (simply-annotate-current-tag-filter nil)
          (simply-annotate-hide-done t)
          (simply-annotate-hide-done-statuses '("closed")))
      (let ((ov (simply-annotate--create-overlay
                 1 5 '((id . "t")
                       (status . "closed")
                       (priority . "normal")
                       (tags "review")
                       (comments ((text . "x")))))))
        ;; No tag filter, but status is hidden -> not visible.
        (should-not (simply-annotate--overlay-visible-p ov))
        ;; Status visible again -> visible.
        (let ((simply-annotate-hide-done nil))
          (should (simply-annotate--overlay-visible-p ov)))
        ;; Tag filter that does not match -> not visible even when status visible.
        (let ((simply-annotate-hide-done nil)
              (simply-annotate-current-tag-filter "other"))
          (should-not (simply-annotate--overlay-visible-p ov)))))))

(ert-deftest sa-test-active-overlays-excludes-hidden ()
  "`--active-overlays' skips threads hidden by status (issue #11)."
  (with-temp-buffer
    (insert "alpha beta gamma")
    (let ((simply-annotate-display-style 'highlight)
          (simply-annotate-overlays nil)
          (simply-annotate-hide-done t)
          (simply-annotate-hide-done-statuses '("closed")))
      (push (simply-annotate--create-overlay
             1 5 '((id . "t1")
                   (status . "open")
                   (priority . "normal")
                   (comments ((text . "open one")))))
            simply-annotate-overlays)
      (push (simply-annotate--create-overlay
             6 10 '((id . "t2")
                    (status . "closed")
                    (priority . "normal")
                    (comments ((text . "closed one")))))
            simply-annotate-overlays)
      (should (= 2 (length simply-annotate-overlays)))
      (should (= 1 (length (simply-annotate--active-overlays))))
      ;; Disabling the switch brings both back.
      (let ((simply-annotate-hide-done nil))
        (should (= 2 (length (simply-annotate--active-overlays))))))))

(ert-deftest sa-test-apply-display-style-full-hide ()
  "A hidden thread gets no visual cue in `full' hide mode (issue #11)."
  (with-temp-buffer
    (insert "some content here")
    (let ((simply-annotate-display-style 'bracket)
          (simply-annotate-inline t)
          (simply-annotate-overlays nil)
          (simply-annotate-hide-done t)
          (simply-annotate-hide-done-statuses '("closed"))
          (simply-annotate-hide-done-style 'full))
      (let ((ov (simply-annotate--create-overlay
                 1 5 '((id . "t")
                       (status . "closed")
                       (priority . "normal")
                       (comments ((text . "done here")))))))
        (simply-annotate--apply-display-style ov)
        (should-not (overlay-get ov 'face))
        (should-not (overlay-get ov 'before-string))
        (should-not (overlay-get ov 'after-string))))))

(ert-deftest sa-test-apply-display-style-indicator ()
  "A hidden thread keeps a fringe indicator in `indicator' mode (issue #11)."
  (with-temp-buffer
    (insert "some content here")
    (let ((simply-annotate-display-style 'bracket)
          (simply-annotate-inline t)
          (simply-annotate-overlays nil)
          (simply-annotate-hide-done t)
          (simply-annotate-hide-done-statuses '("closed"))
          (simply-annotate-hide-done-style 'indicator))
      (let ((ov (simply-annotate--create-overlay
                 1 5 '((id . "t")
                       (status . "closed")
                       (priority . "normal")
                       (comments ((text . "done here")))))))
        (simply-annotate--apply-display-style ov)
        ;; Inline text and face are suppressed, but a fringe indicator remains.
        (should-not (overlay-get ov 'face))
        (should-not (overlay-get ov 'after-string))
        (let ((before (overlay-get ov 'before-string)))
          (should before)
          ;; The before-string carries a left-fringe display spec.
          (should (eq (car-safe (get-text-property 0 'display before)) 'left-fringe)))))))

(ert-deftest sa-test-toggle-hide-done-flips-and-renders ()
  "`simply-annotate-toggle-hide-done' flips the switch and re-renders."
  (with-temp-buffer
    (insert "some content here")
    (let ((simply-annotate-display-style 'highlight)
          (simply-annotate-inline t)
          (simply-annotate-overlays nil)
          (simply-annotate-hide-done t)
          (simply-annotate-hide-done-statuses '("closed"))
          (simply-annotate-hide-done-style 'full)
          (simply-annotate-original-header-line nil))
      (let ((ov (simply-annotate--create-overlay
                 1 5 '((id . "t")
                       (status . "closed")
                       (priority . "normal")
                       (comments ((text . "done")))))))
        (push ov simply-annotate-overlays)
        ;; Toggling off re-renders: the closed thread is now shown (highlight face).
        (simply-annotate-toggle-hide-done)
        (should-not simply-annotate-hide-done)
        (should (eq (overlay-get ov 'face) 'simply-annotate-highlight-face))
        ;; Toggling back on hides it again.
        (simply-annotate-toggle-hide-done)
        (should simply-annotate-hide-done)
        (should-not (overlay-get ov 'face))
        (should-not (overlay-get ov 'after-string))))))

;;; Inline collapse/expand (issue #12)

(ert-deftest sa-test-inline-collapse-key-thread ()
  "`--inline-collapse-key' returns the thread id for thread annotations."
  (with-temp-buffer
    (insert "some text")
    (let ((simply-annotate-display-style 'highlight)
          (simply-annotate-overlays nil))
      (let ((ov (simply-annotate--create-overlay
                 1 5 '((id . "thread-abc")
                       (status . "open")
                       (priority . "normal")
                       (comments ((text . "x")))))))
        (should (string= (simply-annotate--inline-collapse-key ov) "thread-abc"))))))

(ert-deftest sa-test-inline-collapse-key-bare ()
  "`--inline-collapse-key' returns a stable fallback for bare strings."
  (with-temp-buffer
    (insert "some text")
    (let ((simply-annotate-display-style 'highlight)
          (simply-annotate-overlays nil))
      (let ((ov (simply-annotate--create-overlay 1 5 "a plain note")))
        (let ((key (simply-annotate--inline-collapse-key ov)))
          (should (string-prefix-p "bare-1-" key)))))))

(ert-deftest sa-test-inline-expanded-p-default ()
  "Threads are expanded by default; toggling collapses them (issue #12)."
  (with-temp-buffer
    (insert "some text")
    (let ((simply-annotate-display-style 'highlight)
          (simply-annotate-overlays nil)
          (simply-annotate-inline-expanded t)
          (simply-annotate-inline-toggled-threads nil))
      (let ((ov (simply-annotate--create-overlay
                 1 5 '((id . "t1")
                       (status . "open")
                       (priority . "normal")
                       (comments ((text . "x")))))))
        (should (simply-annotate--inline-expanded-p ov))
        ;; Toggle: now collapsed.
        (let ((simply-annotate-inline-toggled-threads (make-hash-table :test 'equal)))
          (puthash "t1" t simply-annotate-inline-toggled-threads)
          (should-not (simply-annotate--inline-expanded-p ov))
          ;; Flip the global default: toggled thread is now expanded again.
          (let ((simply-annotate-inline-expanded nil))
            (should (simply-annotate--inline-expanded-p ov))))))))

(ert-deftest sa-test-inline-expanded-p-global-collapsed ()
  "When global default is collapsed, non-toggled threads are collapsed."
  (with-temp-buffer
    (insert "some text")
    (let ((simply-annotate-display-style 'highlight)
          (simply-annotate-overlays nil)
          (simply-annotate-inline-expanded nil)
          (simply-annotate-inline-toggled-threads nil))
      (let ((ov (simply-annotate--create-overlay
                 1 5 '((id . "t1")
                       (status . "open")
                       (priority . "normal")
                       (comments ((text . "x")))))))
        (should-not (simply-annotate--inline-expanded-p ov))
        ;; Toggle: now expanded.
        (let ((simply-annotate-inline-toggled-threads (make-hash-table :test 'equal)))
          (puthash "t1" t simply-annotate-inline-toggled-threads)
          (should (simply-annotate--inline-expanded-p ov)))))))

(ert-deftest sa-test-inline-text-collapsed-render ()
  "A collapsed thread renders a header-only box with no body lines (issue #12)."
  (with-temp-buffer
    (insert "some content here")
    (let ((simply-annotate-display-style 'highlight)
          (simply-annotate-inline t)
          (simply-annotate-overlays nil)
          (simply-annotate-inline-expanded t)
          (simply-annotate-inline-toggled-threads
           (make-hash-table :test 'equal)))
      (let ((ov (simply-annotate--create-overlay
                 1 5 '((id . "t1")
                       (status . "open")
                       (priority . "normal")
                       (comments ((text . "first") (author . "A"))
                                 ((text . "second") (author . "B")))))))
        (puthash "t1" t simply-annotate-inline-toggled-threads)
        (let ((text (simply-annotate--inline-text ov)))
          (should (string-match-p "┌─" text))
          (should (string-match-p "└" text))
          ;; Collapsed form has a comment-count hint.
          (should (string-match-p "2 comments" text))
          ;; No body line with │ prefix (only header and footer).
          (should-not (string-match-p "^│ [^─]" text)))))))

(ert-deftest sa-test-inline-text-expanded-render ()
  "An expanded thread renders the full body (issue #12)."
  (with-temp-buffer
    (insert "some content here")
    (let ((simply-annotate-display-style 'highlight)
          (simply-annotate-inline t)
          (simply-annotate-overlays nil)
          (simply-annotate-inline-expanded t)
          (simply-annotate-inline-toggled-threads nil))
      (let ((ov (simply-annotate--create-overlay
                 1 5 '((id . "t1")
                       (status . "open")
                       (priority . "normal")
                       (comments ((text . "the body text")))))))
        (let ((text (simply-annotate--inline-text ov)))
          (should (string-match-p "┌─" text))
          (should (string-match-p "└" text))
          ;; Body is present.
          (should (string-match-p "the body text" text)))))))

(ert-deftest sa-test-toggle-inline-collapse-roundtrip ()
  "`simply-annotate-toggle-inline-collapse' flips state and re-renders (issue #12)."
  (with-temp-buffer
    (insert "some content here")
    (let ((simply-annotate-display-style 'highlight)
          (simply-annotate-inline t)
          (simply-annotate-overlays nil)
          (simply-annotate-inline-expanded t)
          (simply-annotate-inline-toggled-threads nil)
          (simply-annotate-original-header-line nil))
      (let ((ov (simply-annotate--create-overlay
                 1 5 '((id . "t1")
                       (status . "open")
                       (priority . "normal")
                       (comments ((text . "body")))))))
        (push ov simply-annotate-overlays)
        (goto-char 1)
        ;; Initially expanded.
        (should (simply-annotate--inline-expanded-p ov))
        ;; Toggle to collapsed.
        (simply-annotate-toggle-inline-collapse)
        (should-not (simply-annotate--inline-expanded-p ov))
        ;; Toggle back to expanded.
        (simply-annotate-toggle-inline-collapse)
        (should (simply-annotate--inline-expanded-p ov))))))

(ert-deftest sa-test-serialize-deserialize-roundtrip ()
  "Annotations survive a serialize/clear/deserialize cycle intact."
  (with-temp-buffer
    (insert "alpha beta gamma\nsecond line here\n")
    (let ((simply-annotate-display-style 'highlight)
          (simply-annotate-overlays nil))
      (push (simply-annotate--create-overlay 1 6 "note-a") simply-annotate-overlays)
      (push (simply-annotate--create-overlay 7 11 "note-b") simply-annotate-overlays)
      (let ((serialized (simply-annotate--serialize-annotations)))
        (should (= 2 (length serialized)))
        ;; Each serialized entry carries a stale-detection hash.
        (should (cl-every (lambda (a) (alist-get 'text-hash a)) serialized))
        (simply-annotate--clear-all-overlays)
        (should (null simply-annotate-overlays))
        (simply-annotate--deserialize-annotations serialized)
        (should (= 2 (length simply-annotate-overlays)))
        (let* ((sorted (sort (copy-sequence simply-annotate-overlays)
                             (lambda (a b) (< (overlay-start a) (overlay-start b)))))
               (texts (mapcar (lambda (o) (overlay-get o 'simply-annotation)) sorted)))
          (should (equal texts '("note-a" "note-b")))
          (should (= (overlay-start (car sorted)) 1))
          (should (= (overlay-end (car sorted)) 6)))))))

(ert-deftest sa-test-stale-detection-on-edit ()
  "An annotation whose text changed and cannot be relocated is marked stale."
  (with-temp-buffer
    (insert "hello world foobar")
    (let ((simply-annotate-display-style 'highlight)
          (simply-annotate-overlays nil))
      (push (simply-annotate--create-overlay 1 6 "x") simply-annotate-overlays)
      (let ((serialized (simply-annotate--serialize-annotations)))
        (simply-annotate--clear-all-overlays)
        (erase-buffer)
        (insert "XXXXX world foobar")    ; "hello" gone, not findable
        (simply-annotate--deserialize-annotations serialized)
        (let ((ov (car simply-annotate-overlays)))
          (should ov)
          (should (eq (overlay-get ov 'simply-annotate-stale) t)))))))

(ert-deftest sa-test-relocation-on-move ()
  "An annotation whose text moved is relocated via its stored context."
  (with-temp-buffer
    (insert "needle in haystack")
    (let ((simply-annotate-display-style 'highlight)
          (simply-annotate-overlays nil))
      (push (simply-annotate--create-overlay 1 7 "n") simply-annotate-overlays) ; "needle"
      (let ((serialized (simply-annotate--serialize-annotations)))
        (simply-annotate--clear-all-overlays)
        (erase-buffer)
        (insert "prefix added needle in haystack") ; "needle" shifted right
        (simply-annotate--deserialize-annotations serialized)
        (let ((ov (car simply-annotate-overlays)))
          (should ov)
          (should (overlay-get ov 'simply-annotate-relocated))
          (should (string= (buffer-substring-no-properties
                            (overlay-start ov) (overlay-end ov))
                           "needle")))))))

;;; Kanban grouping and counts (issue #9)

(ert-deftest sa-test-kanban-group-custom-status-visible ()
  "A new card with a custom default status lands in the first column."
  (let* ((simply-annotate-thread-statuses '("inbox" "doing" "done" "closed"))
         (simply-annotate-default-author "Tester")
         (simply-annotate-prompt-for-author nil)
         (th (simply-annotate--create-thread "card text"))
         (db (list (cons "foo.el"
                         (list (list (cons 'start 1) (cons 'text th))))))
         (groups (simply-annotate--kanban-group-by-status
                  db simply-annotate-thread-statuses)))
    (should (= 1 (length (cdr (assoc "inbox" groups)))))
    (should (= 0 (length (cdr (assoc "doing" groups)))))))

(ert-deftest sa-test-kanban-group-legacy-no-status ()
  "Threads without a status field and plain strings fall to the first column."
  (let* ((simply-annotate-thread-statuses '("inbox" "doing" "done"))
         (thread-no-status '((id . "t1") (comments ((text . "legacy")))))
         (db (list (cons "foo.el"
                         (list (list (cons 'start 1) (cons 'text thread-no-status))
                               (list (cons 'start 2) (cons 'text "plain string"))))))
         (groups (simply-annotate--kanban-group-by-status
                  db simply-annotate-thread-statuses)))
    (should (= 2 (length (cdr (assoc "inbox" groups)))))))

(ert-deftest sa-test-count-statuses ()
  "`--count-statuses' tallies per-status, defaulting missing statuses."
  (let* ((simply-annotate-thread-statuses '("inbox" "doing" "done"))
         (db (list (cons "foo.el"
                         (list (list (cons 'start 1)
                                     (cons 'text '((id . "a") (status . "doing")
                                                   (comments ((text . "x"))))))
                               (list (cons 'start 2) (cons 'text "plain"))))))
         (counts (simply-annotate--count-statuses db simply-annotate-thread-statuses)))
    (should (= 1 (gethash "doing" counts)))
    (should (= 1 (gethash "inbox" counts)))   ; plain string -> first status
    (should (= 0 (gethash "done" counts)))))

;;; Mode / hooks

(ert-deftest sa-test-info-hook-not-registered-in-file-buffers ()
  "Enabling the mode in a non-Info buffer must not touch the global
`Info-selection-hook' (issue I2)."
  (require 'info)
  (let ((saved (default-value 'Info-selection-hook))
        (simply-annotate-database-strategy 'global)
        (simply-annotate-file (expand-file-name
                               (format "sa-test-%d.el" (random 1000000))
                               temporary-file-directory)))
    (set-default 'Info-selection-hook nil)
    (unwind-protect
        (with-temp-buffer
          (insert "plain file buffer")
          (simply-annotate-mode 1)
          (should-not (memq #'simply-annotate--info-selection-hook
                            (default-value 'Info-selection-hook)))
          (simply-annotate-mode -1)
          (should-not (memq #'simply-annotate--info-selection-hook
                            (default-value 'Info-selection-hook))))
      (set-default 'Info-selection-hook saved))))

;;; Keymaps (issues #8, general wiring)

(ert-deftest sa-test-command-map-bindings ()
  "The command map exposes the expected core bindings."
  (should (keymapp simply-annotate-command-map))
  (should (eq (lookup-key simply-annotate-command-map "j") 'simply-annotate-smart-action))
  (should (eq (lookup-key simply-annotate-command-map (kbd "SPC")) 'simply-annotate-menu))
  (should (eq (lookup-key simply-annotate-command-map "d")
              'simply-annotate-toggle-hide-done))
  (should (eq (lookup-key simply-annotate-command-map "c")
              'simply-annotate-toggle-inline-collapse)))

(ert-deftest sa-test-repeat-map-wiring ()
  "The repeat map binds the repeatable commands and tags them (issue #8)."
  (should (keymapp simply-annotate-repeat-map))
  (should (eq (lookup-key simply-annotate-repeat-map "n") 'simply-annotate-next))
  (should (eq (lookup-key simply-annotate-repeat-map "]")
              'simply-annotate-cycle-tag-forward))
  (should (eq (lookup-key simply-annotate-repeat-map "d")
              'simply-annotate-toggle-hide-done))
  (should (eq (lookup-key simply-annotate-repeat-map "c")
              'simply-annotate-toggle-inline-collapse))
  (dolist (cmd '(simply-annotate-next
                 simply-annotate-previous
                 simply-annotate-cycle-display-style
                 simply-annotate-toggle-inline
                 simply-annotate-toggle-hide-done
                 simply-annotate-toggle-inline-collapse
                 simply-annotate-cycle-tag-backward
                 simply-annotate-cycle-tag-forward
                 simply-annotate-update-display-style))
    (should (eq (get cmd 'repeat-map) 'simply-annotate-repeat-map))))

(provide 'simply-annotate-tests)
;;; simply-annotate-tests.el ends here
