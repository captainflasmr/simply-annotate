;;; init.el --- Batch/CI bootstrap for simply-annotate -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Loaded first by every Makefile target (and by CI) to set up a clean,
;; reproducible batch environment:
;;
;; - Points `package-user-dir' at a project-local .elpa so dependency
;;   installs never touch the developer's real package store.
;; - Activates installed packages so `transient' is found on Emacs
;;   versions that do not bundle it (< 28.1).  On 28.1+ the bundled
;;   `transient' is used and no install is needed.
;; - Puts the project root (and tests/) at the FRONT of `load-path' so
;;   the working copy always shadows any MELPA-installed simply-annotate.

;;; Code:

(require 'package)

(setq package-user-dir (expand-file-name ".elpa" default-directory))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

;; Working copy wins over any installed copy.
(add-to-list 'load-path (expand-file-name "tests" default-directory))
(add-to-list 'load-path (expand-file-name default-directory))

(provide 'init)
;;; init.el ends here
