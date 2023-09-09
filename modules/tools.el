;;; tools.el --- Various tools for edit and manage   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  SidBayeck

;; Author: SidBayeck <SidBayeck@outlook.com>
;; Keywords: tools, vc

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Various tools for edit and manage

;;; Code:

;; 
;; (@* "Direnv" )
;;

(use-package direnv
  :hook (after-init . direnv-mode))

;;
;; (@* "Version Control" )
;;

(use-package magit)

;;
;; (@* "Tree-Sitter" )
;;

(use-package treesit
  :ensure nil
  :custom (treesit-font-lock-level 4))

(use-package treesit-auto
  :demand t
  :commands (global-treesit-auto-mode treesit-auto-mode)
  ;; TODO: Add emacs-lisp-ts-mode
  :hook (emacs-lisp-mode . (lambda () (treesit-parser-create 'elisp)))
  ;; TODO: Add haskell-ts-mode
  :hook (haskell-mode . (lambda () (treesit-parser-create 'haskell)))
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode)
  (add-to-list 'treesit-auto-recipe-list
	       (make-treesit-auto-recipe
		:lang 'elisp
		:ts-mode 'emacs-lisp-mode
		:url "https://github.com/Wilfred/tree-sitter-elisp"))
  (add-to-list 'treesit-auto-recipe-list
	       (make-treesit-auto-recipe
		:lang 'haskell
		:ts-mode 'haskell-mode
		:url "https://github.com/tree-sitter/tree-sitter-haskell")))

;;
;; (@* "rainbow brackets" )
;;

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;
;; (@* "Higlight TODO" )
;;

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-ts-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	'(;; For reminders to change or add something at a later date.
          ("TODO" warning bold)
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project.
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold))))


;;
;; (@* "File manager" )
;;

(use-package treemacs
  :commands (treemacs-follow-mode)
  :init
  (let ((cache-dir (concat user-emacs-directory ".local/cache/")))
    (setq treemacs-follow-after-init t
	  treemacs-is-never-other-window t
	  treemacs-sorting 'alphabetic-case-insensitive-asc
	  treemacs-persist-file (concat cache-dir "treemacs-persist")
	  treemacs-last-error-persist-file (concat cache-dir "treemacs-last-error-persist")))
  :bind (:map global-map
	      ("M-o s" . treemacs-select-window)
	      ("M-o l" . treemacs-delete-other-windows)
	      ("M-o t" . treemacs)
	      ("M-o d" . treemacs-select-directory)
	      ("M-o B" . treemacs-bookmark)
	      ("M-o f" . treemacs-find-file)
	      ("M-o T" . treemacs-find-tag))
  :config
  ;; Do't follow the cursor
  (treemacs-follow-mode -1))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;;
;; (@* "Popper" )
;;

(use-package popper
  :commands (popper-mode popper-echo-mode)
  :bind (("C-'" . popper-toggle-latest)
	 ("M-'" . popper-cycle)
	 ("C-M-'" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
	'("\\*Message\\*"
	  "Output\\*$"
	  "\\*Async Shell Command\\*"
	  "\\*Warnings\\*"
	  ;; "\\*Buffer List\\*"
	  Buffer-menu-mode
	  help-mode
	  compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;;
;; (@* "Document" )
;;

(use-package eldoc-box
  :bind ("M-h" . eldoc-box-help-at-point))

;;
;; (@* "Dictionary and Translater" )
;;

;; DONE: Replace to builtin `vc-use-package' if it exist.
(use-package immersive-translate
 :vc (:fetcher github :repo "Elilif/emacs-immersive-translate")
 :custom (immersive-translate-backend 'trans))

(provide 'tools)
;;; tools.el ends here
