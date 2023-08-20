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
;; (@* "Version Control" )
;;

(use-package magit :ensure t)

;;
;; (@* "Tree-Sitter" )
;;

(use-package treesit
  :custom (treesit-font-lock-level 4))

(use-package treesit-auto
  :ensure t
  :demand t
  :hook (emacs-lisp-mode . (lambda () (treesit-parser-create 'elisp)))
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode)
  (add-to-list 'treesit-auto-recipe-list
	       (make-treesit-auto-recipe
		:lang 'elisp
		:ts-mode 'emacs-lisp-mode
		:url "https://github.com/Wilfred/tree-sitter-elisp")))

;;
;; (@* "rainbow brackets" )
;;

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;
;; (@* "File manager" )
;;

(use-package treemacs
  :ensure t
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
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(provide 'tools)
;;; tools.el ends here
