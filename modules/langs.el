;;; langs.el --- Various program languages edit support configurations.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  SidBayeck

;; Author: SidBayeck <SidBayeck@outlook.com>
;; Keywords: languages

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

;; Various program languages edit support configurations.

;;; Code:

(require 'cl-lib)
(require 'const)

(defun add-prettify-symbols (symbols)
  "Generate a lambda to add SYMBOLS to `prettify-symbols-alist'."
  (lambda () (setq prettify-symbols-alist (append symbols basic-prettify-symbols))))

;;
;; (@* "Emacs Lisp" )
;;

(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(let ((elisp-add-symbols (add-prettify-symbols '(("defun" . ?⨐)
						 ("use-package" . ?📦)
						 ("nil" . ?∅)))))
  (cl-loop for hook in '(emacs-lisp-mode-hook ielm-mode-hook)
	   do (add-hook hook elisp-add-symbols)))

;; (add-hook 'emacs-lisp-mode-hook (add-prettify-symbols '(("defun" . ?⨐)
;; 							("use-package" . ?📦)
;; 							("nil" . ?∅))))

;;
;; (@* "C and C++" )
;;

(use-package c-ts-mode
  :hook (c-ts-mode . eglot-ensure)
  :config
  (add-hook 'c-ts-mode-hook (add-prettify-symbols '(("include" . ?💾)
						    ("NULL" . ?∅)))))

;;
;; (@* "Common lisp" )
;;

(use-package lisp-mode
  :ensure nil
  :config
  (load (expand-file-name "~/.roswell/helper.el"))
  (add-hook 'lisp-mode-hook (add-prettify-symbols '(("defun" . ?⨐)
						    ("nil" . ?∅)))))

(use-package sly
  :ensure t
  :config
  ;(setq inferior-lisp-program "ros -Q run")
  (add-hook 'sly-mrepl-hook (add-prettify-symbols '(("defun" . ?⨐)
						    ("nil" . ?∅)))))

;;
;; (@* "Scheme" )
;;

(use-package geiser-guile :ensure t)
;(use-package geiser-chez :ensure t :custom (geiser-chez-binary "chez"))
;(use-package geiser-racket)
;(use-package racket-mode
  ;:hook (racket-mode . 'racket-unicode-input-method-enable)
  ;:hook (racket-repl-mode . 'racket-unicode-input-method-enable))

;;
;; (@* "Haskell" )
;;

(use-package haskell-mode
  :ensure t
  :hook (haskell-mode . eglot-ensure)
  :config
  (setq-local eglot-connect-timeout 100))

(provide 'langs)
;;; langs.el ends here
