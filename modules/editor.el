;;; editor.el --- Editor support.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  SidBayeck

;; Author: SidBayeck <SidBayeck@outlook.com>
;; Keywords: tools

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

;; Editor support.

;;; Code:


(use-package meow
  :defer nil
  :commands
  (meow-mode
   meow-global-mode)
  :functions
  (meow-setup
   meow-motion-overwrite-define-key
   meow-leader-define-key
   meow-normal-define-key)
  :init
  (meow-global-mode)
  (require 'meow)
  (defun meow-setup ()
    "Setup meow mode keybindings."
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("l" . meow-right)
     '("h" . meow-left)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("/" . isearch-forward-regexp)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup))

;;
;; (@* "Auto insert")
;;

(use-package autoinsert
  :init
  (setq auto-insert-query nil)
  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  :hook (find-file . auto-insert)
  :bind (:map minibuffer-local-must-match-map
	      ("C-c C-c" . minibuffer-complete-and-exit)))
;;
;; (@* "Checkers" )
;;

(setq elisp-flymake-byte-compile-load-path
      (append elisp-flymake-byte-compile-load-path load-path))

(use-package flymake-posframe
  :load-path "modules/"
  :commands flymake-posframe-mode
  :hook (flymake-mode . flymake-posframe-mode)
  :custom-face
  (flymake-posframe-background-face ((t (:inherit error))))
  (flymake-posframe-foreground-face ((t (:inherit custom-invalid)))))
;; (flymake-posframe-background-face ((t (:inherit custom-invalid))))
;; (flymake-posframe-foreground-face ((t (:inherit custom-invalid)))))

;; 
;; (@* "Pair" )
;;

;; (defun editor-org/electric-pair-inhibit-predicate (&rest _char)
;;   "If the CHAR is after '_' or '^' it wont be paired."
;;   (not (or (eq ?_ (char-before))
;;       (eq ?^ (char-before)))))

;; (add-hook
;;  'org-mode-hook
;;  (lambda () (setopt electric-pair-inhibit-predicate
;; 	       (lambda (char) (or (editor-org/electric-pair-inhibit-predicate)
;; 			     (electric-pair-conservative-inhibit char))))))

(use-package centered-cursor-mode
  :bind (([remap screen-lock-mode] . centered-cursor-mode)))

;; 
;; (@* "Structure Editor" )
;;

;; (use-package fingertip
;;   :vc (:fetcher github :repo manateelazycat/fingertip)
;;   :commands fingertip-mode
;;   :init
;;   (dolist (hook (list
;; 		 'c-mode-common-hook
;; 		 'c-mode-hook
;; 		 'c++-mode-hook
;; 		 'haskell-mode-hook
;; 		 'emacs-lisp-mode-hook
;; 		 'lisp-interaction-mode-hook
;; 		 'lisp-mode-hook
;; 		 'ielm-mode-hook
;; 		 'sh-mode-hook
;; 		 'python-mode-hook
;; 		 'js-mode-hook
;; 		 'css-mode-hook
;; 		 'ruby-mode-hook
;; 		 'rust-mode-hook
;; 		 'rust-ts-mode-hook
;; 		 'web-mode-hook
;; 		 'markdown-mode-hook
;; 		 'conf-toml-mode-hook
;; 		 'typescript-mode-hook
;; 		 'c-ts-mode-hook
;; 		 'c++-ts-mode-hook
;; 		 'toml-ts-mode-hook
;; 		 'css-ts-mode-hook
;; 		 'js-ts-mode-hook
;; 		 'json-ts-mode-hook
;; 		 'python-ts-mode-hook
;; 		 'bash-ts-mode-hook
;; 		 'typescript-ts-mode-hook
;; 		 ))
;;     (add-hook hook #'(lambda ()
;; 		       (add-hook 'meow-insert-enter-hook #'(lambda () (fingertip-mode 1)))
;; 		       (add-hook 'meow-insert-exit-hook #'(lambda () (fingertip-mode -1))))))
;;   :bind (:map
;; 	 fingertip-mode-map
;; 	 ("(" . fingertip-open-round)
;; 	 ("[" . 'fingertip-open-bracket)
;; 	 ("{" . 'fingertip-open-curly)
;; 	 (")" . 'fingertip-close-round)
;; 	 ("]" . 'fingertip-close-bracket)
;; 	 ("}" . 'fingertip-close-curly)
;; 	 ("=" . 'fingertip-equal)
	 
	 ;; ("%" . 'fingertip-match-paren)
	 ;; ("\"" . 'fingertip-double-quote)
	 ;; ("'" . 'fingertip-single-quote)
	 
	 ;; ("SPC" . 'fingertip-space)
	 ;; ("RET" . 'fingertip-newline)
	 
	 ;; ("C-o" . 'fingertip-backward-delete)
	 ;; ("C-d" . 'fingertip-forward-delete)
	 ;; ("C-k" . 'fingertip-kill)
	 ;; ("C-a" . 'fingertip-wrap-round-pair)
	 ;; ("M-\"" . 'fingertip-wrap-double-quote)
	 ;; ("M-'" . 'fingertip-wrap-single-quote)
	 ;; ("M-[" . 'fingertip-wrap-bracket)
	 ;; ("M-{" . 'fingertip-wrap-curly)
	 ;; ("M-(" . 'fingertip-wrap-round)
	 ;; ("M-)" . 'fingertip-unwrap)
	 ;; ("M-]" . 'fingertip-fix-unbalanced-parentheses)
	 
	 ;; ("M-n" . 'fingertip-jump-right)
	 ;; ("M-p" . 'fingertip-jump-left)
	 ;; ("M-RET" . 'fingertip-jump-out-pair-and-newline)

	 ;; ("C-j" . 'fingertip-jump-up)))

;; 
;; (@* "Backup Directory" )
;;

(setq backup-directory-alist '("." . ".backups/"))

(provide 'editor)
;;; editor.el ends here
