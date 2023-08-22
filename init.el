;;; init.el --- Sid's configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Sid

;; Author: Sid <SidBayeck@outlook.com>
;; Maintainer: Sid <SidBayeck@outlook.com>
;; URL: https://github.com/XaydBayeck/sidemacs
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: lisp config

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Sid's configuration
;;
;;
;;                      ════╦╦╦╦╗
;;                  ╔═══════╩╩╩╩╩═════╗
;;                ══╝ ╔═════════════╗ ║
;;          ════════════════════════╬═╬══╗
;;           \   ╔══╗ ╓  ╥ ╥  ╥     ║ ╚══╬════
;;            ║  ║ ═╦ ║\ ║ ║  ║     ╚════╬════
;;            ║  ╚══╝ ╨ `╜ ╚══╝          ║
;;            ║   ╔══ ╔╗╔╗ ╔═╗ ╔═╕ ╔═╕   ║
;;            ║   ╠═  ║╙╜║ ╟─╢ ║   ╚═╗   ║
;;            ║   ╚══ ╨  ╨ ╨ ╨ ╚═╛ ╘═╝   ║
;;              \═══════════╣  ╠════════/
;;                 ═════════╝  ║
;;                 ════════════╝
;;
;;                [S I D - E M A C S]
;;

;;; Code:

;;
;; (@* "Startup" )
;;

(when (version< emacs-version "29.1")
  (error "This requires Emacs 29.1 and above!"))

(when (featurep 'esup-child)
  (setq gc-cons-threshold most-positive-fixnum))

;;
;; (@* "Version" )
;;

(defconst sidemacs-version "1.0.0"
  "Sidemacs version.")

(defun sidemacs-version ()
  "Show sidemacs version info."
  (interactive)
  (message "Sidemacs %s" sidemacs-version))

;;
;; (@* "Load Core" )
;;

;; proxy
(setopt url-proxy-services '(("http" . "127.0.0.1:7890")
                           ("https" . "127.0.0.1:7890")))

;; load-path
(setq load-path
      (append (mapcar
               (lambda (dir) (concat user-emacs-directory dir))
               '("lisp/"
                 "lisp/lib/"))
              load-path))

;; exec path
;; (setq path-separator " ")
;; (defun set-exec-path-from-shell-PATH ()
;;   "Set up Emacs' variable `exec-path' and PATH environment variable to match
;; that used by the user's shell.

;; This is particularly useful under Mac OS X and macOS, where GUI
;; apps are not started from a shell."
;;   (interactive)
;;   (let ((path-from-shell (replace-regexp-in-string
;; 			  "[ \t\n]*$" "" (shell-command-to-string
;; 					  "$SHELL --login -c 'echo $PATH'"
;; 						    ))))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (set-exec-path-from-shell-PATH)
(setenv "PATH"
	(concat
	 "/home/sid/.cabal/bin" path-separator
	 "/home/sid/.ghcup/bin" path-separator
	 (getenv "PATH")))
(setq exec-path (split-string (getenv "PATH") path-separator))

;;; Initialize
(require 'sid-package)

;;; Utilities
;;; Enviroment
;;; Standardize
(require 'sid-dsp)
(require 'sid-modules)
;;; Others
;;; Custom

;; (provide 'init)
;; local Variables
;; coding: utf-8
;; not-byte-compiled t
;; End
;;; init.el ends here
