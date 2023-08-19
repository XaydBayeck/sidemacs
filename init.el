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
  "Sidemacs version")

(defun sidemacs-version ()
  "Show sidemacs version info."
  (interactive)
  (message "Sidemacs %s" sidemacs-version))

;;
;; (@* "Load Core" )
;;

;; proxy
(setq url-proxy-services '(("http" . "127.0.0.1:7890")
                           ("https" . "127.0.0.1:7890")))

(setq load-path
      (append (mapcar
               (lambda (dir) (concat user-emacs-directory dir))
               '("lisp/"
                 "lisp/lib/"))
              load-path))

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
