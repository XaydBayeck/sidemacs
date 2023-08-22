;;; mail.el --- Read and edit E-mail.                -*- lexical-binding: t; -*-

;; Copyright (C) 2023  SidBayeck

;; Author: SidBayeck <SidBayeck@outlook.com>
;; Keywords: mail

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

;; Read and edit E-MAIL.

;;; Code:

(require 'auth-source)
(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg"))

(setq user-full-name "SidBayeck")
(setq user-login-name "SidBayeck")
(setq mail-host-address "outlook.com")
(setq user-mail-address (concat user-login-name "@" mail-host-address))

(require 'message)
;; (setq send-mail-function 'smtpmail-send-it) ; if you use `mail'
(setq message-send-mail-function 'smtpmail-send-it) ; if you use message/Gnus
(require 'smtpmail)
(setq smtpmail-smtp-server "smtp.office365.com"
      smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls)
;; Debug
(setq smtpmail-debug-info t) ; only to debug problems
(setq smtpmail-debug-verb t)


;; 
;; (@* "gnus" )
;;
(require 'gnus)
(setq gnus-select-method
      '(nnimap "outlook.com"
	       (nnimap-address "outlook.office365.com")
	       (nnimap-server-port 993)
	       (nnimap-stream-type tls)))

(provide 'mail)
;;; mail.el ends here
