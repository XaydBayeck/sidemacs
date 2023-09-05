;;; sid-org.el --- Sid's org-mode configurations.    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  SidBayeck

;; Author: SidBayeck <SidBayeck@outlook.com>
;; Keywords: outlines

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

;; Sid's org-mode configurations.

;;; Code:

(use-package org
  :ensure nil
  :custom
  (org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "WAIT(w)"
				 "|" "DONE(d)" "CANCEL(c)" "STUCK(s)")))
  (org-agenda-files '("~/org/manager/"))
  (org-todo-keyword-faces '(("DOING" . (:foreground "yellow"))
			    ("CANCEL" . (:foreground "gray"))))
  (org-capture-templates '(("t" "Todo [inbox]" entry
			    (file+headline "~/org/manager/inbox.org" "Tasks")
			    "* TODO %i%?")
			   ("T" "Tickler" entry
			    (file+headline "~/org/manager/tickler.org" "Tickler")
			    "* %i%? \n %U")))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (calc . t)
     (python . t)
     ;; (julia . t)
     ;; (rust . t)
     (haskell . t)
     (ruby . t)
     (C . t)
     (gnuplot . t)
     (shell . t)
     (lisp . t)
     (scheme . t)
     (dot . t)
     (ditaa . t))))
;; 
;; (@* "Org 💜 Roam" )
;;

(use-package org-roam
  :hook (org-roam-mode . org-roam-db-autosync-mode)
  :custom
  (org-roam-directory (file-truename "~/org/roam")))

(use-package consult-org-roam
  :after (org-roam consult)
  :commands (consult-org-roam-mode)
  :functions
  consult--customize-put
  :defines
  consult-org-roam-forward-links
  :hook (after-init . consult-org-roam-mode)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r)
  (consult-org-roam-buffer-after-buffers t)
  :config
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-."))
  :bind
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n r" . consult-org-roam-search))

(provide 'sid-org)
;;; sid-org.el ends here
