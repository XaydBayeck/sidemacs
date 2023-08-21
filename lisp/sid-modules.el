;;; sid-modules.el --- Load modules -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Sid
;;
;; Author: Sid <SidBayeck@outlook.com>
;; Maintainer: Sid <SidBayeck@outlook.com>
;; Created: 八月 19, 2023
;; Modified: 八月 19, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/sid/sid-modules
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Load modules
;;
;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "modules/"))

(require 'completion)
(require 'editor)
(require 'tools)
(require 'langs)

(provide 'sid-modules)
;;; sid-modules.el ends here
