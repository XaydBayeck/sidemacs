;;; sid-package.el --- Package Archive related -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Package Archive related
;;
;;; Code:

(require 'package)

;(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
        ;; ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
(setq package-archive-priorities
      '(("gnu"       . 0)
        ("nongnu" . 0)
        ;; ("jsc-elpa" 10)
        ("org"        . 0)
        ("melpa"   . 5)))

(setq package-enable-at-startup nil ; To avoid initialization twice
     package-check-signature nil)


(when noninteractive (package--archives-initialize))
(when (featurep 'esup-child) (package-activate-all))

;;
;; (@* "Packages" )
;;

(setq use-package-always-defer t
      use-package-expand-minimally t)

(use-package async
  :ensure t
  :init (async-bytecomp-package-mode 1))

(provide 'sid-package)
;;; sid-package.el ends here
