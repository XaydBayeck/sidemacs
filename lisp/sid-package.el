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

(setopt use-package-always-defer t
	use-package-always-ensure t
	;; use-package-ensure-function 'quelpa
	use-package-expand-minimally t)

(use-package async
  :hook (package-refresh-contents . async-bytecomp-package-mode))

;; TODO: Replace to builtin `vc-use-package' if it exist.
;; (unless (package-installed-p 'vc-use-package)
;;   (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)
;; TODO: Use vc-use-package to manage vc-use-package
;; (use-package vc-use-package
;;   :vc (vc-use-package :fetcher github :repo "slotThe/vc-use-package"))

(provide 'sid-package)
;;; sid-package.el ends here
