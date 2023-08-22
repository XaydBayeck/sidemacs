;;; sid-dsp.el --- Customize display format -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Customize display format
;;
;;; Code:

;;
;; (@* "Modeline" )
;;

(use-package minions
  :ensure t
  :init
  (setq minions-mode-line-delimiters nil
           minions-mode-line-lighter ""))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode)
  :hook (doom-modeline-mode . column-number-mode)
  :init
  (when (>= emacs-major-version 29)
    (defface doom-modeline-buffer-modified
      '((t (:inherit (error bold) :background unspecified)))
      "Face used for the \\='unsaved\\=' symbol in the mode-line."
      :group 'doom-modeline-faces))

  (setq doom-modeline-bar-width 3
	doom-modeline-github nil
	doom-modeline-mu4e nil
	doom-modeline-persp-name nil
	doom-modeline-minor-modes nil
	doom-modeline-buffer-file-name-style 'relative-from-project
	doom-modeline-buffer-encoding 'nondefault))

;;
;; (@* "Themes")
;;

(use-package doom-themes
  :demand t
  :ensure t
  :commands
  (doom-themes-visual-bell-config
   doom-themes-treemacs-config
   doom-themes-org-config)
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
           doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tokyo-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setopt doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (setq-default line-spacing 2))

;;
;; (@* "Tab bar" )
;;

;;
;; (@* "Font set" )
;;

(when (display-graphic-p)
    ;; Set default font
    (set-face-attribute 'default
                        nil
                        :font "Hurmit Nerd Font Mono"
                        :height 120)

    ;; Fixed-pitch (monospaced) fonts 等宽字体
    (set-face-attribute 'fixed-pitch
                        nil
                        :font "Hurmit Nerd Font Mono"
                        :height 120)
    (set-face-attribute 'fixed-pitch-serif
			nil
			:font "Noto Serif")
    ;; Variable-pitch
    (set-face-attribute 'variable-pitch
                        nil
                        :font "Hurmit Nerd Font Mono"
                        :height 120)
    ;; CJK fonts
    (set-fontset-font t 'han (font-spec :family "霞鹜文楷等宽" :foundry "LXGW" :weight 'bold :slant 'normal :size 15))
    ;; TODO: Set JP font
    (set-fontset-font t 'kana (font-spec :family "Noto Serif CJK JP" :weight 'semi-bold :slant 'normal))
    ;;Unicode
    (set-fontset-font t 'unicode (font-spec :family "Symbola" :weight 'bold) nil 'prepend)
    (set-fontset-font t 'unicode (font-spec :family "JetBrainsMono Nerd Font" :weight 'bold) nil 'prepend)
    ;; Emoji
    (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

;; (use-package mixed-pitch :hook (tex-mode . mixed-pitch-mode))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

;;
;; (@* "Line number" )
;;

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setopt display-line-numbers-type 'relative)

(electric-pair-mode t)
(setopt electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(show-paren-mode t)

;;
;; (@* "Alpha background" )
;;

(set-frame-parameter nil 'alpha-background 96)

(setq fancy-splash-image (expand-file-name "~/Pictures/oldensignal.xpm"))

;;
;; (@* "Pretty" )
;;

(require 'const)

(setq-default prettify-symbols-alist basic-prettify-symbols)
	
(global-prettify-symbols-mode 1)


(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :hook (org-agenda-finalize . org-modern-agenda)
  :custom
  (org-modern-star '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))
  ;; REVIEW inhibit it in math enviroment.
  (org-pretty-entities t)
  ;; (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-image-actual-width '(800))
  (org-startup-with-inline-images t)
  (org-modern-block-fringe nil)
  (org-modern-block-name '("»" . "»"))
  (org-modern-keyword
   '(("title" . "§")
     ("author" . "")
     ("subtitle" . "‡")
     ("options" . "")
     ("html" . "")
     ("filetags" . "")
     (t . t)))
  :custom-face
  (org-document-title ((t (:height 2.0))))
  (org-level-1 ((t (:height 1.4))))
  (org-level-2 ((t (:height 1.3))))
  (org-level-3 ((t (:height 1.22))))
  (org-level-4 ((t (:height 1.16))))
  (org-level-5 ((t (:height 1.10))))
  (org-level-6 ((t (:height 1.06))))
  (org-level-7 ((t (:height 1.04))))
  (org-level-8 ((t (:height 1.02))))
  :hook (org-mode . (lambda ()
		      (setq-local line-spacing 2)
		      (visual-line-mode))))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  ;; (org-appear-autokeywords t)
  (org-appear-inside-latex t)
  )

;; (use-package visual-fill-column
;;   :custom (visual-fill-column-width 88)
;;   :hook (visual-line-mode . visual-fill-column-mode)
;;   :config (setq-default visual-fill-column-center-text t))

(use-package olivetti
  :hook
  (org-mode . olivetti-mode)
  (Custom-mode . olivetti-mode)
  (help-mode . olivetti-mode)
  (Info-mode . olivetti-mode)
  (olivetti-mode . visual-line-mode)
  :init
  (setq-default fill-column 74)
  :custom
  (olivetti-body-width nil)
  (olivetti-style 'fancy)
  :custom-face
  (olivetti-fringe ((t (:background "#121418")))))

;; 
;; (@* "Latex viewer" )
;; 

(use-package aio)
(use-package xenops
  :after aio
  :functions
  (eli/xenops-justify-fragment-overlay
   image-display-size)
  :config
  (plist-put org-format-latex-options :justify 'center)
  (defun eli/xenops-justify-fragment-overlay (element &rest _args)
    "Place xenops' overlays by keyword: `:justify' of `org-format-latex-options'"
    (let* ((ov-beg (plist-get element :begin))
           (ov-end (plist-get element :end))
           (ov (car (overlays-at (/ (+ ov-beg ov-end) 2) t)))
           (position (plist-get org-format-latex-options :justify))
           (inline-p (eq 'inline-math (plist-get element :type)))
           width offset)
      (when (and ov
                 (imagep (overlay-get ov 'display)))
        (setq width (car (image-display-size (overlay-get ov 'display))))
        (cond
         ((and (eq 'center position)
               (not inline-p))
          (setq offset (floor (- (/ fill-column 2)
                                 (/ width 2))))
          (if (< offset 0)
              (setq offset 0))
          (overlay-put ov 'before-string (make-string offset ? )))
         ((and (eq 'right position)
               (not inline-p))
          (setq offset (floor (- fill-column
                                 width)))
          (if (< offset 0)
              (setq offset 0))
          (overlay-put ov 'before-string (make-string offset ? )))))))
  (advice-add 'xenops-math-display-image :after
              #'eli/xenops-justify-fragment-overlay))

(provide 'sid-dsp)
;;; sid-dsp.el ends here
