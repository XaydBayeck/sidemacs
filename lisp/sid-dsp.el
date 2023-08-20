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
	doom-modeline-major-mode-icon nil
	doom-modeline-buffer-file-name-style 'relative-from-project
	doom-modeline-buffer-encoding 'nondefault))

;;
;; (@* "Themes")
;;

(use-package doom-themes
  :demand t
  :ensure t
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
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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

    ;; CJK fonts
    (set-fontset-font t 'han (font-spec :family "Noto Serif CJK SC" :weight 'semi-bold :slant 'normal))
    (set-fontset-font t 'kana (font-spec :family "Noto Serif CJK JP" :weight 'semi-bold :slant 'normal))
    ;; Emoji
    (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

;;
;; (@* "Line number" )
;;

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(electric-pair-mode t)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(show-paren-mode t)

(provide 'sid-dsp)
;;; sid-dsp.el ends here
