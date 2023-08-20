;;; completion.el --- Code completion and operate -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Sid
;;
;; Author: Sid <SidBayeck@outlook.com>
;; Maintainer: Sid <SidBayeck@outlook.com>
;; Created: 八月 19, 2023
;; Modified: 八月 19, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/sid/completion
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Code completion and operate
;;
;;; Code:

;;
;; (@* "Corfu" )
;;

(use-package corfu
  :ensure t
  :custom
  (corfu-preview-current t)
  (corfu-auto t)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match t)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (completion-cycle-threshold 1)
  (tab-always-indent 'complete)
  :hook (prog-mode . global-corfu-mode)
  :bind
  (:map corfu-map
        ("M-SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("C-x C-k" . cape-dict)
        ("C-x C-f" . cape-file)
	("M-n" . corfu-popupinfo-scroll-up)
	("M-p" . corfu-popupinfo-scroll-down))
  :config
  (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
  (setq corfu-popupinfo-delay 0.1)

  ;; Dirty hack to get c completion running
  ;; Discussion in https://github.com/minad/corfu/issues/34
  ;; (when (equal tab-always-indent 'complete)
    ;; (define-key c-mode-base-map [remap c-indent-line-or-region] #'completion-at-point))

  (defun completion--org-return (orig)
    (if (and (featurep 'corfu)
             corfu-mode
             (>= corfu--index 0))
        (corfu-insert)
      (funcall orig)))
  (advice-add '+org/return :around #'completion--org-return))

(use-package kind-icon
  :demand t
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (let ((local-cache-dir (concat user-emacs-directory ".local/cache/")))
    (setq kind-icon-use-icons t
          svg-lib-icons-dir (expand-file-name "svg-lib" local-cache-dir)
          kind-icon-mapping
          '((array "a" :icon "code-brackets" :face font-lock-variable-name-face)
            (boolean "b" :icon "circle-half-full" :face font-lock-builtin-face)
            (class "c" :icon "view-grid-plus-outline" :face font-lock-type-face)
            (color "#" :icon "palette" :face success)
            (constant "co" :icon "pause-circle" :face font-lock-constant-face)
            (constructor "cn" :icon "table-column-plus-after" :face font-lock-function-name-face)
            (enum "e" :icon "format-list-bulleted-square" :face font-lock-builtin-face)
            (enum-member "em" :icon "format-list-checks" :face font-lock-builtin-face)
            (event "ev" :icon "lightning-bolt-outline" :face font-lock-warning-face)
            (field "fd" :icon "application-braces-outline" :face font-lock-variable-name-face)
            (file "f" :icon "file" :face font-lock-string-face)
            (folder "d" :icon "folder" :face font-lock-doc-face)
            (function "f" :icon "sigma" :face font-lock-function-name-face)
            (interface "if" :icon "video-input-component" :face font-lock-type-face)
            (keyword "kw" :icon "image-filter-center-focus" :face font-lock-keyword-face)
            (macro "mc" :icon "lambda" :face font-lock-keyword-face)
            (method "m" :icon "sigma" :face font-lock-function-name-face)
            (module "{" :icon "view-module" :face font-lock-preprocessor-face)
            (numeric "nu" :icon "numeric" :face font-lock-builtin-face)
            (operator "op" :icon "plus-circle-outline" :face font-lock-comment-delimiter-face)
            (param "pa" :icon "cog" :face default)
            (property "pr" :icon "tune-vertical" :face font-lock-variable-name-face)
            (reference "rf" :icon "bookmark-box-multiple" :face font-lock-variable-name-face)
            (snippet "S" :icon "text-short" :face font-lock-string-face)
            (string "s" :icon "sticker-text-outline" :face font-lock-string-face)
            (struct "%" :icon "code-braces" :face font-lock-variable-name-face)
            (t "." :icon "crosshairs-question" :face shadow)
            (text "tx" :icon "script-text-outline" :face shadow)
            (type-parameter "tp" :icon "format-list-bulleted-type" :face font-lock-type-face)
            (unit "u" :icon "ruler-square" :face shadow)
            (value "v" :icon "numeric-1-box-multiple-outline" :face font-lock-builtin-face)
            (variable "va" :icon "adjust" :face font-lock-variable-name-face))))
  (add-hook 'doom-load-theme-hook #'kind-icon-reset-cache)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :demand t
  :ensure t
  :bind (([remap dabbrev-expand] . cape-dabbrev)
	 ("C-c p p" . completion-at-point)
	 ("C-c p t" . complete-tag)
	 ("C-c p d" . cape-dabbrev)
	 ("C-c p h" . cape-history)
	 ("C-c p f" . cape-file)
	 ("C-c p k" . cape-keyword)
	 ("C-c p s" . cape-symbol)
	 ("C-c p a" . cape-abbrev)
	 ("C-c p l" . cape-line)
	 ("C-c p w" . cape-dict)
	 ("C-c p ^" . cape-text)
	 ("C-c p &" . cape-sgml)
	 ("C-c p r" . cape-rfc1345))
  :init
  ;; (add-hook 'latex-mode-hook (defun +corfu--latex-set-capfs ()
                                ;; (add-to-list 'completion-at-point-functions #'cape-tex)))
  ;; (when (modulep! :checkers spell)
    ;; (add-to-list 'completion-at-point-functions #'cape-dict)
    ;; (add-to-list 'completion-at-point-functions #'cape-ispell))
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t))


(use-package corfu-history
  :after corfu
  :hook (corfu-mode . (lambda ()
                        (corfu-history-mode 1)
                        (savehist-mode 1)
                        (add-to-list 'savehist-additional-variables 'corfu-history))))


(use-package corfu-quick
  :after corfu
  :bind (:map corfu-map
         ("M-q" . corfu-quick-complete)
         ("C-q" . corfu-quick-insert)))


;;
;; (@* "Vertico" )
;;

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun completion-crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'completion-crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  :bind (:map vertico-map
              ("M-RET" . vertico-exit-input)
              ("M-SPC" . +vertico/embark-preview)
              ("C-j" . vertico-next)
              ("C-M-j" . vertico-next-group)
              ("C-k" . vertico-previous)
              ("C-M-k" . vertico-previous-group)
              ("DEL" . vertico-directory-delete-char))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  (minibuffer-setup . vertico-repeat-save))

(use-package savehist :init (savehist-mode))

(use-package orderless
  :demand t
  :ensure t
  :init
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; remap bindings in `mode-specific-map'
	 ([remap bookmark-jump]                 . consult-bookmark)
         ;; ([remap evil-show-marks]               . consult-mark)
         ;; ([remap evil-show-jumps]               . +vertico/jump-list)
         ;; ([remap evil-show-registers]           . consult-register)
         ([remap goto-line]                     . consult-goto-line)
         ([remap imenu]                         . consult-imenu)
         ([remap Info-search]                   . consult-info)
         ([remap locate]                        . consult-locate)
         ([remap load-theme]                    . consult-theme)
         ([remap man]                           . consult-man)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap yank-pop]                      . consult-yank-pop)
         ([remap persp-switch-to-buffer]        . +vertico/switch-workspace-buffer)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; M-g bindings in `goto-map'
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '("M-SPC" :debounce 0.5 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ([remap describe-bindings] . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  :config
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;
;; (@* "Snippet" )
;;

;; Configure Tempel
(use-package tempel
  :ensure t
  ;; Require trigger prefix before template name when completing.
  :custom
  ;; (tempel-trigger-prefix "<")
  (tempel-path (concat user-emacs-directory "tempels"))

  :bind (("S-SPC" . tempel-complete) ;; Alternative tempel-expand
         ("S-RET" . tempel-insert)
	 :map tempel-map
	 ("M-]" . tempel-next)
	 ("M-[" . tempel-previous))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
  :ensure t
  :after temple)

(provide 'completion)
;;; completion.el ends here
