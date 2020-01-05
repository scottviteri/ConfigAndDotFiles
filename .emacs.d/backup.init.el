;import elpa
(require 'package)
(add-to-list 'package-archives
    '("melpa-unstable" . "https://melpa.org/packages/")
    '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(use-package evil
  :ensure t
  :init
    (setq evil-want-abbrev-expand-on-insert-exit nil
          evil-want-keybinding nil
          evil-want-integration t)
  :config
    (evil-mode 1)
    (define-key evil-normal-state-map "\M-." nil)
    (define-key evil-motion-state-map "T" nil)
    (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
  :pin melpa-unstable
)


(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)))

(add-to-list 'load-path "~/LocalSoftware/lean-3.4.2-linux")
(use-package lean-mode
  :ensure t)
(use-package helm-lean
  :ensure t)

;; Python

(global-prettify-symbols-mode 1)

(use-package python
    :mode ("\\.py\\'" . python-mode)
    :interpreter ("python3" . python-mode)
)

(let ((python-symbols-list '(lambda ()
        (mapc (lambda (pair) (push pair prettify-symbols-alist))
            '(("def" . "𝒇")
             ("class" . "𝑪")
             ("and" . "∧")
             ("or" . "∨")
             ("in" . "∈")
             ("not in" . "∉")
             ("return" . "⟼")
             ("yield" . "⟻")
             ("for" . "∀")
             ("!=" . "≠")
             (">=" . "≥")
             ("<=" . "≤")
             ("=" . "≝")
             ;; Base Types
             ("int" .      #x2124)
             ("float" .    #x211d)
             ("str" .      #x1d54a)
             ("True" .     #x1d54b)
             ("False" .    #x1d53d))))))
  (use-package python-mode
    :config (setq python-shell-completion-native-enable nil)
    :hook python-symbols-list
  ))


(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package powerline
    :ensure t
    :config (powerline-center-evil-theme)
)

(use-package markdown-mode
    :ensure t
    :init (setq markdown-command "pandoc")
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
)

(use-package org
    :ensure t
    :init
        (org-babel-do-load-languages
         'org-babel-load-languages
         '(
           (C . t)
           (shell . t)
           (python . t)
           (scheme . t)
           (perl . t)
           (org . t)
           (lisp . t)
           (calc . t)
           (haskell . t)
           (gnuplot . t)
           (js . t)
           (ocaml . t)
           (gnuplot . t)
           (lilypond . t)
           (latex . t)
          )
        )
)

(use-package org-bullets
  :ensure t
  :config
    (setq org-highlight-latex-and-related '(latex))
    (setq org-bullets-bullet-list '("∙"))
    (add-hook 'org-mode-hook 'org-bullets-mode))

;(define-key org-mode-map (kbd "C-c C-c")
;    (lambda () (interactive) (org-ctrl-c-ctrl-c)
;                             (org-display-inline-images)))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))

(use-package geiser
    :ensure t
    :mode ("\\.scm\\'" . scheme-mode)
    :config
        (add-hook 'scheme-mode-hook 'turn-on-geiser-mode)
        (add-hook 'scheme-mode-hook 'enable-paredit-mode))

;;; (add-to-list 'load-path "~/.emacs.d/mu/mu4e")
;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;(require 'mu4e)
;(setq mu4e-maildir (expand-file-name "~/Maildir/Gmail")
;      mu4e-drafts-folder "/[Gmail].Drafts"
;      mu4e-sent-folder   "/[Gmail].Sent Mail"
;      mu4e-trash-folder  "/[Gmail].Trash"
;      ;;allow for updating mail using 'U' in the main view:
;      mu4e-get-mail-command "offlineimap"
;      mu4e-compose-signature-auto-include nil
;      mu4e-compose-signature ""
;      mu4e-headers-sort-field :date
;      mu4e-headers-sort-direction 'descending
;      user-mail-address "scottviteri@gmail.com"
;      user-full-name "Scott Viteri"
;)
;
;
;(use-package smtpmail
;    :ensure t
;    :init
;;;(require 'smtpmail)
;        (setq message-send-mail-function 'smtpmail-send-it
;              starttls-use-gnutls t
;              smtpmail-starttls-credentials
;              '(("smtp.gmail.com" 587 nil nil))
;              smtpmail-auth-credentials
;              (expand-file-name "~/.authinfo.gpg")
;              smtpmail-default-smtp-server "smtp.gmail.com"
;              smtpmail-smtp-server "smtp.gmail.com"
;              smtpmail-smtp-service 587 ;              smtpmail-debug-info t) ;)
;

;(add-to-list 'load-path "/home/scottviteri/.emacs.d/evil-collection/")
(use-package evil-collection
  :ensure t
  :config (evil-collection-init))

;;(with-eval-after-load 'mu4e (require 'evil-collection-mu4e) (evil-collection-mu4e-setup))
;(add-hook 'mu4e (prog1 (require 'evil-collection-mu4e) (evil-collection-mu4e-setup)))

(add-to-list 'load-path "/home/scottviteri/.emacs.d/agda-mode/")

(use-package org-mime
    :ensure t
    :config (setq org-mime-library 'mml)
)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-to-list 'flycheck-checkers 'proselint)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq-default flycheck-highlighting-mode 'lines)
  ;; Define fringe indicator / warning levels
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-info))

(flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (text-mode markdown-mode gfm-mode org-mode))

(use-package writegood-mode
    :ensure t
    :bind ("C-c g" . writegood-mode)
    :config
        (add-to-list 'writegood-weasel-words "actionable")
)
(use-package gnuplot
    :ensure t
)
(use-package haskell-mode
    :ensure t
    :config
        (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
)

(use-package eshell
    :ensure t
    :config
        (add-hook 'eshell-mode-hook '(lambda () (setq pcomplete-cycle-completions nil)))
        (setq company-tabnine--disabled 1)
        (company-mode))
; '(eshell-cmpl-compare-entry-function (quote string-lessp))

(use-package irony
  :ensure t
  :config
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c++-mode-hook 'irony-eldoc)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-eldoc)
    (add-hook 'objc-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-eldoc)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
)

(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
    (use-package company-irony :ensure t :defer t)
    (use-package company-tabnine :ensure t)
    (use-package company-lean :ensure t)
    (add-to-list 'company-backends #'company-tabnine)
    (setq company-idle-delay 0)
    (setq company-show-numbers t)
    (global-set-key (kbd "S-SPC") #'company-complete))


(use-package proof-general
    :ensure t)

(use-package company-coq
    :ensure t
    :config
        (setq proof-splash-seen t)
        (add-hook 'coq-mode-hook 'company-coq-mode)
    :pin melpa-unstable)


; (load "/home/scottviteri/opam-coq/coq-8.8/share/emacs/site-lisp/tuareg-site-file")
; (load "/home/scottviteri/opam-coq/coq-8.8.source/share/emacs/site-lisp/tuareg-site-file")

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
  (autoload 'merlin-mode "merlin" nil t nil)
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(browse-url-browser-function (quote w3m))
 '(custom-enabled-themes (quote (manoj-dark)))
 '(custom-safe-themes
   (quote
    ("d1cc05d755d5a21a31bced25bed40f85d8677e69c73ca365628ce8024827c9e3" default)))
 '(eshell-cmpl-compare-entry-function (quote string-lessp))
 '(fci-rule-color "#383838")
 '(geiser-default-implementation nil)
 '(haskell-interactive-popup-errors nil)
 '(lean-message-boxes-enabled-captions
   (quote
    ("check result" "eval result" "print result" "reduce result" "trace output")))
 '(lean-rootdir "/home/scottviteri/LocalSoftware/lean-3.4.2-linux")
 '(make-backup-files nil)
 '(org-babel-python-command "python3")
 '(org-confirm-babel-evaluate nil)
 '(package-selected-packages
   (quote
    (z3-mode helm-lean evil-goggles extempore-mode gnuplot-mode geiser writegood-mode company company-coq company-irony company-irony-c-headers company-lean company-math evil-collection evil-org gnuplot haskell-mode helm-w3m irony irony-eldoc lean-mode math-symbol-lists org-bullets org-mime paredit proof-general eshell-did-you-mean eshell-fixed-prompt flylisp evil-paredit flymake flycheck flycheck-irony company-mode irony-mode esh-autosuggest esh-help pdf-tools w3m ac-haskell-process flymake-hlint iedit merlin-eldoc auto-complete merlin tuareg markdown-mode powerline magit use-package evil)))
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
