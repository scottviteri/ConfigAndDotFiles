
(require 'package)
(add-to-list 'package-archives
    '("melpa-unstable" . "https://melpa.org/packages/")
    '("melpa-stable" . "https://stable.melpa.org/packages/")
)

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
)

(eval-after-load "evil-maps"
  (define-key evil-normal-state-map "\M-." nil))

(use-package company-coq
    :ensure t
    :config
        (defvar proof-splash-seen)
        (setq proof-splash-seen t)
        (add-hook 'coq-mode-hook 'company-coq-mode)
)

;; Python

(global-prettify-symbols-mode 1)

(defvar python-symbols-list)
(setq python-symbols-list '(lambda ()
        (mapc (lambda (pair) (push pair prettify-symbols-alist))
            '(("def" . "𝒇")
             ("class" . "𝑪")
             ("and" . "∧")
             ("or" . "∨")
             ("not" . "￢")
             ("in" . "∈")
             ("not in" . "∉")
             ("return" . "⟼")
             ("yield" . "⟻")
             ("for" . "∀")
             ("!=" . "≠")
             ("==" . "＝")
             (">=" . "≥")
             ("<=" . "≤")
             ("=" . "≝")
             ;; Base Types
             ("int" .      #x2124)
             ("float" .    #x211d)
             ("str" .      #x1d54a)
             ("True" .     #x1d54b)
             ("False" .    #x1d53d)))))

(use-package python-mode
    :hook python-symbols-list
)
(use-package python
    :mode ("\\.py\\'" . python-mode)
    :interpreter ("python3" . 'python-mode)
)
; (add-hook 'python-mode-hook python-symbols-list)
(setq python-shell-completion-native-enable nil)


(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5    ; and how many of the old
)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package magit
    :ensure t)

(use-package tex
    :ensure auctex)

(use-package latex-preview-pane
    :ensure t)

;(use-package hydra
;    :ensure t
;)

(use-package ivy :demand
  :ensure t
  :init (setq ivy-initial-inputs-alist nil)
  :diminish ivy-mode
  :config
    (ivy-mode t)
    (setq ivy-use-virtual-buffers t
          ivy-count-format "%d/%d "))

(use-package powerline
  :ensure t
  :config (powerline-center-evil-theme))

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
           (ditaa . t)
           (coq . t)
          )
        )
)

(use-package org-bullets
  :ensure t
  :config                                     (setq org-highlight-latex-and-related '(latex))
  (setq org-bullets-bullet-list '("∙"))
  (add-hook 'org-mode-hook 'org-bullets-mode))(define-key org-mode-map (kbd "C-c C-c")
  (lambda () (interactive) (org-ctrl-c-ctrl-c)
                           (org-display-inline-images)))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package geiser
    :ensure t
    :config
        (add-to-list 'auto-mode-alist '("\\.scm\\'" . lisp-mode)
        (add-hook 'lisp-mode-hook 'turn-on-geiser-mode)
))


;;; (add-to-list 'load-path "~/.emacs.d/mu/mu4e")
;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;(require 'mu3e)
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
;              smtpmail-smtp-service 587
;              smtpmail-debug-info t)
;)
;

;; could potentially init all of evil collection

;;(require 'evil-collection-mu4e) ; would do (init-evil-collection) or something like that after
;;(with-eval-after-load 'mu4e (require 'evil-collection-mu4e) (evil-collection-mu4e-setup))
(use-package evil-collection
    :after evil
    :ensure t
    :config
        ;(evil-collection-init)
       (evil-collection-init 'info)
       (evil-collection-init 'company)
       (evil-collection-init 'eshell)
       (evil-collection-init 'geiser)
       (evil-collection-init 'elisp-mode)
       (evil-collection-init 'eww)
       (evil-collection-init 'flycheck)
       (evil-collection-init 'help)
       (evil-collection-init 'image)
       (evil-collection-init 'ivy)
       (evil-collection-init 'magit)
       (evil-collection-init 'mu4e)
       (evil-collection-init 'python)
       (evil-collection-init 'comint)
       (evil-collection-init 'w3m)
       (evil-collection-init 'package-menu)
       ;(add-hook 'mu4e (prog1 (require 'evil-collection-mu4e) (evil-collection-mu4e-setup)))
)

(use-package org-mime
    :ensure t
    :config (setq org-mime-library 'mml)
)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-to-list 'flycheck-checkers 'proselint)
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
        (add-to-list 'writegood-weasel-words "actionable"))

(use-package gnuplot
    :ensure t)

(use-package haskell-mode
    :ensure t
    :config
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (add-hook 'haskell-mode-hook 'haskell-indentation-mode))


(use-package paredit
  :ensure t
  :init
  (show-paren-mode 1)
  :bind-keymap ("C-d" . 'paredit-forward-delete)
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
                                        ;(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  )

(add-hook 'paredit-mode-hook 'evil-paredit-mode)
;(add-hook 'emacs-lisp-mode-hook                   'evil-paredit-mode)
;(add-hook 'ielm-mode-hook                         'evil-paredit-mode)
;(add-hook 'lisp-mode-hook                         'evil-paredit-mode)
;(add-hook 'lisp-interaction-mode-hook             'evil-paredit-mode)
;(add-hook 'scheme-mode-hook                       'evil-paredit-mode)

(add-hook 'paredit-mode-hook #'evil-cleverparens-mode)
;(add-hook 'js-mode-hook #'evil-cleverparens-mode)
;(add-hook 'ielm-mode-hook             #'evil-cleverparens-mode)
;(add-hook 'lisp-mode-hook             #'evil-cleverparens-mode)
;(add-hook 'lisp-interaction-mode-hook #'evil-cleverparens-mode)
;(add-hook 'scheme-mode-hook           #'evil-cleverparens-mode)


(require 'js-comint)
(defun inferior-js-mode-hook-setup ()
  "JS mode comint hook"
  (add-hook 'comint-output-filter-functions 'js-comint-process-output))
(add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)

(global-set-key (kbd "S-SPC") #'company-complete)
(add-hook 'after-init-hook 'global-company-mode)

(add-to-list 'auto-mode-alist '("\\.info\\'" . Info-mode))
(load "/home/scottviteri/opam-coq/coq-8.8.source/share/emacs/site-lisp/tuareg-site-file")

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
  (autoload 'merlin-mode "merlin" nil t nil)
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(custom-enabled-themes (quote (manoj-dark)))
 '(custom-safe-themes
   (quote
    ("d1cc05d755d5a21a31bced25bed40f85d8677e69c73ca365628ce8024827c9e3" default)))
 '(display-buffer-alist (quote (("*evil-collection*" display-buffer-same-window))))
 '(fci-rule-color "#383838")
 '(haskell-interactive-popup-errors nil)
 '(org-babel-python-command "python3")
 '(org-confirm-babel-evaluate nil)
 '(org-html-html5-fancy t)
 '(package-selected-packages
   (quote
    (company-lean helm helm-lean extempore-mode pandoc geiser evil-org org-bullets gnuplot writegood-mode company-coq latex-preview-pane org-mime evil-collection evil-paredit evil-cleverparens org-pomodoro js-comint htmlize python-mode proof-general ac-haskell-process flymake-hlint iedit merlin-eldoc auto-complete merlin tuareg lean-mode markdown-mode powerline magit use-package ivy hydra evil)))
 '(send-mail-function (quote smtpmail-send-it))
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))