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

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

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
    ;(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
    (define-key evil-window-map "h" 'evil-window-left)
    (define-key evil-window-map "j" 'evil-window-down)
    (define-key evil-window-map "k" 'evil-window-up)
    (define-key evil-window-map "l" 'evil-window-right)
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)))
;(global-set-key "\M-." 'helm-etags-select)

(add-to-list 'load-path "/home/scottviteri/LocalSoftware/lean")
(use-package lean-mode :ensure t)
(use-package helm-lean :ensure t)

;; Python

(global-prettify-symbols-mode 1)
(defun add-python-tags ()
  (setq-local tags-table-list '("/home/scottviteri/.local/lib/python3.8/site-packages/pysmt"
                                "/home/scottviteri/LocalSoftware/Python-3.8.6/Lib")))
(use-package python
    :mode ("\\.py\\'" . python-mode)
    :interpreter ("python3" . python-mode)
    :config (add-hook 'python-mode-hook 'add-python-tags))
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
    :hook python-symbols-list))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; end Python

(use-package yasnippet
  :ensure t
  :config (use-package yasnippet-snippets :ensure t)
   (yas-reload-all))

(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)

(use-package emms
  :ensure t
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all)
  (setq emms-seek-seconds 5)
  (setq emms-player-list '(emms-player-mpd))
  (setq emms-info-functions '(emms-info-mpd))
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6602"))
(setq mpc-host "localhost:6602")

(defun mpd/start-music-daemon ()
  "Start MPD, connects to it and syncs the metadata cache."
  (interactive)
  (shell-command "mpd")
  (mpd/update-database)
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "MPD Started!"))

(defun mpd/kill-music-daemon ()
  "Stops playback and kill the music daemon."
  (interactive)
  (emms-stop)
  (call-process "killall" nil nil nil "mpd")
  (message "MPD Killed!"))

(defun mpd/update-database ()
  "Updates the MPD database synchronously."
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "MPD Database Updated!"))

(global-set-key (kbd "M-p") 'emms-pause)


(use-package powerline
    :ensure t
    :config (powerline-center-evil-theme))

(use-package markdown-mode
    :ensure t
    :init (setq markdown-command "pandoc")
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode)))


(use-package nov
  :ensure t
  :mode (("\\.epub\\'" . nov-mode)))

(use-package org
    :ensure t
    :init (org-babel-do-load-languages
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
           (dot . t))))

(use-package org-bullets
  :ensure t
  :config
    (setq org-highlight-latex-and-related '(latex))
    (setq org-bullets-bullet-list '("∙"))
    (add-hook 'org-mode-hook 'org-bullets-mode))

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

;; Email

;;; (add-to-list 'load-path "/home/scottviteri/.emacs.d/mu/mu4e")
;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;(require 'mu4e)
;(setq mu4e-maildir (expand-file-name "/home/scottviteri/Maildir/Gmail")
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
;      user-full-name "Scott Viteri")
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
;              (expand-file-name "/home/scottviteri/.authinfo.gpg")
;              smtpmail-default-smtp-server "smtp.gmail.com"
;              smtpmail-smtp-server "smtp.gmail.com"
;              smtpmail-smtp-service 587 ;              smtpmail-debug-info t) ;)
;

;; end Email

;(use-package evil-collection
;  :ensure t
;  :config (evil-collection-init))

;;(with-eval-after-load 'mu4e (require 'evil-collection-mu4e) (evil-collection-mu4e-setup))
;(ad-hook 'mu4e (prog1 (require 'evil-collection-mu4e) (evil-collection-mu4e-setup)))

(add-to-list 'load-path "/home/scottviteri/.emacs.d/agda-mode/")

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
    :fringe-face 'flycheck-fringe-info)
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
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
    :config (add-hook 'haskell-mode-hook 'interactive-haskell-mode))


(use-package eshell
  :ensure t)

(add-to-list 'load-path "/home/scottviteri/.emacs.d/hideshow-orgmode")
(require 'hideshow-orgmode)
(load-file "/home/scottviteri/.emacs.d/hideshow-orgmode/hideshow-orgmode.el")
(evil-define-key nil hs-minor-mode-map (kbd "<tab>") 'hs-cycle)
(evil-define-key nil hs-minor-mode-map (kbd "<backtab>") 'hs-cycle-all)

(defun quarter-window-vertically ()
  "create a new window a quarter size of the current window"
  (split-window-vertically)
  (other-window 1)
  (split-window-vertically)
  (other-window -1)
  (delete-window)
)

(defun open-mini-eshell ()
  "open a mini-eshell in a small window at the bottom of the current window"
  (interactive)
  (quarter-window-vertically)
  (other-window 1)
  (eshell))

(defalias 'eshell/ff 'find-file)
(defalias 'eshell/ffow 'find-file-other-window)

(defun clipboard/set (astring)
  "Copy a string to clipboard"
   (with-temp-buffer
    (insert astring)
    (clipboard-kill-region (point-min) (point-max))))

(defun eshell/copy-pwd ()
 (clipboard/set (eshell/pwd)))

(defun eshell/copy-fpath (fname)
  (let ((fpath (concat (eshell/pwd) "/" fname)))
       (clipboard/set fpath)
       (concat "Copied path: " fpath)))

(setq source-directory "/home/scottviteri/LocalSoftware/emacs-27.1/src")
(setq find-function-C-source-directory "/home/scottviteri/LocalSoftware/emacs-27.1/src")
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local tags-table-list '("/usr/share/emacs/27.1/lisp"))))
(add-hook 'eshell-mode-hook
          (lambda () (setq-local tags-table-list '("/usr/share/emacs/27.1/lisp"))))



(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)
     (add-hook 'eshell-mode-hook
              (lambda () (setq-local company-backends
                        (cons 'company-files (cons 'company-shell (cons 'company-shell-env (cons 'company-fish-shell company-backends)))))))
     (add-hook 'c-mode-hook
              (lambda () (setq-local company-backends
                          (cons 'company-irony (cons 'company-irony-c-headers (cons 'company-clang company-backends))))))
     (add-hook 'c++-mode-hook
              (lambda () (setq-local company-backends
                                (cons 'company-irony (cons 'company-irony-c-headers (cons 'company-clang company-backends))))))
     (add-hook 'python-mode-hook
               (lambda () (setq-local company-backends
                                 (cons 'company-jedi company-backends))))
     (add-hook 'python-inferior-mode-hook
               (lambda () (setq-local company-backends
                                 (cons 'company-jedi company-backends))))))

(use-package irony
  :ensure t
  :config
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c++-mode-hook 'irony-eldoc)
    (add-hook
     'c++-mode-hook
     (lambda () (setq flycheck-clang-include-path
                 (list "/home/scottviteri/MyBrain/BodySelf/FormalEducation/Stanford/CS343D/Assignment1/clang+llvm-6.0.0-x86_64-linux-gnu-ubuntu-16.04/include"))))
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-eldoc)
    (add-hook 'objc-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-eldoc)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))


(use-package company
  :ensure t
  :defer t
  :init
    (setq company-backends '(company-files company-keywords company-etags company-dabbrev company-dabbrev-code company-capf))
  :config
    (use-package company-irony :ensure t :defer t)
    (use-package company-irony-c-headers :ensure t :defer t)
    (use-package company-tabnine :ensure t)
    (use-package company-lean :ensure t)
    (use-package company-shell :ensure t)
    (global-set-key (kbd "S-SPC") #'company-complete)
    (global-company-mode 1))

(use-package proof-general :ensure t)

(use-package company-coq
    :ensure t
    :config
        (setq proof-splash-seen t)
        (add-hook 'coq-mode-hook 'company-coq-mode)
    :pin melpa-unstable)


; (load "/home/scottviteri/opam-coq/coq-8.8/share/emacs/site-lisp/tuareg-site-file")
; (load "/home/scottviteri/opam-coq/coq-8.8.source/share/emacs/site-lisp/tuareg-site-file")

(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
  (autoload 'merlin-mode "merlin" nil t nil)
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)
  (setq merlin-command 'opam)))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(defun my-minibuffer-setup ()
       (set (make-local-variable 'face-remapping-alist)
          '((default :height 2.0))))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(with-current-buffer (get-buffer " *Echo Area 0*")     ; the leading space character is correct
  (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(browse-url-browser-function 'eww-browse-url)
 '(company-idle-delay 0)
 '(company-lean-type-foreground "Purple")
 '(company-show-numbers t)
 '(custom-enabled-themes '(manoj-dark))
 '(custom-safe-themes
   '("d1cc05d755d5a21a31bced25bed40f85d8677e69c73ca365628ce8024827c9e3" default))
 '(eshell-cmpl-compare-entry-function 'string-lessp)
 '(fci-rule-color "#383838")
 '(flycheck-highlighting-mode 'columns)
 '(geiser-default-implementation nil)
 '(haskell-interactive-popup-errors nil)
 '(lean-message-boxes-enabled-captions
   '("check result" "eval result" "print result" "reduce result" "trace output"))
 '(lean-rootdir "/home/scottviteri/LocalSoftware/lean")
 '(make-backup-files nil)
 '(org-babel-python-command "python3")
 '(org-confirm-babel-evaluate nil)
 '(org-export-use-babel nil)
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(package-selected-packages
   '(company-jedi emms yasnippet-snippets clang-format helm-company 0blayout hs-minor-mode irony-eldoc irony company-tabnine company-shell direx helm-eww geiser racket-mode nov pdf-tools company-auctex z3-mode helm-lean evil-goggles extempore-mode gnuplot-mode writegood-mode company company-coq company-irony company-irony-c-headers company-lean company-math evil-collection evil-org gnuplot haskell-mode lean-mode math-symbol-lists org-bullets paredit proof-general eshell-did-you-mean eshell-fixed-prompt flylisp evil-paredit flymake flycheck flycheck-irony company-mode irony-mode esh-autosuggest esh-help w3m ac-haskell-process flymake-hlint iedit merlin-eldoc auto-complete merlin tuareg markdown-mode powerline magit use-package evil))
 '(pdf-view-midnight-colors '("white" . "black"))
 '(prog-mode-hook '(hs-minor-mode hs-cycle-all))
 '(send-mail-function 'smtpmail-send-it)
 '(show-paren-mode t)
 '(tab-stop-list
   '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
 '(tags-file-name nil t)
 '(tags-table-list 'nil))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
