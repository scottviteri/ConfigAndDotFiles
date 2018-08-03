(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)


(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq-default indent-tabs-mode nil)
;(setq-default tab-width 4)
(setq default-tab-width 4);


(package-initialize)

;(add-to-list 'load-path "~/.emacs.d/auto-java-complete/")
;(require 'ajc-java-complete-config)
;(add-hook 'java-mode-hook 'ajc-java-complete-mode)
;(add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

