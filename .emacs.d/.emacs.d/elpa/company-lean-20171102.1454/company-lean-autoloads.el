;;; company-lean-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-lean" "company-lean.el" (0 0 0 0))
;;; Generated autoloads from company-lean.el

(autoload 'company-lean-hook "company-lean" "\


\(fn)" nil nil)

(add-hook 'lean-mode-hook #'company-lean-hook)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-lean" '("company-lean")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-lean-autoloads.el ends here
