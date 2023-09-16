;;; packages.el --- org-beautify layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: jj <jj@jj-vivobooks15x530un>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `org-beautify-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `org-beautify/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `org-beautify/pre-init-PACKAGE' and/or
;;   `org-beautify/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst org-beautify-packages
  '(
    org
    )
  )

(defun org-beautify/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-init
    (progn
      ;; https://emacs-china.org/t/orgmode/9740/18?u=et2010
      (setq org-emphasis-regexp-components
            '("-[:multibyte:][:space:]('\"{" "-[:multibyte:][:space:].,:!?;'\")}\\[" "[:space:]" "." 1))
      (setq org-use-sub-superscripts "{}")
      (setq org-startup-indented t)
      (setq org-pretty-entities t)
      ;; (setq org-highlight-latex-and-related '(latex script entities))
      ;; (setq org-src-window-setup 'current-window)
      )
    :post-config
    (progn
      ;; https://emacs-china.org/t/orgmode/9740/18?u=et2010
      (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
      (org-element-update-syntax))
    )
  )

;;; packages.el ends here
