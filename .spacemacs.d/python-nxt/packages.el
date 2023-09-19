;;; packages.el --- python-nxt layer packages file for Spacemacs.
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
;; added to `python-nxt-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `python-nxt/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `python-nxt/pre-init-PACKAGE' and/or
;;   `python-nxt/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst python-nxt-packages
  '(
    jupyter
    lpy
    lispy
    org
    )
  )

(defun python-nxt/init-jupyter ()
  (use-package jupyter :defer t
    :init
    (progn
      (with-eval-after-load "ob-jupyter"
        (org-babel-jupyter-override-src-block "python")

        ;; https://github.com/nnicandro/emacs-jupyter/issues/366#issuecomment-985758277
        (defun my-display-ansi-colors ()
          (ansi-color-apply-on-region (point-min) (point-max)))

        (add-hook 'org-babel-after-execute-hook #'my-display-ansi-colors))
      )))

(defun python-nxt/init-lpy ()
  (use-package lpy
    :defer t
    :init
    (progn
      ;; start lpy mode with python mode and undo its capf setting
      (defun start-lpy-and-undo-lpy-capf-setting ()
        (let ((capf-funcs completion-at-point-functions))
          (lpy-mode 1)
          (setq-local completion-at-point-functions capf-funcs)))
      (add-hook 'python-mode-hook #'start-lpy-and-undo-lpy-capf-setting 100)
      ;; following advice will append "--pylab=qt --gui=qt" args so that lpy
      ;; could show matplotlib plots in qt window. Without this advice, it will
      ;; probably hang. This advice also fixed the slowing down due to execution
      ;; of "ipython --version" when venv is activated.
      ;; (https://github.com/syl20bnr/spacemacs/issues/13264#issuecomment-600316367)
      (defun spacemacs//python-setup-shell-advice (orig-func &rest args)
        (if (spacemacs/pyenv-executable-find "ipython")
            (progn (setq python-shell-interpreter "ipython")
                   (setq python-shell-interpreter-args  "--simple-prompt -i --pylab=qt --gui=qt"))
          (setq python-shell-interpreter-args "-i")
          (setq python-shell-interpreter "python"))
        )
      (advice-add 'spacemacs//python-setup-shell :around #'spacemacs//python-setup-shell-advice)
      ;; remove `lispy-python-completion-at-point' from
      ;; `completion-at-point-functions' since it slows down imports editing
      ;; significantly
      (with-eval-after-load 'lpy
        (defun lpy-switch-to-shell-advice ()
          (setq completion-at-point-functions
                (delq 'lispy-python-completion-at-point completion-at-point-functions))
          )
        (advice-add 'lpy-switch-to-shell :after #'lpy-switch-to-shell-advice)
        )
      )
    ))

(defun python-nxt/init-lispy ()
  (use-package lispy
    :defer t
    :config
    (progn
      (defun python-nxt//lispy-kill-process (p)
        (with-ivy-window
          (delete-process (cdr p))))
      ;; Add this action so that when things get nasty we can have a fresh
      ;; start. To kill a python process, just x p then M-o d when the cursor is
      ;; at the the beginning of a line
      (ivy-add-actions
       'lispy-set-python-process
       '(("d" python-nxt//lispy-kill-process "delete")))
      )
    ))

(defun python-nxt/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(jupyter . t)))
  )

;;; packages.el ends here
