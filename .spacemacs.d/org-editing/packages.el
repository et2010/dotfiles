;;; packages.el --- org-editing layer packages file for Spacemacs.
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
;; added to `org-editing-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `org-editing/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `org-editing/pre-init-PACKAGE' and/or
;;   `org-editing/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst org-editing-packages
  '(
    counsel
    (aggressive-fill-paragraph :location (recipe :fetcher github :repo "et2010/aggressive-fill-paragraph-mode" :upgrade nil))
    ob-tmux
    org
    (org-autolist :location (recipe :fetcher github :repo "et2010/org-autolist" :upgrade nil))
    org-download
    smartparens
    )
  )

(defconst org-editing-struct-templates
  '(("jpy"  . "src jupyter-python :session py")
    ("jpya" . "src jupyter-python :session py :async yes")
    ("py"   . "src python")
    ("pyr"  . "src python :results output raw")
    ("pyv"  . "src python :results value")
    ("hy"   . "src jupyter-hy")
    ("el"   . "src emacs-lisp")
    ("ell"  . "src emacs-lisp :lexical t")
    ("sh"   . "src sh")
    ("shl"  . "src shell")
    ("rest" . "src restclient")
    ("ver"  . "verbatim"))
  "Custom org templates.")

(defconst org-editing-keyword-templates
  '(("al"  . "attr_latex")
    ("ao"  . "attr_org")
    ("ca"  . "caption")
    ("lc"  . "latex_class")
    ("lco" . "latex_class_options")
    ("lh"  . "latex_header")
    ("cc"  . "call")
    ("n"   . "name")
    ("o"   . "options")
    ("ti"  . "title")
    ("tn"  . "tblname"))
  "Custom org keyword templates.")

(defun org-editing/post-init-counsel ()
  (defun org-editing//insert-selection-as-org-link (x)
    (with-ivy-window
      (insert (format "[[%s]]" x))
      (goto-char (- (point) 2))))

  (defun org-editing//insert-selection-as-inline-image (x)
    (with-ivy-window
      (let ((org-download-timestamp ""))
        (org-download-image x))))

  ;; TODO add actions mode-locally: https://github.com/abo-abo/swiper/issues/1598
  (dolist (sym '(counsel-find-file counsel-locate))
    (ivy-add-actions sym
                     '(("k" org-editing//insert-selection-as-org-link "insert as an org link")
                       ("I" org-editing//insert-selection-as-inline-image "insert as an inline image"))))
  )


(defun org-editing/init-aggressive-fill-paragraph ()
  (use-package aggressive-fill-paragraph
    :defer t
    :init
    (progn
      (defun afp-at-meta-line? ()
        "Is cursor in a meta line (for Nikola blog post)"
        (and (derived-mode-p 'org-mode)
             (save-excursion
               (save-match-data
                 (beginning-of-line)
                 (looking-at "\\.\\. [a-z]+\\:")))))

      (with-eval-after-load 'org
        (add-hook 'org-mode-hook (lambda () (aggressive-fill-paragraph-mode 1)))))

    :config
    (progn
      (setq afp-fill-keys (append afp-fill-keys '(?， ?。)))
      (add-to-list 'afp-suppress-fill-pfunction-list #'afp-at-meta-line?))))

(defun org-editing/init-ob-tmux ()
  (use-package ob-tmux
    :defer t
    :init
    (progn
      (setq
       org-babel-default-header-args:tmux
       '((:results . "silent")	;
         (:session . "default")	; The default tmux session to send code to
         (:socket  . nil)) ; The default tmux socket to communicate with
       ;; The tmux sessions are prefixed with the following string.
       ;; You can customize this if you like.
       org-babel-tmux-session-prefix "ob-"
       org-babel-tmux-terminal "/home/jj/Bin/vterm-send-string.sh"
       org-babel-tmux-terminal-opts nil
       ;; vterm-enable-manipulate-selection-data-by-osc52 t
       )
      (require 'ob-tmux)
      ))
  )

(defun org-editing/post-init-org ()
  (defvar org-smart-tab-match-substring-regexp
    "\\S-[_^]\\(?:\\(?:{\\(?:[^{}]*?\\|\\(?:[^{}]*?{[^{}]*?}\\)+[^{}]*?\\|\\(?:[^{}]*?{\\(?:[^{}]*?{[^{}]*?}\\)+[^{}]*?}\\)+[^{}]*?\\)}\\)\\|\\(?:(\\([^()]*?\\|\\(?:[^()]*?([^()]*?)\\)+[^()]*?\\|\\(?:[^()]*?(\\(?:[^()]*?([^()]*?)\\)+[^()]*?)\\)+[^()]*?\\))\\)\\|\\(?:\\*\\|[+-]?[[:alnum:].,\\]*[[:alnum:]]\\)\\)"
    "Regexp to match super/subscript. Same as `org-match-substring-regexp' except
this doesn't have any capture groups.")

  (defun org-try-smart-tab ()
    "Check if it makes sense to tab-escape a super/subscript, and do it if yes.
It makes sense to do so if the cursor is - inside a
super/subscript"
    (when (and org-pretty-entities
	             org-pretty-entities-include-sub-superscripts
	             (save-excursion
	               (goto-char (max (line-beginning-position)
			                           (- (previous-property-change (point)) 3)))
	               (looking-at org-smart-tab-match-substring-regexp)))
      (goto-char (match-end 0))
      (setq disable-point-adjustment t)
      t))
  (advice-add 'org-try-cdlatex-tab :after 'org-try-smart-tab)

  (require 'org-tempo)
  ;; set up struct (such as src block) templates
  (dolist (templ org-editing-struct-templates)
    (add-to-list 'org-structure-template-alist templ))
  ;; set up keyword (e.g. #+options:) templates
  (dolist (templ org-editing-keyword-templates)
    (add-to-list 'org-tempo-keywords-alist templ))
  )


(defun org-editing/init-org-autolist ()
  (use-package org-autolist
    :defer t
    :init
    (add-hook 'org-mode-hook (lambda () (org-autolist-mode 1)))
    ))


(defun org-editing/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    (sp-local-pair 'org-mode "\\[" "\\]" :trigger "\\[")
    (sp-local-pair '(LaTeX-mode org-mode) "（" "）")
    (sp-local-pair '(LaTeX-mode org-mode) "\\left(" "\\right)" :trigger "\\l(")
    (sp-local-pair '(LaTeX-mode org-mode) "\\left[" "\\right]" :trigger "\\l[")
    (sp-local-pair '(LaTeX-mode org-mode) "\\left{" "\\right}" :trigger "\\l{"))
  (add-hook 'org-mode-hook #'spacemacs/toggle-smartparens))


(defun org-editing/pre-init-org-download ()
  (spacemacs|use-package-add-hook org-download
    :post-init
    (progn
      ;; (setq-default org-download-backend "wget \"%s\" -O \"%s\"")
      (if (spacemacs/system-is-mswindows)
          (progn
            (setq-default org-download-screenshot-file (expand-file-name "~/temp/screenshot.png"))
            (setq-default org-download-screenshot-method "i_view64 /capture=4 /convert=\"%s\""))
        (setq-default org-download-screenshot-method "flameshot gui --raw > %s"))
      (setq-default org-download-image-dir "./img"
                    org-download-image-org-width 400)

      (spacemacs/declare-prefix-for-mode 'org-mode "mod" "download")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "iDc" 'org-download-clipboard
        "iDd" 'org-download-delete
        "iDf" 'org-download-rename-last-file
        "iDr" 'org-download-rename-at-point
        "iDY" 'org-download-yank-replace))
    :post-config
    (progn
      (defun org-download-yank-no-timestamp (orig-func)
        (let ((org-download-timestamp ""))
          (funcall orig-func)))
      (advice-add #'org-download-yank :around #'org-download-yank-no-timestamp))))

(defun org-editing/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      ;; replace fundamental mode by graphiz one
      (setq org-src-lang-modes
            (append '(("conf" . conf)) org-src-lang-modes))

      (with-eval-after-load 'org
        (defun org-babel-execute:sh-with-env (original-func body params)
          "Advice to source .env file before executing shell commands in Org-mode source blocks."
          (let* ((dir (or (cdr (assoc :dir params)) default-directory))
                 (remote (file-remote-p dir))
                 (env-file-path (if remote
                                    (tramp-file-name-localname (tramp-dissect-file-name (expand-file-name ".env" dir)))
                                  (expand-file-name ".env" dir)))
                 (env-file-exists (file-exists-p (if remote (concat remote env-file-path) env-file-path)))
                 (sourced-body (if env-file-exists
                                   (format "set -a; . '%s'; set +a; %s" env-file-path body)
                                 ;; If the .env file does not exist, just use the original body
                                 body)))
            (funcall original-func sourced-body params)))


        (advice-add 'org-babel-execute:sh :around #'org-babel-execute:sh-with-env))
      )))

(defun org-editing/po)

;;; packages.el ends here
