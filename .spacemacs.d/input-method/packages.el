;;; packages.el --- input-method layer packages file for Spacemacs.
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
;; added to `input-method-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `input-method/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `input-method/pre-init-PACKAGE' and/or
;;   `input-method/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst input-method-packages
  '(rime)
  )

(defun input-method/init-rime ()
  (use-package rime
    :defer t
    :init
    (progn
      (setq default-input-method "rime"
            rime-show-candidate 'posframe
            rime-posframe-style 'vertical)

      (defun rime-evil-escape-advice (orig-fun key)
        "advice for `rime-input-method' to make it work together with `evil-escape'.
	Mainly modified from `evil-escape-pre-command-hook'"
        (if rime--preedit-overlay
	          ;; if `rime--preedit-overlay' is non-nil, then we are editing something, do not abort
	          (apply orig-fun (list key))
          (when (featurep 'evil-escape)
	          (let* (
	                 (fkey (elt evil-escape-key-sequence 0))
	                 (skey (elt evil-escape-key-sequence 1))
	                 (evt (read-event nil nil evil-escape-delay))
	                 )
	            (cond
	             ((and (characterp evt)
		                 (or (and (char-equal key fkey) (char-equal evt skey))
		                     (and evil-escape-unordered-key-sequence
			                        (char-equal key skey) (char-equal evt fkey))))
	              (evil-repeat-stop)
	              (evil-normal-state))
	             ((null evt) (apply orig-fun (list key)))
	             (t
	              (apply orig-fun (list key))
	              (if (numberp evt)
		                (apply orig-fun (list evt))
	                (setq unread-command-events (append unread-command-events (list evt))))))))))
      (advice-add 'rime-input-method :around #'rime-evil-escape-advice)
      )))


;;; packages.el ends here
