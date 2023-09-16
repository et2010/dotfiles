;;; packages.el --- org-note layer packages file for Spacemacs.
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
;; added to `org-note-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `org-note/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `org-note/pre-init-PACKAGE' and/or
;;   `org-note/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst org-note-packages
  '(
    (org-media-note :location (recipe :fetcher github :repo "et2010/org-media-note"))
    )
  )

(defun org-note/init-org-media-note ()
  (use-package org-media-note
    :defer t
    :bind (("<f9>" . org-media-note-hydra/body))
    :init
    (progn

      )

    :config
    (progn

      )))




;;; packages.el ends here
