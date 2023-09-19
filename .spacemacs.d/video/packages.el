;;; packages.el --- video layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Jian Wang <jianwang.academic@gmail.com>
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
;; added to `video-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `video/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `video/pre-init-PACKAGE' and/or
;;   `video/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst video-packages
  '(
    (vlc-rc :location (recipe :fetcher github :repo "et2010/vlc-rc"))
    )
  )

(defun video/init-vlc-rc ()
  (use-package vlc-rc
    :defer t
    :init
    (progn
      (define-key global-map (kbd "<f7>") #'vlc/pause)
      (define-key global-map (kbd "<f6>") #'vlc/jump-forward)
      (define-key global-map (kbd "<f5>") #'vlc/jump-backward)
      (define-key global-map (kbd "<f12>") #'vlc/screenshot))
    :config
    (spacemacs/set-leader-keys "v" vlc-rc-map)
    ))

;;; packages.el ends here
