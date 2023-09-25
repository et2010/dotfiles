;;; packages.el --- devtools layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: jj <jj@jian-laptop>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `devtools-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `devtools/init-PACKAGE' to load and initialize the package.
;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `devtools/pre-init-PACKAGE' and/or
;;   `devtools/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst devtools-packages
  '(
    devdocs-browser
    (dotenv :location (recipe :fetcher github :repo "pkulev/dotenv.el"))
    (git-commit-jira-prefix :location (recipe :fetcher github :repo "chrisbarrett/git-commit-jira-prefix"))
    numpydoc
    )
  )

(defun devtools/init-devdocs-browser ()
  (use-package devdocs
    :defer t
    :init
    (spacemacs/set-leader-keys
      "hbi" #'devdocs-browser-open-in
      "hbo" #'devdocs-browser-open)
    )
  )

(defun devtools/init-dotenv ()
  (use-package dotenv
    :defer t
    :init
    (with-eval-after-load 'projectile
      (require 'dotenv)
      (defun dotenv-projectile-hook ()
        (dotenv-update-project-env (projectile-project-root)))

      (add-to-list 'projectile-after-switch-project-hook #'dotenv-projectile-hook)
      )
    )
  )

(defun devtools/init-git-commit-jira-prefix ()
  (use-package git-commit-jira-prefix
    :defer t
    :init
    (with-eval-after-load 'git-commit
      (git-commit-jira-prefix-init))
    ))

(defun devtools/init-numpydoc ()
  (use-package numpydoc
    :defer t
    :init
    (setq numpydoc-insertion-style nil))
  )
