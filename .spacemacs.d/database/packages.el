;;; packages.el --- database layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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
;; added to `database-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `database/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `database/pre-init-PACKAGE' and/or
;;   `database/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst database-packages
  '(
    ejc-sql
    sql-clickhouse
    sql
    org
    )
  )



(defun database/init-ejc-sql ()
  (use-package ejc-sql
    :defer t
    :init
    (progn
      ;; Setup httpd for ejc-sql
      (with-eval-after-load 'clomacs
        (setq clomacs-httpd-default-port 8090)) ; Use a port other than 8080.
      ;; Setup ejc fuzzy matching
      (with-eval-after-load 'ejc-flx
        (setq ejc-use-flx t)
        ;; (setq ejc-flx-threshold 2)
        )
      (with-eval-after-load 'ejc-sql
        (setq ejc-complete-on-dot t)
        (setq ejc-completion-system 'standard)
        (setq ejc-result-table-impl 'ejc-result-mode)
        )

      (with-eval-after-load 'ejc-sql
        (ejc-create-connection
         "mydb"
         :dependencies [[ru.yandex.clickhouse/clickhouse-jdbc "0.2.6"]]
         :dbtype "clickhouse"
         :classname "ru.yandex.clickhouse.ClickHouseDriver"
         :connection-uri (concat "jdbc:clickhouse://127.0.0.1:8123/" "mydb")))

      ;; TODO this doesn't work, why?
      ;; (setq ejc-sql-separator ";")

      (add-hook 'sql-mode-hook (lambda () (ejc-connect "mydb")))
      )
    :config
    (progn

      ;; (add-hook 'ejc-sql-minor-mode-hook
      ;;           (lambda ()
      ;;             (require 'ejc-company)
      ;;             (add-to-list 'company-backends 'ejc-company-backend)
      ;;             (company-mode t)
      ;;             (ejc-eldoc-setup)))

      (add-hook 'ejc-sql-connected-hook
                (lambda ()
                  (ejc-set-fetch-size 50)
                  (ejc-set-max-rows 50)
                  (ejc-set-show-too-many-rows-message t)
                  (ejc-set-column-width-limit 25)
                  (ejc-set-use-unicode t)))
      )
    ))


(defun database/init-sql-clickhouse ()
  (use-package sql-clickhouse
    :defer t)
  )


(defun database/post-init-sql ()
  (spacemacs/set-leader-keys-for-major-mode 'sql-mode
    "," 'ejc-eval-user-sql-at-point)

  (with-eval-after-load 'sql
    (sql-add-product 'clickhouse
                     "ClickHouse"
                     '(:font-lock sql-clickhouse-font-lock-keywords
    				                      :sqli-program sql-clickhouse-program
				                          :prompt-regexp "^:) "
				                          :prompt-length 3
					                        :prompt-cont-regexp "^:-] "
					                        :sqli-login sql-clickhouse-login-params
					                        :sqli-options sql-clickhouse-options
					                        :sqli-comint-func sql-clickhouse-comint)))

  )

(defun database/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(sqlite . t)))
  )
;;; packages.el ends here
