;; init-utils.el --- Ultils Configuration.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(setup clutch
  (:iload clutch)
  (:init
   (setq clutch-connect-timeout-seconds 10
         clutch-read-idle-timeout-seconds 30
         clutch-query-timeout-seconds 20
         clutch-jdbc-rpc-timeout-seconds 15))
  (:when-loaded
    (:require clutch-db-jdbc))
  (:match-file "*.sql"))


(setup chirp
  (:iload chirp))

(provide 'init-utils)
;;; init-utils.el ends here.
