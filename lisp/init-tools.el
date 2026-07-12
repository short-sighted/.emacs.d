;;; init-tools.el --- Package and utility integrations. -*- lexical-binding: t; -*-

(require 'dream-paths)
(require 'dream-runtime)
(require 'dream-setup)

(cl-eval-when (compile)
  (require 'borg)
  (require 'epkg)
  (require 'clutch))

(setq epkg-repository dream-epkg-directory
      borg-compile-function #'borg-byte+native-compile)

(defun dream-tools--configure-borg (&rest args)
  "Keep assimilated Borg submodules quiet in the parent repository."
  (when-let* ((package (car-safe args)))
    (borg--call-git package "config" "-f" borg-gitmodules-file
                    (format "submodule.%s.ignore" package) "untracked")
    (borg--call-git package "add" ".gitmodules")))

(defun dream-tools--with-runtime-compilation (function &rest args)
  "Call explicit Borg build FUNCTION with compilation permission."
  (dream-with-runtime-compilation
    (apply function args)))

(setup borg
  (:autoload borg-clone borg-build borg-remove borg-assimilate
             borg-insert-update-message)
  (:advice borg-build :around dream-tools--with-runtime-compilation)
  (:advice borg-build :after dream-paths-reset-load-path-cache)
  (:advice borg-assimilate :after dream-tools--configure-borg))

(setup clutch
  (:set clutch-connect-timeout-seconds 10
        clutch-read-idle-timeout-seconds 30
        clutch-query-timeout-seconds 20
        clutch-jdbc-rpc-timeout-seconds 15)
  (:when-loaded (require 'clutch-db-jdbc))
  (:match-file "*.sql"))

(provide 'init-tools)
;;; init-tools.el ends here.
