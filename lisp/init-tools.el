;;; init-tools.el --- Package and utility integrations. -*- lexical-binding: t; -*-

(require 'dream-paths)
(require 'dream-setup)

(cl-eval-when (compile)
  (require 'borg)
  (require 'epkg)
  (require 'exec-path-from-shell)
  (require 'clutch))

(setq epkg-repository dream-epkg-directory
      borg-compile-function #'borg-byte+native-compile)

(defun dream-tools-configure-borg (&rest args)
  "Keep assimilated Borg submodules quiet in the parent repository."
  (when-let* ((package (car-safe args)))
    (borg--call-git package "config" "-f" borg-gitmodules-file
                    (format "submodule.%s.ignore" package) "untracked")
    (borg--call-git package "add" ".gitmodules")))

(setup borg
  (:autoload borg-clone borg-build borg-remove borg-assimilate
             borg-insert-update-message)
  (:advice borg-build :after dream-paths-reset-load-path-cache)
  (:advice borg-assimilate :after dream-tools-configure-borg))

(defun dream-tools-initialize-shell-environment ()
  "Import the login shell environment for the first graphical frame."
  (when (memq window-system '(mac ns x pgtk))
    (require 'exec-path-from-shell)
    (dolist (variable '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
                        "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"
                        "LSP_USE_PLISTS"))
      (add-to-list 'exec-path-from-shell-variables variable))
    (exec-path-from-shell-initialize)))

(add-hook 'on-init-ui-hook #'dream-tools-initialize-shell-environment -80)

(setup clutch
  (:set clutch-connect-timeout-seconds 10
        clutch-read-idle-timeout-seconds 30
        clutch-query-timeout-seconds 20
        clutch-jdbc-rpc-timeout-seconds 15)
  (:when-loaded (require 'clutch-db-jdbc))
  (:match-file "*.sql"))

(provide 'init-tools)
;;; init-tools.el ends here.
