;;; config.el --- Borg-specific build configuration. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defun dream-borg--ghostel-module-ready-p (directory)
  "Return non-nil when DIRECTORY contains a compatible Ghostel module."
  (let ((module
         (expand-file-name
          (concat "ghostel-module" module-file-suffix)
          directory))
        (version
         (ghostel--read-module-sidecar-version directory)))
    (and (file-regular-p module)
         version
         (not (version<
               version
               ghostel--minimum-module-version)))))

(defun dream-borg-build-ghostel ()
  "Ensure that Ghostel's native module is installed in its drone."
  (unless (and (boundp 'module-file-suffix)
               module-file-suffix)
    (error "This Emacs lacks dynamic module support"))

  (let* ((root
          (file-name-as-directory
           (borg-worktree "ghostel")))
         (installer
          (expand-file-name
           "lisp/ghostel-module-install.el"
           root)))

    (unless (file-readable-p installer)
      (error "Ghostel module installer is missing: %s"
             installer))

    ;; Explicitly load the installer from this drone.
    (load installer nil 'nomessage)

    (unless (dream-borg--ghostel-module-ready-p root)
      (message
       "Ghostel: downloading native module %s"
       ghostel--minimum-module-version)

      (unless (ghostel--download-module root)
        (error "Failed to download Ghostel native module")))

    ;; Verify the final state instead of trusting only the return value.
    (unless (dream-borg--ghostel-module-ready-p root)
      (error
       "Ghostel native module is missing or incompatible after download"))

    (message
     "Ghostel: native module is ready in %s"
     root)))

(provide 'dream-borg-config)
;;; config.el ends here
