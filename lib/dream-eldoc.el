;;; dream-eldoc.el --- Display ElDoc in a bottom side window. -*- lexical-binding: t; -*-

(require 'eldoc)

(defun dream-eldoc-display-buffer (buffer alist)
  "Display BUFFER in a bottom side window using action ALIST."
  (when-let* ((window (display-buffer-in-side-window buffer alist)))
    (with-current-buffer buffer
      (keymap-local-set "C-g" #'quit-window))
    window))

(add-to-list
 'display-buffer-alist
 '("\\`\\*eldoc\\(?: for .+\\)?\\*\\'"
   (dream-eldoc-display-buffer)
   (side . bottom)
   (slot . 0)
   (window-height . 0.33)))

(provide 'dream-eldoc)
;;; dream-eldoc.el ends here.
