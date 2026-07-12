;;; dream-fonts.el --- Display-aware font management. -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dream-core)
(require 'dream-hooks)

(defgroup dream-fonts nil
  "Display-aware font configuration for Dream Emacs."
  :group 'faces)

(defcustom dream-font nil
  "Primary monospace font family, or nil to select the first fallback."
  :type '(choice (const :tag "Automatic" nil) string)
  :group 'dream-fonts)

(defcustom dream-variable-pitch-font nil
  "Variable-pitch font family, or nil to inherit `dream-font'."
  :type '(choice (const :tag "Primary font" nil) string)
  :group 'dream-fonts)

(defcustom dream-serif-font nil
  "Fixed-pitch serif font family, or nil to inherit `dream-font'."
  :type '(choice (const :tag "Primary font" nil) string)
  :group 'dream-fonts)

(defcustom dream-symbol-font nil
  "Symbol font family, or nil to select the first fallback."
  :type '(choice (const :tag "Automatic" nil) string)
  :group 'dream-fonts)

(defcustom dream-emoji-font nil
  "Emoji font family, or nil to select the first fallback."
  :type '(choice (const :tag "Automatic" nil) string)
  :group 'dream-fonts)

(defcustom dream-cjk-font nil
  "CJK font family, or nil to select the first fallback."
  :type '(choice (const :tag "Automatic" nil) string)
  :group 'dream-fonts)

(defcustom dream-font-size 11.0
  "Default font size in points."
  :type 'number
  :group 'dream-fonts)

(defcustom dream-font-size-step 1.0
  "Point-size change made by each font size command step."
  :type 'number
  :group 'dream-fonts)

(defcustom dream-cjk-font-scale 1.3
  "Scale applied to the selected CJK font family."
  :type 'number
  :group 'dream-fonts)

(defvar dream-after-setting-font-hook nil
  "Hook run after fonts have been applied to a graphical frame.")

(defconst dream-fonts--primary-fallbacks
  '("Cascadia Code" "JetBrains Mono" "SF Mono" "Menlo" "Hack"
    "Source Code Pro" "Monaco" "DejaVu Sans Mono" "Consolas"))

(defconst dream-fonts--symbol-fallbacks
  '("Apple Symbols" "Segoe UI Symbol" "Symbola" "Symbol"))

(defconst dream-fonts--emoji-fallbacks
  '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji"))

(defconst dream-fonts--cjk-fallbacks
  '("LXGW Neo Xihei" "LXGW WenKai Mono" "WenQuanYi Micro Hei Mono"
    "PingFang SC" "Microsoft Yahei UI" "Simhei"))

(defconst dream-fonts--nerd-family "Symbols Nerd Font Mono")
(defconst dream-fonts--missing (make-symbol "dream-fonts--missing"))

(defvar dream-fonts--family-cache (make-hash-table :test #'eql))
(defvar dream-fonts--applied-frames
  (make-hash-table :test #'eq :weakness 'key))
(defvar dream-fonts--rescale-originals (make-hash-table :test #'equal))
(defvar dream-fonts--initial-size nil)

(defun dream-fonts--restore-rescales ()
  "Restore `face-font-rescale-alist' entries changed by Dream."
  (maphash
   (lambda (family original)
     (if (eq original dream-fonts--missing)
         (setq face-font-rescale-alist
               (cl-delete family face-font-rescale-alist
                          :key #'car :test #'equal))
       (setf (alist-get family face-font-rescale-alist nil nil #'equal)
             original)))
   dream-fonts--rescale-originals)
  (clrhash dream-fonts--rescale-originals))

(defun dream-fonts--clear-state ()
  "Clear cached display and frame state, restoring owned rescale entries."
  (dream-fonts--restore-rescales)
  (clrhash dream-fonts--family-cache)
  (clrhash dream-fonts--applied-frames))

(defun dream-fonts--families (frame)
  "Return available font families for FRAME, cached by display."
  (let* ((display (frame-terminal frame))
         (cached (gethash display dream-fonts--family-cache
                          dream-fonts--missing)))
    (if (eq cached dream-fonts--missing)
        (let ((families (font-family-list frame)))
          (puthash display families dream-fonts--family-cache)
          families)
      cached)))

(defun dream-fonts--find-family (family families)
  "Return FAMILY's canonical spelling from FAMILIES, or nil."
  (cl-find family families :test #'string-equal-ignore-case))

(defun dream-fonts--explicit-family (role family families)
  "Resolve explicit FAMILY for ROLE against FAMILIES or signal."
  (unless (stringp family)
    (signal 'wrong-type-argument (list 'stringp family)))
  (or (dream-fonts--find-family family families)
      (signal 'dream-font-error (list role family))))

(defun dream-fonts--fallback-family (role candidates families)
  "Return the first available family from CANDIDATES for ROLE."
  (or (cl-loop for candidate in candidates
               thereis (dream-fonts--find-family candidate families))
      (progn
        (dream-log 3 "font:%s: no fallback is available" role)
        nil)))

(defun dream-fonts--resolve (frame)
  "Resolve every configured font role for FRAME."
  (let* ((families (dream-fonts--families frame))
         (primary
          (if dream-font
              (dream-fonts--explicit-family :default dream-font families)
            (dream-fonts--fallback-family
             :default dream-fonts--primary-fallbacks families))))
    (list
     :default primary
     :variable (if dream-variable-pitch-font
                   (dream-fonts--explicit-family
                    :variable dream-variable-pitch-font families)
                 primary)
     :serif (if dream-serif-font
                (dream-fonts--explicit-family
                 :serif dream-serif-font families)
              primary)
     :symbol (if dream-symbol-font
                 (dream-fonts--explicit-family
                  :symbol dream-symbol-font families)
               (dream-fonts--fallback-family
                :symbol dream-fonts--symbol-fallbacks families))
     :emoji (if dream-emoji-font
                (dream-fonts--explicit-family
                 :emoji dream-emoji-font families)
              (dream-fonts--fallback-family
               :emoji dream-fonts--emoji-fallbacks families))
     :cjk (if dream-cjk-font
              (dream-fonts--explicit-family :cjk dream-cjk-font families)
            (dream-fonts--fallback-family
             :cjk dream-fonts--cjk-fallbacks families))
     :nerd (dream-fonts--find-family dream-fonts--nerd-family families))))

(defun dream-fonts--set-face (face frame family height)
  "Set FACE on FRAME to FAMILY and HEIGHT."
  (if family
      (set-face-attribute face frame :family family :height height)
    (set-face-attribute face frame :height height)))

(defun dream-fonts--set-cjk-rescale (family)
  "Scale FAMILY while preserving any user-owned previous value."
  (when family
    (when (eq (gethash family dream-fonts--rescale-originals
                       dream-fonts--missing)
              dream-fonts--missing)
      (puthash family
               (if-let* ((entry (assoc family face-font-rescale-alist)))
                   (cdr entry)
                 dream-fonts--missing)
               dream-fonts--rescale-originals))
    (setf (alist-get family face-font-rescale-alist nil nil #'equal)
          dream-cjk-font-scale)))

(defun dream-fonts--apply-fontset (frame fonts)
  "Apply fallback FONTS to FRAME's fontset without accumulating entries."
  (when-let* ((family (plist-get fonts :symbol))
              (spec (font-spec :family family)))
    (dolist (script '(symbol mathematical))
      (set-fontset-font nil script spec frame)))
  (when-let* ((family (plist-get fonts :emoji))
              (spec (font-spec :family family)))
    (set-fontset-font nil 'emoji spec frame)
    (set-fontset-font nil 'symbol spec frame 'append))
  (when-let* ((family (plist-get fonts :cjk)))
    (dream-fonts--set-cjk-rescale family)
    (set-fontset-font nil 'han (font-spec :family family) frame))
  (when-let* ((family (plist-get fonts :nerd)))
    (dolist (range '((#xe000 . #xf8ff) (#xf0000 . #xfffff)))
      (set-fontset-font nil range (font-spec :family family) frame))))

(defun dream-fonts--apply-frame (frame)
  "Apply configured fonts to FRAME, returning non-nil when changed."
  (when (and (frame-live-p frame)
             (not (frame-initial-p frame))
             (display-multi-font-p frame)
             (not (gethash frame dream-fonts--applied-frames)))
    (unless (and (numberp dream-font-size) (> dream-font-size 0))
      (user-error "`dream-font-size' must be a positive number"))
    (with-selected-frame frame
      (let* ((fonts (dream-fonts--resolve frame))
             (height (round (* dream-font-size 10))))
        (dream-fonts--set-face 'default frame (plist-get fonts :default) height)
        (dream-fonts--set-face 'fixed-pitch frame
                               (plist-get fonts :default) height)
        (dream-fonts--set-face 'fixed-pitch-serif frame
                               (plist-get fonts :serif) height)
        (dream-fonts--set-face 'variable-pitch frame
                               (plist-get fonts :variable) height)
        (dream-fonts--apply-fontset frame fonts)
        (puthash frame t dream-fonts--applied-frames)
        t))))

(defun dream-fonts-apply (&optional frame)
  "Apply Dream fonts once to FRAME, defaulting to the selected frame."
  (when (dream-fonts--apply-frame (or frame (selected-frame)))
    (dream-run-hooks 'dream-after-setting-font-hook)
    t))

(defun dream-font-reload ()
  "Clear font caches and replay Dream fonts on every graphical frame."
  (interactive)
  (when (fboundp 'clear-font-cache)
    (clear-font-cache))
  (dream-fonts--clear-state)
  (let (changed)
    (dolist (frame (frame-list))
      (when (dream-fonts--apply-frame frame)
        (setq changed t)))
    (when changed
      (dream-run-hooks 'dream-after-setting-font-hook))
    changed))

(defun dream-fonts--adjust-size (steps)
  "Adjust the global font size by STEPS and reload graphical frames."
  (when (display-multi-font-p)
    (let* ((old-size dream-font-size)
           (old-initial-size dream-fonts--initial-size)
           (new-size (+ old-size (* steps dream-font-size-step))))
      (unless (> new-size 0)
        (user-error "Font size must remain positive"))
      (unless dream-fonts--initial-size
        (setq dream-fonts--initial-size old-size))
      (setq dream-font-size new-size)
      (condition-case error
          (dream-font-reload)
        (error
         (setq dream-font-size old-size
               dream-fonts--initial-size old-initial-size)
         (ignore-errors (dream-font-reload))
         (signal (car error) (cdr error)))))))

(defun dream-font-increase (&optional count)
  "Increase the global font size by COUNT steps."
  (interactive "p")
  (dream-fonts--adjust-size (or count 1)))

(defun dream-font-decrease (&optional count)
  "Decrease the global font size by COUNT steps."
  (interactive "p")
  (dream-fonts--adjust-size (- (or count 1))))

(defun dream-font-reset ()
  "Reset global font size and the current buffer's text scale."
  (interactive)
  (when (display-multi-font-p)
    (let ((changed nil))
      (when (and (boundp 'text-scale-mode-amount)
                 (/= text-scale-mode-amount 0))
        (text-scale-set 0)
        (setq changed t))
      (when dream-fonts--initial-size
        (setq dream-font-size dream-fonts--initial-size
              dream-fonts--initial-size nil
              changed t)
        (dream-font-reload))
      changed)))

(add-hook 'dream-init-ui-hook #'dream-fonts-apply)
(add-hook 'after-make-frame-functions #'dream-fonts-apply 90)

(provide 'dream-fonts)
;;; dream-fonts.el ends here.
