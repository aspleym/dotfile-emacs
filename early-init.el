;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings for quick startup and convenience
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DEFINING VARIABLES
(defvar --emacs-frame-title-format "%b - Emacs"
  "Template for displaying the title bar of visible and iconified frame.")
(defvar --emacs-debug (bound-and-true-p init-file-debug)
  "Non-nil to enable debug.")
(defvar --emacs-gc-cons-threshold (* 16 1024 1024)
  "The value of `gc-cons-threshold` after Emacs startup.")
(defvar --emacs-user-directory user-emacs-directory
  "The default value of the `user-emacs-directory` variable.")

(setq load-prefer-newer t)
(setq debug-on-error --emacs-debug)

(setq custom-file (expand-file-name "custom.el" --emacs-user-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optimizing startup with garbage collection handling
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold most-positive-fixnum)
(defun --emacs--restore-gc-cons-threshold ()
  "Restore `--emacs-gc-cons-threshold`."
  (setq gc-cons-threshold --emacs-gc-cons-threshold))
(add-hook 'emacs-statup-hook #'--emacs--restore-gc-cons-threshold 105)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Misc
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-language-environment "UTF-8")
(setq default-input-method nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Performance
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-compacting-font-caches t)

(unless (daemonp)
  (unless noninteractive
    (unless --emacs-debug
      ;; Supress redisplay and redraw during startup
      (defun --emacs--reset-inhibit-redisplay()
        (setq-default inhibit-redisplay nil)
        (remove-hook 'post-command-hook #'--emacs--reset-inhibit-redisplay))
      (setq-default inhibit-redisplay t)
      (add-hook 'post-command-hook #'--emacs--reset-inhibit-redisplay -100)

      (defun --emacs--reset-inhibit-message-during-startup ()
        (setq-default inhibit-message nil)
        (remove-hook 'post-command-hook #'--emacs--reset-inihbit-message-during-startup))
      (setq-default inhibit-message t)
      (add-hook 'post-command-hook #'--emacs--reset-inhibit-message-during-startup -100)

      (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
      (setq-default mode-line-format nil)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (setq mode-line-format nil)))

      )
    ;; Without this, Emacs will try to resize itself to a specific column size
    (setq frame-inhibit-implied-resize t)

    ;; A second, case-insensitive pass over `auto-mode-alist` is time wasted.
    (setq auto-mode-case-fold nil)


    ;; Reduce *Message* noise at startup and scratch buffer crap, or that stupid dashboard
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name)
    (setq initial-buffer-choice nil
          inhibit-startup-buffer-menu t
          inhibit-x-resources t)

    ;; Disable bidirectional text scanning for modest perf boost
    (setq-default bidi-display-reordering 'left-to-right
                  bidi-paragraph-direction 'left-to-right)
    (setq bidi-inhibit-bpa t)

    ;; Remove "For information aboug Gnu Emacs..." message
    (advice-add #'display-startup-echo-area-message :override #'ignore)

    ;; Shave seconds off startup time by starting the scratch in `funamental-mode`
    (setq initial-major-mode 'fundamental-mode
          initial-scratch-message nil)

    (unless --emacs-debug
      ;; Unset command line options irrelevant to the current OS. These options
      ;; are still processed by `command-line-1` but have no effect.
      (unless (eq system-type 'darwin)
        (setq command-line-ns-option-alist nil))
      (unless (memq initial-window-system '(x pgtk))
        (setq command-line-x-option-alist nil)))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Native compilation and Byte compilation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (and (featurep 'native-compile)
    (fboundp 'native-comp-available-p)
    (native-comp-available-p))

    ;; Activate `native-compile`
    (setq native-comp-jit-compilation t
          native-comp-deferred-compilation t
          package-native-compilation t)
  ;; Deactivate if not available
  (setq features (delq 'native-compile features)))

(setq native-comp-warning-on-missing-source --emacs-debug
      native-comp-async-report-warnings-errors (or --emacs-debug 'silent)
      native-comp-verbose (if --emacs-debug 1 0)
      native-comp-debug (if --emacs-debug 1 0 ))

(setq jka-compr-verbose --emacs-debug)
(setq byte-compile-warnings --emacs-debug
      byte-compile-verbose --emacs-debug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   UI Elements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        

(setq frame-title-format --emacs-frame-title-format
      icon-title-format --emacs-frame-title-format)

(setq inhibit-splash-screen t)

(push '(menu-bar-lines . 0) default-frame-alist)
(unless (memq window-system '(mac ns))
  (setq menu-bar-mode nil))

(push '(tool-bar-lines . 0) default-frame-alist)
(setq tool-bar-mode nil)

(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)

(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; Disablee dialogs because they are inconsistent across systems
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Package.el
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

(setq use-package-compute-statistics --emacs-debug)
(setq use-package-expand-minimally (not --emacs-debug))

(setq use-package-minimum-reported-time (if --emacs-debug 0 0.1))
(setq use-package-verbose --emacs-debug)
(setq package-enable-at-startup nil) ; Handled by init.el
(setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(customize-set-variable 'package-archive-priorities '(("gnu"    . 99)
                                                      ("nongnu" . 80)
                                                      ("melpa-stable" . 70)
                                                      ("melpa"  . 0)))
