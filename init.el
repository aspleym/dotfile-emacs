;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Before package
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 2 1024 1024));
(setq process-adaptive-read-buffering nil)

;; Dont ping things that look like domain names.
(setq ffap-machine-p-known 'reject)
(setq native-comp-async-query-on-exit t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Undo/redo
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Package.el
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Feature, warnings and errors
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(setq ad-redefinition-action 'accept)
(setq warning-suppress-types '((lexical-binding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;; Allow nested
(setq enable-recursive-minibuffers t)
;; Keep cursor outside read-only portions
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   User interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;; Reduce update time on idle, Emacs "updates" more often than needed
(setq idle-update-delay 1.0)

;; Shorter responses
(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add #'yes-or-no-p :override #'y-or-no-p))
(defalias #'view-hello-file #'ignore) ;; Never show the hello file

;; No blink of beep
(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Show paren
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Compilation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(setq compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output 'first-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Misc
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(setq whitespace-line-column nil) ; whitespace-mode
;; Aiming to improve performance
(setq rainbow-delimiters-max-face-count 5)

;;
;; LINE NUMBERS
;;
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)
;; Line numbers, relative
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(setq truncate-string-ellipsis "...")
;; Improve responsiveness, delay syntax highlighting during input
(setq redisplay-skip-fontification-on-input t)

;; Collect and display all available docs immediately
(setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)

;; Disable truncation of printed s-expr in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Position underlines
(setq x-underline-at-descent-line t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;; Delete by moving to trash in interactive mode
(setq delete-by-moving-to-trash (not noninteractive))
(setq remote-file-name-inhibit-delete-by-moving-to-trash t)

;; Ignoring this as acceptable since it will redirect to the buffer regardless.
(setq find-file-suppress-same-file-warnings t)

;; Resolve symlinks
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Prefer veertical splits oveer horizontal ones
(setq split-width-threshold 170
      split-height-threshold nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Buffers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(setq uniquify-buffer-name-style 'forward)
;; Command interpreter
(setq comint-prompt-read-only t)
(setq comint-buffer-maximum-size 2048)

;; Skip confirmation when creating a new file
(setq confirm-nonexistent-file-or-buffer nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Backup files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq backup-by-copying-when-linked t)
(setq backup-by-copying t) ;; Copy rather than renaming
(setq delete-old-versions t)
(setq version-control t)
(setq kept-new-versions 5)
(setq kept-old-versions 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(setq vc-git-print-log-follow t)
(setq vc-make-backup-files nil) ; Do not backup version controlled files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Auto saves
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;; Auto-save to safeguard agains crash etc.
;; `recover-file` and `recover-session` can be used to restore auto-saved data
(setq auto-save-default nil)
(setq auto-save-no-message t)

;; Also on big changes
(setq auto-save-include-big-deletions t)

(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))
(setq tramp-auto-save-directory
      (expand-file-name "tramp-autosave/" user-emacs-directory))

;; Auto save options
(setq kill-buffer-delete-auto-save-files t)
;; Remove dupes
(setq kill-do-not-save-duplicates t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Auto revert
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


;; Update on changes to the underlying
(setq revert-without-query (list ".") ; Do not prompt
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)

(setq global-auto-revert-non-file-buffers t)
(setq global-auto-revert-ignore-modes '(Buffer-menu-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Recentf
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(setq recentf-max-saved-items 300)



(which-key-mode 1)


;; History modes
(savehist-mode t)
(save-place-mode t)
(recentf-mode t)
(setq recentf-max-saved-items 50)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Polling to update from disk
;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;;; Load theme
(load-theme 'modus-vivendi)

;; Do not ask for permission to kill a buffer (unless it is modified)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Customize
(setq require-final-newline t)
;;(setq tab-width 4)

;; BASIC CONTROLS
;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here
(when (eq system-type 'darwin)
  (windmove-default-keybindings 'shift) ; If mac, shift will be better
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager
;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; We won't set these, but they're good to know about
(setopt indent-tabs-mode nil)
;; (setopt tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Use common keystrokes by default
(cua-mode)

;; Display line numbers in programming mode
;; (setopt display-line-numbers-width 3)           ; Set a minimum width

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
 (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; SETTING UP VERTICO
(use-package vertico
  :ensure t
  :init
  (vertico-mode))
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)))
(use-package consult
  :ensure t
  :init
  (global-set-key (kbd "C-x b") 'consult-buffer))

