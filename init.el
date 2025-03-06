;; ADDING MELPA REPO
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Removing base stuff
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Standard modes removed and enable which-key
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)  
(which-key-mode 1)

;; Ignore bell
(setq ring-bell-function 'ignore)

;; Line numbers, relative
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)


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
(setq load-prefer-newer t)
;;(setq tab-width 4)

;; BASIC CONTROLS
;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here
(when (eq system-type 'darwin)
  (windmove-default-keybindings 'shift) ; If mac, shift will be better
)

;; Keep auto-save and backup files in one flat directory
;; (instead of next to the original files
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
;; Store custom variables in ~/.emacs.d/custom.el (instead of ~/.emacs)
(setq custom-file (locate-user-emacs-file "custom.el"))

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

