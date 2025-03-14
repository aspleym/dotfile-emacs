;;; pre-early-init.el --- Pre early to init some settings -*- no-byte-compile: t; lexical-binding: t; -*-


;; Reducing clutter in ~/.emacs.d by redirecting files to ~/emacs.d/var/
;; IMPORTANT: This part should be in the pre-early-init.el file
(setq minimal-emacs-var-dir (expand-file-name "var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" minimal-emacs-var-dir))
(setq user-emacs-directory minimal-emacs-var-dir)
