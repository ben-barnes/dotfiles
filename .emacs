:;; === Global configuration ===

;; --- Packages ---

;; Explicitly-added libraries
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; List of installed packages
(setq package-selected-packages
      '(;; Utilities
        company
        flycheck
        paredit
        rainbow-delimiters

        ;; Language modes
        haskell-mode
        racket-mode
        rust-mode
        typescript-mode

        proof-general

        ;; Themes
        zenburn-theme))

;; --- Interface ---

;; Remove the GUI menu bar from the top of the terminal window.
(menu-bar-mode -1)

;; Mark the zenburn theme file as safe and select it.
(add-to-list 'custom-safe-themes
             "04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5")
(load-theme 'zenburn)

;; Enable company mode globally.
(add-hook 'after-init-hook 'global-company-mode)

;; Enable Flycheck.
(global-flycheck-mode)

;; Enable IDO mode.
(require 'ido)
(ido-mode t)

;; Enable windmove.
(windmove-default-keybindings)

;; Use spaces for all indentation by default.
(setq-default indent-tabs-mode nil)

;; --- Global functionality ---

;; Place backups in a global directory
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs-backups")))

;; === Language mode configuration ===

;; ATS
(autoload 'ats-mode
  "ats2-mode"
  "Major mode to edit ATS2 source code"
  t)

;; Coq
(setq coq-compile-before-require t)

;; Racket
(add-hook 'racket-mode-hook 'paredit-mode)
(add-hook 'racket-mode-hook 'rainbow-delimiters-mode)

;; Rust configuration
(setq rust-format-on-save t)

;; Proof General
(setq proof-locked-face '(t (:background "color-236")))
