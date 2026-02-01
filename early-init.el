;;; early-init.el -*- lexical-binding: t; -*-

;; Ensure that we are running the correct version of Emacs.
(let ((minver "30.1"))
  (when (version< emacs-version minver)
    (error "This config requires Emacs v%s or higher." minver)))

;; Temporarily raise the garbage collection threshold to its maximum value
;; during initialization. This improves startup times.
(setopt gc-cons-threshold most-positive-fixnum ; 2^61 bytes
        gc-cons-percentage 1.0)

;; Set the garbage collector to normal values after startup.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setopt gc-cons-threshold (* 16 1024 1024) ; 16mb
                    gc-cons-percentage 0.1)))

;; Show the startup time after initialization is done.
(add-hook 'emacs-startup-hook
          (lambda () (message "*** Emacs loaded in %s seconds"
                              (emacs-init-time "%.3f"))))

;; Set UTF-8 as default.
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
;; Mainly for Windows.
(set-terminal-coding-system 'utf-8-unix)

;; Start with a maximized frame.
(push '(fullscreen . maximized) initial-frame-alist)

;; Disable UI elements.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Font config.
;; Emacs has three faces. The default face is the only one that must have an
;; absolute :height value. Everything else uses a floating point value, which is
;; understood as a multiple of the default.
(set-face-attribute 'default nil :family "Iosevka" :weight 'medium :height 150)
(set-face-attribute 'fixed-pitch nil :family "Iosevka" :weight 'medium :height 1.0)
(set-face-attribute 'variable-pitch nil :family "Sans" :weight 'medium :height 1.0)

;; Set the frame title to the current filename.
(setq frame-title-format '("%b"))

;; By default, resizing a frame rounds its sizes to multiples of the
;; character size. Setting this to true allows for frame sizes to
;; increase/decrease by one pixel.
(setopt frame-resize-pixelwise t)

;; Do not resize the frame when the font, menubar, toolbar, etc. are modified.
;; This improves startup times since resizing the frame is not cheap.
(setopt frame-inhibit-implied-resize t)

;; Do not attempt to load ~/.Xdefaults or ~/.Xresources
(setopt inhibit-x-resources t)

;; The default setting for reporting native compilation errors is too verbose.
;; Make native compilation silent.
(setopt native-comp-async-report-warnings-errors 'silent)

;; Disable package.el in favour of Elpaca.
(setq package-enable-at-startup nil)

(provide 'early-init)
