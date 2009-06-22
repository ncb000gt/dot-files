;; Some common lisp for your liking
(require 'cl)

;;;;;;;;;;;;;
;; Load Files
;;;;;;;;;;;;;

; Load editing modes
(load "~/.emacs.d/site-lisp/js2.elc")
(autoload 'ruby-mode "ruby-mode" "Ruby mode" t)
(autoload 'css-mode "css-mode" "CSS mode" t)
(autoload 'light-symbol-mode "light-symbol" "Light Symbol Mode" t)
(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
(load "~/.emacs.d/site-lisp/nxml-mode-20041004/rng-auto.el")
(add-to-list 'load-path "~/.emacs.d/site-lisp/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/site-lisp/yasnippet/snippets")

;; This is how emacs tells the file type by the file suffix.
(setq auto-mode-alist
      (append '(("\\.js$" . js2-mode))
	      '(("\\.tal$" . nxml-mode))
	      '(("\\.css$" . css-mode))
	      '(("\\.rb$" . ruby-mode))
	      '(("\\.xml$" . nxml-mode))
	      '(("\\.jsp$" . nxml-mode))
	      '(("\\.py$" . python-mode))
	      auto-mode-alist))

;;;;;;;;;;;;;;
;; Few Configs
;;;;;;;;;;;;;;
(global-font-lock-mode t)

;; Don't create backup files
(setq make-backup-files nil)

;; set tab distance to something, so it doesn't change randomly and confuse people
(setq c-basic-offset 4)

;; clipboard functions
(setq x-select-enable-clipboard t)
;(setq interprogram-paste-function 'x-cut-buffer-selection-value)

;; Code Folding
(defun jao-toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))

;; Paren matching mode
(show-paren-mode)

; Lose the GUI
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Kill splashscreen
(setq inhibit-startup-message t)
(put 'downcase-region 'disabled nil)

;; Custom keybinds
(global-set-key "\C-r" 'replace-regexp)
(global-set-key "\C-n" 'setnu-mode)
(global-set-key [f1] 'jao-toggle-selective-display)
(global-set-key "\M- " 'hippie-expand)
(global-set-key [f7] 'call-last-kbd-macro)
(global-set-key "\M-Arrow-Right" 'move-forward-word)

;;;;;
;;Run
;;;;;
(eval-after-load 'js2-mode
  '(progn
     (define-key js2-mode-map (kbd "TAB") (lambda()
                                            (interactive)
                                            (let ((yas/fallback-behavior 'return-nil))
                                              (unless (yas/expand)
                                                (indent-for-tab-command)
                                                (if (looking-back "^\s*")
                                                    (back-to-indentation))))))))