(defvar emacs-dir (concat (getenv "HOME") ".emacs.d/")
  "Directory root for your Emacs stuff.")

;; if you wish to add a new library to emacs just place the dot-el
;; file in this directory and you should then be able to load it.
(defvar site-lisp-dir (concat emacs-dir "site-lisp/")
  "Directory for additional packages.")

;; add the packages directory to the search path.
(add-to-list 'load-path site-lisp-dir)

(require 'cl)

;;;;;;;;;;;;;
;; Load Files
;;;;;;;;;;;;;

; Load editing modes
(add-to-list 'load-path "~/.emacs.d/site-lisp/scala-mode")
(require 'scala-mode-auto)
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
;; set tab distance to something, so it doesn't change randomly and confuse people
(setq c-basic-offset 4)
(setq indent-tabs-mode nil)

;; absolutely force tabs on save in js2-mode
(defun js2-mode-untabify ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
	(untabify (1- (point)) (point-max))))
  nil)

(add-hook 'js2-mode-hook 
	  '(lambda ()
	     (make-local-variable 'write-contents-hooks)
	     (add-hook 'write-contents-hooks 'js2-mode-untabify)))

(global-font-lock-mode t)

;; Font size
(set-face-attribute 'default nil :height 80)

;; Don't create backup files
(setq make-backup-files nil)

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
(global-set-key [?\S- ] 'hippie-expand)
(global-set-key [f7] 'call-last-kbd-macro)
(global-set-key "\M-Arrow-Right" 'move-forward-word)
;; rebind quit emacs to C-x-C-q (I keep hitting C-x-C-c by accident)
(global-unset-key [(control x)(control c)])
(global-set-key [(control x)(control q)]  'save-buffers-kill-emacs)


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

;; works by installing color-theme through emacs-goodies...
;; apt-get install emacs-goodies =\
(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-goodies-el/color-theme.el")
(defun dark ()
  (interactive)
  (color-theme-install
   '(dark
      ((background-color . "#101e2e")
      (background-mode . light)
      (border-color . "#1a1a1a")
      (cursor-color . "#fce94f")
      (foreground-color . "#eeeeec")
      (mouse-color . "black"))
     (fringe ((t (:background "#1a1a1a"))))
     (mode-line ((t (:foreground "#eeeeec" :background "#555753"))))
     (region ((t (:background "#0d4519"))))
     (font-lock-builtin-face ((t (:foreground "#729fcf"))))
     (font-lock-comment-face ((t (:foreground "#888a85"))))
     (font-lock-function-name-face ((t (:foreground "#edd400"))))
     (font-lock-keyword-face ((t (:foreground "#729fcf"))))
     (font-lock-string-face ((t (:foreground "#ad7fa8"))))
     (font-lock-type-face ((t (:foreground"#8ae234"))))
     (font-lock-variable-name-face ((t (:foreground "#eeeeec"))))
     (minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))
(provide 'dark)

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (dark)))