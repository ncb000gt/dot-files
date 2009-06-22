;; ****** Added by emacsw32-setup-base at Mon Nov 20 08:53:14 2006
;; Add EmacsW32/lisp to load-path if found.
(let ((lisp-dir (expand-file-name (concat exec-directory "../../EmacsW32/lisp/"))))
  (unless (file-accessible-directory-p lisp-dir)
    (message "Can't find %s" lisp-dir)
    (sit-for 10))
  (when (file-accessible-directory-p lisp-dir)
    (message "Adding %s to load-path" lisp-dir)
    (add-to-list 'load-path lisp-dir)))
