;; ****** Added by emacsw32-setup-base at Mon Nov 20 08:53:20 2006
;; Load emacsw32 if found.
(progn
  (require 'emacsw32 nil t)
  (unless (featurep 'emacsw32)
    (lwarn '(emacsw32) :error "Could not find emacsw32.el")))
