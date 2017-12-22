(in-package :cl-user)
(eval-when(:compile-toplevel :load-toplevel :execute)
  (unless(find-package :type-unify)
    (rename-package :unify :type-unify)
    (asdf:load-system :cl-unification :force t)))

(defpackage #:vs-haskell
  (:use #:cl)
  (:export
    #:qualified-use
    
    #:defdata

    #:define-type-class
    #:definstance
    )
  (:export ; action.lisp
    #:io-action ; as class
    #:io ; as type constructor.
    #:action ; macro.
    #:<-
    #:put-string #:put-char #:.print #:put-string-line #:get-line #:.sequence
    #:mapmonad #:mapm #:forever #:for-monad #:interact #:bracket #:.return
    #:do-contents #:with-file
    #:io-mode #:read-mode #:write-mode #:append-mode #:read-write-mode
    #:h-close
    #:defaction
    )
  )
(in-package #:vs-haskell)
