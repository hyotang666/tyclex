(in-package :cl-user)
(eval-when(:compile-toplevel :load-toplevel :execute)
  (unless(find-package :type-unify)
    (rename-package :unify :type-unify)
    (let((asdf::*asdf-session* nil))
      (asdf:load-system :cl-unification :force t))))

(defpackage #:vs-haskell
  (:use #:cl)
  (:nicknames "EHCL")
  (:shadow #:list)
  (:export #:list)
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
    #:mapmonad #:mapm #:forever #:for-monad #:interact #:bracket
    #:do-contents #:with-file
    #:io-mode #:read-mode #:write-mode #:append-mode #:read-write-mode
    #:h-close
    #:defaction
    )
  (:export ; newtype
    #:define-newtype #:denew)
  )
(in-package #:vs-haskell)

(setf(symbol-function 'list)#'cl:list)

(deftype list(&optional a)
  (declare(ignore a))
  'cl:list)
(declaim(ftype(function(&rest t)list)list))
