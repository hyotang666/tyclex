; vim: ft=lisp et
(in-package :asdf)
(defsystem :vs-haskell.test
  :depends-on
  (:jingoh "vs-haskell" "millet" "curried-function" "incf-cl" "series")
  :components
  ((:file "vs-haskell")
   ;; Mid
   (:file "setup" :depends-on ("vs-haskell"))
   (:file "lambda-var-decls" :depends-on ("vs-haskell"))
   (:file "adt" :depends-on ("vs-haskell"))
   ;; Mid2
   (:file "type-class" :depends-on ("adt" "setup"))
   (:file "writer" :depends-on ("adt"))
   ;; Mid3
   (:file "newtype" :depends-on ("type-class"))
   (:file "monad" :depends-on ("type-class"))
   ;; Mid4
   (:file "monoid" :depends-on ("type-class" "newtype"))
   ;; Top
   (:file "subtypep" :depends-on ("monoid"))
   )
  :perform
  (test-op(o c)
    (symbol-call :jingoh :examine :vs-haskell)))

#|
(defmethod perform :before
  ((o compile-op)(c (eql (find-component :vs-haskell.test "type-class"))))
    (symbol-call :jingoh.org :delete-subject
                 (find-symbol "DEFINE-TYPE-CLASS" "VS-HASKELL.SPEC")
                 (symbol-call :jingoh.org :find-org :vs-haskell)))
(defmethod perform :before
  ((o compile-op)(c (eql (find-component :vs-haskell.test "newtype"))))
    (symbol-call :jingoh.org :delete-subject
                 (find-symbol "DEFINE-NEWTYPE" "VS-HASKELL.SPEC")
                 (symbol-call :jingoh.org :find-org :vs-haskell)))
(defmethod perform :before
  ((o compile-op)(c (eql (find-component :vs-haskell.test "monoid"))))
    (symbol-call :jingoh.org :delete-subject
                 (find-symbol "PRODUCT" "VS-HASKELL.SPEC")
                 (symbol-call :jingoh.org :find-org :vs-haskell)))
(defmethod perform :before
  ((o compile-op)(c (eql (find-component :vs-haskell.test "monad"))))
    (symbol-call :jingoh.org :delete-subject
                 (find-symbol "MONAD" "VS-HASKELL.SPEC")
                 (symbol-call :jingoh.org :find-org :vs-haskell)))
(defmethod perform :before
  ((o compile-op)(c (eql (find-component :vs-haskell.test "subtypep"))))
    (symbol-call :jingoh.org :delete-subject
                 (find-symbol "SUBTYPE?" "VS-HASKELL.SPEC")
                 (symbol-call :jingoh.org :find-org :vs-haskell)))
(defmethod perform :before
  ((o compile-op)(c (eql (find-component :vs-haskell.test "setup"))))
    (symbol-call :jingoh.org :delete-subject
                 (find-symbol "SETUP" "VS-HASKELL.SPEC")
                 (symbol-call :jingoh.org :find-org :vs-haskell)))
|#
