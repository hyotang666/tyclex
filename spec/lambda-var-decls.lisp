(in-package :vs-haskell.spec)
(named-readtables:in-readtable jingoh.reader:syntax)

(requirements-about LAMBDA-VAR-DECLS :test equal)

;;;; Description:

#+syntax
(LAMBDA-VAR-DECLS lambda-list types) ; => result

;;;; Arguments and Values:

; lambda-list := 

; types := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples:
; required
#?(ehcl::lambda-var-decls '(crc buffer end)
			  '((unsigned-byte 32) (simple-array (unsigned-byte 8) (*)) fixnum))
=> ((DECLARE (TYPE FIXNUM END))
    (DECLARE (TYPE (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) BUFFER))
    (DECLARE (TYPE (UNSIGNED-BYTE 32) CRC)))

; Type T is ignored.
#?(ehcl::lambda-var-decls '(var) '(t))
=> NIL

; Type * is also ignored.
#?(ehcl::lambda-var-decls '(var) '(*))
=> NIL

#?(ehcl::lambda-var-decls '(a b c) '(t fixnum *))
=> ((DECLARE(TYPE FIXNUM B)))

; &OPTIONAL
#?(ehcl::lambda-var-decls '(&optional value)'(&optional BOOLEAN))
=> ((DECLARE(TYPE BOOLEAN VALUE)))

; Some optionals.

#?(ehcl::lambda-var-decls '(&optional a b) '(&optional fixnum fixnum))
=> ((DECLARE(TYPE FIXNUM B))
    (DECLARE(TYPE FIXNUM A)))

; T is ignored.
#?(ehcl::lambda-var-decls '(&optional frag) '(&optional t))
=> NIL

; * is ignored.
#?(ehcl::lambda-var-decls '(&optional var) '(&optional *))
=> NIL

; Case initial-value.
#?(ehcl::lambda-var-decls '(&optional (value +empty-tcell+))'(&optional fixnum))
=> ((DECLARE(TYPE FIXNUM VALUE)))

; Case supplised-p exists.

#?(ehcl::lambda-var-decls '(&optional (value 0 supplied-p)) '(&optional fixnum))
=> ((DECLARE(TYPE FIXNUM VALUE))
    (DECLARE(TYPE BOOLEAN SUPPLIED-P)))

#?(ehcl::lambda-var-decls '(&optional (value nil supplied-p)) '(&optional t))
=> ((DECLARE(TYPE BOOLEAN SUPPLIED-P)))

; combination of required and &optional.
#?(ehcl::lambda-var-decls '(a &optional (b 0)) '(string &optional fixnum))
=> ((DECLARE(TYPE FIXNUM B))
    (DECLARE(TYPE STRING A)))

; &REST
#?(ehcl::lambda-var-decls '(&rest args) '(&rest char))
=> ((DECLARE(TYPE (LIST CHAR) ARGS)))

#?(ehcl::lambda-var-decls '(&rest args) '(&rest t))
=> ((DECLARE(TYPE (LIST T) ARGS)))
#?(ehcl::lambda-var-decls '(&rest args) '(&rest *))
=> ((DECLARE(TYPE (LIST *) ARGS)))

; combinations
#?(ehcl::lambda-var-decls '(a &rest b) '(fixnum &rest t))
=> ((DECLARE(TYPE (LIST T)B))
    (DECLARE(TYPE FIXNUM A)))

; &KEY
#?(ehcl::lambda-var-decls '(&key junk-allowed-p)'(&key (:junk-allowed-p boolean)))
=> ((DECLARE(TYPE BOOLEAN JUNK-ALLOWED-P)))

; initial value exists.
#?(ehcl::lambda-var-decls '(&key (key #'identity)) '(&key (:key function)))
=> ((DECLARE(TYPE FUNCTION KEY)))

; alias exists.
#?(ehcl::lambda-var-decls '(&key ((:stream s)*standard-output*)) '(&key (:stream stream)))
=> ((DECLARE(TYPE STREAM S)))

; t is ignored.
#?(ehcl::lambda-var-decls '(&key var) '(&key (:var t)))
=> NIL

; * is ignored.
#?(ehcl::lambda-var-decls '(&key var) '(&key (:var *)))
=> NIL

; key parameter order is depends-on lambda-list.
#?(ehcl::lambda-var-decls '(&key (a 0)(b "")(c :symbol)) '(&key(:b string)(:c symbol)(:a fixnum)))
=> ((DECLARE(TYPE SYMBOL C))
    (DECLARE(TYPE STRING B))
    (DECLARE(TYPE FIXNUM A)))

; combinations
#?(ehcl::lambda-var-decls '(r &key (k 0)) '(string &key (:k fixnum)))
=> ((DECLARE(TYPE STRING R))
    (DECLARE(TYPE FIXNUM K)))

#?(ehcl::lambda-var-decls '(r &rest args &key a)'(fixnum &rest t &key (:a string)))
=> ((DECLARE(TYPE STRING A))
    (DECLARE(TYPE (LIST T)ARGS))
    (DECLARE(TYPE FIXNUM R)))

; &allow-other-keys
#?(ehcl::lambda-var-decls '(to-type collection &key)'(t vector &key &allow-other-keys))
=> ((DECLARE(TYPE VECTOR COLLECTION)))

