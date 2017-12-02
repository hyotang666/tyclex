(in-package :vs-haskell)

(defun gensyms (list)
  (loop :repeat (length list)
	:collect (gensym)))

(defun envar(thing)
  (intern(format nil "?~A"thing)))

