(in-package :vs-haskell)

(defun gensyms (list)
  (loop :repeat (length list)
	:collect (gensym)))

