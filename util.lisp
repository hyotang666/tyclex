(in-package :vs-haskell)

(defun gensyms(num)
  (loop :repeat num :collect (gensym)))

