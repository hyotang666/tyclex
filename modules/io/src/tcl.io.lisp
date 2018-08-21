(defpackage :tcl.io
  (:use :cl #:tyclex.dsl.defio #:tyclex.dsl.defdata #:tyclex.objects.io-action)
  (:export
    #:put-char #:put-string #:put-string-line #:put-form
    #:get-char #:get-line #:get-form
    #:forever #:interact
    #:bracket
    #:io-mode #:open-file #:h-close #:with-file
    ))
(in-package :tcl.io)

;;;; PUTS
(defio(put-char((char character))null)
  (write-char char)(values))

(defio(put-string((string string))null)
  (write-string string)(force-output)(values))

(defio(put-string-line((string string))null)
  (write-line string)(values))

(defio(put-form((arg t))null)
  (print arg)(values))

;;;; GET
(defio(get-char()character)
  (values(read-char)))

(defio(get-line()string)
  (values(read-line)))

(defio(get-form()T)
  (values(read)))

;;;; OTHERS
(defio(forever((io io-action))null)
  (loop (funcall io)))

(defio(interact(&optional((function #'identity)function))null)
  (loop :for content = (read-line nil nil)
	:while content
	:do (write-line(funcall function content))))

(defio(bracket ((prologue io-action)
		(epilogue (function(t)io-action))
		(body(function(t)io-action)))
	       T)
  (let(handle)
    (unwind-protect(progn (setf handle(funcall prologue))
			  (funcall(funcall body handle)))
      (funcall epilogue handle))))

(defdata io-mode()
  :read-mode :write-mode :append-mode :read-write-mode)

(defio(open-file ((path trivial-types:pathname-designator)
		  (mode io-mode))
		 stream)
  (apply #'open path (ecase mode
		       (:read-mode)
		       (:write-mode `(:direction :output :if-does-not-exist :create :if-exists :supersede))
		       (:append-mode `(:direction :output :if-exists :append :if-does-not-exist :create))
		       (:read-write-mode `(:direction :io :if-does-not-exist :create :if-exists :supersede)))))

(defio(h-close ((handle stream))T)
  (close handle))

(defio(with-file ((file-path trivial-types:pathname-designator)
		  (io-mode io-mode)
		  (function(function(stream)io-action)))
		 T)
  (let((*terminal-io* *terminal-io*))
    (unwind-protect(progn (setq *terminal-io* (apply #'open file-path (ecase io-mode
									(:read-mode)
									(:write-mode `(:DIRECTION :OUTPUT))
									(:append-mode `(:DIRECTION :OUTPUT :IF-EXISTS :APPEND))
									(:read-write-mode `(:DIRECTION :IO)))))
			  (funcall(funcall function *terminal-io*)))
      (close *terminal-io*))))

