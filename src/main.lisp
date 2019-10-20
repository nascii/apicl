(defpackage apicl
  (:use :cl)
  (:export :*app*
	   :mount :start :stop :serve))
(in-package :apicl)

(defvar *app* (make-instance 'ningle:<app>))

(defmacro defroute (app path params ((&rest arguments) &rest body))
  `(setf (ningle:route ,app ,path ,@params)
	 #'(lambda ,arguments ,@body)))

(defun mount (app)
  (defroute app "/" (:method :GET)
	 ((params) "ok")))

(defun start (app)   (clack:clackup app))
(defun stop (server) (clack:stop server))


(defun serve ()
  (mount *app*)
  (start *app*))
