(defpackage apicl
  (:use :cl)
  (:export :*app*
	   :mount :start :stop :serve))
(in-package :apicl)

(defvar *app* (make-instance 'ningle:<app>))

(defmacro defroute (path (&rest params) ((&rest arguments) &body body))
  `(setf (ningle:route *app* ,path ,@params)
	 #'(lambda ,arguments ,@body)))

(defun mount (app)
  (let ((*app* app))
    (defroute "/" (:method :GET)
      ((params) "ok"))))

(defun start (app)   (clack:clackup app))
(defun stop (server) (clack:stop server))

(defun serve ()
  (mount *app*)
  (start *app*))
