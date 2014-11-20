(defpackage :cliche
    (:use     #:cl #:anaphora #:clutch #:cl-markup))

(in-package :cliche)

(setf cl-markup:*auto-escape* nil)

(defvar *cols* (mkhash "home" (tagger::make-col :root-dir "/home/arno" :preset :IMG)))

(hunchentoot:define-easy-handler (list-cols :uri "/") ()
  
  )

(hunchentoot:define-easy-handler (list-cols :uri "/cols") ()
  (setf (hunchentoot:content-type*) "text/json")
  (json:encode-json (mapcar #'tagger::col-name *cols*)))

(hunchentoot:define-easy-handler (list-files :uri "/pics") (col)
  (setf (hunchentoot:content-type*) "text/plain")
  (json:encode-json (mapcar #'str (tagger::list-all-files {*cols* col}))))

(defun start ()
    (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242)))

