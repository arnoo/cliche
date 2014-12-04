(defpackage :cliche
  (:use     #:cl #:anaphora #:clutch #:parenscript #:cl-markup)
  (:shadowing-import-from #:clutch #:while #:join #:in))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (rename-package :hunchentoot :hunchentoot '(:ht :tnbl))
  (rename-package :cl-gd :cl-gd '(:gd))
  (rename-package :tagger :tagger '(:t)))

(in-package :cliche)

(setf cl-markup:*auto-escape* nil)

(defun ->json (lisp)
  (with-output-to-string (s)
    (json:encode-json lisp s)))

(defvar *cols* (mkhash "home" (t::make-col :name "home" :root-dir "/home/arno" :preset :IMG)))

(defmacro defjs (name &rest body)
  `(defun ,(symb "JS-" name) ()
          (parenscript:ps ,@body)))

(defun web-404 ()
  (setf (ht:return-code ht:*reply*) 404)
  (ht:abort-request-handler "Resource not found"))

(defun web-index ()
  (str
    "<!DOCTYPE HTML>"
    (html
        (:head (:meta :http-equiv "Content-type" :content "text/html;charset=UTF-8")
               ; Always force latest IE rendering engine & Chrome Frame
               (:title "Cliché")
               ; Mobile viewport optimized: j.mp/bplateviewport
               (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
               ; Place favicon.ico & apple-touch-icon.png in the root of your domain and delete these references (?)
               ; (:link :rel "shortcut icon"    :href "/favicon.ico")
               (:link :rel "stylesheet" :type "text/css" :href "/style.css")
               ; (:link :rel "apple-touch-icon" :href "/apple-touch-icon.png"))
        (:body 
	   "Hey there !"
	   (:div :id "thumbs" "")
	   ;(:div :id "viewer"
	   ;	 (:div :id "viewer-controls"
	   ;	       (:div :id "viewer-previous" "Previous")
	   ;	       (:div :id "viewer-next" "Next")
	   ;	       (:div :id "viewer-fit" "Fit"))
	   ;	 (:div :id "viewport"
	   ;	       (:div :id "viewer-image"
	   ;		(:img :src "/pics/home/3790"))))
	   (:script :type "text/javascript" :src "js/jquery-1.10.2.min.js" "")
	   (:script :type "text/javascript" :src "js/jquery.panzoom.min.js" "")
	   (:script :type "text/javascript" :src "js/lb.js" "")
	   (:script :type "text/javascript" (js-interface)))))))

(defjs interface
    ;(defun process-scroll ()
    ;  (chain ($ "html") (scroll-t--op))
    ;  (chain ($ "html") (height)))
    ;(let ((scroll-timeout nil))
    ;	(chain ($ "html")
    ;	       (scroll (lambda (evt)
    ;		  (when (= (chain evt event-phase) 3)
    ;		    (when scroll-timeout
    ;		          (window.cancel-t--imeout scroll-timeout) )
    ;		    (window.set-t--imeout 100 process-scroll))))))
    ;($.get "/pics?col=home"
    ;	   (lambda (rep)
    ;	      (chain ($ "#thumbs") (empty))
   ; 	      (loop for id in rep
    	      (loop for id from 0 to 200
    		    do (let ((div ($ (+ "<div class='thumb-box'><img data-src='/thumbs/home/" id "'></div>"))))
			 (chain div (append-t-o ($ "#thumbs")))))

  ;))
    ;(chain ($ "#thumbs img") (lazyload (create :data_attribute "src")))
    (chain ($ "#viewer-image")
	   (panzoom)))

(defun web-thumb ()
  (let* ((elts (split "/" (ht:request-uri*)))
	 (colname {elts -2})
	 (id {elts -1})
	 (img (t:id-file {*cols* colname} id)))
    (unless img (web-404))
    (setf (ht:content-type*) (str "image/" (t:extension img)))
    (gulp (get-thumb-path img) :binary t)))

(defun thumb-relpath (filename &key large)
  (str (if large "large" "normal")
       "/"
       (md5 (str "file://" filename))
       ".png"))

(defun get-thumb-path (filename &key large)
  (gd:with-image-from-file (image filename)
    (let* ((owidth  (gd:image-width  image))
           (oheight (gd:image-height image))
	   (maxwidth (if large 256 128))
	   (maxheight maxwidth))
      (when (and (< owidth (* 1.05 maxwidth))
		 (< oheight (* 1.05 maxheight))) ; 5% tolerance on size (let's not generate thumbnails needlessly)
         (return-from get-thumb-path filename))
      (awhen (shared-thumb-path filename :large large)
	(when (thumb-usable-p filename it)
         (return-from get-thumb-path it)))
      (let ((home-thumb-path (home-thumb-path filename :large large)))
	(when (and home-thumb-path
		   (thumb-usable-p filename home-thumb-path))
	   (return-from get-thumb-path home-thumb-path))
	(sh (str "convert -thumbnail " maxwidth
		 " -set Thumb::MTime '" (- (file-write-date filename) 2208988800) "'"
		 " -set Thumb::URI 'file://" filename "'"
		 " 'file://" filename "'"
		 " '" home-thumb-path "'"))
	;TODO: optipng / pngquant
	home-thumb-path))))
 

(defun home-thumb-path (filename &key large)
  (str (getenv "HOME")
       "/.thumbnails/"
       (thumb-relpath filename :large large)))

(defun shared-thumb-path (filename &key large)
  (str (shared-thumbs-rootpath filename)
       "/"
       (thumb-relpath filename :large large)))

(defun shared-thumbs-rootpath (filename)
  ;TODO
  "/tmp")

(defun thumb-usable-p (filename thumb-path)
  (unless (probe-file thumb-path)
    (return-from thumb-usable-p nil))
  (let ((thumb-mtime (first (~ "/Thumb::MTime: (.*)$/"
			       (split (str #\Newline) (sh (str "identify -verbose '" thumb-path "'")))
			       1))))
    (aand thumb-mtime
          (string= (str (- (file-write-date filename) 2208988800)) it))))

(defun web-img ()
  (let* ((elts (split "/" (ht:request-uri*)))
	 (colname {elts -2})
	 (id {elts -1})
	 (img (t:id-file {*cols* colname} id)))
    (unless img (web-404))
    (setf (ht:content-type*) (str "image/" (t:extension img)))
    (gulp img :binary t)))

(defun api-list-cols ()
  (setf (ht:content-type*) "text/json")
  (->json (mapcar #'t:col-name (kvalues *cols*))))

(defun api-list-files ()
  (let ((col (ht:get-parameter "col")))
    (setf (ht:content-type*) "text/json")
    (t:list-files {*cols* col} :format :bitseq)))

(defun start ()
;  (t:update-master-index {*cols* "home"})
  (ht:start (make-instance 'ht:easy-acceptor :port 4242)))

(setf ht:*dispatch-table*
      (list (ht:create-prefix-dispatcher "/thumbs/"         'web-thumb)
            (ht:create-regex-dispatcher "^/pics/[^/]+/.+"   'web-img)
            (ht:create-regex-dispatcher "^/pics/[^/]+(/|)$" 'api-list-files)
            (ht:create-regex-dispatcher "^/pics(/|)$"       'api-list-cols)
            (ht:create-static-file-dispatcher-and-handler   "/robots.txt" "robots.txt")
            (ht:create-static-file-dispatcher-and-handler   "/favicon.ico" "favicon.ico")
            (ht:create-static-file-dispatcher-and-handler   "/style.css" "style.css")
            (ht:create-prefix-dispatcher                    "/contact" 'page-contact)
            (ht:create-folder-dispatcher-and-handler        "/js/" "js/")
            (ht:create-regex-dispatcher "^/$"               'web-index)
            (ht:create-prefix-dispatcher ""                 'web-404) 
            ))

