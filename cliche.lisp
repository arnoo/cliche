(defpackage :cliche
  (:use     #:cl #:anaphora #:clutch #:parenscript #:cl-markup)
  (:shadowing-import-from #:clutch #:while #:join #:in #:acond #:it))

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
               (:link :rel "stylesheet" :type "text/css" :href (full-url "/style.css"))
               ; (:link :rel "apple-touch-icon" :href "/apple-touch-icon.png"))
        (:body 
	   (:div :id "lightbox"
	         (:input :placeholder "Tags, dates...")
	         (:div :id "thumbs" :contextmenu "thumbmenu" ""))
     (:menu :type "context" :id "thumbmenu"
	          (:menu :label "Add tag..." :icon "/images/share_icon.gif"
                   (:menuitem :label "X" :icon "/images/twitter_icon.gif" :onclick "add")
                   (:menuitem :label "New tag..." :icon "/images/facebook_icon16x16.gif" :onclick "")))
	   (:div :id "viewer"
	   	 (:div :id "viewer-controls"
	   	       (:div :id "viewer-back" "Back")
	   	       (:div :id "viewer-previous" "Previous")
	   	       (:div :id "viewer-next" "Next")
	   	       (:div :id "viewer-fit" "Fit"))
	   	 (:div :id "viewport"
	   	       (:div :id "viewer-image"
	   		   (:img :src "/pics/home/3790"))))
	   (:script :type "text/javascript" :src (full-url "/js/jquery-1.10.2.min.js") "")
	   (:script :type "text/javascript" :src (full-url "/js/jquery.panzoom.min.js") "")
	   (:script :type "text/javascript" :src (full-url "/js/contextMenu.min.js") "")
	   (:script :type "text/javascript" :src (full-url "/js/jquery.binarytransport.js") "")
	   (:script :type "text/javascript" :src (full-url "/js/lb.js") "")
	   (:script :type "text/javascript" (js-interface)))))))

(defun full-url (url)
  (if (boundp 'ht:*request*)
      url
      (str "http://localhost:4242" url)))

(defjs interface
   (defun full-url (url)
     (if (= (chain window location protocol)
	    "file:")
	 (+ "http://localhost:4242" url)
	 url))
    ;($.get "/pics?col=home"
    ;	   (lambda (rep)
    ;	      (chain ($ "#thumbs") (empty))
   ; 	      (loop for id in rep
  (defun view-image (col imgid)
     (chain ($ "#viewer-image img")
	    (attr "src" (+ "/pics/" col "/" imgid)))
     (chain ($ "#viewer-image")
	    (panzoom "resetDimensions")))
  (defun viewer-previous ()
    ;TODO
    )
  (defun viewer-next ()
    ;TODO
    )
  (defun viewer-fit ()
    ;TODO
    )
  (defun init ()
     (chain ($ "#viewer")
            (hide))
     (chain ($ "#viewer-image")
            (panzoom))
     (chain ($ window)
	    (resize (lambda () (chain ($ "#viewer-image")
                                 (panzoom "resetDimensions")))))
     (chain ($ "body")
            (on "click"
            	 ".thumb-box img"
            	 (lambda () (view-image "home" (chain ($ this) (data "src")))
            	       (chain ($ "#lightbox")
            		      (hide))
            	       (chain ($ "#viewer")
            	              (show)))))
     (chain ($ "#viewer-fit")      viewer-fit)
     (chain ($ "#viewer-previous") viewer-previous)
     (chain ($ "#viewer-next")     viewer-next)
     (chain ($ "#viewer-back")
            (click (lambda ()
                      (chain ($ "#viewer")
                             (hide))
                      (chain ($ "#lightbox")
                             (show)))))
     ($.ajax (create "url" "/pics/home"
                     "data" (create)
                     "success" (lambda (res) (let ((bres (new (-uint8-array res)))
                                              (newhtml ""))
                                          (loop for i from 0 below (length bres)
                                             do (loop for j from 0 to 7
                                                      do (when (= (logand (ash (aref bres i) -1)
                                                                          1)
                                                                  1)
                                                           (setf newhtml (+ newhtml
                                                                            "<div class='thumb-box'><img data-src='"
                                                                            (aref bres i)
                                                                            "'></div>")))))
                                          (chain ($ "#thumbs") (empty))
                                          (chain ($ newhtml)
                                                 (append-to ($ "#thumbs")))))
                     "dataType" "binary"
                     "responseType" "arraybuffer"))
     (initlb ($ "#thumbs") ($ window)))
   (chain ($ document)
          (ready init)))

(defun web-index-to-disk ()
    (awith "/tmp/cliche-index.html"
      (ungulp it
	      (web-index)
	      :if-exists :supersede)
      it))

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
  (let* ((elts (remove "" (split "/" (ht:request-uri*))))
	 (colname {elts -2})
	 (id {elts -1})
	 (img (t:id-file {*cols* colname} id)))
    (unless img (web-404))
    (setf (ht:content-type*) (str "image/" (t:extension img)))
    (gulp img :binary t)))

(defun api-list-cols ()
  (setf (ht:content-type*) "text/json")
  (->json (mapcar #'t:col-name (kvalues *cols*))))

(defun separate (predicate list)
    (loop for x in list
          when (funcall predicate x)
          collect x into a
          else collect x into b
          finally (return (list a b))))

(defun api-list-files ()
  (let* ((elts (remove "" (split "/" (ht:request-uri*))))
         (colname {elts -1})
         (tags (separate [char= {_ 0} #\-]
                         (split "," (ht:get-parameter "tags")))))
    (setf (ht:content-type*) "application/binary")
    (t::bitseq-to-byteseq (t:list-files {*cols* colname} :-tags {tags 0} :+tags {tags 1} :format :bitseq))))

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

