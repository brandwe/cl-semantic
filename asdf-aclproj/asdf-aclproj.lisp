 ;;; Copyright 2006 by B. Brandon Werner
 ;;;
 ;;; Licensed under the Apache License, Version 2.0 (the \"License\");
 ;;; you may not use this file except in compliance with the License.
 ;;; You may obtain a copy of the License at
 ;;;
 ;;; http://www.apache.org/licenses/LICENSE-2.0
 ;;; 
 ;;; Unless required by applicable law or agreed to in writing, software
 ;;; distributed under the License is distributed on an \"AS IS\" BASIS,
 ;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 ;;; See the License for the specific language governing permissions and
 ;;; limitations under


(defpackage :asdf-aclproj-impl
  (:use :cl)
  (:export #:lpr-project-file
	   #:define-project
	   #:project
	   #:module
	   #:project-module))

(defpackage :asdf-aclproj
  (:use :asdf-aclproj-impl)
  (:export #:lpr-project-file))

(defpackage :asdf-aclproj-user
  (:use :cl)
  (:import-from :asdf-aclproj
		#:define-project
		#:project
		#:module
		#:project-module))

(in-package :asdf-aclproj-impl)

;;;
;;; Classes for ASDF
;;;
;;;; For use in .asd files

(defclass lpr-project-file (asdf:module)
  ((modules :initform () :accessor project-modules))
  (:documentation
   "Use this as a component in an asdf system definition to include an lpr file."))

;; Ugly hack because otherwise asdf::parse-component-form will clobber our
;; components list.
(defmethod asdf:module-components ((project lpr-project-file))
  (project-modules project))

;;;; For use in .lpr files

(defclass lpr-component () ())
(defclass module (lpr-component asdf:cl-source-file) ())
(defclass project-module (lpr-component lpr-project-file) ())

;; Because we allow goofy names like ../foo/bar.lisp, we want to return the
;; truename of the pathname, if it exists, so children will be merged correctly.
;; I think this is a workaround for a bug in SBCL's pathname merging, but the
;; bug could be in my understanding of CL pathnames -tfb
(defmethod asdf:component-pathname ((component lpr-component))
  (let ((path (call-next-method)))
    (or (probe-file path) path)))

#+unix
;; Translate from MSWindows-style path seperators
(defmethod asdf:component-name ((component lpr-component))
  (substitute #\/ #\\ (call-next-method)))

(defmethod asdf:component-relative-pathname ((project lpr-project-file))
  (make-pathname :name (asdf:component-name project)
		 :type "lpr"
		 :host (pathname-host
			(if (asdf:component-parent project)
			    (asdf:component-pathname
			     (asdf:component-parent project))
			    *default-pathname-defaults*))))

(defmethod asdf:component-relative-pathname ((component module))
  (let* ((namestring (asdf:component-name component))
	 (pathname (let ((*default-pathname-defaults*
			  (asdf:component-pathname
			   (asdf:component-parent component))))
		     (parse-namestring namestring))))
    (flet ((pathname-relativep (path)
	     (not (eql (car (pathname-directory path)) :absolute))))
      (assert (pathname-relativep pathname)
	      (pathname namestring)))
    pathname))

;;;
;;; Communicating the contents of the define-project form back to asdf
;;;
;;;; on the asdf side

(defvar *project-file* "(no project file is being loaded)")
(defvar *project-modules* ())
(defvar *this-project* nil)

(defun load-project (project)
  (let ((*project-file* (asdf:component-pathname project))
	(*project-modules* ())
	(*this-project* project))
    (ensure-cgu-package)
    (load *project-file*)
    (setf (project-modules project) *project-modules*)))

(defmethod reinitialize-instance :after ((project lpr-project-file) &rest args)
  (declare (ignorable args))
  (load-project project))

(defmethod initialize-instance :after ((project project-module) &rest args)
  (declare (ignorable args))
  (load-project project))

(defmethod initialize-instance :around ((component lpr-component) &rest args)
  (let ((args (if (getf args :parent)
		  args
		  (list* :parent *this-project* args))))
    (apply #'call-next-method component args)))

(defun ensure-cgu-package ()
  #+(and acl mswindows) (progn (aclwin-warning)
			       (return-from ensure-cgu-package))
  (unless (find-package "COMMON-GRAPHICS-USER")
    (let ((user-package (find-package "ASDF-ACLPROJ-USER")))
      (assert (not (null user-package)))
      (rename-package user-package "ASDF-ACLPROJ-USER"
		      (list* "COMMON-GRAPHICS-USER"
			     (package-nicknames user-package))))))

(defun aclwin-warning ()
  (cerror
   "Continue loading project file."
   "About to load an lpr project file using asdf-aclproj: ~A

This probably won't work if the project file was generated with
Allegro's GUI tool.  Normally these files start with
an (in-package :common-graphics-user) form.  In other Lisps, we
make COMMON-GRAPHICS-USER a nickname for the ASDF-ACLPROJ-USER
package.  If you wish to continue using asdf to load this
project, edit the project file to be in the ASDF-ACLPROJ-USER
package."
   *project-file*))

;;;; on the lpr side

(defmacro define-project (&key projects modules &allow-other-keys)
  `(let ((modules ,modules)
	 (projects ,projects))
     (process-project projects modules)))

(defun process-project (projects modules)
  (labels ((serialize (list)
	     (loop with so-far = ()
		   for component in list
		   for depends-on = (reverse so-far)
		   do (setf (slot-value component 'asdf::do-first)
			    `((asdf:compile-op (asdf:load-op ,@depends-on)))
			    (slot-value component 'asdf::in-order-to)
			    `((asdf:load-op (asdf:load-op ,@depends-on))
			      (asdf:compile-op (asdf:compile-op ,@depends-on))))
		   (push component so-far)
		   finally (return (reverse so-far)))))
    (setf *project-modules*
	  (serialize (append projects modules)))))

;;; Misc

(defmethod asdf::component-self-dependencies ((o asdf:operation) (c lpr-component))
  (let ((all-deps (asdf:component-depends-on o c)))
    (remove-if-not (lambda (x) (member c (cdr x)))
		   all-deps)))

;; FIXME: asdf is compiling the source, then loading the source, not
;; FIXME: the fasl.  This is gross, but it at least causes asdf to load
;; FIXME: the fasl, until I track down why it's getting the wrong pathname.
(defmethod asdf::input-files ((op asdf:load-op) (c module))
  (list (compile-file-pathname (asdf:component-pathname c))))
