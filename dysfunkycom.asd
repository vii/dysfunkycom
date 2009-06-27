(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cl-fad))

(loop for addon in (remove-if-not 'cl-fad:directory-pathname-p (cl-fad:list-directory "addons"))
      do
      (pushnew addon asdf:*central-registry* :test #'equalp))

(asdf:defsystem :dysfunkycom
  :name "dysfunkycom"
  :author "dysfunkycom"
  :version "prerelease"
  :description "ICFP 2009"

  :components (
	       (:module :src
			:components (
				     (:file "package")
				     (:file "simulate" :depends-on ("package"))
				     (:file "visualise" :depends-on ("package")))))

  :depends-on (
	       :iterate
	       :alexandria
	       :cl-fad
	       :lispbuilder-sdl))


