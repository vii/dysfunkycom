(asdf:defsystem :dysfunkycom
  :name "dysfunkycom"
  :author "dysfunkycom"
  :version "prerelease"
  :description "ICFP 2009"

  :components (
	       (:module :src
			:components (
				     (:file "package")
				     (:file "simulate" :depends-on ("package")))))

  :depends-on (
	       :iterate
	       :alexandria))


