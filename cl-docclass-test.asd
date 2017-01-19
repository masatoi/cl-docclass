#|
  This file is a part of cl-docclass project.
|#

(in-package :cl-user)
(defpackage cl-docclass-test-asd
  (:use :cl :asdf))
(in-package :cl-docclass-test-asd)

(defsystem cl-docclass-test
  :author ""
  :license ""
  :depends-on (:cl-docclass
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-docclass"))))
  :description "Test system for cl-docclass"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
