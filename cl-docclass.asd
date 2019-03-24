#|
  This file is a part of cl-docclass project.
|#

(in-package :cl-user)
(defpackage cl-docclass-asd
  (:use :cl :asdf))
(in-package :cl-docclass-asd)

(defsystem cl-docclass
  :version "0.1"
  :author "Satoshi Imai"
  :license "MIT"
  :depends-on (:igo :cl-online-learning :alexandria)
  :components ((:module "src"
                :components
                ((:file "cl-docclass"))))
  :description "Document classification using cl-online-learning"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-docclass-test))))
