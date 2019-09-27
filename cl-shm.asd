#|
  This file is a part of cl-shm project.
  Copyright (c) 2019 Selwyn Simsek
|#

#|
  System V shared memory API in Common Lisp

  Author: Selwyn Simsek
|#

(defsystem "cl-shm"
  :version "0.1.0"
  :author "Selwyn Simsek"
  :license ""
  :depends-on ("cffi" "trivial-gray-streams")
  :components ((:module "src"
                :components
                ((:file "cl-shm"))))
  :description "System V shared memory API in Common Lisp"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "cl-shm-test"))))
