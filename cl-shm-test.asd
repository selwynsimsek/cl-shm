#|
  This file is a part of cl-shm project.
  Copyright (c) 2019 Selwyn Simsek
|#

(defsystem "cl-shm-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Selwyn Simsek"
  :license ""
  :depends-on ("cl-shm"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-shm"))))
  :description "Test system for cl-shm"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
