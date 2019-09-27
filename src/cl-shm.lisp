(defpackage cl-shm
  (:use :cl))
(in-package :cl-shm)

(export '(shmat shmdt attach-shared-memory-pointer detach-shared-memory-pointer shared-memory-pointer->stream))
;; shmat and shmdt sufficient for now
(cffi:defcfun "shmat" :pointer
  (shmid :int)
  (shmaddr :pointer)
  (shmflag :int))

(cffi:defcfun "shmdt" :int
  (shmaddr :pointer))
(let ((error-pointer (cffi:make-pointer (1- (ash 1 64)))))
  (defun attach-shared-memory-pointer (shared-memory-id &key (flags 0))
    (let ((result (shmat shared-memory-id (cffi:null-pointer) flags)))
      (if (cffi:pointer-eq result error-pointer
                           )
          (error "shmat returned (void*) -1: ~a" result)
          result))))

(defun detach-shared-memory-pointer (shared-memory-id)
  (zerop (shmdt shared-memory-id)))

(defun shared-memory-array (pointer &optional (length 100))
  (coerce 
          (loop for i from 0 below length collecting
                (cffi:mem-ref pointer :uint8 i))
          'vector))

(deftype octet () '(unsigned-byte 8))
(defclass foreign-memory-input-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((pointer :initarg :pointer :initform (cffi:null-pointer))
   (position :initform 0)))

(defmethod trivial-gray-streams::stream-element-type ((stream foreign-memory-input-stream))
  'octet)

(defmethod trivial-gray-streams::stream-read-byte ((stream foreign-memory-input-stream))
  (with-slots (pointer position) stream
    (prog1 (cffi:mem-ref pointer :uint8 position)
      (incf position))))

(defun shared-memory-pointer->stream (shared-memory-pointer)
  (make-instance 'foreign-memory-input-stream
                 :pointer shared-memory-pointer))
