;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Partial implementation of System V shared memory API in Common Lisp

(defpackage cl-shm
  (:use :cl))
(in-package :cl-shm)

(export '(shmat shmdt attach-shared-memory-pointer detach-shared-memory-pointer
          shared-memory-pointer->stream))

;;; Expose shared memory system calls to CFFI

;; shmat and shmdt sufficient for now.
;; Exposing shmctl and shmget probably needs some grovelling of system-dependent headers
(cffi:defcfun "shmat" :pointer
  (shmid :int)
  (shmaddr :pointer)
  (shmflag :int))

(cffi:defcfun "shmdt" :int
  (shmaddr :pointer))

;;; Higher level API for shared memory

(let ((error-pointer (cffi:make-pointer (1- (ash 1 64)))))
  (defun attach-shared-memory-pointer (shared-memory-id &key (flags 0))
    "Attaches the shared memory segment identified by shared-memory-id to the current lisp process and returns a pointer to it."
    (let ((result (shmat shared-memory-id (cffi:null-pointer) flags)))
      (if (cffi:pointer-eq result error-pointer)
          (error "shmat returned (void*) -1: ~a" result)
          result))))

(defun detach-shared-memory-pointer (shared-memory-id)
  "Detaches the shared memory segment that shared-memory-id points to. Returns T in case of success, otherwise NIL."
  (zerop (shmdt shared-memory-id)))

(defun shared-memory-array (pointer &optional (length 100))
  (coerce 
          (loop for i from 0 below length collecting
                (cffi:mem-ref pointer :uint8 i))
          'vector))

;;; Custom stream class to allow treating shared memory as a stream.

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

;;; Convenience method for accessing shared memory as a stream

(defun shared-memory-pointer->stream (shared-memory-pointer)
  "Returns a stream that reads foreign memory starting from shared-memory-pointer."
  (make-instance 'foreign-memory-input-stream :pointer shared-memory-pointer))
