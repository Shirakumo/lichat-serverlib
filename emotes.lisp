#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.serverlib)

(defvar *emotes* (make-hash-table :test 'equalp))
(defvar *allowed-emote-content-types* '("image/png" "image/jpeg" "image/gif"))

(defun emote (name)
  (gethash name *emotes*))

(defun (setf emote) (emote name)
  (destructuring-bind (content-type data) (coerce-emote emote)
    (unless (find content-type *allowed-emote-content-types* :test #'string-equal)
      (error "The content-type ~s is not allowed for emotes." content-type))
    (setf (gethash name *emotes*) (list content-type (coerce-emote-data data)))))

(defun add-emote (pathname)
  (setf (emote (pathname-name pathname)) pathname))

(defun add-emotes (directory)
  (dolist (path (directory (make-pathname :name :wild
                                          :type :wild
                                          :defaults directory)))
    (with-simple-restart (continue "Don't add ~a." path)
      (add-emote path))))

(defun remove-emote (name)
  (remhash name *emotes*))

(defun coerce-emote (emote)
  (etypecase emote
    (list
     emote)
    (pathname
     (list (trivial-mimes:mime-lookup emote)
           emote))))

(defun coerce-emote-data (data)
  (etypecase data
    (string
     data)
    (vector
     (cryptos:to-base64 data))
    (stream
     (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)
                                    :adjustable T
                                    :fill-pointer 0)))
       (loop for i from 0 by 4096
             for read = (read-sequence buffer data :start i :end (+ i 4096))
             until (< read 4096)
             finally (setf (fill-pointer buffer) (+ i read)))
       (coerce-emote-data buffer)))
    (pathname
     (with-open-file (stream data :direction :input
                                  :element-type '(unsigned-byte 8))
       (coerce-emote-data stream)))))

(define-update-handler emotes (connection update)
  (check-from connection update)
  (check-permitted connection update)
  (let ((needed (loop for name being the hash-keys of *emotes*
                      unless (find name (lichat-protocol:names update) :test #'string-equal)
                      collect name)))
    (dolist (name needed)
      (destructuring-bind (content-type payload) (emote name)
        (send! connection 'emote
               :name name
               :content-type content-type
               :payload payload)))))
