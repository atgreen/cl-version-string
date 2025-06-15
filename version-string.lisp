;;; version-string.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(defpackage #:version-string
  (:use #:cl)
  (:export #:make-version-string #:get-base-version #:define-version-parameter))

(in-package #:version-string)

(defun get-git-hash (&optional (length 7))
  "Get the current git commit hash (short form by default)"
  (handler-case
      (string-trim '(#\Newline #\Return #\Space)
                   (with-output-to-string (stream)
                     (sb-ext:run-program "git"
                                       (list "rev-parse"
                                             (format nil "--short=~d" length)
                                             "HEAD")
                                       :output stream
                                       :error nil
                                       :search t)))
    (error () nil)))

(defun get-git-dirty-p ()
  "Check if the working directory has uncommitted changes"
  (handler-case
      (let ((output (with-output-to-string (stream)
                      (sb-ext:run-program "git"
                                        '("status" "--porcelain")
                                        :output stream
                                        :error nil
                                        :search t))))
        (not (string= (string-trim '(#\Newline #\Return #\Space) output) "")))
    (error () nil)))

(defun get-base-version (system)
  "Extract version from the .asd file"
  (handler-case
      (asdf:component-version (asdf:find-system system))
    (error () "0.0.0")))

(defun make-version-string (&key include-git-p)
  "Create a version string, optionally including git information"
  (let ((base-version (get-base-version)))
    (if include-git-p
        (let ((git-hash (get-git-hash))
              (dirty-p (get-git-dirty-p)))
          (format nil "~a~@[-g~a~]~:[~;+dirty~]"
                  base-version
                  git-hash
                  dirty-p))
        base-version)))

(defmacro define-version-parameter (symbol system)
  `(progn
     (defparameter ,symbol (version-string:get-base-version ,system))
     (defmethod asdf:perform :before ((op asdf:program-op)
                                      (system (eql (asdf:find-system ,system))))
       (setf ,symbol (version-string:make-version-string :include-git-p t)))))
