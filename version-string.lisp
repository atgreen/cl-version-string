;;; version-string.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(defpackage #:version-string
  (:use #:cl)
  (:export #:make-version-string #:get-base-version #:define-version-parameter))

(in-package #:version-string)

(defun get-git-describe ()
  "Get version info from git describe (e.g., v1.0.0-14-g2414721)"
  (handler-case
      (let ((output (string-trim '(#\Newline #\Return #\Space)
                                 (uiop:run-program '("git" "describe" "--tags" "--always")
                                                   :output :string
                                                   :error-output nil
                                                   :ignore-error-status t))))
        (if (zerop (length output)) nil output))
    (error () nil)))

(defun get-git-dirty-p ()
  "Check if the working directory has uncommitted changes"
  (handler-case
      (let ((output (uiop:run-program '("git" "status" "--porcelain")
                                     :output :string
                                     :error-output nil
                                     :ignore-error-status t)))
        (not (string= (string-trim '(#\Newline #\Return #\Space) output) "")))
    (error () nil)))

(defun get-base-version (system)
  "Extract version from the .asd file"
  (handler-case
      (asdf:component-version (asdf:find-system system))
    (error () "0.0.0")))

(defun make-version-string (system &key include-git-p)
  "Create a version string, optionally including git information"
  (let ((base-version (get-base-version system)))
    (if include-git-p
        (let ((git-describe (get-git-describe))
              (dirty-p (get-git-dirty-p)))
          (if git-describe
              (format nil "~a~:[~;+dirty~]" git-describe dirty-p)
              base-version))
        base-version)))

(defmacro define-version-parameter (symbol system)
  `(progn
     (defparameter ,symbol (version-string:get-base-version ,system))
     (defmethod asdf:perform :before ((op asdf:program-op)
                                      (system (eql (asdf:find-system ,system))))
       (setf ,symbol (version-string:make-version-string ,system :include-git-p t)))))
