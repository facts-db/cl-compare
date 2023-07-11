;; cl-compare - generic comparison functions
;; Copyright 2022,2023 Thomas de Grivel <thodg@kmx.io>
;;
;; Permission is hereby granted to use this software granted
;; the above copyright notice and this permission paragraph
;; are included in all copies and substantial portions of this
;; software.
;;
;; THIS SOFTWARE IS PROVIDED "AS-IS" WITHOUT ANY GUARANTEE OF
;; PURPOSE AND PERFORMANCE. IN NO EVENT WHATSOEVER SHALL THE
;; AUTHOR BE CONSIDERED LIABLE FOR THE USE AND PERFORMANCE OF
;; THIS SOFTWARE.

(defpackage :compare
  (:use :cl)
  (:export #:compare))

(in-package :compare)

(defvar *types-order*
  (list 'null
        'character
        'number
        'package
        'symbol
        'string
        'vector
        'pathname))

(defgeneric compare (a b))

(defun princ-compare (a b)
  (compare (princ-to-string a) (princ-to-string b)))

(defmethod compare (a b)
  (dolist (type *types-order*)
    (let ((ta (typep a type))
          (tb (typep b type)))
      (cond ((and ta tb)
             (princ-compare a b))
            ((not ta) (return-from compare -1))
            ((not tb) (return-from compare 1)))))
  (princ-compare a b))

(defmethod compare ((a number) (b number))
  (cond ((< a b) -1)
        ((= a b) 0)
        (t 1)))

(defmethod compare ((a package) (b package))
  (compare (package-name a)
           (package-name b)))

(defmethod compare ((a symbol) (b symbol))
  (ecase (compare (symbol-package a) (symbol-package b))
    (-1 -1)
    (1 1)
    (0 (compare (symbol-name a) (symbol-name b)))))

(defmethod compare ((a string) (b string))
  (cond ((eq a b) 0)
        ((string< a b) -1)
        ((string= a b) 0)
        (t 1)))

(defmethod compare ((a cons) (b cons))
  (ecase (compare (car a) (car b))
    (-1 -1)
    (1 1)
    (0 (compare (cdr a) (cdr b)))))

(defmethod compare ((a vector) (b vector))
  (let ((len-a (length a))
        (len-b (length b)))
    (dotimes (i (min len-a len-b))
      (let ((ai (elt a i))
            (bi (elt b i)))
        (ecase (compare ai bi)
          (-1 (return-from compare -1))
          (1 (return-from compare 1))
          (0 nil))))
    (compare len-a len-b)))

(defun pathname-string (p)
  (declare (type pathname p))
  (with-output-to-string (s)
    (print-object p s)))

(defmethod compare ((a pathname) (b pathname))
  (compare (pathname-string a) (pathname-string b)))
