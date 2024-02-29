;;; -*- Mode: Lisp -*-

(defpackage :clwc-system
  (:use :cl :asdf))

(in-package :clwc-system)

(asdf:defsystem :clwc
  :name "Common Lisp Wayland Compositor"
  :description "A tiling wayland compositor."
  :version "0.1"
  :author "Edgar Denny <edgar1denny@gmail.com>"
  :licence "GPLv3"
  :serial t
  :depends-on (#:alexandria
               #:cffi
               #:cffi-libffi
               #:cffi-object
               #:osicat)
  :components ((:file "package")
               (:file "cwr")
               (:file "tinywl")))
