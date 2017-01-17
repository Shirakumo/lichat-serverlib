#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem lichat-serverlib
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Tools to help build a server using the lichat protocol."
  :homepage "https://github.com/Shirakumo/lichat-serverlib"
  :serial T
  :components ((:file "package")
               (:file "server-objects")
               (:file "server-operations")
               (:file "connection-maintenance")
               (:file "update-handlers")
               (:file "documentation"))
  :depends-on (:lichat-protocol
               :crypto-shortcuts
               :documentation-utils))
