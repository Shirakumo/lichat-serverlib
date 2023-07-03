(asdf:defsystem lichat-serverlib
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tools to help build a server using the lichat protocol."
  :homepage "https://Shirakumo.github.io/lichat-serverlib/"
  :bug-tracker "https://github.com/Shirakumo/lichat-serverlib/issues"
  :source-control (:git "https://github.com/Shirakumo/lichat-serverlib.git")
  :serial T
  :components ((:file "package")
               (:file "server-objects")
               (:file "server-operations")
               (:file "connection-maintenance")
               (:file "update-handlers")
               (:file "emotes")
               (:file "documentation"))
  :depends-on (:lichat-protocol
               :crypto-shortcuts
               :trivial-mimes
               :documentation-utils))
