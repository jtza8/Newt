(defsystem "newt-physics"
  :description "Newt game engine physics"
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :components ((:file "physics_package")
               (:file "presence" :depends-on ("physics_package"))))