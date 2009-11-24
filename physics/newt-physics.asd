(defsystem "newt-physics"
  :description "Newt game engine physics"
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :components ((:file "physics_package")
               (:file "tools"
                :depends-on ("physics_package"))
               (:file "unit_vector"
                :depends-on ("physics_package" "tools"))
               (:file "presence"
                :depends-on ("physics_package" "unit_vector"))))