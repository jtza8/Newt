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
               (:file "directional_vector"
                :depends-on ("physics_package"))
               (:file "point"
                :depends-on ("physics_package"))
               (:file "shape"
                :depends-on ("physics_package" "unit_vector"
                             "directional_vector" "point"))
               (:file "matter"
                :depends-on ("physics_package" "unit_vector"
                             "directional_vector" "point" "shape"))))