(defsystem "newt-physics-tests"
  :description "Newt game engine physics tests"
  :version "0.1"
  :author "Jens Thiede"
  :licence "BSD-Style License"
  :depends-on ("newt-physics" "xlunit")
  :components ((:file "physics_tests_package")
               (:file "test_functions"
                :depends-on ("physics_tests_package"))
               (:file "tools_test"
                :depends-on ("physics_tests_package"))
               (:file "unit_vector_test"
                :depends-on ("physics_tests_package"))
               (:file "directional_vector_test"
                :depends-on ("physics_tests_package"))
               (:file "interval_test"
                :depends-on ("physics_tests_package"))
               (:file "point_test"
                :depends-on ("physics_tests_package"))
               (:file "shape_test"
                :depends-on ("physics_tests_package" "test_functions"))
               (:file "matter_test"
                :depends-on ("physics_tests_package" "test_functions"))))