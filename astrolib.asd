(defsystem astrolib
  :description "CL wrapper for an astronomical library"
  :author "Kari Lentz  <karilentz@att.net>"
  :components
  ((:module src
    :components
    ((:file "astrolib"))))
  :depends-on (cffi utility))
