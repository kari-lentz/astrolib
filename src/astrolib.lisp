;;; Nothing special about the "CFFI-USER" package.  We're just
;;; using it as a substitute for your own CL package.
(defpackage :astrolib
  (:shadow cl:+)
  (:export :make-mjd 
	   :sun-pos 
	   :gst
	   + 
	   :mjd
	   :make-astro-date-now)
  (:use :common-lisp :cffi))
   
(in-package :astrolib)

(pushnew #P"/usr/local/lib/" *foreign-library-directories*
           :test #'equal)

(define-foreign-library libastro
  (:unix (:or "libastro.so.1" "libastro.so"))
  (t (:default "libastro")))

(use-foreign-library libastro)

;;; Initialize libcurl with FLAGS.
(defcfun "cal_mjd" :void (mn :int) (dy :double) (yr :int) (mjd :pointer))
(defcfun "mjd_cal"  :void (mjd :double) (mn :pointer) (dy :pointer) (yr :pointer))
(defcfun "utc_gst" :void (mjd :double) (utc :double) (gst :pointer))
(defcfun "eq_ecl" :void (mj :double) (ra :double) (dec :double) (lt :pointer) (lg :pointer))
(defcfun "ecl_eq" :void (mj :double) (lt :double) (lg :double) (ra :pointer) (dec :pointer))
(defcfun "sunpos" :void (mj :double) (lsn :pointer) (rsn :pointer) (bsn :pointer))

(defgeneric add(x y))

(defmethod add(x y)
  (cl:+ x y))

(defun +(&rest args)
  (reduce (lambda(x y) (add x y) )args))

(defstruct astro-date-t mjd)
(defstruct astro-vector-t eq)

(defun astro-date-t-from-parts(year month day hour minutes seconds &optional (dst-p nil) (tz 0))
  (with-foreign-object (mjd :double)
    (let ((dy (+ day (/ (+ hour (/ minutes 60) (/ seconds 3600) ) 24))))
      (cal-mjd month (coerce dy 'double-float) year mjd) 
      (make-astro-date-t :mjd (+ (mem-ref mjd :double) (/ (+ tz (if dst-p -1 0)) 24))))))

(defun astro-date-t-to-parts( astro-date &optional (tz 0) (dst-p nil))
  (with-foreign-objects ((mn :int) (dy :double) (yr :int))
    (let ((mjd (+ (- (astro-date-t-mjd astro-date) (/ tz 24)) (if dst-p 1.0d0 0.0d0)))) 
      (mjd-cal mjd mn dy yr)
      (multiple-value-bind (day daypart)(floor (mem-ref dy :double))
	(multiple-value-bind (hour hourpart) (floor (* 24 daypart))
	  (multiple-value-bind (minute minutepart) (floor (* hourpart 60)) 
	    (let ((second (* minutepart 60)))
	      (values
	       (mem-ref yr :int)
	       (mem-ref mn :int)
	       day
	       hour
	       minute
	       second))))))))

(defun astro-date-t-from-mjd( mjd )
  (make-astro-date-t :mjd mjd))

(defun astro-date-t-to-mjd( astro-date )
  (astro-date-t-mjd astro-date))

(defmethod print-object ((dt astro-date-t) s)
  (multiple-value-bind (year month day hour minute second)(astro-date-t-to-parts dt)
    (format s "~a-~a-~a ~a:~a:~a UTC" year month day hour minute second)))

(defparameter *mjd* nil)

(defun make-astro-date-now()
  (multiple-value-bind (seconds minutes hours day month year dow dst-p tz) (get-decoded-time) (when dow (astro-date-t-from-parts year month day hours minutes seconds dst-p tz))))

(defun mjd()
  (astro-date-t-mjd (or *mjd* (make-astro-date-now))))

(defun gst()
  (with-foreign-objects ((gst :double))
    (multiple-value-bind (whole fraction)(floor (+ (mjd) 0.5))
      (utc-gst (coerce (- whole 0.5) 'double-float) (coerce (* fraction 24) 'double-float) gst)
      (mem-ref gst :double))))

(defun astro-vector-t-from-eq(r dec ra)
  (let ((cos-dec (cos dec)))
    (make-astro-vector-t :eq (vector (* r cos-dec (cos ra)) (* r cos-dec (sin ra)) (* r (sin dec))))))

(defun astro-vector-t-from-ecp(r lat long)
  (with-foreign-objects ((pra :double) (pdec :double))
    (ecl-eq (mjd) lat long pra pdec)
    (let ((ra (mem-ref pra :double))
	  (dec (mem-ref pdec :double)))
      (let ((cos-dec (cos dec)))
	(make-astro-vector-t :eq (vector (* r cos-dec (cos ra)) (* r cos-dec (sin ra)) (* r (sin dec))))))))

(defun astro-vector-t-to-eq-cart( astro-vector-o )
  (astro-vector-t-eq astro-vector-o))

(defmethod add( (x astro-vector-t) (y astro-vector-t) )
  (map 'vector (lambda(xx yy) (add xx yy)) (astro-vector-t-eq x) (astro-vector-t-eq y)))

(defmethod print-object ((avo astro-vector-t) s)
  (format s "~a <equatorial coords>" (astro-vector-t-eq avo)))
 
(defun sun-pos()
  (with-foreign-objects ((lsn :double) (rsn :double) (bsn :double))
    (sunpos (mjd) lsn rsn bsn)
    (astro-vector-t-from-ecp 
     (mem-ref rsn :double)
     (mem-ref lsn :double)
     (mem-ref bsn :double))))

