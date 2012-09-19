;;; Nothing special about the "CFFI-USER" package.  We're just
;;; using it as a substitute for your own CL package.
(defpackage :astrolib
  (:export :make-mjd 
	   :sun-pos 
	   :gst
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

(defstruct astro-date mjd)
(defstruct astro-vector eq)

(defun astro-date(year month day &optional (hour 0) (minutes 0) (seconds 0) (dst-p nil) (tz 0))
  (with-foreign-object (mjd :double)
    (let ((dy (+ day (/ (+ hour (/ minutes 60) (/ seconds 3600) ) 24))))
      (cal-mjd month (coerce dy 'double-float) year mjd) 
      (make-astro-date :mjd (+ (mem-ref mjd :double) (/ (+ tz (if dst-p -1 0)) 24))))))

(defun astro-date-parts( astro-date &optional (tz 0) (dst-p nil))
  (with-foreign-objects ((mn :int) (dy :double) (yr :int))
    (let ((mjd (add (- (astro-date-mjd astro-date) (/ tz 24)) (if dst-p 1.0d0 0.0d0)))) 
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

(defmethod print-object ((dt astro-date) s)
  (multiple-value-bind (year month day hour minute second)(astro-date-parts dt)
    (format s "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d UTC <astro-date>" year month day hour minute second)))

(defun astro-date-now()
  (multiple-value-bind (seconds minutes hours day month year dow dst-p tz) (get-decoded-time) (when dow (astro-date year month day hours minutes seconds dst-p tz))))

(defun gst(&optional astro-date)
  (let ((mjd (astro-date-mjd (or astro-date (astro-date-now)))))
    (with-foreign-objects ((gst :double))
      (multiple-value-bind (whole fraction)(floor (+ mjd 0.5))
	(utc-gst (coerce (- whole 0.5) 'double-float) (coerce (* fraction 24) 'double-float) gst)
	(mem-ref gst :double)))))

(defun astro-vector-from-eq(r dec ra)
  (let ((cos-dec (cos dec)))
    (make-astro-vector :eq (vector (* r cos-dec (cos ra)) (* r cos-dec (sin ra)) (* r (sin dec))))))

(defun astro-vector-from-ecp(r lat long mj)
  (with-foreign-objects ((pra :double) (pdec :double))
    (ecl-eq mj lat long pra pdec)
    (let ((ra (mem-ref pra :double))
	  (dec (mem-ref pdec :double)))
      (let ((cos-dec (cos dec)))
	(make-astro-vector :eq (vector (* r cos-dec (cos ra)) (* r cos-dec (sin ra)) (* r (sin dec))))))))

(defun astro-vector-to-eq-cart( astro-vector-o )
  (astro-vector-eq astro-vector-o))

(defmethod add( (x astro-vector) (y astro-vector) )
  (map 'vector (lambda(xx yy) (add xx yy)) (astro-vector-eq x) (astro-vector-eq y)))

(defmethod print-object ((avo astro-vector) s)
  (format s "~a <equatorial coords>" (astro-vector-eq avo)))
 
(defun sun-pos(&optional astro-date)
  (let ((mj (astro-date-mjd (or astro-date (astro-date-now)))))
    (with-foreign-objects ((lsn :double) (rsn :double) (bsn :double))
      (sunpos mj lsn rsn bsn)
      (astro-vector-from-ecp 
       (mem-ref rsn :double)
       (mem-ref lsn :double)
       (mem-ref bsn :double)
       mj))))

