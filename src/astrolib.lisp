;;; Nothing special about the "CFFI-USER" package.  We're just
;;; using it as a substitute for your own CL package.
(defpackage :astrolib
  (:export :make-mjd 
	   :sun-pos 
	   :gst
	   :mjd
	   :astro-date-now
	   :astro-date
	   :astro-vector-eq)
  (:use :common-lisp :cffi :utility))
   
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

(defcenum planet-codes
  :mercury
  :venus
  :mars
  :jupiter
  :saturn
  :uranus
  :neptune
  :pluto
  :sun
  :moon
  :nobj	)

(defcfun "plans" :void (mj :double) (p :int) (lpd0 :pointer) (psi0 :pointer)(rp0 :pointer)(rho0 :pointer)(lam :pointer)(bet :pointer)(dia :pointer)(mag :pointer))

;/* given a modified Julian date, mj, and a planet, p, find:
; *   lpd0: heliocentric longitude, 
; *   psi0: heliocentric latitude,
; *   rp0:  distance from the sun to the planet, 
; *   rho0: distance from the Earth to the planet,
; *         none corrected for light time, ie, they are the true values for the
; *         given instant.
; *   lam:  geocentric ecliptic longitude, 
; *   bet:  geocentric ecliptic latitude,
; *         each corrected for light time, ie, they are the apparent values as
; *	   seen from the center of the Earth for the given instant.
; *   dia:  angular diameter in arcsec at 1 AU, 
; *   mag:  visual magnitude
; *
; * all angles are in radians, all distances in AU.
; *
; * corrections for nutation and abberation must be made by the caller. The RA 
; *   and DEC calculated from the fully-corrected ecliptic coordinates are then
; *   the apparent geocentric coordinates. Further corrections can be made, if
; *   required, for atmospheric refraction and geocentric parallax.
; */
;void
;plans (double mj, PLCode p, double *lpd0, double *psi0, double *rp0,
;double *rho0, double *lam, double *bet, double *dia, double *mag)

(defgeneric add(x y))

(defstruct astro-date mjd)
(defstruct astro-vector eq)

(defun astro-date(year month day &optional (hour 0) (minutes 0) (seconds 0) (dst-p nil) (tz 0))
  (with-foreign-object (mjd :double)
    (let ((dy (+ day (/ (+ hour (/ minutes 60) (/ seconds 3600) ) 24))))
      (cal-mjd month (coerce dy 'double-float) year mjd) 
      (make-astro-date :mjd (+ (mem-ref mjd :double) (/ (+ tz (if dst-p -1.0 0.0)) 24.0))))))

(defun astro-date-parts( astro-date &optional (tz 0) (dst-p nil))
  (with-foreign-objects ((mn :int) (dy :double) (yr :int))
    (let ((mjd (+ (- (astro-date-mjd astro-date) (/ tz 24)) (if dst-p 1.0d0 0.0d0)))) 
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
       (mem-ref bsn :double)
       (mem-ref lsn :double)
       mj))))

(defmacro call-reference((&rest param-specs) function-call)
  `(with-foreign-objects ,param-specs
     ,function-call 
     (values ,@(loop for (name type) in param-specs collecting `(mem-ref ,name ,type)))))

(defun ephemeris(planet-code &optional astro-date)
  (let ((mj (astro-date-mjd (or astro-date (astro-date-now)))))
    (call-reference 
     ((heliocentric-longitude :double)(heliocentric-latitude :double)(sun-distance :double)(earth-distance :double)(geocentric-longitude :double)(geocentric-latitude :double)(angular-diameter :double)(visual-magnitude :double)) 
     (plans mj (foreign-enum-value 'planet-codes planet-code) heliocentric-longitude heliocentric-latitude sun-distance earth-distance geocentric-longitude geocentric-latitude angular-diameter visual-magnitude))))

(defun test()
  (macroexpand-1
   '(call-reference 
    ((heliocentric-longitude :double)(heliocentric-latitude :double)(sun-distance :double)(earth-distance :double)(geocentric-longitude :double)(geocentric-latitude :double)(angular-diameter :double)(visual-magnitude :double)) 
     (plans mj planet-code heliocentric-longitude heliocentric-latitude sun-distance earth-distance geocentric-longitude geocentric-latitude angular-diameter visual-magnitude)))) 

(defun map-nth(nth zipped-list &optional (function #'identity))
  (mapcar (lambda(row)(funcall function (nth nth row))) zipped-list))

;; (defmacro foreign-call((&rest input-param-bindings) (&rest output-param-specs) function-call)
;;   (append 
;;    (loop for (name value) in input-param-bindings collecting name)
;;    (loop for (name type) in output-param-specs collecting name)

;;   `(with-foreign-objects ,param-specs
;;      ,function-call 
;;      (values ,@(loop for (name type) in param-specs collecting `(mem-ref ,name ,type)))))
