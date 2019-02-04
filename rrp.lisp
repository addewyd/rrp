;;;
(defparameter *mf* 1.0e20)
(defvar *w* 1024)
(defvar *h* 768)

(defun pow (x n)
  (cond ((zerop n) 1)
        (t (* x (pow x (- n 1))))))

(defclass Vec3f ()
    ((v :initarg :v :initform (list 0 0 0) :accessor Vec)))

(defgeneric VecX (vec3f))
(defgeneric VecY (vec3f))
(defgeneric VecZ (vec3f))


(defmethod VecX ((CL Vec3f))
    (nth 0 (slot-value CL 'v)))

(defmethod VecY ((CL Vec3f))
    (nth 1 (slot-value CL 'v)))

(defmethod VecZ ((CL Vec3f))
    (nth 2 (slot-value CL 'v)))

(defmethod dot ((CL1 Vec3f) (CL2 Vec3f))
    (+ (* (vecx cl1) (vecx cl2)) (* (vecy cl1) (vecy cl2)) (* (vecz cl1) (vecz cl2))))
;;    (reduce #'+ (mapcar #'(lambda (x y) (* x y)) (Vec cl1) (Vec cl2))))

(defmethod norm ((CL Vec3f))
   (sqrt (dot CL CL)))

(defun CVec3f (&optional v) 
    (if (null v)
        (make-instance 'vec3f)
        (make-instance 'vec3f :v v)))

(defmethod mul ((CL Vec3f) n)
    (CVec3f (mapcar #'(lambda (x) (* x n)) (vec cl))))


(defmethod normalize ((CL Vec3f))
    (mul cl (/ 1 (norm cl))))

(defmethod vdiff ((CL1 Vec3f) (CL2 Vec3f))
    (CVec3f (mapcar #'(lambda (x y) (- x y)) (Vec cl1) (Vec cl2))))

(defmethod vadd ((CL1 Vec3f) (CL2 Vec3f))
    (CVec3f (mapcar #'(lambda (x y) (+ x y)) (Vec cl1) (Vec cl2))))
;;;


(defclass light ()
    ( (pos :initarg :pos :accessor pos :initform (CVec3f))
        (intens :initarg :intens :accessor intens :initform 0)))

(defclass material ()
    (
        (refr :initarg :refr :accessor refr :initform 1)
        (vec :initarg :vec :accessor vec :initform (CVec3f))
        (a0 :initarg :a0 :accessor a0 :initform 1)
        (a1 :initarg :a1 :accessor a1 :initform 0)
        (a2 :initarg :a2 :accessor a2 :initform 0)
        (a3 :initarg :a3 :accessor a3 :initform 0)
        (spec :initarg :spec :accessor spec :initform 0)))

(defmethod copy-vec ((m Vec3f))
    (make-instance 'Vec3f :v (list (VecX m) (VecY m) (VecZ m))))


(defmethod copy-mat ((m material))
    (make-instance 'material
        :refr (refr m)
        :a0 (a0 m)
        :a1 (a1 m)
        :a2 (a2 m)
        :a3 (a3 m)
        :spec (spec m)
        :vec (Cvec3f (list (VecX (vec m)) (Vecy (vec m)) (Vecz (vec m))))))

(defclass sphere (Vec3f)
    ((mat :initarg :mat :accessor mat :initform (make-instance 'material))
        (radius :initarg :radius :accessor radius :initform 1)))

(defmethod cr ((s sphere))
    (nth 0 (vec (vec (mat s)))))
(defmethod cg ((s sphere))
    (nth 1 (vec (vec (mat s)))))
(defmethod cb ((s sphere))
    (nth 2 (vec (vec (mat s)))))

(defmethod center ((s sphere)) (vec s))

(defmethod ray-intersect ((s sphere) (orig Vec3f) (dir Vec3f))
    (let* (
        (t0 *mf*)
        (L (vdiff (center s) orig))
        (tca (dot L dir))
        (d2 (- (dot L L) (* tca tca)))
        (rr (* (radius s) (radius s)))
        (r (> d2 rr)) )
        (if r (list nil 0)
            (let* (
                (thc (sqrt (- rr d2)))
                (t0 (- tca thc))
                (t1 (+ tca thc))
                 )
                (when (< t0 0) (setf t0 t1))
                (if (< t0 0) (list nil 0) (list t t0))))))

(defmethod reflect ((i Vec3f) (n Vec3f))
    (vdiff i (mul n (* (dot i n) 2))))

(defmethod refract ((i Vec3f) (n Vec3f) eta-t eta-i)
    (let
        (
            (cosi (- 0 (max -1 (min (dot i n) 1)))))
        (if (< cosi 0)
            (refract I (vdiff (CVec3f) n) eta-i eta-t)
            (let*
                (   (eta (/ eta-i eta-t))
                    (k  (- 1 (* eta (* eta (- 1 (* cosi cosi)))))))
                (if (< k 0)
                    (CVec3f (list 1 0 0))
                    (vadd (mul i eta) (mul n (- (* eta cosi) (sqrt k)))))))))


(defun scene-intersect (orig dir spheres)
  (let (
        (matt (make-instance 'material :vec (CVec3f(list 0.99 0 0))))
        (dist *mf*)
        (cdist *mf*)
        (hit (CVec3f))
        (n (CVec3f))  )
    (loop for s in spheres do
        ;(let*
        (progn
            ;(
                (setq rr (ray-intersect s orig dir))
                (setq dd (second rr))
            ;)
            (when (and (car rr) (< dd dist))
                (setf dist dd)
                (setf hit (vadd orig (mul dir dist)))
                (setf n (normalize (vdiff hit (center s))))
                (setf matt (copy-mat (mat s)))
            )
        )
    )
    (when (> (abs (VecY dir)) 0.001)
        (let* (
                (d  (- 0 (/ (+ (VecY orig) 4) (VecY dir))))
                (pt (vadd orig (mul dir d))))
            (when (and (> d 0) (< (abs (VecX pt)) 10) (< (VecZ pt) -10) (> (VecZ pt) -30) (< d dist))
                (setf cdist d)
                (setf hit (CVec3f (vec pt)))
                (setf n (CVec3f (list 0 1 0)))
                (setf (vec matt)
                    (if  (oddp (+ (floor (+ (/ (VecX hit) 2.0) 1000))  (floor (/ (VecZ hit) 2.0))))
                        (CVec3f (list 0.3 0.3 0.3))
                        (CVec3f (list 0.1 0.2 0.3)))))))
    (list (< (min dist cdist) 1000) hit n matt)))


(defun cast-ray (orig dir spheres lights depth)
  (let 
    ((si (scene-intersect orig dir spheres)))
    (if (or (> depth 4) (null (nth 0 si)))
        (CVec3f (list 0.8 0.7 0.2))
        (let* (     (dli 0)
                    (sli 0)
                    (reflect-color (CVec3f))
                    (refract-color (CVec3f))

                    (n (copy-vec (nth 2 si)))
                    (mat (copy-mat (nth 3 si)))
                    (point (copy-vec (nth 1 si)))
                    (reflect-dir (normalize (reflect dir n)))
                    (refract-dir (normalize (refract dir n (refr mat) 1)))
                    (reflect-orig (if (< (dot reflect-dir n) 0) (vdiff point (mul n 0.001)) (vadd point (mul n 0.001))))
                    (refract-orig (if (< (dot refract-dir n) 0) (vdiff point (mul n 0.001)) (vadd point (mul n 0.001))))
               )
               (setf reflect-color (cast-ray reflect-orig reflect-dir spheres lights (+ depth 1)))
               (setf refract-color (cast-ray refract-orig refract-dir spheres lights (+ depth 1)))
            
            (loop 
                with light-dir and light-dist with shadow-orig with si2 with b with shadow-pt with sh
                for L in lights do
                        (setf light-dir (normalize (vdiff (pos L) point)))
                        (setf light-dist (norm (vdiff (pos L) point)))
                        (setf shadow-orig (if (< (dot light-dir n) 0) (vdiff point (mul n 0.001)) (vadd point (mul n 0.001))))
                        (setf si2 (scene-intersect shadow-orig light-dir spheres))
                        (setf b (nth 0 si2))
                        (setf shadow-pt (nth 1 si2))
                        (setf sh (<  (norm (vdiff shadow-pt shadow-orig)) light-dist))

                    (when (not (and b  sh)) ; sh
                        (setf dli (+ dli (* (intens L) (max 0 (dot light-dir n)))))
                        (setf sli (+ sli (* (expt (max 0 (- 0 (dot (reflect (mul light-dir -1) n) dir))) (spec mat)) (intens L))))))
            (vadd
                (vadd
                    (vadd (mul (vec mat) (* dli (a0 mat)))
                        (mul (CVec3f (list 1 1 1)) (* sli (a1 mat))))
                            (mul reflect-color (a2 mat)))
                                (mul refract-color (a3 mat)))))))

(defun render (o spheres lights)
  (setq p "test2.ppm")
  (with-open-file (s p :direction :output :if-exists :supersede)
    (format s "~A~C" "P6" #\Newline)
    (format s "~D~C~D~C" *w* (code-char 32) *h* #\Newline)
    (format s "~D~C" 255 #\Newline))
  (with-open-file (s p :direction :output :if-does-not-exist :create
                          :if-exists :append :element-type 'unsigned-byte)
    (loop for h from 0 to (- *h* 1)
        do
        (progn
          (print h)
          (loop with b and g and r and m 
                for w from 0 to (- *w* 1)
            do (let* (
                    (fov 1.0)
                    (x  (* (* (-  (* 2 (/ (+ w 0.5) *w*)) 1)   (tan (/ fov 2))) (/  *w* *h*)))
                    (y (- 0 (* (-  (* 2 (/ (+ h 0.5) *h*)) 1)   (tan (/ fov 2)))))
                    (dir (normalize (CVec3f (list x y -1))))
                    (v (cast-ray o dir spheres lights 0)))
                (setf b (VecX v))
                (setf g (VecY v))
                (setf r (VecZ v))
                (setf m (max b g r))
                (when (> m 1)
                    (setq b  (/ b m))
                    (setq g  (/ g m))
                    (setq r  (/ r m)))

                (write-byte (floor (* 255 r)) s)
                (write-byte (floor (* 255 g)) s)
                (write-byte (floor (* 255 b)) s)
                (vec v)))))))

;;;

(defvar *lights*
    (list
    (make-instance 'light :pos (CVec3f (list -20 20 20)) :intens 1.5)
    (make-instance 'light :pos (CVec3f (list 30 50 -25)) :intens 1.8)
    (make-instance 'light :pos (CVec3f (list 30 20 30)) :intens 1.7)))

(defvar mat1 (make-instance 'material :refr 1
        :spec 50 :a0 0.6 :a1 0.3 :a2 0.1 :a3 0 :vec (CVec3f (list 0.3 0.4 0.4))  ))

(defvar mat2 (make-instance 'material :refr 1.5
        :spec 125 :a0 0 :a1 0.9 :a2 0.1 :a3 0.8  :vec (CVec3f (list 0.8 0.7 0.8))  ))

(defvar mat3 (make-instance 'material  :refr 1 
        :spec 10 :a0 0.9 :a1 0.1 :a2 0 :a3 0  :vec (CVec3f (list 0.1 0.1 0.3))  ))

(defvar mat4 (make-instance 'material  :refr 1 
        :spec 1425 :a0 0 :a1 10 :a2 0.8 :a3 0  :vec  (CVec3f (list 1 1 1))  ))



(defvar mat5 (make-instance 'material  :refr 1 
        :spec 0 :a0 0.9 :a1 0.0 :a2 0 :a3 0  :vec (CVec3f (list 0.5 0.1 0.1))  ))

(defvar mat6 (make-instance 'material  :refr 1 
        :spec 0 :a0 0.9 :a1 0.0 :a2 0 :a3 0  :vec (CVec3f (list 0.1 0.7 0.2))  ))

(defvar mat7 (make-instance 'material  :refr 1
        :spec 0 :a0 0.9 :a1 0.0 :a2 0 :a3 0  :vec (CVec3f (list 0.0 0.5 0.6))  ))

;;;

(defvar =s1 (make-instance 'sphere :v (CVec3f (list -3 0 -16)) :radius 2 :mat mat1))
(defvar =s2 (make-instance 'sphere :v (CVec3f (list -1 -1.5 -12)) :radius 2 :mat  mat2))
(defvar =s3 (make-instance 'sphere :v (CVec3f (list 1.5 -0.5 -18)) :radius 3 :mat  mat3))
(defvar =s4 (make-instance 'sphere :v (CVec3f (list 7 5 -18)) :radius 4 :mat mat4))


(defvar *spheres* (list =s1 =s2 =s3 =s4))
(render (CVec3f (list 0 0.2 0)) *spheres* *lights*)
(print (vec (vec mat3)))
(exit)
