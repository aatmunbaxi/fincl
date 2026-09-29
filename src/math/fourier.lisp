;;;; math/fourier.lisp --- fast Fourier transform

(in-package #:fincl)

(defun fft! (v &key inverse)
  "Replace V, a (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)) whose length is a
power of 2, by its discrete Fourier transform sum_j v_j exp(-2 pi i j k / n),
or with INVERSE by the unnormalized inverse (sign +). Returns V.

  (fft! (make-array 4 :element-type '(complex double-float)
                      :initial-contents '(#c(1d0 0d0) 0 0 0)))
  => #(#C(1d0 0d0) #C(1d0 0d0) #C(1d0 0d0) #C(1d0 0d0))"
  (declare (type (simple-array (complex double-float) (*)) v))
  (let ((n (length v)))
    (assert (and (plusp n) (zerop (logand n (1- n)))) ()
            "FFT length ~D is not a power of 2." n)
    ;; Bit-reversal permutation.
    (let ((j 0))
      (declare (fixnum j))
      (loop for i fixnum from 1 below n
            do (let ((bit (ash n -1)))
                 (declare (fixnum bit))
                 (loop while (logtest j bit)
                       do (setf j (logxor j bit)
                                bit (ash bit -1)))
                 (setf j (logxor j bit))
                 (when (< i j) (rotatef (aref v i) (aref v j))))))
    ;; Butterflies. Twiddles come from CIS directly rather than a running
    ;; product, so their error does not grow with the transform length.
    (let ((sign (if inverse 2d0 -2d0)))
      (loop for len fixnum = 2 then (* 2 len)
            while (<= len n)
            do (let ((half (ash len -1))
                     (angle (/ (* sign pi) len)))
                 (dotimes (k half)
                   (let ((w (cis (* angle k))))
                     (loop for i fixnum from k below n by len
                           do (let* ((top (aref v i))
                                     (bottom (* w (aref v (+ i half)))))
                                (setf (aref v i) (+ top bottom)
                                      (aref v (+ i half)) (- top bottom)))))))))
    v))
