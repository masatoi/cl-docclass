(in-package :cl-docclass)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *livedoor-data*
  (list
   (ls #P"/home/wiz/datasets/livedoor/text/kaden-channel/")
   (ls #P"/home/wiz/datasets/livedoor/text/peachy/")
   (ls #P"/home/wiz/datasets/livedoor/text/sports-watch/")
   (ls #P"/home/wiz/datasets/livedoor/text/dokujo-tsushin/")
   (ls #P"/home/wiz/datasets/livedoor/text/livedoor-homme/")
   (ls #P"/home/wiz/datasets/livedoor/text/topic-news/")
   (ls #P"/home/wiz/datasets/livedoor/text/it-life-hack/")
   (ls #P"/home/wiz/datasets/livedoor/text/movie-enter/")
   (ls #P"/home/wiz/datasets/livedoor/text/smax/")))

(defparameter *livedoor-data-files* (alexandria:flatten *livedoor-data*))

(init-word-hash! *livedoor-data-files*)
;; 6.147 seconds of real time

(defparameter tf-idf-list (make-tf-idf-list *livedoor-data-files*))
;; 10.589 seconds of real time

;; ;; actual dimension histogram
;; (ql:quickload :clgplot)
;; (clgp:plot-histogram (mapcar #'sparse-vector-length tf-idf-list) 20)

(defparameter class-labels
  (alexandria:flatten
   (loop for class-id from 0 to (1- (length *livedoor-data*))
         for dim in (mapcar #'length *livedoor-data*)
         collect
      (make-list dim :initial-element class-id))))

(defparameter index-shuffle-table
  (let ((v (make-array (length class-labels))))
    (clol.vector::dovec v i (setf (aref v i) i))
    (clol.utils:shuffle-vector v)))

(defparameter dataset-vector
  (let ((tv (make-array (length tf-idf-list))))
    (loop for i from 0 to (1- (length tf-idf-list))
          for class-label in class-labels
          for tf-idf in tf-idf-list
          do
       (setf (aref tv (aref index-shuffle-table i))
             (cons class-label tf-idf)))
    tv))

(defparameter train-vector (subseq dataset-vector 0 6367))
(defparameter test-vector (subseq dataset-vector 6367 7367))

(defparameter arow-learner (make-one-vs-one *word-index* 9 'sparse-arow 5d0))
(time (defparameter result
        (loop repeat 10 collect
          (progn (train arow-learner train-vector)
                 (cons (test arow-learner train-vector)
                       (test arow-learner test-vector))))))

(defparameter arow-learner-1vr (make-one-vs-rest *word-index* 9 'sparse-arow 5d0))
(time (defparameter result-1vr
        (loop repeat 50 collect
          (progn (train arow-learner-1vr train-vector)
                 (cons (test arow-learner-1vr train-vector)
                       (test arow-learner-1vr test-vector))))))

(time (loop repeat 150 do (train arow-learner-1vr train-vector))) ; => 9.190 seconds
(time (test arow-learner-1vr test-vector)) ; => Accuracy: 96.8%, Correct: 968, Total: 1000

(time (train arow-learner-1vr train-vector)) ; => 0.192 seconds
(test arow-learner-1vr test-vector) ; => Accuracy: 90.0%, Correct: 900, Total: 1000

(defparameter scw-learner-1vr (make-one-vs-rest *word-index* 9 'sparse-scw 0.9d0 0.1d0))
(time (defparameter result-scw-1vr
        (loop repeat 50 collect
          (progn (train scw-learner-1vr train-vector)
                 (cons (test scw-learner-1vr train-vector)
                       (test scw-learner-1vr test-vector))))))

;; (clgp:plots (list (mapcar #'car result)
;;                   (mapcar #'cdr result))
;;             :title-list '("training" "test")
;;             :output "/home/wiz/Dropbox/tmp/livedoor-result.png")

;; ;; check between the prediction and the actual article
;; (one-vs-one-predict arow-learner (cdr (aref dataset-vector 0)))
;; (nth (position 0 index-shuffle-table) *livedoor-data-files*)

;; output dataset to file (libsvm format)

(defun output-datum (datum &optional (stream *standard-output*))
  (let ((sorted-index-value-pair-list
         (sort (mapcar #'cons
                       (coerce (sparse-vector-index-vector (cdr datum)) 'list)
                       (coerce (sparse-vector-value-vector (cdr datum)) 'list))
               (lambda (v1 v2)
                 (<= (car v1) (car v2))))))
    (format stream "~A " (1+ (floor (car datum))))
    (loop for index-value-pair in sorted-index-value-pair-list do
      (format stream "~A:~F " (1+ (car index-value-pair)) (cdr index-value-pair)))
    (format stream " ~%")))

;; These data are must be sorted by the index.

(defun output-dataset (dataset-vector file)
  (with-open-file (f file :direction :output :if-exists :supersede)
    (loop for datum across dataset-vector do
      (output-datum datum f))))

(output-dataset train-vector "/home/wiz/datasets/livedoor/libsvmdata-train")
(output-dataset test-vector "/home/wiz/datasets/livedoor/libsvmdata-test")

;; ~/datasets/livedoor $ liblinear-train libsvmdata-train libsvmdata.model
;; *
;; optimization finished, #iter = 8
;; Objective value = -465.448160
;; nSV = 2727
;; *
;; optimization finished, #iter = 7
;; Objective value = -310.024436
;; nSV = 2235
;; *
;; optimization finished, #iter = 7
;; Objective value = -654.374309
;; nSV = 2798
;; *
;; optimization finished, #iter = 7
;; Objective value = -345.652530
;; nSV = 2341
;; *
;; optimization finished, #iter = 7
;; Objective value = -247.613220
;; nSV = 1939
;; *
;; optimization finished, #iter = 7
;; Objective value = -554.662671
;; nSV = 2386
;; *
;; optimization finished, #iter = 7
;; Objective value = -408.987899
;; nSV = 2568
;; *
;; optimization finished, #iter = 7
;; Objective value = -534.409442
;; nSV = 3000
;; *
;; optimization finished, #iter = 6
;; Objective value = -393.738729
;; nSV = 2203
;; ~/datasets/livedoor $ liblinear-predict  libsvmdata-train libsvmdata.model out
;; Accuracy = 99.4817% (6334/6367)
;; ~/datasets/livedoor $ liblinear-predict  libsvmdata-test libsvmdata.model out
;; Accuracy = 95.4% (954/1000)

;; ~/datasets/livedoor $ opal-multi libsvmdata-train libsvmdata.opal libsvmdata-test
;; mode: BOTH
;; loading examples..done.
;; PA1 iter=10  #ex=63670    saving..done.
;; loading..done (# features = 54240).
;; acc. 0.9680 (corr 968) (incorr 32)
;; classify  : 0.0040 ms./trial (3.95082837/1000)

;; train     : 235.2475 ms.
;; test      : 15.3453 ms.

(defparameter read-train-data
  (clol.utils:read-data "/home/wiz/datasets/livedoor/libsvmdata-train" *word-index*
                        :sparse-p t :multiclass-p t))

(defparameter arow-learner (make-one-vs-one *word-index* 9 'sparse-arow 10d0))

(defparameter result
  (loop repeat 50 collect
    (progn (train arow-learner train-vector)
           (cons (test arow-learner train-vector)
                 (test arow-learner test-vector)))))
