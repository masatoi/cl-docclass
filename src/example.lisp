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
  (let ((v (make-array (length class-labels) :element-type 'fixnum)))
    (dovec index-shuffle-table i (setf (aref v i) i))
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

(defparameter result
  (loop repeat 50 collect
    (progn (train arow-learner train-vector)
           (cons (test arow-learner train-vector)
                 (test arow-learner test-vector)))))

;; (clgp:plots (list (mapcar #'car result)
;;                   (mapcar #'cdr result))
;;             :title-list '("training" "test"))

;; ;; check between the prediction and the actual article
;; (one-vs-one-predict arow-learner (cdr (aref dataset-vector 0)))
;; (nth (position 0 index-shuffle-table) *livedoor-data-files*)
