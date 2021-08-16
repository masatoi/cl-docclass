;;; -*- coding:utf-8; mode:lisp; -*-

(ql:quickload :cl-online-learning)
(ql:quickload :wiz-util)
(ql:quickload :cl-store)

(defpackage clol-reuters
  (:use :cl :clol :clol.utils :wiz-util)
  (:shadowing-import-from :clol.utils :shuffle-vector))

(in-package :clol-reuters)

(defparameter reuters-dim 77822)

(time
 (defparameter reuters-dataset
   (read-data "/home/wiz/datasets/reuters-dataset-market-equally-60mins"
              reuters-dim
              :sparse-p t
              :multiclass-p t)))
;; 128.699 seconds

(defparameter test-dataset
  (read-data "/home/wiz/datasets/reuters-testset"
             reuters-dim
             :sparse-p t
             :multiclass-p t))

(loop repeat 1000 do
  (train learner reuters-dataset)
  (format t "Train: ")
  (test learner reuters-dataset)
  (format t "Test:  ")
  (test learner test-dataset))

(cl-store:store learner "/home/wiz/cl-docclass/src/arow-learner.store")

;;; 汎化性能的にはliblinearとほぼ同等
;;; ここからさらに良くするには
;;;    (1) マーケットに関連した内容に限定する。タグからマーケット関連のデータのみ抜き出す
;;;    (2) タイムスパンをもっと長くしてみる(これはすぐにできる)

(defparameter train-dataset
  (subseq reuters-dataset 1000 40000))

(defparameter test-dataset
  (subseq reuters-dataset 0 1000))

(defparameter learner (make-one-vs-one reuters-dim 3 'sparse-arow 100d0))

(loop repeat 5 do
  (train learner train-dataset)
  (format t "Train: ")
  (test learner train-dataset)
  (format t "Test: ")
  (test learner test-dataset))

;;; 1000個ずつずらして3万個のデータで訓練し、1000個のデータでテストするのを繰り返す

(loop for stop-time from 0 to 100000 by 1000 do
  (let ((test-dataset (subseq reuters-dataset stop-time (+ stop-time 1000)))
        (train-dataset (subseq reuters-dataset (+ stop-time 1000) (+ stop-time 60000)))
        (learner (make-one-vs-one reuters-dim 3 'sparse-arow 100d0)))
    (loop repeat 5 do
      (train learner train-dataset))
    (test learner test-dataset)))

;; Accuracy: 40.5%, Correct: 405, Total: 1000
;; Accuracy: 38.4%, Correct: 384, Total: 1000
;; Accuracy: 39.6%, Correct: 396, Total: 1000
;; Accuracy: 31.2%, Correct: 312, Total: 1000
;; Accuracy: 36.3%, Correct: 363, Total: 1000
;; Accuracy: 31.7%, Correct: 317, Total: 1000
;; Accuracy: 31.5%, Correct: 315, Total: 1000
;; Accuracy: 33.3%, Correct: 333, Total: 1000
;; Accuracy: 32.9%, Correct: 329, Total: 1000
;; Accuracy: 34.8%, Correct: 348, Total: 1000
;; Accuracy: 38.2%, Correct: 382, Total: 1000
;; Accuracy: 38.3%, Correct: 383, Total: 1000
;; Accuracy: 40.8%, Correct: 408, Total: 1000
;; Accuracy: 28.8%, Correct: 288, Total: 1000
;; Accuracy: 29.9%, Correct: 299, Total: 1000
;; Accuracy: 33.5%, Correct: 335, Total: 1000
;; Accuracy: 33.399998%, Correct: 334, Total: 1000
;; Accuracy: 36.1%, Correct: 361, Total: 1000
;; Accuracy: 36.1%, Correct: 361, Total: 1000
;; Accuracy: 34.9%, Correct: 349, Total: 1000
;; Accuracy: 35.4%, Correct: 354, Total: 1000
;; Accuracy: 32.600002%, Correct: 326, Total: 1000
;; Accuracy: 38.9%, Correct: 389, Total: 1000
;; Accuracy: 32.7%, Correct: 327, Total: 1000
;; Accuracy: 29.300001%, Correct: 293, Total: 1000
;; Accuracy: 29.499998%, Correct: 295, Total: 1000
;; Accuracy: 36.5%, Correct: 365, Total: 1000
;; Accuracy: 32.2%, Correct: 322, Total: 1000
;; Accuracy: 35.600002%, Correct: 356, Total: 1000
;; Accuracy: 36.1%, Correct: 361, Total: 1000
;; Accuracy: 30.1%, Correct: 301, Total: 1000
;; Accuracy: 20.7%, Correct: 207, Total: 1000
;; Accuracy: 19.8%, Correct: 198, Total: 1000
;; Accuracy: 18.199999%, Correct: 182, Total: 1000
;; Accuracy: 32.9%, Correct: 329, Total: 1000
;; Accuracy: 11.5%, Correct: 115, Total: 1000
;; Accuracy: 28.7%, Correct: 287, Total: 1000
;; Accuracy: 24.3%, Correct: 243, Total: 1000
;; Accuracy: 20.4%, Correct: 204, Total: 1000
;; Accuracy: 22.6%, Correct: 226, Total: 1000
;; Accuracy: 32.0%, Correct: 320, Total: 1000
;; Accuracy: 19.9%, Correct: 199, Total: 1000
;; Accuracy: 22.2%, Correct: 222, Total: 1000
;; Accuracy: 27.000002%, Correct: 270, Total: 1000
;; Accuracy: 32.600002%, Correct: 326, Total: 1000
;; Accuracy: 43.0%, Correct: 430, Total: 1000
;; Accuracy: 48.6%, Correct: 486, Total: 1000
;; Accuracy: 51.0%, Correct: 510, Total: 1000
;; Accuracy: 50.800003%, Correct: 508, Total: 1000
;; Accuracy: 59.7%, Correct: 597, Total: 1000
;; Accuracy: 48.100002%, Correct: 481, Total: 1000
;; Accuracy: 32.100002%, Correct: 321, Total: 1000
;; Accuracy: 21.699999%, Correct: 217, Total: 1000
;; Accuracy: 24.5%, Correct: 245, Total: 1000
;; Accuracy: 24.2%, Correct: 242, Total: 1000
;; Accuracy: 25.3%, Correct: 253, Total: 1000
;; Accuracy: 28.5%, Correct: 285, Total: 1000
;; Accuracy: 28.9%, Correct: 289, Total: 1000
;; Accuracy: 23.9%, Correct: 239, Total: 1000
;; Accuracy: 33.0%, Correct: 330, Total: 1000
;; Accuracy: 26.6%, Correct: 266, Total: 1000
;; Accuracy: 31.099998%, Correct: 311, Total: 1000
;; Accuracy: 34.3%, Correct: 343, Total: 1000
;; Accuracy: 36.199997%, Correct: 362, Total: 1000
;; Accuracy: 33.399998%, Correct: 334, Total: 1000
;; Accuracy: 33.7%, Correct: 337, Total: 1000
;; Accuracy: 42.2%, Correct: 422, Total: 1000
;; Accuracy: 35.5%, Correct: 355, Total: 1000
;; Accuracy: 38.600002%, Correct: 386, Total: 1000
;; Accuracy: 33.8%, Correct: 338, Total: 1000
;; Accuracy: 35.5%, Correct: 355, Total: 1000
;; Accuracy: 41.600002%, Correct: 416, Total: 1000
;; Accuracy: 36.6%, Correct: 366, Total: 1000
;; Accuracy: 39.6%, Correct: 396, Total: 1000
;; Accuracy: 33.199997%, Correct: 332, Total: 1000
;; Accuracy: 34.3%, Correct: 343, Total: 1000
;; Accuracy: 36.399998%, Correct: 364, Total: 1000
;; Accuracy: 37.8%, Correct: 378, Total: 1000
;; Accuracy: 38.800003%, Correct: 388, Total: 1000
;; Accuracy: 42.6%, Correct: 426, Total: 1000
;; Accuracy: 40.9%, Correct: 409, Total: 1000
;; Accuracy: 39.8%, Correct: 398, Total: 1000
;; Accuracy: 42.0%, Correct: 420, Total: 1000
;; Accuracy: 43.5%, Correct: 435, Total: 1000
;; Accuracy: 42.399998%, Correct: 424, Total: 1000
;; Accuracy: 40.0%, Correct: 400, Total: 1000
;; Accuracy: 44.5%, Correct: 445, Total: 1000
;; Accuracy: 45.2%, Correct: 452, Total: 1000
;; Accuracy: 40.4%, Correct: 404, Total: 1000
;; Accuracy: 43.3%, Correct: 433, Total: 1000
;; Accuracy: 40.2%, Correct: 402, Total: 1000
;; Accuracy: 40.6%, Correct: 406, Total: 1000
;; Accuracy: 36.0%, Correct: 360, Total: 1000
;; Accuracy: 40.1%, Correct: 401, Total: 1000
;; Accuracy: 43.6%, Correct: 436, Total: 1000
;; Accuracy: 38.9%, Correct: 389, Total: 1000
;; Accuracy: 35.4%, Correct: 354, Total: 1000
;; Accuracy: 30.4%, Correct: 304, Total: 1000
;; Accuracy: 30.000002%, Correct: 300, Total: 1000
;; Accuracy: 24.2%, Correct: 242, Total: 1000
;; Accuracy: 21.3%, Correct: 213, Total: 1000
;; Accuracy: 22.2%, Correct: 222, Total: 1000
;; Accuracy: 23.0%, Correct: 230, Total: 1000
;; Accuracy: 17.0%, Correct: 170, Total: 1000
;; Accuracy: 20.900002%, Correct: 209, Total: 1000
;; Accuracy: 30.899998%, Correct: 309, Total: 1000
;; Accuracy: 26.199999%, Correct: 262, Total: 1000
;; Accuracy: 35.600002%, Correct: 356, Total: 1000
;; Accuracy: 26.699999%, Correct: 267, Total: 1000
;; Accuracy: 25.6%, Correct: 256, Total: 1000
;; Accuracy: 36.8%, Correct: 368, Total: 1000
;; Accuracy: 35.4%, Correct: 354, Total: 1000
;; Accuracy: 35.4%, Correct: 354, Total: 1000
;; Accuracy: 35.7%, Correct: 357, Total: 1000
;; Accuracy: 37.2%, Correct: 372, Total: 1000
;; Accuracy: 42.899998%, Correct: 429, Total: 1000
;; Accuracy: 40.7%, Correct: 407, Total: 1000
;; Accuracy: 40.1%, Correct: 401, Total: 1000
;; Accuracy: 50.300003%, Correct: 503, Total: 1000
;; Accuracy: 46.1%, Correct: 461, Total: 1000
;; Accuracy: 37.8%, Correct: 378, Total: 1000
;; Accuracy: 39.2%, Correct: 392, Total: 1000
;; Accuracy: 41.0%, Correct: 410, Total: 1000
;; Accuracy: 43.0%, Correct: 430, Total: 1000
;; Accuracy: 37.7%, Correct: 377, Total: 1000
;; Accuracy: 35.0%, Correct: 350, Total: 1000
;; Accuracy: 28.9%, Correct: 289, Total: 1000
;; Accuracy: 30.8%, Correct: 308, Total: 1000
;; Accuracy: 26.6%, Correct: 266, Total: 1000
;; Accuracy: 32.8%, Correct: 328, Total: 1000
;; Accuracy: 37.3%, Correct: 373, Total: 1000
