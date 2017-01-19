;;; -*- coding:utf-8; mode:lisp -*-

(in-package :cl-user)

(defpackage cl-docclass
  (:use :cl :cl-online-learning :cl-online-learning.utils :cl-online-learning.vector :wiz)
  (:nicknames :cl-docclass)
  (:shadow :shuffle-vector)
  ;; (:export :hoge)
  )

(in-package :cl-docclass)

;; Read igo dictionary
(igo:load-tagger "/home/wiz/igo/ipadic/")

(defun extract-word (text)
  (mapcar #'car
	  (remove-if-not (lambda (word)
			   (or (string= (subseq (cadr word) 0 2) "名詞")
			       (string= (subseq (cadr word) 0 3) "形容詞")
			       (string= (subseq (cadr word) 0 2) "動詞")))
			 (igo:parse text))))

;; word -> index hashtable
(defparameter *word-hash* (make-hash-table :test 'equal))
(defparameter *word-index* 0)

(defun add-words-to-hash! (file &optional (word-hash *word-hash*))
  (let ((words (extract-word (alexandria:read-file-into-string file))))
    (loop for word in words do
      (when (null (gethash word word-hash))
        (setf (gethash word word-hash) *word-index*)
        (incf *word-index*)))))

(defun add-words-hash-livedoor-data ()
  (dolist (file (append
                 (ls #P"/home/wiz/datasets/livedoor/text/kaden-channel/")
                 (ls #P"/home/wiz/datasets/livedoor/text/peachy/")
                 (ls #P"/home/wiz/datasets/livedoor/text/sports-watch/")
                 (ls #P"/home/wiz/datasets/livedoor/text/dokujo-tsushin/")
                 (ls #P"/home/wiz/datasets/livedoor/text/livedoor-homme/")
                 (ls #P"/home/wiz/datasets/livedoor/text/topic-news/")
                 (ls #P"/home/wiz/datasets/livedoor/text/it-life-hack/")
                 (ls #P"/home/wiz/datasets/livedoor/text/movie-enter/")
                 (ls #P"/home/wiz/datasets/livedoor/text/smax/")))
    (add-words-to-hash! file)
    *word-hash*))

;; (time (add-words-hash-livedoor-data))
;; Evaluation took:
;;   7.306 seconds of real time
;;   7.299407 seconds of total run time (7.198806 user, 0.100601 system)
;;   [ Run times consist of 0.189 seconds GC time, and 7.111 seconds non-GC time. ]
;;   99.90% CPU
;;   24,782,539,324 processor cycles
;;   7,769,195,328 bytes consed

;; *word-index* => 73021 ; it means word-vec dimensions

(defun make-word-count-vec (text &optional (word-hash *word-hash*))
  (let* ((words (extract-word text))
         (indices (mapcar (lambda (w) (gethash w word-hash)) words))
         (indices-unique (remove-duplicates indices :test #'=))
         (len (length indices-unique))
         (index-vector (make-array len :element-type 'fixnum :initial-contents indices-unique))
         (value-vector (make-array len :element-type 'double-float :initial-element 0d0)))
    (loop for i from 0 to (1- len) do
      (loop for index in indices do
        (when (= (aref index-vector i) index)
          (incf (aref value-vector i) 1d0))))
    (make-sparse-vector index-vector value-vector)))

(defun sum-vec (vec)
  (declare (type (simple-array double-float) vec))
  (let ((sum 0d0))
    (declare (type double-float sum))
    (loop for i fixnum from 0 to (1- (length vec)) do
      (incf sum (aref vec i)))
    sum))

;; (defun tf (text &optional (word-hash *word-hash*))
;;   (let ((wc-vec (make-word-count-vec text word-hash)))
    

;; ;; (defparameter *keyword-list* (extract-keyword-from-file "/home/wiz/twitter-svm/sample-tweet-list-json.lisp"))
;; ;; (multiple-value-bind (hash size)
;; ;;     (keyword-hash-from-file "/home/wiz/twitter-svm/sample-tweet-list-json.lisp")
;; ;;   (defparameter *keyword-hash* hash)
;; ;;   (defparameter *keyword-hash-size* size))

;; (defun file-read-length (file)
;;   (with-open-file (f file)
;;     (nlet iter ((i 0))
;;       (if (read f nil nil)
;; 	(iter (1+ i))
;; 	i))))

;; ;;; データ点を表すベクタの教師信号を除く部分が一致しているかどうか調べる述語
;; (defun equal-data-point-p (d1 d2)
;;   (if (not (= (length d1)
;; 	      (length d2)))
;;     (error "length of arguments are not equal.")
;;     (let ((len (length d1)))
;;       (nlet iter ((i 0))
;; 	(cond ((= i (- len 1)) t) ; termination
;; 	      ((= (aref d1 i) (aref d2 i)) (iter (1+ i))) ; 合えばカウンタを上げて次の再帰へ
;; 	      (:else nil)))))) ; 一つでも違えばその時点でnilを返す

;; ;; clml-svmへの入力形式のベクターと、キーワードからインデックスに変換するハッシュを返す
;; (defun make-svm-vector-from-file (file)
;;   (multiple-value-bind (hash size)
;;       (keyword-hash-from-file file)
;;     (with-open-file (f file)
;;       (let* ((product-array (make-array (file-read-length file))))
;; 	(nlet iter ((read-data (read f nil nil))
;; 		    (i 0))
;; 	  (when read-data
;; 	    (let ((keyword-list (extract-keyword (cut-out-unnecessary-str (cdr read-data))))
;; 		  (dvec (vector:make-dvec (1+ size) 0d0)))
;; 	      ;; 対応するキーワードがハッシュにあれば、データ点の対応する次元に1を設定する。他は全て0
;; 	      (dolist (keyword keyword-list)
;; 		(setf (aref dvec (gethash keyword hash)) 1d0))
;; 	      ;; 正例ならベクタの最後の要素は1に。負例なら-1にする
;; 	      (if (car read-data)
;; 		(setf (aref dvec size) 1d0)
;; 		(setf (aref dvec size) -1d0))
;; 	      ;; 設定したdvecをproduct-arrayに代入
;; 	      (setf (aref product-array i) dvec)
;; 	      (iter (read f nil nil) (1+ i)))))
;; 	(values ;; product-array
;; 	 (remove-duplicates product-array :test #'equal-data-point-p) ; 重複を除去
;; 	 hash)))))

;; ;; 実際の分類時にテキストとキーワードハッシュからSVMへの入力を作る
;; (defun make-svm-vector-from-text (text keyword-hash)
;;   (let ((keyword-list (extract-keyword (cut-out-unnecessary-str text)))
;; 	(dvec (vector:make-dvec (hash-table-count keyword-hash) 0d0)))
;;     ;; 対応するキーワードがハッシュにあれば、データ点の対応する次元に1を設定する。他は全て0
;;     (dolist (keyword keyword-list)
;;       (aif (gethash keyword keyword-hash)
;; 	(setf (aref dvec it) 1d0)))
;;     dvec))

;; ;; 予測
;; (defun prediction (trained-svm keyword-hash text)
;;   (svm:discriminate trained-svm (make-svm-vector-from-text text keyword-hash)))
