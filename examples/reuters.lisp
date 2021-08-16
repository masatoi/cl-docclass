;;; -*- coding:utf-8; mode:lisp -*-

(in-package :cl-docclass)

;;; Make word-hash

;; initialize word-hash
(defparameter *word-hash* (make-hash-table :test 'equal))

(defun add-words-to-hash-reuters-day-file! (file-path)
  (let ((articles (with-open-file (f file-path) (read f))))
    (dolist (article articles)
      (add-words-to-hash! (concatenate 'string (elt article 3) (elt article 4))
                          *word-hash*))))

;; (add-words-to-hash-reuters-day-file! "/home/wiz/datasets/reuters/20170101-20170102")

(let ((file-path-list (ls "/home/wiz/datasets/reuters/")))
  (dolist (file-path file-path-list)
    (print file-path)
    (add-words-to-hash-reuters-day-file! file-path)))

;; Removes infrequent words
(setf *word-hash* (remove-infrequent-words *word-hash* 10))

;;; Serialize hash-table
(cl-store:store *word-hash* "/home/wiz/cl-docclass/dict/reuters-jp-word-hash")

;; restore
(defvar *word-hash*)
(setf *word-hash* (cl-store:restore "/home/wiz/cl-docclass/dict/reuters-jp-word-hash"))

;;; Make input data (List of sparse vectors)

;; Store timestamp and sparse-vector
;; make-tf-idf-list は全テキストのリストを必要とする(数GB)
;; インクリメンタルにできないか？

;; まず価格変動に関係あるかないかで分類する
;; 上下問わず1%以上の変動があるかどうか。スポーツ関係の話題などは変動に関係しないはず

;; ここまでの処理をするだけでもサービスになりそう
;; RSSリーダーのようなもので、フィードを登録しておけば、価格変動に関係するものを抽出して表示することができる
;; スマホアプリのようなもので、ローカルで判定する

;; wc-vec-listは疎ベクトルのリスト
;; doc-count-vecは密ベクトルだが1つだ

(defparameter wc-vec-list nil)

(let ((file-path-list (ls "/home/wiz/datasets/reuters/")))
  (dolist (file-path file-path-list)
    (print file-path)
    (let ((articles (with-open-file (f file-path) (read f))))
      (dolist (article articles)
        (let ((text (concatenate 'string (elt article 3) (elt article 4))))
          (push (make-word-count-vec text *word-hash*) wc-vec-list))))))

(defparameter doc-count-vec (make-doc-count-vec (hash-table-count *word-hash*) wc-vec-list))

(progn
  (doc-count-vec->idf! doc-count-vec)
  'quit)

(dolist (wc-vec wc-vec-list)
  (wc->tf! wc-vec)
  (ds2s-v* doc-count-vec wc-vec wc-vec))

;; (cl-store:store wc-vec-list "/home/wiz/cl-docclass/dict/reuters-jp-wc-vec-list")
;; ;; restore
;; (defvar wc-vec-list)
;; (setf wc-vec-list (cl-store:restore "/home/wiz/cl-docclass/dict/reuters-jp-wc-vec-list"))

;; まずインクリメンタルにwc-vecを作りながらdoc-count-vecに足していく (全体の情報が必要なのはここだけ)
;; あとはインクリメンタルにできるのでファイルに逐次出力しながらでいい

;;; 記事のUNIX時間をリストにpushする
;; wc-vec-listとunix-time-listは反転している

(ql:quickload :local-time)

(defparameter unix-time-list nil)

(let ((file-path-list (ls "/home/wiz/datasets/reuters/")))
  (dolist (file-path file-path-list)
    (print file-path)
    (let ((articles (with-open-file (f file-path) (read f))))
    (dolist (article articles)
      (push (local-time:timestamp-to-unix (local-time:parse-timestring (elt article 1)))
            unix-time-list)))))

(cl-store:store unix-time-list "/home/wiz/cl-docclass/dict/reuters-unix-time-list")

(defvar unix-time-list)
(setf unix-time-list (cl-store:restore "/home/wiz/cl-docclass/dict/reuters-unix-time-list"))

;;; カテゴリリスト

(defparameter category-list nil)

(let ((file-path-list (ls "/home/wiz/datasets/reuters/")))
  (dolist (file-path file-path-list)
    (print file-path)
    (let ((articles (with-open-file (f file-path) (read f))))
    (dolist (article articles)
      (push (elt article 2) category-list)))))

(cl-store:store category-list "/home/wiz/cl-docclass/dict/reuters-category-list")

(defvar category-list)
(setf category-list (cl-store:restore "/home/wiz/cl-docclass/dict/reuters-category-list"))

;;; カテゴリでunix-time-listをフィルターする
(defparameter unix-time-list-markets
  (let ((unix-time-list-markets nil))
    (loop for unix-time in unix-time-list
          for category in category-list
          do (when (string= category "Markets")
               (push unix-time unix-time-list-markets)))
    (nreverse unix-time-list-markets)))

;;; 教師信号を作る
;; 20分後の3方向で分類

(defun subset-data (dir-path start-date end-date)
  (let* ((click-data-list (ls dir-path))
         (start (position-if (lambda (p)
                               (string= (pathname-name p) start-date))
                             click-data-list))
         (end (position-if (lambda (p)
                             (string= (pathname-name p) end-date))
                           click-data-list)))
    (subseq click-data-list start (1+ end))))

(defparameter subset (subset-data "/home/wiz/click-data/" "2011-08-12" "2016-02-26"))

(defparameter data-arr (make-array (* (length subset) 60 24)))

(defun set-ave-rate (data-arr subset)
  (loop for i from 0 to (1- (length subset))
        for file in subset
        do (with-open-file (f file)
             (format t "open: ~A~%" file)
             (nlet iter ((read-data (read f nil nil))
                         (j 0))
               (if read-data
                   (let* ((unix-time (local-time:timestamp-to-unix
                                      (local-time:parse-timestring (car read-data))))
                          (rate-pair (cadr read-data))
                          (rate-ave (/ (+ (car rate-pair) (cadr rate-pair)) 2))) ; USDJPY
                     (setf (aref data-arr (+ (* i 1440) j)) (cons unix-time rate-ave))
                     (iter (read f nil nil) (1+ j)))
                   'quit)))))

(set-ave-rate data-arr subset)

(defun binary-search-by-unix-time (unix-time start-index end-index data-arr)
  ;;(format t "start-index: ~A, end-index: ~A~%" start-index end-index)
  (if (= (1+ start-index) end-index)
      (aref data-arr start-index)
      (let ((median-index (floor (/ (+ start-index end-index) 2))))
        'quit
        (if (< unix-time (car (aref data-arr median-index)))
            (binary-search-by-unix-time unix-time start-index median-index data-arr)
            (binary-search-by-unix-time unix-time median-index end-index data-arr)))))

(binary-search-by-unix-time 1313193561 0 (1- (length data-arr)) data-arr)

;; 土日を除外
;; reutersの時刻リストから20分後のclick-dataのレートが存在しているものをフィルターする？

(defun truncate-seconds (unix)
  (let* ((timestamp (local-time:unix-to-timestamp unix))
         (seconds (local-time:timestamp-second timestamp)))
    (- unix seconds)))

;;; 特定の条件でunix-time-listをフィルターする
;; reutersの記事時刻のうち、20分後のレートデータが存在するもの(土日などは除外される)
;; マーケットに関連するもの
(defparameter future-exist-list
  (remove-if-not
   (lambda (unix)
     (let* ((truncated (truncate-seconds unix))
            (searched (car (binary-search-by-unix-time truncated 0 (1- (length data-arr)) data-arr)))
            (future (+ truncated (* 60 20)))
            (searched-future (car (binary-search-by-unix-time future 0 (1- (length data-arr)) data-arr))))
       (and (= truncated searched)
            (= future searched-future))))
   unix-time-list))

(loop for day-of-week from 0 to 6
      collect
      (count-if (lambda (unix)
                  (= (local-time:timestamp-day-of-week (local-time:unix-to-timestamp unix))
                     day-of-week))
                future-exist-list))

;; (0 63485 87573 87302 89840 93739 11938)
;; 土曜でも朝07:00まではレートが存在する

(defparameter direction-list
  (mapcar
   (lambda (unix)
     (let* ((truncated (truncate-seconds unix))
            (searched (binary-search-by-unix-time truncated 0 (1- (length data-arr)) data-arr))
            (future (+ truncated (* 60 60))) ; (* 60 20)
            (searched-future (binary-search-by-unix-time future 0 (1- (length data-arr)) data-arr)))
       (if (and (= truncated (car searched))
                (= future (car searched-future)))
           (cond ((> (cdr searched-future) (* (cdr searched) 1.001)) 0)
                 ((< (cdr searched-future) (* (cdr searched) 0.999)) 1)
                 (t 2)))))
   unix-time-list-markets))

(format t "0: ~A, 1: ~A, 2: ~A, NIL: ~A~%"
        (count 0 direction-list)
        (count 1 direction-list)
        (count 2 direction-list)
        (count NIL direction-list))

;; 0: 170800, 1: 167342, 2: 95735, NIL: 108724
;; 0: 63243, 1: 61764, 2: 35080, NIL: 20537 ; Marketsでフィルターした場合

(defun print-wc-vec-libsvm (stream target wc-vec)
  (format stream "~A" (1+ target))
  (let* ((consed-list (map 'list #'cons
                           (clol.vector:sparse-vector-index-vector wc-vec)
                           (clol.vector:sparse-vector-value-vector wc-vec)))
         (sorted-list (sort consed-list (lambda (a b) (< (car a) (car b))))))
    (loop for index-value-pair in sorted-list
          do (format stream " ~D:~f" (1+ (car index-value-pair)) (cdr index-value-pair))))
  (format stream "~%"))

;; debug
(print-wc-vec-libsvm *standard-output* (nth 80624 direction-list) (nth 80624 wc-vec-list))

(with-open-file (testset "/home/wiz/datasets/reuters-testset-market" :direction :output :if-exists :supersede)
  (with-open-file (dataset "/home/wiz/datasets/reuters-dataset-market" :direction :output :if-exists :supersede)
    (let ((cnt 0))
      (loop for d in direction-list
            for wc-vec in wc-vec-list
            do (when d
                 (if (< cnt 30000)
                     (print-wc-vec-libsvm testset d wc-vec)
                     (print-wc-vec-libsvm dataset d wc-vec))
                 (incf cnt))))))

(with-open-file (testset "/home/wiz/datasets/reuters-dataset-market-equally-60mins" :direction :output :if-exists :supersede)
  (let ((cnt 0))
    (loop for d in direction-list
          for wc-vec in wc-vec-list
          do (when d
               (print-wc-vec-libsvm testset d wc-vec)
               (incf cnt)))))
