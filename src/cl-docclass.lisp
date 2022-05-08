;;; -*- coding:utf-8; mode:lisp -*-

(in-package :cl-user)

(defpackage cl-docclass
  (:use :cl :cl-online-learning :cl-online-learning.utils :cl-online-learning.vector)
  (:nicknames :docclass)
  (:shadow :shuffle-vector)
  (:export
   :*igo-dict-dir* :load-igo-dict :extract-word
   :add-words-to-hash!
   :add-words-to-hash-from-file!
   :remove-infrequent-words
   :make-tf-idf-list
   :make-tf-idf-list-from-files))

(in-package :cl-docclass)

#| Prepare for cl-igo

Download IPA dictionary for mecab
$ wget https://sourceforge.net/projects/mecab/files/mecab-ipadic/2.7.0-20070801/

Download igo-0.4.5.jar
$ wget https://osdn.net/projects/igo/downloads/55029/igo-0.4.5.jar/

Make Igo dictionary
$ java -cp ./igo-0.4.5.jar net.reduls.igo.bin.BuildDic ipadic mecab-ipadic-2.7.0-20070801 EUC-JP

|#

(defparameter *igo-dict-dir* (merge-pathnames #P"igo/ipadic/" (user-homedir-pathname)))

;; Read igo dictionary
(defun load-igo-dict ()
  (igo:load-tagger *igo-dict-dir*))

(defun extract-word (text)
  (mapcar #'car
	  (remove-if-not (lambda (word)
			   (or (string= (subseq (cadr word) 0 2) "名詞")
			       (string= (subseq (cadr word) 0 3) "形容詞")
			       (string= (subseq (cadr word) 0 2) "動詞")))
			 (igo:parse text))))

#|
(load-igo-dict)

(defparameter text "国際宇宙ステーション（ＩＳＳ）での約半年間の滞在を終えて今月２日に地球に帰還した宇宙飛行士の野口聡一さん（５６）が２７日、日本の報道陣向けにオンラインで会見した。今後について、「日本社会も高齢化社会に適応し、定年も伸びている。")

(extract-word text)
=> ("国際" "宇宙" "ステーション" "ＩＳＳ" "半年" "間" "滞在" "終え" "今月" "２" "日" "地球" "帰還" "し" "宇宙" "飛行" "士" "野口" "聡" "一" "さん" "５" "６" "２" "７" "日" "日本" "報道陣" "向け" "オンライン" "会見" "し" "今後" "日本" "社会" "高齢" "化" "社会" "適応" "し" "定年" "伸び" "いる")
|#

;; word -> index hashtable
(defun add-words-to-hash! (text word-hash)
  (let ((words (extract-word text)))
    (loop for word in words do
      (if (gethash word word-hash)
          (incf (cdr (gethash word word-hash)))
          (setf (gethash word word-hash) (cons (hash-table-count word-hash) 1))))))

(defun add-words-to-hash-from-file! (file word-hash)
  (add-words-to-hash! (alexandria:read-file-into-string file) word-hash))

(defun remove-infrequent-words (word-hash threshold)
  (let ((new-hash (make-hash-table :test 'equal)))
    (maphash (lambda (key val)
               (when (>= (cdr val) threshold)
                 (setf (gethash key new-hash)
                       (cons (hash-table-count new-hash) (cdr val)))))
             word-hash)
    new-hash))

(defun make-word-hash-from-files (files &key (infrequent-threshold 10))
  (check-type files list)
  (check-type infrequent-threshold alexandria:positive-integer)
  (assert (every #'pathnamep files))
  (let ((word-hash (make-hash-table :test #'equal)))
    (dolist (file files)
      (add-words-to-hash-from-file! file word-hash))
    (remove-infrequent-words word-hash infrequent-threshold)))

;; Count the number of words which appears in the text
(defun make-word-count-sparse-vector (text word-hash)
  (let* ((words (extract-word text))
         (indices (remove nil (mapcar (lambda (w) (car (gethash w word-hash))) words)))
         (indices-unique (remove-duplicates indices :test #'=))
         (len (length indices-unique))
         (index-vector (make-array len :element-type 'fixnum :initial-contents indices-unique))
         (value-vector (make-array len :element-type 'single-float :initial-element 0.0)))
    (loop for i from 0 below len do
      (loop for index in indices do
        (when (= (aref index-vector i) index)
          (incf (aref value-vector i) 1.0))))
    (make-sparse-vector index-vector value-vector)))

(defun wc->tf! (wc-vec)
  (flet ((sum-vec (vec)
           ;;(declare (type (simple-array single-float) vec))
           (let ((sum 0.0))
             ;;(declare (type single-float sum))
             (loop for i fixnum from 0 below (length vec) do
               (incf sum (aref vec i)))
             sum)))
    (let* ((value-vector (sparse-vector-value-vector wc-vec))
           (sum (sum-vec value-vector)))
      (loop for i from 0 below (length value-vector) do
        (setf (aref value-vector i) (/ (aref value-vector i) sum)))
      wc-vec)))

;; Count the number of documents in which words appeared
(defun make-doc-count-vec (number-of-words word-count-sparse-vectors)
  (let ((doc-count-vec (make-array number-of-words :element-type 'single-float
                                                   :initial-element 1.0 ; to avoid divide-by-zero
                                                   )))
    (dolist (wc-vec word-count-sparse-vectors)
      (loop for i from 0 below (sparse-vector-length wc-vec) do
        (let ((word-index (aref (sparse-vector-index-vector wc-vec) i)))
          (incf (aref doc-count-vec word-index) 1.0))))
    doc-count-vec))

(defun doc-count-vec->idf! (doc-count-vec)
  (let ((len (length doc-count-vec)))
    (loop for i from 0 to (1- len) do
      (setf (aref doc-count-vec i)
            (+ (log (/ len (aref doc-count-vec i))) 1.0)))
    doc-count-vec))

(defun make-tf-idf-list (texts word-hash)
  (let* ((word-count-sparse-vectors (mapcar (lambda (text)
                                              (make-word-count-sparse-vector text word-hash))
                                            texts))
         (doc-count-vec (make-doc-count-vec (hash-table-count word-hash) word-count-sparse-vectors)))
    (doc-count-vec->idf! doc-count-vec)
    (dolist (wc-vec word-count-sparse-vectors)
      (wc->tf! wc-vec)
      (ds2s-v* doc-count-vec wc-vec wc-vec))
    word-count-sparse-vectors))

(defun make-tf-idf-list-from-files (files &key (infrequent-threshold 10))
  (let ((word-hash (make-word-hash-from-files files :infrequent-threshold infrequent-threshold)))
    (values (make-tf-idf-list (mapcar #'alexandria:read-file-into-string files)
                              word-hash)
            word-hash)))
