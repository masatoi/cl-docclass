;;; -*- coding:utf-8; mode:lisp -*-

(in-package :cl-user)

(defpackage cl-docclass
  (:use :cl :cl-online-learning :cl-online-learning.utils :cl-online-learning.vector :wiz)
  (:nicknames :docclass)
  (:shadow :shuffle-vector))

(in-package :cl-docclass)

#| Prepare for cl-igo

Download IPA dictionary for mecab
$ wget https://sourceforge.net/projects/mecab/files/mecab-ipadic/2.7.0-20070801/

Download igo-0.4.5.jar
$ wget https://osdn.net/projects/igo/downloads/55029/igo-0.4.5.jar/

Make Igo dictionary
$ java -cp ./igo-0.4.5.jar net.reduls.igo.bin.BuildDic ipadic mecab-ipadic-2.7.0-20070801 EUC-JP

|#

;; Read igo dictionary
(igo:load-tagger (format-directory (merge-pathnames #P"igo/ipadic/" (user-homedir-pathname))))

(defun extract-word (text)
  (mapcar #'car
	  (remove-if-not (lambda (word)
			   (or (string= (subseq (cadr word) 0 2) "名詞")
			       (string= (subseq (cadr word) 0 3) "形容詞")
			       (string= (subseq (cadr word) 0 2) "動詞")))
			 (igo:parse text))))

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

;; Count the number of words which appears in the text
(defun make-word-count-vec (text word-hash)
  (let* ((words (extract-word text))
         (indices (remove nil (mapcar (lambda (w) (car (gethash w word-hash))) words)))
         (indices-unique (remove-duplicates indices :test #'=))
         (len (length indices-unique))
         (index-vector (make-array len :element-type 'fixnum :initial-contents indices-unique))
         (value-vector (make-array len :element-type 'double-float :initial-element 0d0)))
    (loop for i from 0 to (1- len) do
      (loop for index in indices do
        (when (= (aref index-vector i) index)
          (incf (aref value-vector i) 1d0))))
    (make-sparse-vector index-vector value-vector)))

(defun wc->tf! (wc-vec)
  (flet ((sum-vec (vec)
           (declare (type (simple-array double-float) vec))
           (let ((sum 0d0))
             (declare (type double-float sum))
             (loop for i fixnum from 0 to (1- (length vec)) do
               (incf sum (aref vec i)))
             sum)))
    (let* ((value-vector (sparse-vector-value-vector wc-vec))
           (sum (sum-vec value-vector)))
      (loop for i from 0 to (1- (length value-vector)) do
        (setf (aref value-vector i) (/ (aref value-vector i) sum)))
      wc-vec)))

;; Count the number of documents in which words appeared
(defun make-doc-count-vec (word-count wc-vec-list)
  (let ((doc-count-vec (make-array word-count :element-type 'double-float :initial-element 0d0)))
    (dolist (wc-vec wc-vec-list)
      (loop for i from 0 to (1- (sparse-vector-length wc-vec)) do
        (incf (aref doc-count-vec (aref (sparse-vector-index-vector wc-vec) i)) 1d0)))
    doc-count-vec))

(defun doc-count-vec->idf! (doc-count-vec)
  (let ((len (length doc-count-vec)))
    (loop for i from 0 to (1- len) do
      (setf (aref doc-count-vec i)
            (+ (log (/ len (aref doc-count-vec i))) 1d0)))
    doc-count-vec))

(defun make-tf-idf-list (texts word-hash)
  (let* ((wc-vec-list (mapcar (lambda (text) (make-word-count-vec text word-hash))
                              texts))
         (doc-count-vec (make-doc-count-vec (hash-table-count word-hash) wc-vec-list)))
    (doc-count-vec->idf! doc-count-vec)
    (dolist (wc-vec wc-vec-list)
      (wc->tf! wc-vec)
      (ds2s-v* doc-count-vec wc-vec wc-vec))
    wc-vec-list))

(defun make-tf-idf-list-from-files (files word-hash)
  (make-tf-idf-list (mapcar #'alexandria:read-file-into-string files)
                    word-hash))
