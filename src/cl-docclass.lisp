;;; -*- coding:utf-8; mode:lisp -*-

(in-package :cl-user)

(defpackage cl-docclass
  (:use :cl :cl-online-learning :cl-online-learning.utils :cl-online-learning.vector :wiz)
  (:nicknames :docclass)
  (:shadow :shuffle-vector))

(in-package :cl-docclass)

;; IPA辞書をダウンロード https://sourceforge.net/projects/mecab/files/mecab-ipadic/2.7.0-20070801/
;; igo-0.4.5.jarをダウンロード https://osdn.net/projects/igo/downloads/55029/igo-0.4.5.jar/
;; java -cp ./igo-0.4.5.jar net.reduls.igo.bin.BuildDic ipadic mecab-ipadic-2.7.0-20070801 EUC-JP

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
(defvar *word-hash*)
(defvar *word-index*)

(defun add-words-to-hash! (file &optional (word-hash *word-hash*))
  (let ((words (extract-word (alexandria:read-file-into-string file))))
    (loop for word in words do
      (when (null (gethash word word-hash))
        (setf (gethash word word-hash) *word-index*)
        (incf *word-index*)))))

(defun init-word-hash! (files)
  (setf *word-hash* (make-hash-table :test 'equal)
        *word-index* 0)
  (dolist (file files)
    (add-words-to-hash! file *word-hash*))
  *word-hash*)

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

(defun wc->tf! (wc-vec)
  (let* ((value-vector (sparse-vector-value-vector wc-vec))
         (sum (sum-vec value-vector)))
    (loop for i from 0 to (1- (length value-vector)) do
      (setf (aref value-vector i) (/ (aref value-vector i) sum)))
    wc-vec))

(defun make-wc-vec-list-livedoor-data ()
  (mapcar (lambda (file)
            (make-word-count-vec (alexandria:read-file-into-string file)))
          *livedoor-data-files*))

(defun make-word-appear-doc-count-vec (dimension wc-vec-list)
  (let ((doc-count-vec (make-array dimension :element-type 'double-float :initial-element 0d0)))
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

(defun make-tf-idf-list (files &optional (word-hash *word-hash*) (dimension *word-index*))
  (let* ((wc-vec-list (mapcar
                       (lambda (file)
                         (make-word-count-vec (alexandria:read-file-into-string file) word-hash))
                       files))
         (doc-count-vec (make-word-appear-doc-count-vec dimension wc-vec-list)))
    (doc-count-vec->idf! doc-count-vec)
    (dolist (wc-vec wc-vec-list)
      (wc->tf! wc-vec)
      (ds2s-v* doc-count-vec wc-vec wc-vec))
    wc-vec-list))
