#lang planet neil/sicp

; Huffman coding
;   a source string to a list of number(2 base)

; huffman-tree-node
(define (make-leaf-node count character) (list count character))
(define (leaf-node-count node) (car node))
(define (leaf-node-character node) (cadr node))
(define (leaf-node? node)
  (and
   (list? node)
   (= 2 (length node))
   (number? (leaf-node-count node))
   (char? (leaf-node-character node))))

(define (make-branch-node count left right) (list count left right))
(define (branch-node-count node) (car node))
(define (branch-node-left node) (cadr node))
(define (branch-node-right node) (caddr node))
(define (branch-node? node)
  (and
   (list? node)
   (= 3 (length node))
   (number? (branch-node-count node))
   (let ((left (branch-node-left node))
         (right (branch-node-right node)))
     (or (leaf-node? left) (branch-node? left))
     (or (leaf-node? right) (branch-node? right)))))

(define (node-count node)
  (if (leaf-node? node)
      (leaf-node-count node)
      (branch-node-count node)))


; compression: string -> huffman-tree (listof number)
(define (compression source)
  (let ((huffman-tree (create-tree source)))
    (list huffman-tree (encode source huffman-tree))))

; create-tree: string -> huffman-tree
(define (create-tree source)
  (car (make-leaf-node/all (counting source))))

; counting: string -> huffman-tree
(define (counting source)
  (define (counting-iter source tree)
    (if (null? source)
        tree
        (let ((accumulated (accumulate-count (string-ref source 0) tree)))
          (if (= (string-length source) 1)
              accumulated
              (counting-iter (substring source 1) accumulated)))))
  (counting-iter source '()))

; accumulate-count: character huffman-tree -> huffman-tree
(define (accumulate-count character huffman-tree)
  (if (null? huffman-tree)
      (list (make-leaf-node 1 character))
      (let ((count (leaf-node-count (car huffman-tree)))
            (char (leaf-node-character (car huffman-tree)))
            (first (car huffman-tree))
            (rest (cdr huffman-tree)))        
        (if (eq? character char)
            (cons (make-leaf-node (inc count) character) rest)
            (cons first (accumulate-count character rest))))))

; make-leaf-node/all: huffman-tree -> huffman-tree
(define (make-leaf-node/all tree)
  (if (<= (length tree) 1)
      tree
      (make-leaf-node/all (merge/front-pair (sort tree)))))

; merge/front-pair: huffman-tree -> huffman-tree
(define (merge/front-pair tree)
  (let ((left (cadr tree))
        (right (car tree))
        (count (+ (node-count (cadr tree)) (node-count (car tree)))))    
    (cons (make-branch-node count left right) (cddr tree))))

; sort: huffman-tree -> huffman-tree
(define (sort tree)
  (if (null? tree)
      '()
      (insert (car tree) (sort (cdr tree)))))

; insert: huffman-tree-node huffman-tree -> huffman-tree
(define (insert node tree)
  (cond
    ((null? tree) (list node))
    ((<= (node-count node) (node-count (car tree))) (cons node tree))
    (else (cons (car tree) (insert node (cdr tree))))))

; encode: string huffman-tree -> (listof number)
(define (encode source tree)
  (if (null? source)
      '()
      (let ((code (get-code (string-ref source 0) tree)))
        (if (= (string-length source) 1)
            code
            (append code (encode (substring source 1)  tree))))))

; get-code: character huffman-tree -> (listof number)
(define (get-code character tree)
  (cond
    ((null? tree) false)
    ((leaf-node? tree)
     (if (eq? character (leaf-node-character tree))
         '()
         false))
    (else
     (let ((found-left (get-code character (branch-node-left tree))))
       (if (list? found-left)
           (cons 0 found-left)
           (cons 1 (get-code character (branch-node-right tree))))))))



; decompression: (listof number) huffman-tree -> string

; decode: (listof number) huffman-tree -> string

; get-charactor: (listof number) huffman-tree -> character
