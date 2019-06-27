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


; compression: string -> huffman-tree (listof number)
(define (compression source)
  (encode source (create-tree source)))

; create-tree: string -> huffman-tree
(define (create-tree source)
  (make-leaf-node/all (counting source)))

; counting: string -> huffman-tree
(define (counting source))

; make-leaf-node/all: huffman-tree -> huffman-tree
(define (make-leaf-node/all tree))

; encode: string huffman-tree -> (listof number)
(define (encode source tree))

; get-code: character huffman-tree -> (listof number)

; decompression: (listof number) huffman-tree -> string

; decode: (listof number) huffman-tree -> string

; get-charactor: (listof number) huffman-tree -> character
