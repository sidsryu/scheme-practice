#lang planet neil/sicp

; Huffman coding
;   a source string to a list of numbers(2 base)

; huffman-tree-node
;   leaf-node
(define (make-leaf-node count char)
  (list count char))

(define (leaf-node-count node)
  (car node))

(define (leaf-node-character node)
  (cadr node))

(define (leaf-node? node)
  (and (list? node)
       (= 2 (length node))
       (number? (leaf-node-count node))
       (char? (leaf-node-character node))))

;   branch-node
(define (make-branch-node count left right)
  (list count left right))

(define (branch-node-count node)
  (car node))

(define (branch-node-left node)
  (cadr node))

(define (branch-node-right node)
  (caddr node))

(define (branch-node? node)
  (and (list? node)
       (= 3 (length node))
       (number? (branch-node-count node))
       (let ((left (branch-node-left node))
             (right (branch-node-right node)))
         (or (leaf-node? left) (branch-node? left))
         (or (leaf-node? right) (branch-node? right)))))

;   node common procedure
(define (node-count node)
  (if (leaf-node? node)
      (leaf-node-count node)
      (branch-node-count node)))


; compression: string -> node (listof numbers)
;   a source text to a list of numbers(2base) with huffman tree
(define (compression source)
  (let ((huffman-tree (create-huffman-tree source)))
    (list huffman-tree (encode source huffman-tree))))


; create-huffman-tree: string -> node
;   return a root node of huffman tree
(define (create-huffman-tree source)
  (frequency-table->huffman-tree (make-frequency-table source)))


; make-frequency-table: string -> (listof nodes)
;   the frequency table is a list of leaf nodes
(define (make-frequency-table source)
  (define (iter begin end nodes)
    (if (= begin end)
        nodes
        (iter (inc begin) end
              (increase-count (string-ref source begin) nodes))))
  (define (increase-count character nodes)
    (define (make-new-node)
      (make-leaf-node 1 character))
    (define (make-increased-node)
      (make-leaf-node (inc (leaf-node-count (car nodes))) (leaf-node-character (car nodes))))
    (define (found-node?)
      (equal? character (leaf-node-character (car nodes))))
    (cond
      ((null? nodes) (list (make-new-node)))
      ((found-node?) (cons (make-increased-node) (cdr nodes)))
      (else (cons (car nodes) (increase-count character (cdr nodes))))))
  (iter 0 (string-length source) '()))


; frequency-table->huffman-tree: (listof nodes) -> node
;    merge all nodes below a single root
(define (frequency-table->huffman-tree nodes)
  (define (merge/all l)
    (if (<= (length l) 1)
        l
        (merge/all (merge/front-two (sort/ascending l)))))
  (define (merge/front-two l)
    (let ((left (cadr l))
          (right (car l)))
      (cons (make-branch-node (+ (node-count left) (node-count right))
                              left
                              right)
            (cddr l))))
  (car (merge/all nodes)))


; sort/ascending: (listof nodes) -> (listof nodes)
;   insertion sort
(define (sort/ascending nodes)
  (define (insert v l)
    (cond
      ((null? l) (list v))
      ((<= (node-count v) (node-count (car l))) (cons v l))
      (else (cons (car l) (insert v (cdr l))))))
  (if (null? nodes)
      '()
      (insert (car nodes) (sort/ascending (cdr nodes)))))


; encode: string node -> (listof number)
;   string with huffman tree to a list of numbers(2base)
(define (encode source tree)
  (define (iter begin end)
    (if (= begin end)
        '()
        (append (get-code (string-ref source begin) tree)
                (iter (inc begin) end))))
  (define (get-code character node)
    (cond
      ((null? node) false)
      ((leaf-node? node)
       (if (equal? character (leaf-node-character node))
           '()
           false))
      (else
       (let ((found-left (get-code character (branch-node-left node))))
         (if (list? found-left)
             (cons 0 found-left)
             (cons 1 (get-code character (branch-node-right node))))))))
  (iter 0 (string-length source)))


; decompression: (listof numbers) node -> string
;   a list of numbers(2base) with huffman tree to string
(define (decompression source tree)
  (define (decode s node)
    (cond
      ((leaf-node? node) (string-append (string (leaf-node-character node)) (decode s tree)))
      ((null? s) "")
      (else (decode (cdr s) (next-node (car s) node)))))
  (define (next-node bit branch)
    (if (= bit 0)
        (branch-node-left branch)
        (branch-node-right branch)))
  (decode source tree))


; Test
(define Lyric
"Is this the real life?
Is this just fantasy?
Caught in a landslide
No escape from reality
Open your eyes
Look up to the skies and see
I'm just a poor boy, I need no sympathy
Because I'm easy come, easy go
A little high, little low
Anyway the wind blows, doesn't really matter to me, to me
Mama, just killed a man
Put a gun against his head
Pulled my trigger, now he's dead
Mama, life had just begun
But now I've gone and thrown it all away
Mama, oh oh 
Didn't mean to make you cry
If I'm not back again this time tomorrow
Carry on, carry on, as if nothing really matters
Too late, my time has come
Sends shivers down my spine
Body's aching all the time
Goodbye everybody I've got to go
Gotta leave you all behind and face the truth
Mama, oh oh (anyway the wind blows)
I don't want to die
Sometimes wish I'd never been born at all
I see a little silhouetto of a man
Scaramouch, Scaramouch will you do the Fandango
Thunderbolt and lightning very very frightening me
Gallileo, Gallileo, Gallileo, Gallileo, Gallileo, figaro, magnifico
I'm just a poor boy and nobody loves me
He's just a poor boy from a poor family
Spare him his life from this monstrosity
Easy come easy go will you let me go
Bismillah, no we will not let you go, let him go
Bismillah, we will not let you go, let him go
Bismillah, we will not let you go, let me go
(Will not let you go) let me go (never, never let you go) let me go (never let me go)
Oh oh no, no, no, no, no, no, no
Oh mama mia, mama mia, mama mia let me go
Beelzebub has a devil put aside for me for me for me
So you think you can stop me and spit in my eye
So you think you can love me and leave me to die
Oh baby can't do this to me baby
Just gotta get out just gotta get right outta here
Oh oh oh yeah, oh oh yeah
Nothing really matters
Anyone can see
Nothing really matters 
Nothing really matters to me
Anyway the wind blows")

(define HuffmanCoding (compression Lyric))
(define HuffmanTree (car HuffmanCoding))
(define Compressed (cadr HuffmanCoding))

(define Decompressed (decompression Compressed HuffmanTree))

(equal? Lyric Decompressed)
