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
  (define (counting-iter source begin end tree)
    (if (= begin end)
        tree
        (counting-iter source (inc begin) end
                       (accumulate-count (string-ref source begin) tree))))
  (counting-iter source 0 (string-length source) '()))

; accumulate-count: character huffman-tree -> huffman-tree
(define (accumulate-count character huffman-tree)
  (if (null? huffman-tree)
      (list (make-leaf-node 1 character))
      (let ((count (leaf-node-count (car huffman-tree)))
            (char (leaf-node-character (car huffman-tree)))
            (first (car huffman-tree))
            (rest (cdr huffman-tree)))        
        (if (equal? character char)
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
     (if (equal? character (leaf-node-character tree))
         '()
         false))
    (else
     (let ((found-left (get-code character (branch-node-left tree))))
       (if (list? found-left)
           (cons 0 found-left)
           (cons 1 (get-code character (branch-node-right tree))))))))



; decompression: (listof number) huffman-tree -> string
(define (decompression source tree)
  (list->string (decode source tree tree)))

; decode: (listof number) huffman-tree huffman-tree -> string
(define (decode source node tree)
  (cond
    ((leaf-node? node) (cons (leaf-node-character node) (decode source tree tree)))
    ((null? source) '())
    (else (decode (cdr source) (next-node (car source) node) tree))))

; next-node: number huffman-tree -> huffman-tree
(define (next-node n tree)
  (if (= n 0)
      (branch-node-left tree)
      (branch-node-right tree)))


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
