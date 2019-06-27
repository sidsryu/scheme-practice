#lang planet neil/sicp

; Huffman coding
;   a source string to a list of number(2 base)

; compression: string -> huffman-tree (listof number)

; create-tree: string -> huffman-tree

; make-leaf-node/all: huffman-tree -> huffman-tree

; encode: string huffman-tree -> (listof number)

; get-code: character huffman-tree -> (listof number)


; decompression: (listof number) huffman-tree -> string

; decode: (listof number) huffman-tree -> string

; get-charactor: (listof number) huffman-tree -> character

