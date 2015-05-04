# Data-Compressor-BWHuffman (OCamL)
implementation of a text compression-decompression software based on Burrows-Wheeler transform and Huffman encoding.

##Launch
####Compression
*src/huffman -e textfile*
output: textfile.encoded
####Decompression
*src/huffman -d textfile.encoded*
output: textfile.decoded

##Complexity
|           Step          |Compression|Decompression|
|-------------------------|-----------|-------------|
|Burrows-Wheeler transform|     n²    |      n²     |
|Move-to-Front transform  |      n    |       n     |
|huffman encoding         |    n²*m   |      n*m    |

##Tests
tests

##Principle
To encode/decode data, there are three steps:
![BWHuffman principle](https://github.com/pleleux-enseeiht/Data-Compressor-BWHuffman/blob/master/Graphics/Principle.png "BWHuffman principle")

##Compression
####Burrows-Wheeler transform
1. generate the square matrix with all circular permutations of the text to encode,
2. lexicaly sort the lines of the matrix,
3. return the pair (line number of the original text, last column of the matrix).

*Example with __TEXTE__:*
```
EXTET     sort      ETEXT
XTETE   lexicaly    EXTET
TETEX       =>      TETEX   =>    (4 TTXEE)
ETEXT               TEXTE
TEXTE               XTETE
```

####Move to Front transform
This transformation consists in coding recursively each element of a string with a value (alphabet table for example with values from 0 to 25) then giving it the value 0 and incrementing the values of all elements with a previously inferior value.

*Example with __(4, TTXEE)__:*

|Iteration Letter|   Sequence   |              List             |
|----------------|--------------|-------------------------------|
|        T       |19            |   abcdefghijklmnopqrstuvwxyz  |
|        T       |19,0          | **t**abcdefghijklmnopqrsuvwxyz|
|        X       |19,0,23       | **t**abcdefghijklmnopqrsuvwxyz|
|        E       |19,0,23,6     | **xt**abcdefghijklmnopqrsuvwyz|
|        E       |19,0,23,6,0   |**ext**abcdefghijklmnopqrsuvwyz|

=> (4, (19,0,23,6,0))

####Huffman encoding
1. count the occurences of each element in the sequence to get a list of pairs (element, number of occurences),
2. transform this list into a list of sorted weighted trees,
3. recursively merge the pair of trees with lower weight to get a unique tree,
4. encode each element in the sequence (now a leaf in the tree) by bits going through the tree: 0 when taking the left branch and 1 taking the right branch until we get to the leaf of the element,
5. from the encoding of the elements, compose the encoding of the sequence,
6. the compressed text is the pair: **(encoding of the sequence, huffman tree)**

*Example with __(4, (19,0,23,6,0))__:*
```
(19,0,23,6,0) => {(0,2),(23,1),(19,1),(6,1)}

    |2   |1   |1   |1       |2   |1      |2      |2      |3               |5      (FINAL)
=>  |    |    |    |    =>  |    |      / \  =>  |      / \     =>      0/ \1     ( TREE)
    0    23   19   6        0    23    19  6     0     23  |2           0   |3
                                                          / \             0/ \1
                                                         19  6            23  |2
                                                                            0/ \1
                                                                            19  6
=>  0=0, 23=10, 19=110, 6=111   =>  (1100101110, FINAL TREE) => (4, 1100101110, FINAL TREE)
```

##Decompression
####Huffman decoding
1. From the tree recompose the transition table,
2. Decode the bit sequence (only one possible output thanks to the tree structure).
3. 
*Example with __(4, 1100101110, {0=0, 23=10, 19=110, 6=111})__:*

```
1100101110 => 19,0101110  =>  19,0,101110
=>  19,0,23,1110  =>  19,0,23,6,0 => 19,0,23,6,0
=> (4, (19,0,23,6,0))
```

####Move to Front transform
Exactly the same but we take the element of the table corresponding to value before updating the table.

*Example with __(4, TTXEE)__:*

|Iteration Element|Sequence |              List             |
|-----------------|---------|-------------------------------|
|        19       |T        |   abcdefghijklmnopqrstuvwxyz  |
|        0        |T,T      | **t**abcdefghijklmnopqrsuvwxyz|
|        23       |T,T,X    | **t**abcdefghijklmnopqrsuvwxyz|
|        6        |T,T,X,E  | **xt**abcdefghijklmnopqrsuvwyz|
|        0        |T,T,X,E,E|**ext**abcdefghijklmnopqrsuvwyz|

=> (4, TTXEE)

####Burrows-Wheeler transform
1. recreate the permutation matrix: paste recursively the sequence obtained previously and sort lexicaly the obtained matrix,
2. pick the good line, it's the original text.

*Example with __(4, TTXEE)__:*
```
    T     E     TE      ET      TET   4  ETE
    T     E     TE      EX      TEX     EXT
=>  X =>  T =>  XT  =>  TE  =>  XTE =>  TET   
    E     T     ET      TE      ETE     TEX
    E     X     EX      XT      EXT     XTE

    TETE    ETEX      TETEX     ETEXT   pick
    TEXT    EXTE      TEXTE     EXTET   4th
=>  XTET => TETE  =>  XTETE =>  TETEX    =>  TEXTE
    ETEX    TEXT      ETEXT     TEXTE
    EXTE    XTET      EXTET     XTETE
```
