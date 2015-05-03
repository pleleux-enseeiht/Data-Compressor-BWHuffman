(***********************************************)
(***   Compression/d�compression  de Huffman ***)
(***********************************************)

(** Type de l'arbre binaire utilis� pour coder le document
*)
type 'a huffmantree

(** Prend une liste d'�l�ment et construit l'arbre de codage
*)
val build_tree : 'a list -> 'a huffmantree

(** Prend une liste d'�l�ments et renvoie le couple 
    (arbre de codage de huffman, codage binaire de la liste)
*)
val encode : 'a list -> 'a huffmantree * bool list

(** Prend un arbre de codage de huffman et une liste de bool�ens 
    et reconstruit la liste d'�l�ments d�cod�e
*)
val decode : 'a huffmantree * bool list -> 'a list
