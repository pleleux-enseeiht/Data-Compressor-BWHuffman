
(****************************************************)
(***   Fonctions de compression / d�compression   ***)
(***   par d�faut. Aucune compression.            ***)
(***   Fonctionne avec toute unit�.               ***)
(****************************************************)


(***********************************************)
(***   Compression/d�compression  de Huffman ***)
(***********************************************)

type 'a huffman_defaut = unit

(** Construit la valeur ()
*)
val build_hfm : 'a list -> 'a huffman_defaut

(** Encode sans compression en associant directement � chaque
    valeur sa repr�sentation m�moire interne
*)
val encode_hfm : 'a list -> 'a huffman_defaut * bool list

(** Fonction r�ciproque de la pr�c�dente.
    Attention � bien d�coder une valeur du m�me type
    que la valeur encod�e, sous peine d'arr�t brutal
    de l'ex�cution ! Une solution pour s'en assurer 
    est de typer explicitement les utilisations de 
    cette fonction, i.e. d'�crire:
    (decode_hfm ((), bl) : le_type_des_unit�s)
*)
val decode_hfm : 'a huffman_defaut * bool list -> 'a list


(***********************************************)
(***            Move to front                ***)
(***********************************************)

(** Encode une liste d'unit�s gr�ce � une fonction
    de codage par d�faut. Aucun move-to-front n'est
    effectu�, chaque unit� a toujours le m�me code
*)
val encode_mtf : ('a -> int) -> 'a list -> int list

(** Fonction r�ciproque de la pr�c�dente.
*)
val decode_mtf : (int -> 'a) -> int list -> 'a list


(***********************************************)
(*****   Transform�e de Burrows-Wheeler    *****)
(***********************************************)

(**
   Encode en utilisant la permutation identit�.
   L'entier renvoy� est toujours 0.
*)
val encode_bw : 'a list -> int * 'a list

(**
   Fonction r�ciproque de la pr�c�dente.
   N'utilise pas l'entier pass� en argument.
*)
val decode_bw : int * 'a list -> 'a list
