(***********************************************)
(***            Move to front                ***)
(***********************************************)

(** Encode une liste d'unit�s gr�ce � une fonction 
    de codage arbitraire par d�faut. Pour les caract�res,
    cela peut �tre le code ASCII par exemple
*)
val encode : ('a -> int) -> 'a list -> int list

(** D�code la liste d'unit�s gr�ce � une fonction
    de d�codage par d�faut. Cette fonction
    doit �tre la r�ciproque de celle utilis�e
    par la fonction 'encode'
*)
val decode : (int -> 'a) -> int list -> 'a list
