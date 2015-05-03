(***********************************************)
(*****   Transform�e de Burrows-Wheeler    *****)
(***********************************************)

(**
   Encode en utilisant la transform�e de Burrows-Wheeler. 
   Renvoie l'indice de position ainsi que la s�quence 
   des derniers caract�res 
*)
val encode : 'a list -> int * 'a list

(**
   D�code la s�quence � partir de l'indice de position
   et la s�quence des derniers caract�res
*)
val decode : int * 'a list -> 'a list
