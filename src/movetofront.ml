                            (*****************************)
                            (**       Projet CAML       **)
                            (**      Move To Front      **)
                            (*****************************)



(****************************)
(** Codage Fonction Encode **)
(****************************)


	(*************************)
	(**Fonctions auxiliaires**)
	(*************************)

	(**********************************************************************)
	(*Cette fonction renvoie vrai si l'element est dans la liste          *)
	(*                                                                    *)
	(*Parametres : element : element a chercher dans la liste             *)
	(*             liste : liste dans laquelle on cherche                 *)
	(*                                                                    *)
	(*Retour : booleen                                                    *)
	(*                                                                    *)
	(*Fonction : occurence :  'a -> 'a list -> bool = <fun>               *)
	(**********************************************************************)
	(*tests :                                                             *)
	(*# occurrence 1 [1;2;3];;                                            *)
	(*- : bool = true                                                     *)
	(*# occurrence 't' [];;                                               *)
	(*- : bool = false                                                    *)
	(*# occurrence [1] [[2;3];[56]];                                      *)
	(*- : bool = false                                                    *)
	(**********************************************************************)
	let rec appartient element liste = 
		match liste with
			|[] -> false;
			|t::q -> if t = element then true
					    	else appartient element q;;

	(**********************************************************************)
	(*Deplace l'element donne en tete de la liste                         *)
	(*                                                                    *)
	(*Parametres : element : element a inserer dans la liste              *)
	(*             liste : liste dans laquelle on insere                  *)
	(*                                                                    *)
	(*Retour : liste                                                      *)
	(*                                                                    *)
	(*Fonction : deplacer_en_tete : 'a -> 'a list -> 'a list = <fun>      *)
	(**********************************************************************)
	(*tests :                                                             *)
	(*#deplacer_en_tete 4 [];;                                            *)
	(*- : int list = [];;                                                 *)
	(*#deplacer_en_tete 5 [4;4;3;5;3];;                                   *)
	(*- : int list = [5;4;4;3;3]                                          *)
	(**********************************************************************)
	let deplacer_en_tete element liste =
	        let rec aux element liste =
	                match liste with
	                |[] -> []
	                |t::q -> if t=element then q
      	                                    else t::(aux element q) 
		in element::(aux element liste);;

	(**********************************************************************)
	(*Renvoie la position de l'element dans la liste                      *) 
	(*  (premier element -> position 0)                                   *)
	(*                                                                    *)
	(*Parametres : element : element dont on veut trouver la position     *)
	(*             liste : liste dans laquelle on cherche                 *)
	(*                                                                    *)
	(*Retour : entier : position de element                               *)
	(*                                                                    *)	
	(*Precondition : l'element est dans la liste                          *)
	(*                                                                    *)
	(*Fonction : position : 'a -> 'a list -> int = <fun>                  *)
	(**********************************************************************)
	(*tests :                                                             *)
	(*#position 4 [];;                                                    *)
	(*Exception: Failure "position : element absent"                      *)
	(*#position 5 [4;4;3;5;3];;                                           *)
	(*- : int = 3                                                         *)
	(**********************************************************************)
	let rec position element liste =
		match liste with
    			|[] -> failwith("position : element absent")
    			|t::q -> if t = element then 0
             				    else 1+(position element q);;

(******************************************************************************)
(*Encode une liste d'unites grâce a une fonction de codage arbitraire par     *) 
(*defaut.  Pour les caracteres, cela peut etre le code ASCII par exemple      *)
(*                                                                            *)
(*Parametres : f : fonction de codage arbitraire                              *) 
(*             liste : liste a encoder                                        *)
(*                                                                            *)
(*Retour : liste encodee                                                      *)
(*                                                                            *)
(*Fonction : encode : ('a -> int) -> 'a list -> int list = <fun>              *)
(******************************************************************************)
(*tests :                                                                     *)
(*#encode Char.code [];;                                                      *)
(*- : int list = []                                                           *)
(*#encode Char.code ['t';'e';'s';'t'];;                                       *)
(*- : int list = [116; 102; 116; 2]                                           *)
(*#encode Char.code ['m';'o';'v';'e';' ';'t';'o';' ';'f';'r';'o';'n';'t'];;   *)
(*- : int list = [109; 111; 118; 104; 36; 117; 4; 2; 106; 116; 3; 114; 5]     *)           
(******************************************************************************)

let rec encode fonction liste =
	let rec aux fonction liste_ini liste_fin liste_passe =
		match liste_ini with
			|[] -> List.rev liste_fin
			|(t::q) -> if (appartient t liste_passe) 
			  then (aux fonction q ((position t liste_passe)::liste_fin)
                               (deplacer_en_tete t liste_passe))
                          else aux fonction q (-(fonction t)::liste_fin) (t::liste_passe)
   	in aux fonction liste [] [];;



(****************************)
(** Codage Fonction Decode **)
(****************************)


	(*************************)
	(**Fonctions auxiliaires**)
	(*************************)

	(**********************************************************************)
	(*renvoie l entier la position n                                      *)
	(*                                                                    *)
	(*Parametres : entier : position dans la liste                        *)
	(*             liste : liste dans laquelle on prend l'entier          *)
	(*                                                                    *)
	(*Retour : entier                                                     *)
	(*                                                                    *)
	(*Precondition : la liste contient plus de n elements                 *)
	(*                                                                    *)
	(*Fonction : precn: int -> int list -> int = <fun>                    *)
	(**********************************************************************)
	(*tests :                                                             *)
	(*# precn 0 [1;5;4;2];;                                               *)
	(*- : int = 1                                                         *)
	(*# precn 3 [1;5;3;6;8;7;1];;                                         *)
	(*- : int = 6                                                         *)
	(*# occurrence 0 [];                                                  *)
	(*Exception: Failure "entier_n : liste vide".                        *)
	(**********************************************************************)
	let entier_n n liste =
		let rec aux n liste liste_passe = 
			match n,liste with
				|_,[] -> failwith("entier_n : liste vide")      
				|0,t::q -> if appartient t liste_passe 
						then aux 0 q liste_passe
         					else t
     				|_,t::q -> if appartient t liste_passe 
						then aux n q liste_passe
 						else aux(n-1) q (t::liste_passe)
   		in aux n liste [];;

(******************************************************************************)
(*Cette fonction doit decoder une liste codee par la methode move to front    *)
(*  en prenant une fonction de codage arbitraire en parametre                 *)
(*                                                                            *)
(*Parametres : f : fonction de codage arbitraire                              *) 
(*             liste : liste a decoder                                        *)
(*                                                                            *)
(*Retour : liste decodee                                                      *)
(*                                                                            *)
(*Fonction : encode : (int -> 'a) -> int list -> 'a list = <fun>              *)
(******************************************************************************)
(*tests :                                                                     *) 
(*#decode Char.chr [];;                                                       *)
(*- : char list = []                                                          *)
(*#decode Char.chr [116; 102; 116; 2];;                                       *)
(*- : char list = ['t'; 'e'; 's'; 't']                                        *)
(*#decode Char.chr [109; 111; 118; 104; 36; 117; 4; 2; 106; 116; 3; 114; 5];; *)
(*- : char list = ['m';'o';'v';'e';' ';'t';'o';' ';'f';'r';'o';'n';'t']       *)
(******************************************************************************)
let decode fonction liste =
	let rec aux fonction liste_codee liste_decodee = 
		match liste_codee with
			|[] -> liste_decodee
        		|t::q -> if t<0 then aux fonction q ((fonction (-t))::liste_decodee)
                    			else aux fonction q ((entier_n t liste_decodee)::liste_decodee)
	in List.rev (aux fonction liste []);;
