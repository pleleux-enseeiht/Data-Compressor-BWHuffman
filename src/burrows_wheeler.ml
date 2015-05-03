                            (*****************************)
                            (**       Projet CAML       **)
                            (**     Burrows-Wheeler     **)
                            (*****************************)



(****************************)
(** Codage Fonction Encode **)
(****************************)


	(*************************)
	(**Fonctions auxiliaires**)
	(*************************)

	(*********************************************************************)
	(*Cette fonction cree les rotations d'une liste                      *)
	(*                                                                   *)
	(*Parametres : liste : liste donc on fait les rotations              *)
	(*             n : indice de parcours : List.length liste a 0        *)
	(*                                                                   *)
	(*Retour : liste des rotations                                       *)
	(*                                                                   *)
	(*Fonction :                                                         *)
	(* creer_permutations : 'a list -> int -> 'a list list = <fun>       *)
	(*********************************************************************)
	(*tests :                                                            *)
	(*#creer_permutations [];;                                           *)
	(*- : 'a list list = []                                              *)
	(*#creer_permutations ['h';'a';'h';'a'];;                            *)
	(*- : char list list =                                               *)
	(*[['a'; 'h'; 'a'; 'h']; ['h'; 'a'; 'h'; 'a']; ['a'; 'h'; 'a'; 'h']; *)
 	(*['h'; 'a'; 'h'; 'a']]                                              *)
	(*********************************************************************)
	let creer_permutations liste =		
		let rec aux liste n =
			match n,liste with
			       |_,[] -> []
			       |0,_ -> []
			       |n,t::q -> [q@[t]]@(aux (q@[t]) (n-1))
		in aux liste (List.length liste);;

	(**********************************************************************)
	(*Cette fonction cree la relation d'ordre alphabetique                *)
	(*                                                                    *)
	(*Parametres : liste1 et liste2 : listes a comparer                   *)
	(*                                                                    *)
	(*Retour : booleen vrai si liste1 inferieure ou egale a liste2        *)
	(*                                                                    *)
	(*Fonction : ordre_alphabetique : 'a list -> 'a list -> bool = <fun>  *)
	(**********************************************************************)
	(*#ordre_alphabetique [] ['t';'e';'s';'t'];;                          *)
	(*- : bool = true                                                     *)
	(*#ordre_alphabetique ['t';'e';'s';'t'] [];;                          *)
	(*- : bool = false                                                    *)
	(*#ordre_alphabetique [] [];;                                         *)
	(*- : bool = true                                                     *)
	(*#ordre_alphabetique ['h';'a';'h';'a'] ['h';'a';'h';'a'];;           *)
	(*- : bool = true                                                     *)
	(*#ordre_alphabetique ['h';'a';'h';'a'] ['t';'e';'s';'t'];;           *)
	(*- : bool = true                                                     *)
	(*#ordre_alphabetique ['t';'e';'s';'t'] ['h';'a';'h';'a'];;           *)
	(*- : bool = false                                                    *)
	(**********************************************************************)
let rec ordre_alphabetique liste1 liste2 =
		match liste1,liste2 with
			|[],[] -> true
			|[],_ -> true
			|_,[] -> false
			|t1::q1,t2::q2 -> if t1>t2 then false
					      else if t2>t1 then true
					         else ordre_alphabetique q1 q2;;

	(**********************************************************************)
	(*Cette fonction decompose une liste en deux listes de meme taille    *)
	(*                                                                    *)
	(*Parametres : liste : liste a decomposer                             *)
	(*                                                                    *)
	(*retour : liste1 et liste2 : les deux moitiees de liste              *)
	(*                                                                    *)
	(*Fonction : decomp : 'a list -> 'a list -> 'a list = <fun>           *)
	(**********************************************************************)
	(*tests :                                                             *)
	(*#decomp [];;                                                        *)
	(*- : 'a list * 'a list = [],[]                                       *)
	(*#decomp ['t';'e';'s';'t'];;                                         *)
	(*- : char list * char list = (['s'; 't'], ['t'; 'e'])                *)
	(*#decomp ['b';'u';'r';'r';'o';'w';'s'];;                             *)
	(*- : char list * char list = (['w'; 'r'; 'u'], ['s'; 'o'; 'r'; 'b']) *)
	(**********************************************************************)
	let decomp liste =
		let rec aux liste1 liste2 liste=
			match liste with
				|[] -> liste1,liste2
				|t::q -> aux liste2 (t::liste1) q
		in aux [] [] liste;;

	(**********************************************************************)
	(*Cette fonction fusionne deux listes triees                          *)
	(*                                                                    *)
	(*Parametres : liste1 et liste2 : les deux listes a fusionner         *)
	(*                                                                    *)
	(*retour : liste : liste fusionnee                                    *)
	(*                                                                    *)
	(*Precondition : les listes doivent etre triees                       *)
	(*Postcondition : la liste doit etre triee                            *)
	(*                                                                    *)
	(*Fonction : compter_occurence : 'a -> 'a list = <fun>                *)
	(**********************************************************************)
	(*tests :                                                             *)
	(*#recomp [['h';'a';'h';'a'];['t';'e';'s';'t']]                       *)
	(*[['a';'l';'o';'r';'s'];['a';'t']];;                                 *)
	(*- : char list list =                                                *)
	(*[['a'; 'l'; 'o'; 'r'; 's']; ['a'; 't']; ['h'; 'a'; 'h'; 'a'];       *)
	(*  ['t'; 'e'; 's'; 't']]                                             *)                     
	(*#recomp [] [['h';'a';'h';'a'];['t';'e';'s';'t']];;                  *)
	(*- : char list [['h';'a';'h';'a'];['t';'e';'s';'t']]                 *)
	(*recomp [['h';'a';'h';'a'];['t';'e';'s';'t']] [];;                   *)
	(*- : char list [['h';'a';'h';'a'];['t';'e';'s';'t']]                 *)
	(*recomp [] [];;                                                      *)
	(*- : 'a list list= []                                                *)
	(**********************************************************************)
	let rec recomp liste1 liste2 =
		match liste1,liste2 with
			|[],_ -> liste2
			|_,[] -> liste1
			|t1::q1,t2::q2 -> if (ordre_alphabetique t1 t2) 
						      then t1::recomp q1 liste2
        	                          else t2::recomp liste1 q2;;

	(**********************************************************************)
	(*Cette fonction doit trier une liste par la methode de tri fusion    *)
	(*                                                                    *)
	(*Parametres : liste : liste a trier                                  *)
	(*                                                                    *)
	(*retour : liste : liste triee                                        *)
	(*                                                                    *)
	(*Fonction : triFusion : 'a list -> 'a list = <fun>                   *)
	(**********************************************************************)
	(*#triFusion [['t';'e';'s';'t'];['h';'a';'h';'a'];['a';'t']];;        *)
	(*- : char list list = [['a'; 't']; ['h'; 'a'; 'h'; 'a'];             *) 
	(*  ['t'; 'e'; 's'; 't']]                                             *)
	(*#triFusion [];;                                                     *)
	(*- : char list list = []                                             *)
	(*#triFusion [['h';'a';'h';'a']];;                                    *)
	(*- : char list list = [['h'; 'a'; 'h'; 'a']]                         *)
	(**********************************************************************)
	let rec triFusion liste =
		match liste with
			|[] -> []
			|[_] -> liste
			|_ -> let(liste1,liste2)=decomp liste
			       in recomp (triFusion liste1) (triFusion liste2);;

	(**********************************************************************)
	(*Cette fonction donne l'emplacement deliste dans la liste des permut *)
	(*                                                                    *)
	(*Parametres : liste : liste a chercher                               *)
	(*             liste_perm : liste des permutations                    *)
	(*             n : indice de parcours des permutations                *)
	(*                                                                    *)
	(*retour : indice : emplacement dans la liste des permutations        *)
	(*                                                                    *)
	(*Fonction : emplacement_liste : 'a list ->'a list list -> int = <fun>*)
	(**********************************************************************)
	(*#emplacement_liste ['h'; 'a'; 'h'; 'a'] [['t';'e';'s';'t'];         *)
	(*['h';'a';'h';'a']; ['a';'t']];;                                     *)
	(*- : int = 2                                                         *)
	(*#emplacement_liste [' '] [];;                                       *)
	(*Exception: Failure "emplacement_texte : texte absent".              *)
	(*#emplacement_liste [] [];;                                          *)
	(*Exception: Failure "emplacement_texte : liste vide".                *)
	(**********************************************************************)
	let emplacement_liste liste liste_perm =
		if liste=[] then failwith("emplacement_liste : liste vide")
		else let rec aux liste liste_perm n =
			match liste_perm with
			     |[] -> failwith("emplacement_texte : texte absent")
			     |t::q -> if t=liste then n
							else aux liste q (n+1)
		     in aux liste liste_perm 1;;

	(**********************************************************************)
	(*Cette fonction donne le dernier element d'une liste                 *)
	(*                                                                    *)
	(*Parametres : liste_perm : liste des permutations                    *)
	(*                                                                    *)
	(*retour : indice : emplacement dans la liste des permutations        *)
	(*                                                                    *)
	(*Fonction : derniere_lettre : 'a list -> 'a = <fun>*)
	(**********************************************************************)
	(*#derniere_lettre ['a'; 'h'; 'a'; 'h'];;                             *)
	(*- : char list = 'h'                                                 *)
	(*#derniere_lettre [];;                                               *)
	(*Exception: Failure "derniere_lettre : liste vide".                  *)
	(*#derniere_lettre [' '];;                                            *)
	(*- : char list = ' '                                                 *)
	(**********************************************************************)
	let rec derniere_lettre liste_perm =
		match liste_perm with
			|[] -> failwith("derniere_lettre : liste vide")
			|[t] -> t
			|t::q -> derniere_lettre q;;

	(**********************************************************************)
	(*Cette fonction donne la liste composee de la derniere lettre        *) 
	(*   de chaque permutation                                            *)
	(*                                                                    *)
	(*Parametres : liste_perm : liste des permutations                    *)
	(*                                                                    *)
	(*retour : liste : liste des dernieres lettres                        *)
	(*                                                                    *)
	(*Fonction : derniere_colonne_perm : 'a list list -> 'a list = <fun>       *)
	(**********************************************************************)
	(*#derniere_lettre [['a'; 'h'; 'a'; 'h']; ['h'; 'a'; 'h'; 'a'];       *)
	(*  ['a'; 'h'; 'a'; 'h']                                              *)
 	(*;['h'; 'a'; 'h'; 'a']]                                              *)
	(*- : char list = ['h'; 'a'; 'h'; 'a']                                *)
	(*#derniere_lettre [];;                                               *)
	(*Exception: Failure "derniere_lettre : liste vide".                  *)
	(*#derniere_lettre [[' ']];;                                          *)
	(*- : char list = [' ']                                               *)
	(**********************************************************************)
	let rec derniere_colonne_perm liste_perm =
		match liste_perm with
		       |[] -> []
		       |t::q -> (derniere_lettre t)::(derniere_colonne_perm q);;


(******************************************************************************)
(*Encode en utilisant la transformee de Burrows-Wheeler.                      *)
(*  Renvoie l'indice de position ainsi que la sequence des derniers caracteres*)
(*                                                                            *)
(*Parametres : liste : liste a encoder                                        *)
(*                                                                            *)
(*Retour : liste encodee                                                      *)
(*                                                                            *)
(*Fonction : encode : 'a list -> int * 'a list = <fun>                        *)
(******************************************************************************)
(*#encode ['b';'u';'r';'r';'o';'w';'s';' ';'w';'h';'e';'e';'l';'e';'r'];;     *)
(*- : int * char list =                                                       *)
(*(2,['s'; 'r'; 'h'; 'e'; 'l'; 'w'; 'e'; 'r'; 'e';'r';'u';'w'; 'b'; ' '; 'o'])*)
(*#encode ['t';'e';'x';'t';'e'];;                                             *)
(*- : int * char list = (4, ['t';'t';'x';'e';'e'])                            *)
(*#encode [];;                                                                *)
(*Exception: Failure "emplacement_texte : liste vide".                        *)
(*#encode [' '];;                                                             *)
(*- : int * char list = (1, [' '])                                            *)
(******************************************************************************)
let encode liste =
    let liste_perm=(triFusion (creer_permutations liste)) in
       (emplacement_liste liste liste_perm),(derniere_colonne_perm liste_perm);;


(****************************)
(** Codage Fonction Decode **)
(****************************)


	(*************************)
	(**Fonctions auxiliaires**)
	(*************************)

	(**********************************************************************)
	(*Cette fonction colle lettre par lettre la liste obtenu par encodage *) 
	(*  a une liste                                                       *)
	(*                                                                    *)
	(*Parametres : listeBW : liste obtenue a l encodage                   *)
	(*             liste : liste de mots                                  *)
	(*                                                                    *)
	(*Retour : liste de couples (code binaire, element)                   *)
	(*                                                                    *)
	(*Fonction : coller_texte :'a list->'a list list->'a list list = <fun>*)
	(**********************************************************************)
	(*#coller_texte ['t';'t';'x';'e';'e'] [];;                            *)
	(*- : char list list = [['t'];['t'];['x'];['e'];['e']]                *)
	(*#coller_texte['t';'t';'x';'e';'e'] [['e'];['e'];['t'];['t'];['x']];;*)
	(*- : char list list = [['t'; 'e']; ['t'; 'e']; ['x'; 't'];           *)
	(*  ['e'; 't']; ['e'; 'x']]                                           *)
	(*#coller_texte [] [['t'];['t'];['x'];['e'];['e']]                    *)
	(*- : char list list = []                                             *)
	(**********************************************************************)
	let rec coller_texte listeBW liste =
		match listeBW,liste with
			|[],_ -> []
		(*on ne peut avoir qu une liste et une chaine de meme longueur*)
		(*ou une liste vide et une chaine a transformer en liste      *)
			|listeBW,[] -> List.map (fun a -> [a]) listeBW 
			|tBW::qBW,t::q -> (tBW::t)::(coller_texte qBW q);;

	(**********************************************************************)
	(*Cette fonction renvoie le n ieme mot d'une liste de mots            *)
	(*                                                                    *)
	(*Parametres : liste : liste de mots                                  *)
	(*             n : indice du mot que l'on veut                        *)
	(*                                                                    *)
	(*Retour : liste : n ieme mot                                         *)
	(*                                                                    *)
	(*Precondition : n<=(List.length liste) et n>0                        *)
	(*                                                                    *)
	(*Fonction : n_ieme_mot : 'a list list -> int -> 'a list = <fun>      *)
	(**********************************************************************)
	(*#n_ieme_mot [['t'];['t'];['x'];['e'];['e']] 3;;                     *)
	(*- : char list = ['x']                                               *)
	(*#n_ieme_mot [] 1;;                                                  *)
	(*- : 'a list = []                                                    *)
	(**********************************************************************)
	let rec n_ieme_mot liste indice =
		if liste = [] then []
			 else if indice=1 then (List.hd liste)
			            else n_ieme_mot (List.tl liste) (indice-1);;

	(**********************************************************************)
	(*Cette fonction recree la liste des permutations triee               *)
	(*                                                                    *)
	(*Parametres : listeBW : liste obtenue a l encodage                   *)
	(*             n : indice de parcours : List.length listeBW a 0       *)
	(*             liste_perm : la liste des permutations (vide au depart)*)
	(*                                                                    *)
	(*Retour : liste des permutations                                     *)
	(*                                                                    *)
	(*Postcondition : la liste est triee par ordre alphabetique           *)
	(*                                                                    *)
	(*Fonction:                                                           *)
	(*recreer_permutations_triees:                                        *)
	(*	'a list->int->'a list list->'a list list=<fun>                *)
	(**********************************************************************)
	(*tests :                                                             *)
	(*#recreer_permutations_triees ['t';'t';'x';'e';'e'] 5 [];;           *)
	(*- : char list list = [['e'; 't'; 'e'; 'x'; 't'];                    *)
	(*  ['e'; 'x'; 't'; 'e'; 't']; ['t'; 'e'; 't'; 'e'; 'x'];             *)
	(*  ['t'; 'e'; 'x'; 't'; 'e']; ['x'; 't'; 'e'; 't'; 'e']]             *)
	(*#recreer_permutations_triees [] 0 [];;                              *)
	(*- : 'a list list = []                                               *)
	(**********************************************************************)
	let rec recreer_permutations_triees listeBW n liste_perm =
		match n with
			|0 -> liste_perm
			|n -> recreer_permutations_triees listeBW (n-1) (triFusion (coller_texte listeBW liste_perm));;

(******************************************************************************)
(*Decode la sequence a partir de l'indice de position et la sequence des      *)
(*  derniers caracteres                                                       *)
(*                                                                            *)
(*Parametres : (indice,liste):(indice mot code,derniere colonne des permut    *)
(*                                                                            *)
(*Retour : liste : liste decodee                                              *)
(*                                                                            *)
(*Fonction : decode : int * 'a list -> 'a list = <fun>                        *)
(******************************************************************************)
(*tests :                                                                     *)
(*#decode (4, ['t';'t';'x';'e';'e']);;                                        *)
(*- : char list = ['t';'e';'x';'t';'e']                                       *)
(*#decode (1, [' ']);;                                                        *)
(*- : char list = [' ']                                                       *)
(*#decode (2,['s'; 'r'; 'h'; 'e'; 'l'; 'w'; 'e'; 'r'; 'e'; 'r'; 'u'; 'w'; 'b';*)
(*   ' '; 'o'])                                                               *)
(*- : char list =['b';'u';'r';'r';'o';'w';'s';' ';'w';'h';'e';'e';'l';'e';'r']*)
(******************************************************************************)
let decode (indice,liste) =
	n_ieme_mot (recreer_permutations_triees liste (List.length liste) []) indice;;
