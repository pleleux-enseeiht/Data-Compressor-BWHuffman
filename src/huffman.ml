                            (*****************************)
                            (**       Projet CAML       **)
                            (**         Huffman         **)
                            (*****************************)



(*****************************)
(**  Type arbre de huffman  **)
(*****************************)

(******************************************************************************)
(*Type de l'arbre binaire utilise pour coder le document                      *) 
(******************************************************************************)

type 'a huffmantree = 
	|Feuille of 'a * int 
	|Noeud of 'a huffmantree * int * 'a huffmantree;;



(********************************)
(** Codage Fonction Build_tree **)
(********************************)


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
	(*Fonction : occurence : 'a -> 'a list -> bool = <fun>                *)
	(**********************************************************************)
	(*tests :                                                             *)
	(*# occurrence 1 [1;2;3];;                                            *)
	(*- : bool = true                                                     *)
	(*# occurrence 't' [];;                                               *)
	(*- : bool = false                                                    *)
	(*# occurrence [1] [[2;3];[56]];                                      *)
	(*- : bool = false                                                    *)
	(**********************************************************************) 
	let rec occurrence element liste=
		match liste with
			|[] -> false
			|t::q when t=element -> true
			|t::q -> occurrence element q;;


	(**********************************************************************)
	(*Cette fonction cree un ensemble a partir d'une liste                *)
	(*                                                                    *)
	(*Parametres : liste : liste a simplifier                             *)
	(*                                                                    *)
	(*retour : liste (ensemble)                                           *)
	(*                                                                    *)
	(*Fonction : set_of_list : 'a list -> 'a list = <fun>                 *)
	(**********************************************************************)
	(*tests :                                                             *)
	(*# set_of_list [1;2;3];;                                             *)
	(*- : int list = [1;2;3]                                              *)
	(*# occurrence 't' [];;                                               *)
	(*- : char list = []                                                  *)
	(*# set_of_list [[56];[2;3];[56]];                                    *)
	(*- : int list = [[2;3];[56]]                                         *)
	(**********************************************************************)
	let rec set_of_list liste=
		match liste with
			|[] -> []
			|t::q -> let res_q=(set_of_list q)
				 in if (occurrence t res_q) then res_q
						       else t::res_q;;

	(**********************************************************************)
	(*Cette fonction compte le nb d'occurence d'un element dans la liste  *)
	(*                                                                    *)
	(*Parametres : element : element a chercher dans la liste             *)
	(*             liste : liste dans laquelle on cherche                 *)
	(*                                                                    *)
	(*retour : entier : nombre d'occurence de l'element                   *)
	(*                                                                    *)
	(*Fonction : compter_occurence : 'a -> 'a list = <fun>                *)
	(**********************************************************************)
	(*tests :                                                             *)
	(*#compter_occurence 'a' ['h';'a';'h';'a'];;                          *)
	(*- : int = 2                                                         *)
	(*#compter_occurence 'r' ['e'];;                                      *)
	(*- : int = 0                                                         *)
	(*#compter_occurence 'e' [];;                                         *)
	(*- : int = 0                                                         *)
	(**********************************************************************)
	let rec compter_occurence element liste =
		match liste with
		      |[] -> 0
		      |t::q -> if t=element then 1+(compter_occurence element q)
			                    else (compter_occurence element q);;

	(**********************************************************************)
	(*Cette fonction cree la liste de couple (element, poids)             *)
	(*                                                                    *)
	(*Parametres : ensemble : ensemble cree a partir de la liste          *)
	(*             liste : liste de depart                                *)
	(*                                                                    *)
	(*retour : liste de couple                                            *)
	(*                                                                    *)
	(*Fonction : compter : 'a list -> 'a list -> ('a * int) list = <fun>  *)
	(**********************************************************************)
	(*tests :                                                             *)                                                          
	(*#compter [5;5;4;5;6;6;6;3;4];;                                      *)         
	(*- : (int * int) list = [(5, 3); (6, 3); (3, 1); (4, 2)]             *)
	(*#compter [];;                                                       *)
	(*- : ('a * int) list = []                                            *)
	(**********************************************************************)
	let compter liste =
		let rec aux ensemble liste =			
			match ensemble with
				|[] -> []
				|t::q -> (t,(compter_occurence t liste))::(aux q liste)
		in aux (set_of_list liste) liste;;

	(**********************************************************************)
	(*Cree une liste d'arbre a partir d'une liste de couples              *)
	(*  (element, poids)                                                  *)
	(*                                                                    *)
	(*Parametres : liste : liste a transformer                            *)
	(*                                                                    *)
	(*retour : liste d'arbres de huffman                                  *)
	(*                                                                    *)
	(*Fonction : listree_of_list : 'a list -> 'a huffmantree list = <fun> *)
	(**********************************************************************)
	(*tests :                                                             *)
	(*#listree_of_list [(5, 3); (6, 3); (3, 1); (4, 2)];;                 *)
	(*- : int huffmantree list = [Feuille (5, 3); Feuille (6, 3);         *) 
	(* Feuille (3, 1); Feuille (4, 2)]                                    *)
	(*#listree_of_list [];;                                               *)
	(*- : 'a huffmantree list = []                                        *)
	(**********************************************************************)
	let rec listree_of_list liste =
		match liste with
		   |[] -> []
		   |t::q -> let (n,m)=t in (Feuille(n,m))::(listree_of_list q);;
		

	(**********************************************************************)
	(*Cette fonction cree la relation d'ordre entre deux arbres :         *)
	(*  renvoie vrai lorsque l'arbre 1 a un poids inferieur ou egal       *)  
        (*                                                                    *)
	(*Parametres : arbre1 et arbre2 : les arbres a comparer               *)
	(*                                                                    *)
	(*retour : booleen                                                    *)
	(*                                                                    *)
	(*Fonction : ordre_arbre:'a huffmantree->'a huffmantree-> bool = <fun>*)
	(**********************************************************************)
	(*tests :                                                             *)
	(*ordre_arbre (Feuille(2,3)) (Feuille(3,4));;                         *)
	(*- : bool = true                                                     *)
	(*ordre_arbre (Feuille(2,3)) (Noeud(Feuille(2,4),11,Feuille(3,7)));;  *)
	(*- : bool = true                                                     *)
	(*ordre_arbre (Noeud(Feuille(2,4),11,Feuille(3,7))) (Feuille(2,3));;  *)
	(*- : bool = false                                                    *)
	(*ordre_arbre (Noeud(Feuille(2,4),5,Feuille(3,7)))                    *)
	(*(Noeud(Feuille(1,2),4,Feuille(3,2)));;                              *)
	(*- : bool = false                                                    *)
	(**********************************************************************)
	let ordre_arbre arbre1 arbre2 =
		match arbre1,arbre2 with
		|Feuille(n1,m1),Feuille(n2,m2) -> m1<=m2
		|Feuille(n1,m1),Noeud(g,m2,d) -> m1<=m2
		|Noeud(g,m1,d),Feuille(n2,m2) -> m1<=m2
		|Noeud(g1,m1,d1),Noeud(g2,m2,d2) -> m1<=m2;;

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
	(*#decomp [4;3;9;7];;                                                 *)
	(*- : int list*int list = [4;3],[9;7]                                 *)
	(*#decomp [9.;8.;4.;5.];;                                             *)
	(*- : float list*float list = [9.;8.],[4.;5.]                         *)
	(*#decomp ['a';'b';'c'];;                                             *)
	(*- : char list*char list ['a';'b'],['c']                             *)
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
	(*#recomp [Feuille(2,3)] [];;                                         *)
	(*- : int huffmantree list = [Feuille(2,3)]                           *)
	(*#recomp [] [Feuille(2,3)];;                                         *)
	(*- : int huffmantree list = [Feuille(2,3)]                           *)
	(*#recomp [Feuille(2,3)] [Noeud(Feuille(1,2),8,Feuille(3,6))];;       *)
	(*- : int huffmantree list = [Feuille (2, 3);Noeud (Feuille (1, 2), 8,*) 
	(*  Feuille (3, 6))]                                                  *)
	(**********************************************************************)
	let rec recomp liste1 liste2 =
		match liste1,liste2 with
			|[],_ -> liste2
			|_,[] -> liste1
			|t1::q1,t2::q2 -> if (ordre_arbre t1 t2) 
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
	(*tests :                                                             *)
	(*#triFusion [Feuille(2,3);Noeud(Feuille(1,2),8,Feuille(3,6));        *)
	(*  Feuille(1,2)];;                                                   *)
	(*- : int huffmantree list = [Feuille (1, 2); Feuille (2, 3);         *)
	(*Noeud (Feuille (1, 2),8, Feuille (3, 6))]                           *)                    
        (*#triFusion [];;                                                     *)
	(*- : 'a huffmantree list = []                                        *)
	(**********************************************************************)
	let rec triFusion liste =
		match liste with
			|[] -> []
			|[_] -> liste
			|_ -> let(liste1,liste2)=decomp liste
			      in recomp (triFusion liste1) (triFusion liste2);;


(******************************************************************************)
(*Prend une liste d'element et construit l'arbre de codage                    *)
(*                                                                            *)
(*Parametres : liste : liste a encoder                                        *)
(*                                                                            *)
(*Retour : arbre de codage de huffman                                         *)
(*                                                                            *)
(*Fonction : build_tree : 'a list -> 'a huffmantree = <fun>                   *)
(******************************************************************************)
(*tests :                                                                     *)
(*build_tree [5;5;4;5;6;6;6;3;4];;                                            *)
(*- : int huffmantree = Noeud (Noeud (Feuille (3, 1), 3, Feuille (4, 2)), 9,  *)
(*  Noeud (Feuille (5, 3), 6, Feuille (6, 3)))                                *)
(*build_tree [];;                                                             *)
(*Exception: Failure "build_tree : liste vide".                               *)
(******************************************************************************)

let build_tree liste =
	let listree = listree_of_list (compter liste) in
	let rec aux listree =
		match listree with
			|[] -> failwith("build_tree : liste vide")
			|[Feuille(n,m)] -> [Feuille(n,m)]
			|[Noeud(g,n,d)] -> [Noeud(g,n,d)]
			|Feuille(n1,m1)::Feuille(n2,m2)::q -> (aux (triFusion (Noeud(Feuille(n1,m1),m1+m2,Feuille(n2,m2))::q)))
			|Feuille(n1,m1)::Noeud(g,m2,d)::q -> (aux (triFusion (Noeud(Feuille(n1,m1),m1+m2,Noeud(g,m2,d))::q)))
			|Noeud(g,m1,d)::Feuille(n2,m2)::q -> (aux (triFusion (Noeud(Noeud(g,m1,d),m1+m2,Feuille(n2,m2))::q)))
			|Noeud(g1,m1,d1)::Noeud(g2,m2,d2)::q -> (aux (triFusion (Noeud(Noeud(g1,m1,d1),m1+m2,Noeud(g2,m2,d2))::q)))
	in let tree = List.hd (aux (triFusion listree))
		in tree;;


(****************************)
(** Codage Fonction Encode **)
(****************************)


	(*************************)
	(**Fonctions auxiliaires**)
	(*************************)

	(**********************************************************************)
	(*Cette fonction cree la table de transcription element->code binaire *)
	(*                                                                    *)
	(*Parametres : tree : arbre a partir duquel on construit la table     *)
	(*                                                                    *)
	(*Retour : liste de couples (element, code binaire)                   *)
	(*                                                                    *)
	(*Fonction : liste_encode :'a huffmantree->('a*bool list) list = <fun>*)
	(**********************************************************************)
	(*liste_encode (build_tree [5;5;4;5;6;6;6;3;4]);;                     *)
	(*- : (int * bool list) list = [(3, [false; false]);(4,[true; false]);*) 
	(*  (5, [false; true]); (6, [true; true])]                            *)
	(*liste_encode (Feuille(2,3));;                                       *)
	(*- : (int * bool list) list = [(2, [])]                              *)
	(**********************************************************************)
	let liste_encode tree =	
		match tree with
			|Feuille(n,m) -> [(n,[false])]
			|Noeud(g,n,d) -> let rec aux tree =
					 match tree with
					     |Feuille(n,m) -> [(n,[])]
					     |Noeud(g,n,d) -> (List.map (fun (n,l) -> (n,false::l) ) (aux g))@(List.map (fun (n,l) -> (n,true::l)) (aux d))
					in aux tree;;

	(**********************************************************************)
	(*Cette fonction transcris la liste de depart en message binaire      *)
	(*                                                                    *)
	(*Parametres : liste : la liste de depart                             *)
	(*             tree : l'arbre de huffman                              *)	
	(*                                                                    *)
	(*Retour : liste de boolean : le code binaire de la liste             *)
	(*                                                                    *)
	(*Fonction :                                                          *)
	(*code_binaire : 'a liste -> 'a huffmantree -> bool list = <fun>      *)
	(**********************************************************************)
	(*tests :                                                             *)
	(*#code_binaire [5;5;4;5;6;6;6;3;4];;                                 *)
	(*- : bool list = [false; true; false; true; true; false; false; true;*) 
	(*  true; true; true; true; true; true; false; false; true; false]    *)
	(*#code_binaire [];;                                                  *)
	(*- : 'a list = []                                                    *)
	(*#code_binaire [4;4];;                                               *)
	(*- : bool list = [false]                                             *)
	(**********************************************************************)
	let code_binaire liste =
		let table = liste_encode (build_tree liste) in
			let rec aux liste =		
				match liste with
					|[] -> []
					|t::q -> (List.assoc t table)@(aux q)
			in aux liste;;


(******************************************************************************)
(*Prend une liste d'elements et renvoie le couple                             *) 
(*    (arbre de codage de huffman, codage binaire de la liste)                *)
(*                                                                            *)
(*Parametres : liste : liste a encoder                                        *)
(*                                                                            *)
(*Retour : arbre de codage de huffman, code binaire de la liste               *)
(*                                                                            *)
(*Fonction : encode : 'a list -> 'a huffmantree * bool list = <fun>       *)
(******************************************************************************)
(*tests :                                                                     *)
(*#encode [5;5;4;5;6;6;6;3;4];;                                               *)
(*- : int huffmantree * bool list = (Noeud (Noeud (Feuille (3, 1), 3,         *)
(*Feuille (4, 2)), 9,Noeud (Feuille (5, 3), 6, Feuille (6, 3))), [false; true;*)
(*  false; true; true; false; false;true;true;true;true;true;true;true;false; *)
(*  false; true; false])                                                      *)
(*#encode [];;                                                                *)
(*Exception: Failure "build_tree : liste vide".                               *)
(*#encode [4;4];;                                                             *)
(*- : int huffmantree * bool list = (Feuille (4, 2), [false])                 *)
(******************************************************************************)

let encode liste =
	let tree = build_tree liste
	in tree,(code_binaire liste);;


(****************************)
(** Codage Fonction Decode **)
(****************************)


	(*************************)
	(**Fonctions auxiliaires**)
	(*************************)

	(**********************************************************************)
	(*Cette fonction cree la table de transcription code binaire->element *)
	(*                                                                    *)
	(*Parametres : tree : arbre a partir duquel on construit la table     *)
	(*                                                                    *)
	(*Retour : liste de couples (code binaire, element)                   *)
	(*                                                                    *)
	(*Fonction : liste_decode:'a huffmantree->(bool list*'a) list = <fun> *)
	(**********************************************************************)
	(*tests :                                                             *)
	(*#liste_decode (build_tree [5;5;4;5;6;6;6;3;4]);;                    *)
	(*- : (bool list * int) list = [([false; false], 3); ([false;         *)
	(*  true], 4);([true; false], 5); ([true; true], 6)]                  *)
	(*#liste_decode (Feuille(2,3));;                                      *)
	(*- : (bool list * int) list = [([false], 2)]                         *)
	(**********************************************************************)
	let liste_decode tree =	
		match tree with
			|Feuille(n,m) -> [([false],n)]
			|Noeud(g,n,d) -> let rec aux tree = 
					    match tree with
					        |Feuille(n,m) -> [([],n)]
			                        |Noeud(g,n,d) -> (List.map (fun (l,n) -> (false::l,n) ) (aux g))@(List.map (fun (l,n) -> (true::l,n) ) (aux d))
					in aux tree;;

	(**********************************************************************)
	(*Cette fonction renvoie (indice, vrai) si la liste est trouvee       *)
	(* dans la table de decodage et 0,false sinon                         *)
	(*                                                                    *)
	(*Parametres : liste : code binaire a decoder                         *)
	(*             table : table de decodage                              *)
	(*                                                                    *)
	(*Retour : couple (element, booleen)                                  *)
	(*                                                                    *)
	(*Fonction :                                                          *)
	(*  occurence_liste:bool list->(bool list*'a) list->int * bool = <fun>*)
	(**********************************************************************)
	(*tests :                                                             *)
	(*#occurence_liste [false; false] (liste_decode (build_tree           *)
	(*  [5;5;4;5;6;6;6;3;4]));;                                           *)
	(*- : int * bool = (3, true)                                          *)
	(*#occurence_liste [false] (liste_decode (build_tree                  *)
	(* [5;5;4;5;6;6;6;3;4]));;                                            *)
	(*- : int * bool = (0, false)                                         *)
	(**********************************************************************)
	let rec occurence_liste liste table =	
		match table with
			|[] -> false
			|(l,n)::q -> if l=liste then true
						else (occurence_liste liste q);;

	(**********************************************************************)
	(*Cette fonction decode la liste de booleen avec la table de decodage *)
	(*                                                                    *)
	(*Parametres : liste : code binaire a decoder                         *)
	(*             table : table de decodage                              *)
	(*                                                                    *)
	(*Retour : liste decodee                                              *)
	(*                                                                    *)
	(*Fonction : find : bool list ->(bool list * 'a) list ->'a list= <fun>*)
	(**********************************************************************)
	(*tests :                                                             *)
	(*#find [false;true;false;true;true;false;false;true;true;true;true;  *)
	(* true;true;true;false;false;true;false] (liste_decode(build_tree    *) 
	(*[5;5;4;5;6;6;6;3;4]));;                                             *)
	(*- : int list = [5; 5; 4; 5; 6; 6; 6; 3; 4]                          *)
	(*#find [false] (liste_decode(Feuille(2,3)));;                        *)
	(*- : int list = [2]                                                  *)
	(**********************************************************************)
	let rec find liste table =
		if liste=[] then failwith("find : liste vide")
			    else let rec aux liste reste_liste =
					match liste,(occurence_liste liste table),reste_liste with
						|_,true,[] -> [(List.assoc liste table)]
						|[],_,_ -> []	
						|_,true,reste_liste -> (List.assoc liste table)::(find reste_liste table)
						|_,false,_ -> aux (liste@[List.hd reste_liste]) (List.tl reste_liste)
				 in aux [List.hd liste] (List.tl liste);;


(******************************************************************************)
(*Prend un arbre de codage de huffman et une liste de booleens et reconstruit *)
(*          la liste d'elements decodee                                       *)
(*                                                                            *)
(*Parametres : (tree,liste) : (arbre de huffman, liste de booleens)           *)
(*                                                                            *)
(*Retour : liste : liste decodee                                              *)
(*                                                                            *)
(*Fonction : decode : 'a huffmantree * bool list -> 'a list = <fun>           *)
(******************************************************************************)
(*tests :                                                                     *)
(*#decode (Noeud (Noeud (Feuille (3, 1), 3, Feuille (4, 2)), 9, Noeud (       *)
(*Feuille (5, 3), 6,Feuille (6, 3))),[false; true; false; true; true; false;  *)
(*false; true; true; true; true;true; true; true; false;false;true; false]);; *)
(*- : in list = [5; 5; 4; 5; 6; 6; 6; 3; 4]                                   *)
(*#decode (Feuille(2,3),[false;false;false]);;                                *)
(*  - : int list = [2; 2; 2]                                                  *)
(******************************************************************************)
let decode (tree,liste) =
	let table = liste_decode tree
	in find liste table;;	





