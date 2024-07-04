
(** [voisines n p casse] revoie un liste des voisins de [casse] dans un grille de taill [n]x[p] 
	*)
val voisines_carres : int -> int -> int -> int list

(** [murs n p] revoie une liste des murs (couple de cases separes)*)
val murs : int -> int -> (int * int) list
val melange_murs : (int * int) list -> (int * int) list

val generer_labyrinthe : int -> int -> (int * int) list
val generer_labyrinthe_positive : int -> int -> (int * int) list

val affichier_laby_carre: int -> int -> (int * int) list -> unit
val laby_to_graphe : int -> (int * int) list -> int list array

(* for testing *)
val me_sort : 'a list -> 'a list