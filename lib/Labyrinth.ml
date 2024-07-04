
let voisines_carres n p casse = 
  let i = casse / p in
  let j = casse mod p in
    (if 0   < j then [casse -1] else [])
  @ (if p-1 > j then [casse +1] else [])
  @ (if 0   < i then [casse -p] else [])
  @ (if n-1 > i then [casse +p] else [])
;;

let voisines_southeast n p casse = 
  let i = casse / p in
  let j = casse mod p in
    (if p-1 > j then [casse +1] else [])
  @ (if n-1 > i then [casse +p] else [])
;;

let murs n p =
  List.init (n*p) (fun case -> 
    voisines_southeast n p case
    |> List.map (fun voisin -> (case, voisin) ))
  |> List.concat
;;

let rec me_sort (list : 'a list) : 'a list =
  let rec merge lista listb =
    match lista, listb with
    | [], l | l, [] -> l
    | head_a :: tail_a , head_b :: _ when head_a < head_b -> head_a :: merge tail_a listb
    | _ :: _ , head_b :: tail_b  ->  head_b ::merge lista tail_b
  in
  let split list =
    let l = List. length list in
    let rec aux_split n list acc =
      match list with
      | _ when n <= 0 -> (List.rev acc, list)
      | [] -> (List.rev acc, list)
      | head :: tail -> aux_split (n-1) tail (head :: acc)
    in
    aux_split (l/2) list []
  in

  match list with
  | [] | [_] -> list
  | _ ->
  begin
    let (right_half, left_half) = split list in
    merge (me_sort right_half) (me_sort left_half)
  end
;;

let melange_murs murs =
  let l = List.length murs in
  Random.self_init ();
  List.map (fun mur -> (Random.int 16*l, mur)) murs
  |> me_sort
  |> List.map snd
;;

let generer_labyrinthe n p = 
  let composantes_connexes = UnionFind.new_uf (n*p) in
  murs n p
  |> melange_murs
  |> List.filter (
    fun (a , b) -> 
      let connectes = UnionFind.meme_classe composantes_connexes a b in
      if not connectes then UnionFind.union composantes_connexes a b;
      connectes
  )
;;

let generer_labyrinthe_positive n p = 
  let composantes_connexes = UnionFind.new_uf (n*p) in
  murs n p
  |> melange_murs
  |> List.filter (
    fun (a , b) -> 
      let connectes = UnionFind.meme_classe composantes_connexes a b in
      if not connectes then UnionFind.union composantes_connexes a b;
      not connectes
  )
;;

let affichier_laby_carre n p laby =
  let representation = Array.make_matrix (2*n+1) (2*p +1) 'X' in 
  print_newline ();
  for i = 0 to n-1 do
    for j = 0 to p-1 do
      representation.(2*i+1).(2*j+1) <- ' '
    done
  done;
  
  List.iter (fun (deb, fin) -> 
    let i_d = (deb / p) in
    let j_d = (deb mod p) in

    let i_f = (fin / p) in
    let j_f = (fin mod p) in
    let d_i = i_f - i_d in
    let d_j = j_f - j_d in

    representation.(2*i_d+1 + d_i).(2*j_d+1 + d_j) <- ' ') 
  laby;
  
  representation
  |> Array.iter (fun row -> 
    Array.iter print_char row; 
    print_newline () )
;;

let laby_to_graphe nb murs = 
  let res = Array.make nb [] in
  List.iter (fun (x, y) ->
    res.(x) <- y :: res.(x);
    res.(y) <- x :: res.(y)
    ) murs;
  res
;;