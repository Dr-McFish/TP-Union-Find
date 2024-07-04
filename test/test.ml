open TP

(* test find *)
let () =
  let classes = Array.copy UnionFind.classes_refferance in
  let res = UnionFind.find 7 classes in
  (* Printf.printf "%d" res; *)
  assert(1 = res);
  assert ([|-1; -3; 1; 4; -2; 1; 1; 1|] = classes);
;;

(* test meme_classes *)
let () =
let classes = Array.copy UnionFind.classes_refferance in
  
  assert(UnionFind.meme_classe classes 1 2 );
  assert(UnionFind.meme_classe classes 1 5 );
  assert(UnionFind.meme_classe classes 2 6 );
  assert(UnionFind.meme_classe classes 7 6 );

  assert(UnionFind.meme_classe classes 4 1 |> not);

;;

let () = (*test union*)
  let classes = Array.copy UnionFind.classes_refferance in
  assert(UnionFind.meme_classe classes 4 1 |> not);
  
  UnionFind.union classes 4 1;

  assert(UnionFind.meme_classe classes 4 1 )
;;

let () = (*test nesw_uf*)
  ()
;;


(* ************************************************************************************** *)

let () = (* Labyrinth.voisines_carres *)
  let n = 3 in
  let p = 4 in

  assert(4 = List.length (Labyrinth.voisines_carres n p 5));
  assert(4 = List.length (Labyrinth.voisines_carres n p 6));

  assert(3 = List.length (Labyrinth.voisines_carres n p 4));
  assert(3 = List.length (Labyrinth.voisines_carres n p 7));
  assert(3 = List.length (Labyrinth.voisines_carres n p 9));
  assert(3 = List.length (Labyrinth.voisines_carres n p 10));
  assert(3 = List.length (Labyrinth.voisines_carres n p 1));
  assert(3 = List.length (Labyrinth.voisines_carres n p 2));

  assert(2 = List.length (Labyrinth.voisines_carres n p 0));
  assert(2 = List.length (Labyrinth.voisines_carres n p 3));
  assert(2 = List.length (Labyrinth.voisines_carres n p 8));
  assert(2 = List.length (Labyrinth.voisines_carres n p 11));
  ()
;;

let () = (* Labyrinth.murs *)
  let murs = Labyrinth.murs 3 4 in
  for casse = 0 to 3*4 -1 do
    List.iter (fun voisin -> assert(
      List.mem (voisin, casse) murs
      || List.mem (casse, voisin) murs)) 
    (Labyrinth.voisines_carres 3 4 casse)
  done
;;

let () = (* me_sort *)
  [[93;3;89;79;32;96;97;97;54;69;47;56;53;60;67;10;77;74;27;6;16;57;10;33;38;76;96;64;15;11];
  [74;96;97;35;57;68;66;76;49;2;49;32;38;46;65;33;86;73;72;49;18;17;11;30;36;97;46;47;30;6];
  [88;43;44;60;80;43;33;52;13;72;96;61;47;83;75;35;12;90;85;81;62;69;69;83;29;57;9;71;53;49];
  [88;43;44;60;80;43;33;52;13;72;96;61;47;83;75;35;12;90;85;81;62;69;69;83;29;57;9;71;53;49];
  [55;77;85;39;93;21;57;84;99;97;59;36;71;31;66;27;62;100;16;83;40;58;44;99;11;83;93;49;22;92]]
  |> List.iter (fun l -> assert( (List.sort compare l) = (Labyrinth.me_sort l)))
;;

let () = (* Labyrinth.melage_murs *)
  let murs = Labyrinth.murs 3 4 in
  for _ = 0 to 10 do
    let melange = Labyrinth.melange_murs murs in
    List.iter (fun mur ->
      assert(List.mem mur melange)) murs;
    List.iter (fun mur ->
      assert(List.mem mur murs)) melange;
  done
;;

let() = (* Labyrinth.generer_labyrinthe (verification d'arbre)*)
  let (n, p)  = (2,14) in
  let laby = Labyrinth.generer_labyrinthe_positive n p in
  let visite = Array.make (n*p) false in
  (*nb_casses visites, nb_arettes*)
  let rec dfs debut : (int * int) = 
    if visite.(debut) then 
      (0, 0)
    else begin
      visite.(debut) <- true;

      let voisins = Labyrinth.voisines_carres n p debut 
      |> List.filter (fun x  -> List.mem (x, debut) laby || List.mem (debut, x) laby) in
      let result = List.map (dfs) voisins in
      let (sum_v, sum_e) = List.fold_left (fun (a, x) (b,y) -> (a+b, x+y)) (0,0) result in
      let uaccounted_vertecies = 
        result
        |> List.filter (fun x -> x <> (0, 0))
        |> List.length
      in
      (1 + sum_v, uaccounted_vertecies + sum_e)
    end
  in
  let (v, e) = dfs 0 in
  Printf.printf "|E| = %d, |V| = %d\n" e v ;
  assert(v = n*p); (* Connexity *)
  assert(e = v-1) (* tree caracterisation *)
;;

Printf.printf "Tests Finished Sucsessfully!"