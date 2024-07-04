open TP

let () =
  let (n, p) = 10, 20 in
  Labyrinth.generer_labyrinthe_positive n p
  |> Labyrinth.affichier_laby_carre n p
;;

