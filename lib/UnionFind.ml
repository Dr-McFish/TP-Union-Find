type classes = int array

type member = int

let classes_refferance = [| -1; -3; 1; 4; -2; 1; 2; 6|]

let rec find u classes =
  if classes.(u) < 0 then begin
    u
  end else begin
    let res = find classes.(u) classes in
    classes.(u) <- res;
    res
  end
;;


(* let find u classes =
  let rec aux_find u classes acc =
    if classes.(u) < 0 then
      begin
        List.iter (fun x -> classes.(x) <- u) acc;
        u
      end
    else
      aux_find classes.(u) classes (u :: acc)
  in
  aux_find u classes []
;; *)

let meme_classe classes u v = 
  find u classes = find v classes
;;

let union classes u v =
  let rep_u = find u classes in
  let rep_v = find v classes in
  let rang_u = -classes.(rep_u) in
  let rang_v = -classes.(rep_v) in
  if (rang_u < rang_v) then
    begin
      classes.(rep_u) <- rep_v
    end
  else if (rang_u > rang_v) then
    begin
      classes.(rep_v) <- rep_u
    end
  else
    begin
      classes.(rep_v) <- rep_u;
      classes.(rep_u) <- -(1 + rang_u)
    end
;;

let new_uf (taille : int) = Array.make (taille + 1) (-1)