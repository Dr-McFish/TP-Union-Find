open TP

let sec_to_millisec = 1E3

let benchmark n = 
  let classes = UnionFind.new_uf n in

  for _=1 to n do
    let int1 = Random.int n in
    let int2 = Random.int n in
    UnionFind.union classes int1 int2
  done
;;

let runs = 1

let () =
  (* argumet parsing *)
  if 3  <> Array.length Sys.argv then begin
    Printf.printf "Error: invalid number of argumets.\nUsage: >bechmark n_min n_max" ;
    exit 1
  end;
  let (n_min, n_max) =
    match int_of_string_opt Sys.argv.(1), int_of_string_opt Sys.argv.(2) with
    | Some(n_min), Some(n_max) -> (n_min, n_max)
    | _ -> 
      begin
        Printf.printf "Error: argumets are not a number.\nUsage: >bechmark n_min n_max" ;
        exit 1
      end
  in


  (* Randomness *)
  Random.self_init ();

  (* control test *)
  let z = 100 in
  let zeri_times_s = Array.make z 0. in
  for i = 0 to z-1 do
    let start_time = Sys.time () in

    benchmark 0;

    let end_time = Sys.time () in
    zeri_times_s.(i) <- end_time -. start_time
  done;
  let avrage_zero_s = (1. /. (float_of_int z)) *. Array.fold_left (+.) 0. zeri_times_s in
  
  (* Testing *)
  let execution_times_s = Array.make (n_max - n_min +1) 0. in
  for n = n_min to  n_max do
    
    let execution_times_n_s = Array.make (runs) 0. in
    for r = 0 to runs -1 do
      let start_time = Sys.time () in
      
      benchmark n;
      
      Gc.major ();
      let end_time = Sys.time () in
      execution_times_n_s.(r) <- (end_time -. start_time) -. avrage_zero_s;
    done;
    let i = n - n_min in
    (*avreging*)
    execution_times_s.(i) <- (1./. (float_of_int runs)) *. (Array.fold_left (+.) 0. execution_times_n_s)
  done;
  
  (* Results *)
  Printf.printf "n,millis\n";
  for n = n_min to  n_max do
    let i = n - n_min in
    Printf.printf "%d,%f\n" n ( (execution_times_s.(i) *. sec_to_millisec) ) (*/. (float_of_int n)*)
  done
  (* Printf.printf ",,Avrage zero: %.30fms\n" (avrage_zero_s *. sec_to_millisec); *)
;;