
type classes = int array
type member = int

val classes_refferance : int array

val find : member -> classes ->  member
val meme_classe : classes -> member -> member -> bool
val union: classes -> member -> member -> unit
val new_uf: int -> classes