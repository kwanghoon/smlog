
signature ordsig = sig
    type ord_key
    val compare : ord_key * ord_key -> order
end

structure Iord:ordsig = struct 
    type ord_key = int
    val compare = Int.compare
end

structure Cord:ordsig = struct 
    type ord_key = char
    val compare = Char.compare
end

structure Sord:ordsig = struct 
    type ord_key = string
    val compare = String.compare
(*     fun compare (x,y) =  *)
(* 	let val (a,b) = (valOf(Int.fromString x),valOf(Int.fromString y)) *)
(* 	in Int.compare (a,b) *)
(* 	end *)
(*         handle Option => String.compare (x,y) *)
end

structure SLord:ordsig = struct 
    type ord_key = string list
    fun compare (x::xs,y::ys) = 
           (case String.compare (x,y) of
              EQUAL => compare (xs,ys)
            | order => order)
      | compare ([], []) = EQUAL
      | compare ([], _) = LESS
      | compare (_,[]) = GREATER
end

structure IEnv = BinaryMapFn(Iord)
structure CEnv = BinaryMapFn(Cord)
structure SEnv = BinaryMapFn(Sord)
structure SLEnv = BinaryMapFn(SLord)

structure ISet = BinarySetFn(Iord)
structure CSet = BinarySetFn(Cord)
structure SSet = BinarySetFn(Sord)
structure SLSet = BinarySetFn(SLord)

(* fun IEnvToISet m = IEnv.foldli (fn (i,_,s) => ISet.add(s,i)) ISet.empty m *)
(* fun SEnvToSSet m = SEnv.foldli (fn (i,_,s) => SSet.add(s,i)) SSet.empty m *)
