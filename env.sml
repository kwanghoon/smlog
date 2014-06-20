
structure Env =

struct

local

open Type Identifier

in

datatype idinfo =
    RelId of (ty * inout option) list    (* *)
  | ConId of ty * tyvar list * id        (* \/tyvar list . ty = id *)
  | VarId of id * ty                     (* Useless ?              *)

datatype tyinfo = 
    DtInfo of (tyvar list * (id * ty) list) option
  | SynInfo of tyvar list * ty

type Env = { idenv:idinfo SEnv.map, tyenv:tyinfo SEnv.map}

end

end