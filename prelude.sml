
structure Prelude =
struct

open Ast Identifier Type

(*

datatype unit
datatype bool = true | false
datatype int
datatype word
datatype real
datatype char
datatype string
datatype 'a list = nil | :: of 'a * 'a list

datatype 'a ref = ref of 'a
datatype exn

*)

val consConTy = ForallTy (["'a"],
                        ConTy ([arrowTycon], 
                                  [VarTy "'a", 
                                   ConTy ([listTycon],[VarTy "'a"])]))
val nilConTy = ForallTy (["'a"], ConTy ([listTycon],[VarTy "'a"]))

val refConTy = ForallTy (["'a"],
                        ConTy ([arrowTycon],
                                  [VarTy "'a",
                                   ConTy ([refTycon],[VarTy "'a"])]))

val datatypeDecs = 
    [ 
      (stringTycon, [],     Constrs []),   (* String type should be the first *)
                                           (* because every type uses it.     *)

      (arrowTycon, ["'a", "'b"], Constrs []),

      (unitTycon,   [],     Constrs []),
      (boolTycon,   [],     Constrs [(trueCon,NONE), (falseCon,NONE)]),
      (intTycon,    [],     Constrs []),
      (wordTycon,   [],     Constrs []),

(*       (realTycon,   [],     Constrs []), *)

      (charTycon,   [],     Constrs []),
      (listTycon,   ["'a"],
        Constrs [(nilCon,NONE), 
                 (neutralConsCon,SOME (TupleTy [VarTy "'a", ConTy (["list"], [VarTy "'a"])]))])
    ]

val preludeDeclList =
    foldr (fn ((tyc,tyvars,rhs),declList) =>
                DatatypeDec ([ Db {tyc=tyc,tyvars=tyvars,rhs=rhs} ], NONE) :: declList)
           [] datatypeDecs

end