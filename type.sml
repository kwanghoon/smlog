
structure Type =

struct

local

open Identifier

in

type tyvar = string

datatype inout = IN | OUT

datatype ty = 
    VarTy    of tyvar
  | ConTy    of path * ty list
  | RecordTy of (id * ty) list
  | TupleTy  of ty list
  | ForallTy of tyvar list * ty

fun pr (VarTy tyvar) = tyvar

  | pr (ConTy (path,[])) = Identifier.pr path 
  | pr (ConTy (path,[ty])) = pr ty ^ " " ^ Identifier.pr path
  | pr (ConTy (path,tyList)) = 
        if length path=1 
        andalso hd path=Identifier.arrowTycon
        andalso length tyList=2
        then "(" ^ pr (List.nth (tyList,0)) ^ " " 
                 ^ Identifier.pr path ^ " " 
                 ^ pr (List.nth (tyList,1)) ^ ")"
        else "(" ^ prs tyList " , " ^ ") " ^ Identifier.pr path

  | pr (RecordTy stringTyList) =
      "{" ^ foldl (fn ((l,ty),"") => l ^ ":" ^ pr ty
                    | ((l,ty),s) => s ^ ", " ^ l ^ ":" ^ pr ty)
                ""
                stringTyList ^ "}"

  | pr (TupleTy tyList) = "(" ^ prs tyList " * " ^ ")"

  | pr (ForallTy (tyvarList,ty)) = 
      "\\/" ^ prs (map VarTy tyvarList) "," ^ "." ^ pr ty

and prs tys del = foldl (fn (ty,"") => pr ty
                          | (ty,s) => s ^ del ^ pr ty) "" tys

(* Primitive types *)

val boolTy = ConTy ([boolTycon], [])
val intTy = ConTy ([intTycon], [])
val wordTy = ConTy ([wordTycon], [])
val realTy = ConTy ([realTycon], [])
val stringTy = ConTy ([stringTycon], [])
val charTy = ConTy ([charTycon], [])
val exnTy = ConTy ([exnTycon], [])

fun isVecTy (ConTy ([con],[ty])) =
       if con=vecTycon 
       then SOME ty else NONE
  | isVecTy (ConTy ([str,con],[ty])) =
       if str=vectorStr andalso con=vecTycon 
       then SOME ty else NONE
  | isVecTy _ = NONE

fun isListTy (ConTy ([con],[ty])) =
       if con=listTycon 
       then SOME ty else NONE
  | isListTy (ConTy ([str,con],[ty])) =
       if str=listStr andalso con=listTycon 
       then SOME ty else NONE
  | isListTy _ = NONE

fun forallty ([],ty) = ty
  | forallty (tyvarList,ty) = ForallTy (tyvarList,ty)

fun arrowty (NONE, ty) = ty
  | arrowty (SOME ty, ty') = ConTy ([arrowTycon], [ty, ty'])

fun tuplety [ty] = ty
  | tuplety tys = TupleTy tys 

end

end

