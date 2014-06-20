(**
 * Utility functions manipulating abstract syntax tree.
 * @author Copyright 1992 by AT&T Bell Laboratories 
 * @author YAMATODANI Kiyoshi
 * @version $Id: AstUtil.sml,v 1.2 2004/10/20 03:18:39 kiyoshiy Exp $
 *)
structure AstUtil =
struct

  (***************************************************************************)

local 

open Ast Type

in

structure I = Identifier

val unitPat = RecordPat{def=nil,flexibility=false}
val unitExp = RecordExp nil
val trueDcon = [I.trueCon]
val falseDcon = [I.falseCon]
val quoteDcon = ["SMLofNJ", "QUOTE"]
val antiquoteDcon = ["SMLofNJ", "ANTIQUOTE"]
val arrowTycon = I.arrowTycon
val exnID = I.exnTycon
val bogusID = "BOGUS"
val symArg = "<Parameter>"
val itsym = ["it"]

(* layered patterns *)

fun lay3 ((x as VarPat _), y) = LayeredPat{varPat=x,expPat=y}
  | lay3 (ConstraintPat{pattern,constraint}, y) =
         (case lay3 (pattern,y)
           of LayeredPat{varPat,expPat} =>
             LayeredPat{varPat=varPat,
                        expPat=ConstraintPat{pattern=expPat,
                                             constraint=constraint}}
            | pat => pat)
  | lay3 (FlatAppPat[x],y) = y
  | lay3 (x,y) = y
 
fun lay2 (ConstraintPat{pattern,constraint}, y) =
         (case lay2 (pattern,y)
           of LayeredPat{varPat,expPat} =>
             LayeredPat{varPat=varPat,
                        expPat=ConstraintPat{pattern=expPat,
                                             constraint=constraint}}
            | pat => pat)
  | lay2 (FlatAppPat[item],y) = lay3(item,y)
  | lay2 p = lay3 p
 
fun lay (ConstraintPat{pattern,constraint}, y) =
         (case lay2 (pattern,y)
           of LayeredPat{varPat,expPat} =>
             LayeredPat{varPat=varPat,
                        expPat=ConstraintPat{pattern=expPat,
                                             constraint=constraint}}
            | pat => pat)
  | lay p = lay2 p

val layered = lay

(* types *)

fun conty (NONE, dtid, tyvarList) =
      ConTy (dtid, map VarTy tyvarList)
  | conty (SOME ty, dtid, tyvarList) =
      ConTy ([arrowTycon], [ty, conty (NONE, dtid, tyvarList)])

fun subst tyvarList tyList ty = 
    let val env = 
            foldl (fn ((tyvar,ty),env) => SEnv.insert (env, tyvar, ty))
               SEnv.empty (ListPair.zip (tyvarList,tyList))
    in  subst_ env ty
    end

and subst_ env (VarTy tyvar) =
       (case SEnv.find (env, tyvar) of
         SOME ty => ty
        | NONE => (VarTy tyvar))
  | subst_ env (ConTy (path,tyList)) =
       ConTy (path, map (subst_ env) tyList)
  | subst_ env (RecordTy idTyList) = 
       RecordTy (map (fn (id,ty) => (id, subst_ env ty)) idTyList)
  | subst_ env (TupleTy tyList) = 
       TupleTy (map (fn ty => subst_ env ty) tyList)
  | subst_ env (ForallTy (tyvarList,ty)) =
       ForallTy (tyvarList, 
          subst_
          (foldl (fn (tyvar,env) => #1 (SEnv.remove (env,tyvar))) env tyvarList) ty)


exception ErrUnfoldSyn of string

fun unfoldSynWith f tyenv (VarTy tyvar) = VarTy tyvar

  | unfoldSynWith f tyenv (ConTy ([id],tyList)) = 

       (* function type *)
       if id = I.arrowTycon andalso length tyList=2
       then
          ConTy ([id], map (unfoldSynWith f tyenv) tyList)

       (* constructor type *)
       else
          (case SEnv.find (tyenv, id) of
             SOME x =>
      
             (case f x of
                (Env.DtInfo _) => ConTy ([id],tyList)    (* Check the existence of a DtInfo entry *)
              | (Env.SynInfo (tyvarList,ty)) => 
                   if length tyvarList=length tyList
                   then unfoldSynWith f tyenv (subst tyvarList tyList ty)
                   else raise (ErrUnfoldSyn 
                               ("Type constructor " ^ id ^
                                "given " ^ 
                                Int.toString (length tyList) ^ 
                                " arguments, wants " ^
                                Int.toString (length tyvarList) )))

           | NONE => (print ("Unbound type constructor: " ^ id); 
                      raise (ErrUnfoldSyn ("Unbound type constructor: " ^ id))))

  | unfoldSynWith f tyenv (RecordTy idTyList) = 
       RecordTy (map (fn (id,ty) => (id, unfoldSynWith f tyenv ty)) idTyList)

  | unfoldSynWith f tyenv (TupleTy tyList) = TupleTy (map (unfoldSynWith f tyenv) tyList)

  | unfoldSynWith f tyenv (ForallTy (tyvarList,ty)) = ForallTy (tyvarList, unfoldSynWith f tyenv ty)

  | unfoldSynWith f tyenv _ = raise (ErrUnfoldSyn "Not implemented at elabDeclList")

fun unfoldSyn tyenv ty = unfoldSynWith (fn x => x) tyenv ty

  (***************************************************************************)

end

end (* structure *)


