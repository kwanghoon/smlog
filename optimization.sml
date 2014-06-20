
structure Optimization =

struct

open TypedAst

exception ErrOptimization of string

fun optimization folList = 
    let 
        val env = {idenv=SEnv.empty, tyenv=SEnv.empty}
        val folList = optDeclList env folList
    in  
        folList
    end

and optDeclList env [] = []

  | optDeclList env ((decl as RelationSigDec _) :: declList) = 
    let
        val declList = optDeclList env declList
    in
        (decl :: declList)
    end

  | optDeclList env ((decl as DatatypeDec _) :: declList) = 
    let
        val declList = optDeclList env declList
    in
        (decl :: declList)
    end

  | optDeclList env ((decl as TypeDec _) :: declList) = 
    let
        val declList = optDeclList env declList
    in
        (decl :: declList)
    end

  | optDeclList env (FolDec (idFolList,tyvarList) :: declList) = 
    let val idFolList = 
            map (fn (id,fol) => (id,optFol env fol)) idFolList

        val decl = FolDec (idFolList,tyvarList)
        val declList = optDeclList env declList
    in  
        (decl :: declList)
    end

  | optDeclList env ((decl as _) :: declList) = 
        raise (ErrOptimization "Unexpected declaration")

and optFol env (fol as Assign (id,ty,exp)) = fol
  | optFol env (fol as Unify (ty,exp1,exp2)) = fol

  | optFol env (fol as Pred (path,expList,negation)) = fol

  | optFol env (Conj folList) = 
    let 
        val folList = map (optFol env) folList

        (* Eliminate trivial conjuncts i.e. Conj [] *)

        val folList = 
            List.filter
               (fn fol => fol <> folTrue) folList
               
        (* Flatten conjuncts *)

        val folList = 
            List.concat 
               (map (fn Conj fols => fols
                      | fol       => [fol]) folList)

    in  Conj folList
    end

  | optFol env (Disj folList) = 
    let 
        val folList = map (optFol env) folList

        (* Flatten disjuncts *)

        val folList = 
            List.concat 
               (map (fn Disj fols => fols
                      | fol       => [fol]) folList)

    in  Disj folList
    end

  | optFol env (Exists (idTyList,fol)) = 
    let val fol = optFol env fol
    in  Exists (idTyList, fol)
    end

  | optFol env (Forall (idTyList,fol)) = 
    let val fol = optFol env fol
    in  Forall (idTyList, fol)
    end

end