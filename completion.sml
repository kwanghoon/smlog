
structure Completion =
struct

local

open TypedAst Env 

in

structure I = Identifier
structure T = Type

exception ErrCompletion of string

(* Completion transformation *)

fun completion declList = 
    let val env = {idenv=SEnv.empty, tyenv=SEnv.empty}
        val (_,declList) = compDeclList env declList
    in
        declList
    end

and compDeclList env [] = (env, [])

  | compDeclList env (RelationSigDec (id,tyInoutList) :: declList) =
       let val {idenv=idenv,tyenv=tyenv} = env
           val idenv = SEnv.insert (idenv, id, RelId tyInoutList)

           val env = {idenv=idenv,tyenv=tyenv}
           val decl = RelationSigDec (id,tyInoutList)
           val (env,declList) = compDeclList env declList
       in
           (env, decl :: declList)
       end

  | compDeclList env (DatatypeDec (dbOriginList,NONE) :: declList) =
       let 
           val env =
               foldl 
                  (fn ((Db {tyc=id,tyvars=tyvarList,rhs=Constrs idTyOptionList}, _),
                          {idenv=idenv,tyenv=tyenv}) =>

                       let val ty = T.ConTy ([id], map T.VarTy tyvarList)
                           val idTyList = map (fn (cid,tyOption) => 
                                                  (cid, T.arrowty (tyOption,ty)))
                                               idTyOptionList
                       in  {idenv=foldl (fn ((cid,ty),idenv) => 
                                         SEnv.insert (idenv, cid, ConId (ty,tyvarList,id)))
                                         idenv
                                         idTyList,

                            tyenv=SEnv.insert (tyenv, id, DtInfo (SOME(tyvarList,idTyList)))}
                       end 

                    | ((Db {tyc=id,tyvars=tyvarList,rhs=Repl [defid]}, _),
                          {idenv=idenv,tyenv=tyenv}) => 
                          (case SEnv.find (tyenv, defid) of
                             SOME (dtinfo as DtInfo _) => 
                               {idenv=idenv,tyenv=SEnv.insert(tyenv,id,dtinfo)}
                           | _ => raise (ErrCompletion ("Undefined type: " ^ defid)))
          
                    | _ => raise (ErrCompletion ("Not implemented at compDeclList")))

                  env dbOriginList

           val decl = DatatypeDec (dbOriginList,NONE)
           val (env,declList) = compDeclList env declList

       in 
           (env, decl :: declList)
       end

  | compDeclList env (TypeDec tbList :: declList) =
      let val {idenv=idenv,tyenv=tyenv} = env
          
          val tyenv =
              foldl (fn ((id,tyinfo),tyenv) => SEnv.insert(tyenv,id,tyinfo))
                 tyenv
                 (map (fn tb as Tb {tyc=id,tyvars=tyvarList,def=ty} =>
                      (id, SynInfo (tyvarList, ty))) tbList)

          val env = {idenv=idenv, tyenv=tyenv}
          val decl = TypeDec tbList
          val (env, declList) = compDeclList env declList
      in
          (env, decl :: declList)
      end

  | compDeclList env (RelationDec (idList,rbListList,tyvarList) :: declList) =
    let 
        val (_,tyvarenv) =
            foldl (fn (tyvar,(n,tyvarenv)) => 
                       (n+1,IEnv.insert (tyvarenv,n,tyvar)))
                      (1,IEnv.empty) tyvarList

        val {idenv=idenv,tyenv=tyenv} = env

        val idRbList = ListPair.zip (idList, rbListList)

        val decl = 
            FolDec
              (map (fn (id,rbList) =>
                    let val fol = compRelationDec env (id,rbList)
                    in  (id,fol)
                    end) idRbList,
               tyvarList)

        val (env,declList) = compDeclList env declList
    in  
        (env, decl :: declList)
    end

  | compDeclList env (decl :: declList) =
        raise (ErrCompletion ("Unexpected declaration"))

and compRelationDec env (relationid,rb) =
    let
        val {idenv=idenv,...} = env

        val tyInoutOptionList = 
            case SEnv.find (idenv,relationid) of
              SOME (RelId tyInoutOptionList) => tyInoutOptionList
            | _ => raise (ErrCompletion ("Relation name not found: " ^ relationid))

        val (tyList, inoutOptionList) = ListPair.unzip tyInoutOptionList

        val n = length tyInoutOptionList

        val idList = List.tabulate (n, fn i => I.arg (Int.toString i))

        val idTyList = ListPair.zip (idList,tyList)

        val fol = compRb env idTyList rb
    in
        Forall (idTyList,fol)
    end

and compRb env idTyList (Rb (relationList,b)) =
    compRelationList env idTyList relationList

and compRelationList env idTyList relationList =
    Disj (map (fn Relation r => compRelation env idTyList r) relationList)

and compRelation env idTyList {args=patList,body=pathExpListBoolList,bvenv=bvenv} =
    let val (expList,folpat) = compPatList env patList
        val folbody = compPathExpLists env pathExpListBoolList

        val idTyListExpList = ListPair.zip (idTyList,expList)

        val folcall = map (fn ((id,ty),exp) => 
                          let val v = VarExp ([id],ty)
                          in  Unify (ty,v,exp)
                          end) idTyListExpList

        val bvTyList = SEnv.listItemsi bvenv

        val fol = Conj (folcall @ [folpat,folbody])
    in  
        Exists (bvTyList, fol)
    end

and compPatList env patList =
    foldl (fn (pat,(expList,folaux)) =>
           let val (exp,fol) = compPat env pat
           in
               (expList @ [exp], Conj [folaux,fol])  
           end)
          ([],folTrue) patList

and compPat env pat = 
    case pat of 
      VarPat (path,ty) => (VarExp (path,ty), folTrue)

    | IntPat i => (IntExp i, folTrue)

    | WordPat i => (WordExp i, folTrue)

    | CharPat s => (CharExp s, folTrue)

    | StringPat s => (StringExp s, folTrue)

    | WildPat (id,ty) => (VarExp ([id],ty), folTrue)

    | RecordPat {def=stringPatList,flexibility=b} =>
      let val stringExpFolList = 
              map (fn (l,pat) => 
                      let val (exp,fol) = compPat env pat
                      in  ((l,exp), fol)
                      end) stringPatList
          val (idExpList,folList) = ListPair.unzip stringExpFolList
          val fol = Conj folList
      in  
          (RecordExp idExpList, fol)
      end

    | TuplePat patList =>
      let val expFolList = map (compPat env) patList
          val (expList,folList) = ListPair.unzip expFolList
          val fol = Conj folList
      in
          (TupleExp expList,fol)
      end 

    | ConPat (path,patTyList) => 
      let val (patList,tyList) = ListPair.unzip patTyList
          val expFolList = map (compPat env) patList
          val (expList,folList) = ListPair.unzip expFolList
          val fol = Conj folList
          val expTyList = ListPair.zip (expList,tyList)
      in
          (ConExp (path,expTyList),fol)
      end 

    | LayeredPat (id,ty,pat) =>
      let val (exp,fol) = compPat env pat
          val v = VarExp ([id],ty)
          val fol = Conj [ fol, Unify (ty,v,exp) ]
      in
          (VarExp ([id],ty), fol)
      end

    | VectorPat patList => 
      let val expFolList = map (compPat env) patList
          val (expList,folList) = ListPair.unzip expFolList
          val fol = Conj folList
      in
          (VectorExp expList,fol)
      end 

    | OrPat (id,ty,patList) =>
      let val expFolList = map (compPat env) patList
          val folList = map (fn (exp,fol) =>
                             let val v = VarExp ([id],ty)
                                 val fol = Conj [fol, Unify (ty,v,exp)]
                             in  fol
                             end) expFolList
          val fol = Disj folList
      in
          (VarExp ([id],ty), fol)
      end 

    | ConstraintPat (pat,ty) => 
      let val (exp,fol) = compPat env pat
      in
          (ConstraintExp (exp,ty), fol)
      end

    | FlatAppPat (id, ty, patTyList as _ :: _ :: _) =>
      let 
          val (patList,tyList) = ListPair.unzip patTyList
          val expFolList = map (compPat env) patList
          val (expList, folList) = ListPair.unzip expFolList
          val fol = Conj folList
          val expTyList = ListPair.zip (expList,tyList)

          val v = VarExp ([id],ty)
          val fol = Conj [fol, Assign (id,ty,FlatAppExp (id,ty,expTyList))]
      in
          (v, fol)
      end 

    | FlatAppPat _ =>
      raise (ErrCompletion ("FlatAppPat with less than two patterns"))

and compPathExpLists env pathExpListBoolList =
    foldr (fn (pathExpListBool,fol2) => 
           let val fol1 = compPathExpList env pathExpListBool
           in  Conj [fol1, fol2]
           end)
           folTrue
           pathExpListBoolList

and compPathExpList env (path,expList, negation) = 
    let val (expList,fol) = compExpList env expList
    in
        Conj [fol, Pred (path, expList, negation)]
    end

and compExp env exp = 
    case exp of 
      VarExp (path,ty) => (exp, folTrue)
    | ConExp (path,expTyList) => 
      let val (expTyList,fol) = compExpTyList env expTyList
      in 
          (ConExp (path,expTyList), fol)
      end
    | IntExp i => (exp, folTrue)
    | WordExp i => (exp, folTrue)
    | RealExp s => (exp, folTrue)
    | StringExp s => (exp, folTrue)
    | CharExp s => (exp, folTrue)
    | RecordExp idExpList =>
      let val (idExpList,fol) = compIdExpList env idExpList
      in
          (RecordExp idExpList,fol)
      end
    | TupleExp expList =>
      let val (expList,fol) = compExpList env expList
      in
          (TupleExp expList, fol)
      end
    | SelectorExp s => (exp, folTrue)
    | ConstraintExp (exp,ty) => 
      let val (exp,fol) = compExp env exp
      in
          (ConstraintExp (exp,ty), fol)
      end
    | VectorExp expList =>
      let val (expList,fol) = compExpList env expList
      in
          (VectorExp expList, fol)
      end
    | FlatAppExp (id,ty,expTyList) =>
      let val (expTyList,fol) = compExpTyList env expTyList
          val fol = Conj [fol, Assign (id,ty,FlatAppExp (id,ty,expTyList))]
      in
          (VarExp ([id],ty), fol)
      end

and compExpList env expList = 
    let val expFolList = map (fn exp => 
                    let val (exp,fol) = compExp env exp
                    in  (exp,fol)
                    end) expList
                  
        val (expList,folList) = ListPair.unzip expFolList
        val fol = Conj folList
    in
        (expList,fol)
    end

and compExpTyList env expTyList = 
    let val expTyFolList = map (fn (exp,ty) => 
                    let val (exp,fol) = compExp env exp
                    in  ((exp,ty),fol)
                    end) expTyList
        val (expTyList,folList) = ListPair.unzip expTyFolList
        val fol = Conj folList
    in
        (expTyList,fol)
    end

and compIdExpList env idExpList = 
    let val idExpFolList = map (fn (id,exp) => 
                    let val (exp,fol) = compExp env exp
                    in  ((id,exp),fol)
                    end) idExpList
        val (idExpList,folList) = ListPair.unzip idExpFolList
        val fol = Conj folList
    in
        (idExpList,fol)
    end

end

end
