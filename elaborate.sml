
structure Elaborate =

struct

local

open Ast AstUtil Env Type Identifier

structure I = Identifier
structure T = Type
structure PP = PrettyPrint

in

exception ErrElaboration of string

val counter = ref 1

fun newName () = 
    let val n = !counter
    in  (counter := !counter+1; Int.toString n)
    end

fun mergeBvTyEnv bvTyEnv bvTyEnv' = 
       SEnv.unionWithi
          (fn (id,ty,ty') => 
               if ty=ty' 
               then ty
               else raise (ErrElaboration 
                       ("Type mismatch for pattern variable " ^ id 
                        ^ ": " ^ Type.pr ty ^ ", " ^ Type.pr ty')))
          (bvTyEnv,bvTyEnv')

fun elaboration declList = 
    let 
        val env = {idenv=SEnv.empty,tyenv=SEnv.empty}
        val origin = TypedAst.Prelude
        val (env,typedPreludeDeclList)= elabDeclList origin env Prelude.preludeDeclList
        val origin = TypedAst.User
        val (env,typedDeclList)= elabDeclList origin env declList
    in
        typedPreludeDeclList @ typedDeclList
    end

and elabDeclList origin env [] = (env, [])

  | elabDeclList origin env (RelationSigDec (id,tyInoutList) :: declList) =
       let val {idenv=idenv,tyenv=tyenv} = env
           val idenv = SEnv.insert (idenv, id, RelId tyInoutList)
           val env = {idenv=idenv,tyenv=tyenv}

           val typedDecl = TypedAst.RelationSigDec (id,tyInoutList)
           val (env,typedDeclList) = elabDeclList origin env declList
       in
           (env, typedDecl :: typedDeclList)
       end

  | elabDeclList origin env (DatatypeDec (dbList,NONE) :: declList) =
       let 
           val env =
               foldl 
                  (fn (Db {tyc=id,tyvars=tyvarList,rhs=Constrs idTyOptionList},
                          {idenv=idenv,tyenv=tyenv}) =>

                       let val ty = ConTy ([id], map VarTy tyvarList)
                           val idTyList = map (fn (cid,tyOption) => 
                                                  (cid, arrowty (tyOption,ty)))
                                               idTyOptionList
                       in  {idenv=foldl (fn ((cid,ty),idenv) => 
                                         SEnv.insert (idenv, cid, ConId (ty,tyvarList,id)))
                                         idenv
                                         idTyList,

                            tyenv=SEnv.insert (tyenv,id,DtInfo (SOME (tyvarList,idTyList))) }
                       end 

                    | (Db {tyc=id,tyvars=tyvarList,rhs=Repl [defid]},
                          {idenv=idenv,tyenv=tyenv}) => 
                          (case SEnv.find (tyenv, defid) of
                             SOME (dtinfo as DtInfo _) => 
                               {idenv=idenv,tyenv=SEnv.insert(tyenv,id,dtinfo)}
                           | _ => raise (ErrElaboration ("Undefined type: " ^ defid)))
          
                    | _ => raise (ErrElaboration ("Not implemented at elabDeclList")))

                  env dbList

           val dbOriginList = map (fn db => (db,origin)) dbList
           val typedDecl = TypedAst.DatatypeDec (dbOriginList,NONE)
           val (env,typedDeclList) = elabDeclList origin env declList
       in 
           (env, typedDecl :: typedDeclList)
       end

  | elabDeclList origin env (TypeDec tbList :: declList) =
      let val {idenv=idenv,tyenv=tyenv} = env
          
          val tyenv =
              foldl (fn ((id,tyinfo),tyenv) => SEnv.insert(tyenv,id,tyinfo))
                 tyenv
                 (map (fn tb as Tb {tyc=id,tyvars=tyvarList,def=ty} =>
                      (id, SynInfo (tyvarList, ty))) tbList)

          val env = {idenv=idenv, tyenv=tyenv}
          val typedDecl = TypedAst.TypeDec tbList
          val (env, typedDeclList) = elabDeclList origin env declList
      in
          (env, typedDecl :: typedDeclList)
      end

  | elabDeclList origin env (RelationDec (rbList,tyvarList) :: declList) =
    let 
        val idList = map (fn Rb (id,_,_) => id) rbList
        val idRbList = ListPair.zip (idList,rbList)
        val rbList = map (fn (id,rb) => elabRb env tyvarList id rb) idRbList

        val typedDecl = TypedAst.RelationDec (idList, rbList, tyvarList)
        val (env, declList) = elabDeclList origin env declList

    in  (env, typedDecl :: declList)
    end

  | elabDeclList origin env (_ :: declList) =
       raise (ErrElaboration "Not implemented at elabDeclList")


and elabRb env tyvarList relationid (Rb (id, relations, isrecursive)) =
    let 
        val _ = if relationid=id then ()
                else raise (ErrElaboration ("Name mismatch: " ^ relationid ^ " <> " ^ id ))

        val {idenv=idenv,tyenv=tyenv} = env

        val tyInoutOptionList = 
            case SEnv.find (idenv, id) of
              SOME (RelId tyInoutOptionList) => tyInoutOptionList
            | _ => raise (ErrElaboration (id ^ " not found"))

        val typedrelations = 
            map (elabRelation env tyvarList id tyInoutOptionList) relations
    in
        TypedAst.Rb (typedrelations, isrecursive)
    end

and elabRelation env tyvarList rid tyInoutOptionList 
                     (Relation {name=id,args=patList,body=pathExpListIdOptionList}) =
    let 
        val _ = if rid=id then ()
                else raise (ErrElaboration 
                       ("Clauses don't all have function name: " ^ rid ^ ", " ^ id))

        val (tyList,inoutOptionList) = ListPair.unzip tyInoutOptionList

        val _ = let val n = length patList
                    val m = length tyList
                in  if m=n then ()
                    else raise (ErrElaboration ("Arity mismatch for " 
                            ^ id ^ ": " 
                            ^ Int.toString m ^ " args required, but " 
                            ^ Int.toString n ^ " args given"))
                end

        val patTyList = ListPair.zip (patList, tyList)

        val (typedPatList,bvTyEnv) = elabPats env tyvarList patTyList

        val (typedPathExpListBoolList,bvTyEnv) = 
            elabPathExpLists env tyvarList bvTyEnv pathExpListIdOptionList
    in  
        TypedAst.Relation {args=typedPatList,
                       body=typedPathExpListBoolList,
                       bvenv=bvTyEnv}
    end

and elabPat env tyvarList (pat,ty) =
    let val ty = AstUtil.unfoldSyn (#tyenv env) ty
    in

    case pat of

      VarPat (path as [id]) => 
         let val {idenv=idenv,tyenv=tyenv} = env
         in
             case SEnv.find (idenv, id) of
                SOME (ConId _ ) =>
                let 
                    val _ = case ty of
                              ConTy (_,[]) => ()
                            | _ => raise (ErrElaboration (Type.pr ty ^ 
                                             ": type for variable " ^ id))
                in
                    (TypedAst.ConPat (path,[]), SEnv.empty)
                end
             | _ => 
                let val bvTyEnv = SEnv.singleton (id, ty)
                in
                    (TypedAst.VarPat (path,ty), bvTyEnv)
                end
         end

    | VarPat path =>
         raise (ErrElaboration ("Not yet implemented: VarPat " ^ Identifier.pr path))

    | IntPat i => 
      if ty=intTy then (TypedAst.IntPat i, SEnv.empty)
      else raise (ErrElaboration ("Not int type: " ^ Type.pr ty))
     
    | WordPat i => 
      if ty=wordTy then (TypedAst.WordPat i, SEnv.empty)
      else raise (ErrElaboration ("Not word type: " ^ Type.pr ty))

    | StringPat s => 
      if ty=stringTy then (TypedAst.StringPat s, SEnv.empty)
      else raise (ErrElaboration ("Not string type: " ^ Type.pr ty))

    | CharPat s => 
      if ty=charTy then (TypedAst.CharPat s, SEnv.empty)
      else raise (ErrElaboration ("Not char type: " ^ Type.pr ty))

    | WildPat => 
      let val id = Identifier.wildId (newName ())
          val bvTyEnv = SEnv.singleton (id, ty)
      in
          (TypedAst.WildPat (id,ty), bvTyEnv)
      end

    | RecordPat {def=stringPatList,flexibility=b} =>
      let
          val stringTyList = 
              case ty of
                 RecordTy stringTyList => stringTyList
              | _ => raise (ErrElaboration ("Not record type: " ^ Type.pr ty))

          val _ = if length stringTyList=length stringPatList orelse b
                  then ()
                  else raise (ErrElaboration ("Type mismatch in record pattern: " ^ Type.pr ty))

          val rectyEnv = 
              foldl (fn ((l,ty),rectyEnv) => SEnv.insert(rectyEnv,l,ty))
                     SEnv.empty
                     stringTyList

          val (typedStringPatList, bvTyEnv, rectyEnv) =
              foldl (fn ((l,pat),(typedStringPatList,bvTyEnv,rectyEnv)) => 
                     case SEnv.find (rectyEnv,l) of
                       SOME ty => 
                       let val (pat,bvTyEnv') = elabPat env tyvarList (pat, ty)
                           val bvTyEnv'' = mergeBvTyEnv bvTyEnv bvTyEnv'
                           val (rectyEnv,_) = SEnv.remove (rectyEnv,l)
                       in
                           (typedStringPatList@[(l,pat)], bvTyEnv'',rectyEnv)
                       end
                     | NONE => raise (ErrElaboration (l ^ " is not found in " ^ Type.pr ty))
                    ) ([],SEnv.empty,rectyEnv) stringPatList

         val stringTyList = SEnv.listItemsi rectyEnv

         val typedStringPatList = 
                typedStringPatList @ 
                map (fn (l,ty)=>(l,TypedAst.VarPat ([l],ty))) stringTyList

         val bvTyEnv = foldl (fn ((l,ty),bvTyEnv) => 
                          SEnv.insert (bvTyEnv,l,ty)) bvTyEnv stringTyList

      in
          (TypedAst.RecordPat {def=typedStringPatList, flexibility=b}, bvTyEnv)
      end

    | ListPat patList =>
      let 
          val elemTy = 
              case isListTy ty of
                SOME ty => ty
              | NONE => raise (ErrElaboration ("Not list type: " ^ Type.pr ty))
          val n = length patList
          val tyList = List.tabulate (n,fn _ => elemTy)
          val patTyList = ListPair.zip (patList,tyList)

          val (typedPatList,bvTyEnv) = elabPats env tyvarList patTyList

          val typedPat =
              foldr (fn (typedPat,pat) =>
                 TypedAst.ConPat ([neutralConsCon], [(typedPat,elemTy), (pat,ty)]))
                 (TypedAst.ConPat ([nilCon],[]))
                 typedPatList

      in
          (typedPat, bvTyEnv)
      end

    | TuplePat patList =>
      let 
          val tyList = 
              case ty of
                TupleTy tyList => tyList
              | _ => raise (ErrElaboration ("Not tuple type: " ^ Type.pr ty))

          val _ = 
              if length patList=length tyList then ()
              else raise (ErrElaboration ("Type mismatch in tuple pattern : " ^ Type.pr ty))

          val patTyList = ListPair.zip (patList,tyList)

          val (typedPatList,bvTyEnv) = elabPats env tyvarList patTyList
      in
          (TypedAst.TuplePat typedPatList, bvTyEnv)
      end

    | FlatAppPat flatAppPats =>

(***
   Idea:

   case flatAppPats of

     [pat] => pat*

   | [pat1,pat2] 
           => if IsConstr (pat1) 
              then ConPat ( pat1*, pat2* )
              else AppPat ( pat1*, pat2* )

   | (pat1::pat2::pat3::pats) 
           => if IsListCons (pat2)
              then ListPat ( pat1*, (pat3::pats)* )
              else ( [pat1,pat2] :: pat3 :: pats )*
***)

    (case flatAppPats of

       [pat] => elabPat env tyvarList (pat,ty)

     | [pat1, pat2] =>

       let fun isConstr (VarPat (path as [id])) =
               let
                  val {idenv=idenv,tyenv=tyenv} = env
               in 
                  case SEnv.find (idenv,id) of
                     SOME (ConId (_,_,_)) => true
                  | _ => false
               end

             | isConstr (_) = false

       in

       if isConstr pat1
       then
          let 
             val (path, id) = case pat1 of
                                (VarPat (path as [id])) => (path, id)
                              | _ => raise (ErrElaboration ("Impossible"))

             val {idenv=idenv,tyenv=tyenv} = env

             val ty' = 
                 case SEnv.find (idenv,id) of
                     SOME (ConId (ty',_,_)) => ty'
                   | _ => raise (ErrElaboration ("Constructor not found : " ^ id))

             val ty = case ty' of 
                       ConTy ([conid],[ty1,ty2]) =>
                       if conid=arrowTycon andalso ty=ty2 
                       then ty1
                       else raise (ErrElaboration 
                                ("Either " ^ conid ^ " <> " ^ arrowTycon 
                                ^ " or " ^ Type.pr ty ^ " <> " ^ Type.pr ty2))
                     | _ => raise (ErrElaboration 
                               ("Not function type : " ^ Type.pr ty'))

             val (typedPat,bvTyEnv) = elabPat env tyvarList (pat2,ty)

          in
             (TypedAst.ConPat ([id], [(typedPat,ty)]), bvTyEnv)
          end

       else
          let 
             val (pat1,funty) = 
                  case pat1 of
                    ConstraintPat {pattern=pat1,constraint=ty} => (pat1,ty)
                  | _ => raise (ErrElaboration "Type annotation demanded by function patterns")

             val (ty1,ty2) = 
                  case funty of 
                    ConTy (con as [arrowcon], [ty1,ty2]) =>
                      if arrowcon=I.arrowTycon andalso ty2=ty
                      then (ty1,ty2) 
                      else
                         raise (ErrElaboration ("Type annotation error:" ^ T.pr funty))
                  | _ => raise (ErrElaboration "Type annotation should be function types")

             val patTyList = [ (pat1,funty), (pat2,ty1) ]
             val (typedPats,bvTyEnv) = elabPats env tyvarList patTyList
             val typedPatTyList = ListPair.zip (typedPats, [funty,ty1])

             val id = Identifier.flatappId (newName ())
             val bvTyEnv = SEnv.insert (bvTyEnv, id, ty)
          in
             (TypedAst.FlatAppPat (id,ty,typedPatTyList), bvTyEnv)
          end

       end

     | (pat1 :: pat2 :: pat3 :: pats) =>

       let fun isListCons (VarPat (path as [id])) = 
                  if id=I.infixConsCon
                  then true
                  else false
             | isListCons (_) = false

       in

       if isListCons pat2 
       then
           let val tyElem = 
                   case isListTy ty of 
                     SOME tyElem => tyElem
                   | NONE => raise (ErrElaboration ("Not list type: " ^ Type.pr ty))

               val (typedLeftPat,leftBvTyEnv) = elabPat env tyvarList (pat1,tyElem)
               val (typedRightPat,rightBvTyEnv) = elabPat env tyvarList (FlatAppPat (pat3::pats), ty)
               val bvTyEnv = mergeBvTyEnv leftBvTyEnv rightBvTyEnv
               val typedPatTyList = [(typedLeftPat,tyElem),(typedRightPat,ty)]
           in
               (TypedAst.ConPat ([I.neutralConsCon], typedPatTyList), bvTyEnv)
           end
       else
           elabPat env tyvarList (FlatAppPat (FlatAppPat [pat1,pat2] :: pat3 :: pats), ty)
       end

     | [] => raise (ErrElaboration ("Empty FlatAppPat"))

    )


    | ConstraintPat {pattern=pat,constraint=ty'} =>
      let 
          val _ = if ty=ty' then ()
                  else raise (ErrElaboration 
                         ("pattern type " ^ Type.pr ty ^ 
                          " and constraint " ^ Type.pr ty' ^ " don't agree"))

          val (typedPat,bvTyEnv) = elabPat env tyvarList (pat,ty)
      in
          (TypedAst.ConstraintPat (typedPat,ty'), bvTyEnv)
      end 

    | LayeredPat {varPat=VarPat [id],expPat=expPat} =>
      let
          val (typedExpPat,bvTyEnv) = elabPat env tyvarList (expPat,ty)
          val bvTyEnv = 
              case SEnv.find(bvTyEnv, id) of
                NONE => SEnv.insert (bvTyEnv,id,ty)
              | SOME _ => raise (ErrElaboration 
                            ("Recursive pattern variable" ^ id))
      in
          (TypedAst.LayeredPat (id,ty,typedExpPat), bvTyEnv)
      end

    | LayeredPat {varPat=varPat,expPat=expPat} =>
      raise (ErrElaboration ("Not variable for pattern to left of \"as\"" ))

    | VectorPat patList =>
      let 
          val ty = 
              case isVecTy ty of
                SOME ty => ty
              | NONE => raise (ErrElaboration ("Not vector type: " ^ Type.pr ty))

          val n = length patList
          val tyList = List.tabulate (n,fn _ => ty)
          val patTyList = ListPair.zip (patList,tyList)

          val (typedPatList,bvTyEnv) = elabPats env tyvarList patTyList
      in
          (TypedAst.VectorPat typedPatList, bvTyEnv)
      end

    | OrPat patList =>
      let 
          val n = length patList
          val tyList = List.tabulate (n,fn _ => ty)
          val patTyList = ListPair.zip (patList,tyList)
          val (typedPatList,bvTyEnv) = elabPats env tyvarList patTyList

          val id = Identifier.orpatId (newName ())
          val bvTyEnv = SEnv.insert (bvTyEnv, id, ty)
      in
          (TypedAst.OrPat (id,ty,typedPatList), bvTyEnv)
      end

    end

and elabPats env tyvarList patTyList = 
    foldr (fn (patTy,(typedPatList,bvTyEnv)) => 
           let
               val (typedPat, bvTyEnv') = elabPat env tyvarList patTy
               val bvTyEnv'' = mergeBvTyEnv bvTyEnv bvTyEnv'
           in
              (typedPat::typedPatList,bvTyEnv'')
           end
           )
          ([],SEnv.empty) patTyList

and elabPathExpList env tyvarList bvTyEnv (path as [id], expList, idOption) =
    let
        val {idenv=idenv,tyenv=tyenv} = env

        val tyInoutOptionList =
            case SEnv.find (idenv, id) of
              SOME (RelId tyInoutOptionList) => tyInoutOptionList
            | _ => raise (ErrElaboration ("Not found: " ^ id))

        val (tyList,inoutOptionList) = ListPair.unzip tyInoutOptionList

        val _ = let val n = length expList
                    val m = length tyList
                in  if m=n then ()
                    else raise (ErrElaboration ("Arity mismatch for " 
                            ^ Identifier.pr path ^ ": " 
                            ^ Int.toString m ^ " args required, but " 
                            ^ Int.toString n ^ " args given"))
                end

        val expTyList = ListPair.zip (expList, tyList)
 
        val (typedExpList,bvTyEnv) = elabExps env tyvarList bvTyEnv expTyList

        val negation = case idOption of
                         NONE => false
                       | SOME "not" => true
                       | SOME s => raise (ErrElaboration ("\"not\" should be used for negation, instead of " ^ s))
    in
        ((path, typedExpList, negation), bvTyEnv)
    end

  | elabPathExpList env tyvarList bvTyEnv (path, _, _) = 
    raise (ErrElaboration ("Not yet supported: " ^ Identifier.pr path))

and elabPathExpLists env tyvarList bvTyEnv pathExpListIdOptionList = 
    foldl (fn (pathExpListIdOption,(typedPathExpListList,bvTyEnv)) => 
           let
               val (typedPathExpList, bvTyEnv') =
                   elabPathExpList env tyvarList bvTyEnv pathExpListIdOption
               val bvTyEnv'' = mergeBvTyEnv bvTyEnv bvTyEnv'
           in
               (typedPathExpListList @ [typedPathExpList], bvTyEnv'')
           end
          )
          ([],bvTyEnv) 
          pathExpListIdOptionList

and elabExp env tyvarList bvTyEnv (exp,ty) =
    let val ty = AstUtil.unfoldSyn (#tyenv env) ty
    in

    case exp of
      VarExp (path as [id]) => 
         let val {idenv=idenv,tyenv=tyenv} = env
         in
             case SEnv.find (idenv, id) of
               SOME (ConId _ ) =>
               let val _ = case ty of
                             ConTy (_,[]) => ()
                           | _ => raise (ErrElaboration (Type.pr ty ^ 
                                           ": type for variable " ^ id))
               in
                   (TypedAst.ConExp (path,[]), bvTyEnv)
               end
             | _ =>
               let val bvTyEnv = 
                       case SEnv.find (bvTyEnv,id) of
                         SOME ty' =>
                           if ty=ty' then bvTyEnv
                           else raise (ErrElaboration ("Type mismatch for " ^ id 
                                  ^ ": " ^ Type.pr ty ^ ", " ^ Type.pr ty'))
                       | NONE => SEnv.insert(bvTyEnv,id,ty)

               in  
                   (TypedAst.VarExp (path, ty), bvTyEnv)
               end
         end

    | VarExp path => 
         raise (ErrElaboration ("Not yet implemented: VarExp " ^ Identifier.pr path))

    | FlatAppExp flatAppExps =>

    (case flatAppExps of

       (SelectorExp label :: _) =>
               raise (ErrElaboration  
                         ("Not implemented: FlatAppExp [SelectorExp label, ...]"))

     | [exp] => elabExp env tyvarList bvTyEnv (exp,ty)

     | [exp1, exp2] => 

       let fun isConstr (VarExp (path as [id])) =
               let
                  val {idenv=idenv,tyenv=tyenv} = env
               in
                  case SEnv.find (idenv,id) of
                    SOME (ConId (_,_,_)) => true
                  | _ => false
               end

             | isConstr _ = false

       in

       if isConstr exp1
       then

          let
             val (id,exp) = case (exp1,exp2) of
                              (VarExp [id], exp) => (id,exp)
                            | _ => raise (ErrElaboration ("Impossible"))
             
             val {idenv=idenv,tyenv=tyenv} = env

             val ty' = 
                 case SEnv.find (idenv,id) of
                   SOME (ConId (ty',_,_)) => ty'
                 | _ => raise (ErrElaboration ("Constructor not found : " ^ id))

             val ty = 
                 case ty' of 
                   ConTy ([conid],[ty1,ty2]) =>
                     if conid=arrowTycon andalso ty=ty2 
                     then ty1
                     else raise (ErrElaboration 
                            ("Either " ^ conid ^ " <> " ^ arrowTycon 
                             ^ " or " ^ Type.pr ty ^ " <> " ^ Type.pr ty2))
                 | _ => raise (ErrElaboration 
                            ("Not function type : " ^ Type.pr ty'))

             val (typedExp,bvTyEnv) = elabExp env tyvarList bvTyEnv (exp,ty)
          in
             (TypedAst.ConExp ([id], [(typedExp,ty)]), bvTyEnv)
          end

       else

          let
             val (exp1,funty) = 
                  case exp1 of
                    ConstraintExp {expr=exp1,constraint=ty} => (exp1,ty)
                  | _ => (PP.ppExp exp1;
                         raise (ErrElaboration ("Type annotation demanded by function exprs: ")))

             val (ty1,ty2) = 
                  case funty of
                    ConTy (con as [arrowcon], [ty1,ty2]) =>
                      if arrowcon=I.arrowTycon andalso ty2=ty
                      then (ty1,ty2)
                      else
                         raise (ErrElaboration ("Type annotation error:" ^ T.pr funty))
                  | _ => raise (ErrElaboration "Type annotation should be function types")

             val expTyList = [ (exp1,funty), (exp2,ty1) ]
             val (typedExpList,bvTyEnv) = elabExps env tyvarList bvTyEnv expTyList
             val typedExpTyList = ListPair.zip (typedExpList, [funty,ty1])

             val id = Identifier.flatappId (newName ())
             val bvTyEnv = SEnv.insert (bvTyEnv, id, ty)
          in
             (TypedAst.FlatAppExp (id,ty,typedExpTyList), bvTyEnv)
          end

       end

    | (exp1 :: exp2 :: exp3 :: exps) =>  

       let fun isListCons (VarExp (path as [id])) =
                  if id=I.infixConsCon
                  then true
                  else false
             | isListCons (_) = false

       in

       if isListCons exp2
       then
          let val tyElem =
                   case isListTy ty of
                     SOME tyElem => tyElem
                   | NONE => raise (ErrElaboration ("Not list type: " ^ Type.pr ty))
              val (typedLeftExp,bvTyEnv) = elabExp env tyvarList bvTyEnv (exp1,tyElem)
              val (typedRightExp,bvTyEnv) = elabExp env tyvarList bvTyEnv (FlatAppExp (exp3::exps),ty)
              val typedExpTyList = [(typedLeftExp,tyElem),(typedRightExp,ty)]
          in
              (TypedAst.ConExp ([I.neutralConsCon], typedExpTyList), bvTyEnv)
          end
       else

          elabExp env tyvarList bvTyEnv (FlatAppExp (FlatAppExp [exp1,exp2] :: exp3 :: exps), ty)
       end


    | [] => raise (ErrElaboration "Empty FlatAppExp")

    )

    | AppExp {function=funExp,argument=argExp} =>
         raise (ErrElaboration ("Not implemented: AppExp")) 

    | IntExp i =>
         if ty=intTy then (TypedAst.IntExp i, bvTyEnv)
         else raise (ErrElaboration ("Not int type: " ^ Type.pr ty))

    | WordExp i =>
         if ty=wordTy then (TypedAst.WordExp i, bvTyEnv)
         else raise (ErrElaboration ("Not word type: " ^ Type.pr ty))

    | RealExp s =>
         if ty=realTy then (TypedAst.RealExp s, bvTyEnv)
         else raise (ErrElaboration ("Not real type: " ^ Type.pr ty))

    | StringExp s =>
         if ty=stringTy then (TypedAst.StringExp s, bvTyEnv)
         else raise (ErrElaboration ("Not string type: " ^ Type.pr ty))

    | CharExp s =>
         if ty=charTy then (TypedAst.CharExp s, bvTyEnv)
         else raise (ErrElaboration ("Not char type: " ^ Type.pr ty))

    | RecordExp stringExpList =>
         let 
             val stringTyList =
                 case ty of
                    RecordTy stringTyList => stringTyList
                 | _ => raise (ErrElaboration ("Not record type: " ^ Type.pr ty))

             val _ = if length stringTyList=length stringExpList
                     then ()
                     else raise (ErrElaboration ("Type mismatch in record expr: " ^ Type.pr ty))

             val rectyEnv =
                    foldl (fn ((l,ty),rectyEnv) => SEnv.insert(rectyEnv,l,ty))
                    SEnv.empty
                    stringTyList   

             val (typedStringExpList, bvTyEnv) = 
                  foldl (fn ((l,exp),(typedStringExpList,bvTyEnv)) => 
                     case SEnv.find (rectyEnv,l) of
                       SOME ty => 
                       let val (exp,bvTyEnv)=elabExp env tyvarList bvTyEnv (exp, ty)
                       in
                           (typedStringExpList@[(l,exp)], bvTyEnv)
                       end

                     | NONE => raise (ErrElaboration (l ^ " is not found in " ^ Type.pr ty))
                    ) ([],SEnv.empty) stringExpList
             val typedStringExpList = Util.sortLabels typedStringExpList
         in
            (TypedAst.RecordExp typedStringExpList, bvTyEnv)
         end

    | ListExp expList =>
        let
            val tyElem = 
                case isListTy ty of
                  SOME ty => ty
                | NONE => raise (ErrElaboration ("Not list type: " ^ Type.pr ty))
            val n = length expList
            val tyList = List.tabulate (n, fn _ => tyElem)
            val expTyList = ListPair.zip (expList, tyList)
            
            val (typedExpList,bvTyEnv) = elabExps env tyvarList bvTyEnv expTyList

            val typedNilExp  = TypedAst.ConExp ([nilCon],[])
            fun mkCons (e,l) = TypedAst.ConExp ([neutralConsCon],[(e,tyElem),(l,ty)])

            val typedExp = foldr mkCons typedNilExp typedExpList
        in
            (typedExp, bvTyEnv)
        end

    | TupleExp expList =>
         let 
             val tyList = 
                 case ty of
                   TupleTy tyList => tyList
                 | _ => raise (ErrElaboration ("Not tuple type: " ^ Type.pr ty))

             val _ = 
                 if length expList=length tyList then ()
                 else raise (ErrElaboration ("Type mismatch in tuple expression : " 
                                             ^ Type.pr ty))

             val expTyList = ListPair.zip (expList,tyList)

             val (typedExpList,bvTyEnv) = elabExps env tyvarList bvTyEnv expTyList
         in
             (TypedAst.TupleExp typedExpList, bvTyEnv)
         end

    | SelectorExp s =>
         raise (ErrElaboration ("Error: record selector " ^ s ^ " not in FlatAppExp"))

    | ConstraintExp {expr=exp,constraint=ty'} =>
         let 
             val _ = if ty=ty' then ()
                     else raise (ErrElaboration 
                            ("expr type " ^ Type.pr ty ^ 
                             " and constraint " ^ Type.pr ty' ^ " don't agree"))

             val (typedExp,bvTyEnv) = elabExp env tyvarList bvTyEnv (exp,ty)
         in
             (TypedAst.ConstraintExp (typedExp,ty'), bvTyEnv)
         end 

    | VectorExp expList =>
         let 
             val ty = 
                 case isVecTy ty of
                   SOME ty => ty
                 | NONE => raise (ErrElaboration ("Not vector type: " ^ Type.pr ty))

             val n = length expList
             val tyList = List.tabulate (n,fn _ => ty)
             val expTyList = ListPair.zip (expList,tyList)

             val (typedExpList,bvTyEnv) = elabExps env tyvarList bvTyEnv expTyList
         in
             (TypedAst.VectorExp typedExpList, bvTyEnv)
         end

   | _ => raise (ErrElaboration "Unexpected expression at elabExp")

   end

and elabExps env tyvarList bvTyEnv expTyList =
    foldl (fn (expTy,(typedExpList,bvTyEnv)) => 
           let
               val (typedExp, bvTyEnv') = elabExp env tyvarList bvTyEnv expTy
               val bvTyEnv'' = mergeBvTyEnv bvTyEnv bvTyEnv'
           in
               (typedExpList @ [typedExp], bvTyEnv'')
           end
          )
          ([],bvTyEnv) 
          expTyList

end

end