
structure Lifting =

struct

local

open Env TypedAst Type

in

structure I = Identifier

exception ErrLifting of string

type id = I.id

type replenv  = id SEnv.map
type dtenv    = (id * tyvar list * (id * ty option) list * origin) SEnv.map
type rectyenv = (id * tyvar list * (id * ty option) list * origin) SLEnv.map
type tuptyenv = (id * tyvar list * (id * ty option) list * origin) IEnv.map

type LiftingEnv = { tyenv:tyinfo SEnv.map, idenv:idinfo SEnv.map, 
                    replenv:replenv, dtenv:dtenv, 
                    rectyenv:rectyenv, tuptyenv:tuptyenv,
                    rectys:id list list, tuptys:int list}

(* Auxiliary functions *)

fun updidenv {idenv=_,tyenv=tyenv,replenv=replenv,dtenv=dtenv,
             rectyenv=rectyenv,tuptyenv=tuptyenv,
             rectys=rectys,tuptys=tuptys} idenv =
    {idenv=idenv,tyenv=tyenv,replenv=replenv,dtenv=dtenv,
     rectyenv=rectyenv,tuptyenv=tuptyenv,rectys=rectys,tuptys=tuptys}

fun updtyenv {idenv=idenv,tyenv=_,replenv=replenv,dtenv=dtenv,
             rectyenv=rectyenv,tuptyenv=tuptyenv,
             rectys=rectys,tuptys=tuptys} tyenv =
    {idenv=idenv,tyenv=tyenv,replenv=replenv,dtenv=dtenv,
     rectyenv=rectyenv,tuptyenv=tuptyenv,rectys=rectys,tuptys=tuptys}

fun updreplenv {idenv=idenv,tyenv=tyenv,replenv=_,dtenv=dtenv,
             rectyenv=rectyenv,tuptyenv=tuptyenv,
             rectys=rectys,tuptys=tuptys} replenv =
    {idenv=idenv,tyenv=tyenv,replenv=replenv,dtenv=dtenv,
     rectyenv=rectyenv,tuptyenv=tuptyenv,rectys=rectys,tuptys=tuptys}

fun upddtenv {idenv=idenv,tyenv=tyenv,replenv=replenv,dtenv=_,
             rectyenv=rectyenv,tuptyenv=tuptyenv,
             rectys=rectys,tuptys=tuptys} dtenv =
    {idenv=idenv,tyenv=tyenv,replenv=replenv,dtenv=dtenv,
     rectyenv=rectyenv,tuptyenv=tuptyenv,rectys=rectys,tuptys=tuptys}

fun updrectyenv {idenv=idenv,tyenv=tyenv,replenv=replenv,dtenv=dtenv,
             rectyenv=_,tuptyenv=tuptyenv,
             rectys=rectys,tuptys=tuptys} rectyenv =
    {idenv=idenv,tyenv=tyenv,replenv=replenv,dtenv=dtenv,
     rectyenv=rectyenv,tuptyenv=tuptyenv,rectys=rectys,tuptys=tuptys}

fun updtuptyenv {idenv=idenv,tyenv=tyenv,replenv=replenv,dtenv=dtenv,
             rectyenv=rectyenv,tuptyenv=_,
             rectys=rectys,tuptys=tuptys} tuptyenv =
    {idenv=idenv,tyenv=tyenv,replenv=replenv,dtenv=dtenv,
     rectyenv=rectyenv,tuptyenv=tuptyenv,rectys=rectys,tuptys=tuptys}

fun updrectys {idenv=idenv,tyenv=tyenv,replenv=replenv,dtenv=dtenv,
             rectyenv=rectyenv,tuptyenv=tuptyenv,rectys=_,tuptys=tuptys} rectys =
    {idenv=idenv,tyenv=tyenv,replenv=replenv,dtenv=dtenv,
             rectyenv=rectyenv,tuptyenv=tuptyenv,rectys=rectys,tuptys=tuptys}

fun updtuptys {idenv=idenv,tyenv=tyenv,replenv=replenv,dtenv=dtenv,
             rectyenv=rectyenv,tuptyenv=tuptyenv,rectys=rectys,tuptys=_} tuptys=
    {idenv=idenv,tyenv=tyenv,replenv=replenv,dtenv=dtenv,
             rectyenv=rectyenv,tuptyenv=tuptyenv,rectys=rectys,tuptys=tuptys}

(* Lifting identifiers with String.translate *)
fun liftTyconid id = I.liftTranslate id

fun liftConid id = I.liftTranslate id

fun THEConid id = I.itselfTranslate id

fun getLogicvarTy (env : LiftingEnv) tyconid tyvarList =
    let 
        val replenv = #replenv env
        val logicvarTycon = 
            case SEnv.find(replenv,Unification.logicvarTycon) of
              SOME renamedtyconid => renamedtyconid
            | NONE => raise (ErrLifting ("Not found: " ^ Unification.logicvarTycon))

        val stringTy = ConTy ([logicvarTycon],[])
        val itselfTy = ConTy ([tyconid],map VarTy tyvarList)
        val optionItselfTy = ConTy ([I.optionStr,I.optionTycon], [ itselfTy ])
        val refOptionItselfTy = ConTy ([I.refTycon], [optionItselfTy])
        val logicVarTy = TupleTy [stringTy, refOptionItselfTy]
    in
        logicVarTy
    end

fun replIds env dbOriginList =
    let 
        val replenv = #replenv env
        fun replId ((Db {tyc=tycid,...},_),replenv) = 
            let
                val rtycid = I.replicate tycid
                val replenv = SEnv.insert (replenv,tycid,rtycid)
            in
                replenv
            end
        val replenv = foldl replId replenv dbOriginList

        val env = updreplenv env replenv 
    in
        env
    end

fun allocLiftIds env dbOriginList = 
    let val tyenv = #tyenv env
        val dtenv = #dtenv env

        fun f ((Db {tyc=id,tyvars=tyvarList,...},origin),(tyenv,dtenv)) =
            let val liftedid = liftTyconid id
            in            
                ( SEnv.insert (tyenv, id, DtInfo NONE),
                  SEnv.insert (dtenv, id, (liftedid,tyvarList,[],origin)) )
            end

        val (tyenv, dtenv) = foldl f (tyenv,dtenv) dbOriginList

        val env = updtyenv env tyenv
        val env = upddtenv env dtenv
    in
        env
    end

(* 
   It is annoying that this function addLogicVars not only adds a logic varriable to
   each lifted datatype, but it also lifts the identifier of each lifted datatype.
   That is, the second argument liftedDbOriginList has unlifted identifiers as 
   type constructor identifiers. Lifting them is another duty of this function, though
   the name of the function does not give any hint on its performing the duty. 
   Actually, adding a logic variable naturally implies lifting data constructors.
*)

fun addLogicVars env liftedDbOriginList equality =
    let 
        val dtenv = #dtenv env
        val replenv = #replenv env

        fun liftIdTyList tyconid tyvarList [] = 
            let val renamedtyconid =
                    if tyconid=I.arrowTycon
                    then tyconid
                    else case SEnv.find(replenv,tyconid) of
                           SOME renamedtyconid => renamedtyconid
                         | NONE => raise (ErrLifting ("Renamed tycon name not found for: "^tyconid))
                val liftedtyconid = liftTyconid tyconid
                val tyconiditself = THEConid tyconid
            in
                ([ (liftedtyconid, SOME (getLogicvarTy env liftedtyconid tyvarList)),
                   (tyconiditself, SOME (ConTy ([renamedtyconid],map VarTy tyvarList))) ], 
                    Lift (Boundary equality,FromDatatype [tyconid]))
            end

          | liftIdTyList tyconid tyvarList idTyOptionList = 
            let
                val liftedtyconid = liftTyconid tyconid
            in 
               ([ (liftedtyconid, SOME (getLogicvarTy env liftedtyconid tyvarList)) ] @ idTyOptionList,
                Lift (Center, FromDatatype [tyconid]))
            end

        fun gather ((Db {tyc=tycid,tyvars=tyvarList,rhs=Constrs idTyOptionList},origin),(dtenv,l)) = 
            let val liftedtycid = liftTyconid tycid
                val (idtyoptionlist,origin) = liftIdTyList tycid tyvarList idTyOptionList
                val e = (liftedtycid, tyvarList, idtyoptionlist, origin)
                val d = (Db {tyc=liftedtycid,tyvars=tyvarList,rhs=Constrs idtyoptionlist}, origin)
            in
                (SEnv.insert (dtenv, tycid, e), l @ [d])
            end

          | gather _ = raise (ErrLifting ("Uexpected Repl of Db"))

        val (dtenv,liftedDbOriginList) = foldl gather (dtenv,[]) liftedDbOriginList

        val env = upddtenv env dtenv
    in
        (env,liftedDbOriginList)
    end

fun getIndirCon [(_,_),(id,_)] = id
  | getIndirCon _ = raise (ErrLifting ("getIndirCon"))

fun addTupleTy (env : LiftingEnv) arity =
    case IEnv.find (#tuptyenv env,arity) of
      SOME _ => env
    | NONE => 
      let
         val tyconid = I.tupletyid arity
         val tupconid = I.tupleconid arity
         fun mkTyvar n = "'a" ^ Int.toString n
         val tyvarList = List.tabulate (arity,mkTyvar)
         val conidTyOptionList = 
             [ (tyconid, SOME (getLogicvarTy env tyconid tyvarList)), 
               (tupconid, SOME (TupleTy (map VarTy tyvarList))) ]

         val e = (tyconid,tyvarList,conidTyOptionList,Lift (Center,FromTupletype arity))
         val tuptyenv = IEnv.insert (#tuptyenv env,arity,e)
         val env = updtuptyenv env tuptyenv
         val env = updtuptys env (arity :: #tuptys env)
      in
         env
      end

fun addRecordTy (env : LiftingEnv) labels =
    case SLEnv.find (#rectyenv env,labels) of
      SOME _ => env
    | NONE => 
      let
         val tyconid = I.recordtyid labels
         val recconid = I.recordconid labels
         fun mkTyvar n = "'a" ^ Int.toString n
         val arity = length labels
         val tyvarList = List.tabulate (arity,mkTyvar)
         val labelTyList = ListPair.zip (labels,map VarTy tyvarList)
         val conidTyList = 
             [ (tyconid, SOME (getLogicvarTy env tyconid tyvarList)),
               (recconid, SOME (RecordTy labelTyList)) ]
         val e = (tyconid,tyvarList,conidTyList,Lift (Center, FromRecordtype labels))
         val rectyenv = SLEnv.insert (#rectyenv env,labels,e)
         val env = updrectyenv env rectyenv
         val env = updrectys env (labels :: #rectys env)
      in
         env
      end

fun mkDatatypeDec []     = []
  | mkDatatypeDec dbOriginList = [DatatypeDec (dbOriginList, NONE)]

fun mkTypeDec [] = []
  | mkTypeDec tbList = [TypeDec tbList]

fun mkAuxDatatypeDec (env : LiftingEnv) = 
    let 
        val {tuptys=tuptys,rectys=rectys,
             tuptyenv=tuptyenv,rectyenv=rectyenv, ...} = env

        fun f (i,xs) = case IEnv.find (tuptyenv,i) of
                         SOME x => x :: xs
                       | NONE => xs

        fun g (labels,xs) = case SLEnv.find (rectyenv,labels) of
                              SOME x => x :: xs
                            | NONE => xs

        fun mkDb (id,tyvarList,idtyoptions,origin) = 
              (Db {tyc=id,tyvars=tyvarList,rhs=Constrs idtyoptions}, origin)

        val tuptyoriginenv = foldr f [] tuptys
        val tupleDeclList = mkDatatypeDec (map mkDb tuptyoriginenv)

        val rectyoriginenv = foldr g [] rectys
        val recordDeclList = mkDatatypeDec (map mkDb rectyoriginenv)

        val env = updtuptys env []
        val env = updrectys env []
    in
        (env, tupleDeclList @ recordDeclList)
    end

(* Entry function *)

fun lifting declList = 
    let
        val env = {tyenv=SEnv.empty, idenv=SEnv.empty, 
                   replenv=SEnv.empty, dtenv=SEnv.empty,
                   rectyenv=SLEnv.empty, tuptyenv=IEnv.empty,
                   rectys=[], tuptys=[]}

        val (env,declList) = liftDeclList env declList

        val {tyenv=tyenv, idenv=idenv, 
                   replenv=replenv, dtenv=dtenv,
                   rectyenv=rectyenv, tuptyenv=tuptyenv,
                   rectys=rectys, tuptys=tuptys} = env
    in
        declList
    end

and liftDeclList env (RelationSigDec (id,tyInoutOptionList) :: declList) =
    let 
        val idenv = #idenv env
        val idenv = SEnv.insert (idenv, id, RelId tyInoutOptionList)
        val env = updidenv env idenv 
        fun f (a,(e,l)) = 
              let val (e',a') = liftTyInoutOption e a
              in  (e', l @ [a'])
              end
        val (env,tyInoutOptionList) = foldl f (env,[]) tyInoutOptionList
        val decl = RelationSigDec (id,tyInoutOptionList)

        val (env,auxDeclList) = mkAuxDatatypeDec env
        val (env,declList) = liftDeclList env declList
    in
        (env,  auxDeclList @ (decl :: declList))
    end

  | liftDeclList env (DatatypeDec (dbOriginList as 
          [(Db {tyc=id,tyvars=tyvarList,rhs=Constrs[]},Prelude)],NONE) :: declList) =

    let 
        val env = if id=I.arrowTycon
                  then env
                  else replIds env dbOriginList

        val env = allocLiftIds env dbOriginList

        val (env, dbOriginList, replDbOriginList, liftedDbOriginList)
                = foldl liftDb (env,[],[],[]) dbOriginList

        val decls = mkDatatypeDec dbOriginList

        val replDecls = if id=I.arrowTycon
                        then []
                        else case replDbOriginList of
                              [ (Db {tyc=replid,tyvars=repltyvars,rhs=_},_) ] => 
                                 mkTypeDec [Tb {tyc=replid,tyvars=repltyvars,def=ConTy ([id],[])}]
                             | _ => raise (ErrLifting ("Error in replication of datatype: " ^ id))

        val equality = if id=I.arrowTycon then NONEQ else EQ

        val (env,liftedDbOriginList) = addLogicVars env liftedDbOriginList equality

        val liftedDecls = mkDatatypeDec liftedDbOriginList

        val (env,auxDeclList) = mkAuxDatatypeDec env

        val (env,declList) = liftDeclList env declList
    in
        (env, auxDeclList @ decls @ replDecls @ liftedDecls @ declList)
    end

  | liftDeclList env (DatatypeDec (dbOriginList,NONE) :: declList) =
    let 
        val env = replIds env dbOriginList

        val env = allocLiftIds env dbOriginList

        val (env,dbOriginList,replDbOriginList,liftedDbOriginList) = 
             foldl liftDb (env,[],[],[]) dbOriginList

        val decls = mkDatatypeDec dbOriginList

        (* datatype replication should be declared alone. *)
        val replDecls = List.concat (map mkDatatypeDec (map Util.list replDbOriginList))

        val equality = EQ

        val (env,liftedDbOriginList) = addLogicVars env liftedDbOriginList equality

        val liftedDecls = mkDatatypeDec liftedDbOriginList

        val (env,auxDeclList) = mkAuxDatatypeDec env

        val (env,declList) = liftDeclList env declList
    in
        (env, auxDeclList @ decls @ replDecls @ liftedDecls @ declList)
    end


  | liftDeclList env (DatatypeDec _ :: declList) =
      raise (ErrLifting ("Not supported yet: type declaration in datatype"))


  | liftDeclList env (TypeDec tbList :: declList) =
    let 
        val tyenv = #tyenv env
        fun f ((id,tyinfo),tyenv) = SEnv.insert(tyenv,id,tyinfo)
        val idSyninfoList = (map (fn tb as Tb {tyc=id,tyvars=tyvarList,def=ty} =>
                                          (id, SynInfo (tyvarList, ty))) tbList)
        val tyenv = foldl f tyenv idSyninfoList
        val env = updtyenv env tyenv 
        val decl = TypeDec tbList

        val (env,auxDeclList) = mkAuxDatatypeDec env
        val (env, declList) = liftDeclList env declList
    in
        (env, auxDeclList @ (decl :: declList))
    end

  | liftDeclList env (FolDec (idFolList,tyvarList) :: declList) =
    let
        fun f ((id,fol),(env,l)) = 
              let val (env,fol) = liftFol env fol
              in  (env, l @ [(id,fol)])
              end
        val (env,idFolList) = foldl f (env,[]) idFolList
        val decl = FolDec (idFolList,tyvarList)
        val (env,auxDeclList) = mkAuxDatatypeDec env
        val (env,declList) = liftDeclList env declList
    in
        (env, auxDeclList @ (decl :: declList))
    end

  | liftDeclList env (RelationDec _ :: decls) =
      raise (ErrLifting ("Unexpected declaration"))

  | liftDeclList env [] = (env, [])

and liftDb ((Db {tyc=id,tyvars=tyvarList,rhs=Constrs idTyOptionList},origin),
           (env,dbOriginList,replDbOriginList,liftedDbOriginList)) =

    let 
        (* environment maintenance *)
        val ty = ConTy ([id], map VarTy tyvarList)
        val idTyList = map (fn (cid,tyOption) => (cid,arrowty(tyOption,ty))) idTyOptionList

        val idenv = #idenv env
        val idenv = foldl (fn ((cid,ty),idenv) => 
                          SEnv.insert (idenv, cid, ConId (ty,tyvarList,id)))
                          idenv idTyList
        val env = updidenv env idenv

        val tyenv = #tyenv env
        val tyenv = SEnv.insert (tyenv, id, DtInfo (SOME (tyvarList,idTyList)))
        val env = updtyenv env tyenv

        (* datatype replication for avoiding name collision *)

        val (dbOriginList,replDbOriginList) = 
             if id=I.arrowTycon
             then
               (dbOriginList, replDbOriginList)
             else 
               let
                  val replenv = #replenv env
                  val replid = case SEnv.find (replenv, id) of
                                 SOME replid => replid
                               | NONE => raise (ErrLifting "Impossible to happen for replenv")

                  val dbOriginList = dbOriginList 
                        @ [(Db{tyc=id,tyvars=tyvarList,rhs=Constrs idTyOptionList},origin)]
                  val replDbOriginList = replDbOriginList 
                        @ [(Db{tyc=replid,tyvars=[],rhs=Repl [id]},Lift(Center,FromDatatype [id]))]
               in
                  (dbOriginList,replDbOriginList)
               end

        (* lifting datatype *)

        val (env,liftedIdTyOptionList) = 
            foldl (fn ((cid,tyOption),(env,liftedIdTyOptionList)) =>
              let
                 val liftedcid = liftConid cid
                 val (env,liftedTyOption) = liftTyOption env tyOption
                 val liftedIdTyOptionList = liftedIdTyOptionList @ [(liftedcid,liftedTyOption)]
              in
                 (env,liftedIdTyOptionList)
              end) (env,[]) idTyOptionList

        val liftedDbOrigin = (Db {tyc=id,tyvars=tyvarList,rhs=Constrs liftedIdTyOptionList},origin)
        val liftedDbOriginList = liftedDbOriginList @ [liftedDbOrigin]
    in  
        (env,dbOriginList,replDbOriginList,liftedDbOriginList)
    end 

  | liftDb ((Db {tyc=id,tyvars=tyvarList,rhs=Repl [defid]},origin),
           (env,dbOriginList,replDbOriginList,liftedDbOriginList)) =

    let val tyenv = #tyenv env
        val env = case SEnv.find (tyenv, defid) of
                    SOME (dtinfo as DtInfo _)
                      => updtyenv env (SEnv.insert(tyenv,id,dtinfo))
                  | _ => raise (ErrLifting ("Undefined type: " ^ defid))

        val dbOriginList = dbOriginList @ [ (Db {tyc=id,tyvars=tyvarList,rhs=Repl [defid]},
                                             origin) ]
        val replenv = #replenv env
        val replid = case SEnv.find (replenv, id) of
                       SOME replid => replid
                     | NONE => raise (ErrLifting "Impossible to happen for replenv")

        val replDbOrigin = (Db {tyc=replid,tyvars=[],rhs=Repl [id]}, 
                            Lift (Center,FromDatatype [id]))
        val replDbOriginList = replDbOriginList @ [ replDbOrigin ]

        val liftedDbOriginList = liftedDbOriginList @ []
    in
        (env, dbOriginList, replDbOriginList, liftedDbOriginList)
    end
          
  | liftDb _ = raise (ErrLifting 
             ("Not supported yet: replicated datatype with qualifed identifiers"))

and liftTyInoutOption env (ty,inoutOption) =
    let val (env,liftedty) = liftTy env ty
    in  (env, (liftedty,inoutOption))
    end

and liftTyOption env (SOME ty) = 
    let val (env,ty) = liftTy env ty
    in  (env, SOME ty)
    end
  | liftTyOption env NONE = (env, NONE)

and liftTy env (VarTy tyvar) = (env, VarTy tyvar)

  | liftTy env (ty as ConTy (path as [tyconid],tyList)) = 

    if tyconid=I.arrowTycon andalso length tyList=2
    then

    let
        val liftedtyconid = liftTyconid tyconid

        val (env, tyList) = foldl (fn (ty,(env,tyList)) =>
                              let val (env,ty) = liftTy env ty
                              in  (env,tyList @ [ty])
                              end) (env,[]) tyList
    in
        (env, ConTy ([liftedtyconid], tyList))
    end

    else

    let 
        val (env, ty) = 
             case AstUtil.unfoldSyn (#tyenv env) ty of
               ConTy ([tyconid],tyList) => 
               let
                  val dtenv = #dtenv env
                  val liftedtyconid =
                      case SEnv.find(dtenv, tyconid) of
                        SOME (liftedtyconid,_,_,_) => liftedtyconid
                      | NONE => raise (ErrLifting ("Something wrong : " ^ tyconid))
                  val (env, tyList) = foldl (fn (ty,(env,tyList)) =>
                              let val (env,ty) = liftTy env ty
                              in  (env,tyList @ [ty])
                              end) (env,[]) tyList
               in
                  (env, ConTy ([liftedtyconid], tyList))
               end
             | RecordTy idTyList => liftTy env (RecordTy idTyList)
             | TupleTy tyList => liftTy env (TupleTy tyList)
             | _ => raise (ErrLifting ("Something wrong : unfoldSyn " ^ Type.pr ty))
    in
        (env,ty)
    end

  | liftTy env (ConTy (path,tyList)) = 
    raise (ErrLifting ("Not supported: " ^ I.pr path))

  | liftTy env (RecordTy idTyList) = 
    let 
        val (labels,tyList) = ListPair.unzip idTyList
        val env = addRecordTy env labels
        val tyconid = 
            case SLEnv.find (#rectyenv env, labels) of
              SOME (tyconid,_,_,_) => tyconid
            | NONE => raise (ErrLifting "Impossible to happen")

        val (env,tyList) = foldl (fn (ty,(env,tyList)) =>
                              let val (env,ty) = liftTy env ty
                              in  (env,tyList @ [ty])
                              end) (env,[]) tyList

        val recty = ConTy ([tyconid], tyList)
    in     
        (env, recty)
    end

  | liftTy env (TupleTy tyList) = 
    let
        val arity = length tyList
        val env = addTupleTy env arity
        val tyconid = case IEnv.find (#tuptyenv env, arity) of
                        SOME (tyconid,_,_,_) => tyconid
                      | NONE => raise (ErrLifting "Impossible to happen")

        val (env,tyList) = foldl (fn (ty,(env,tyList)) =>
                              let val (env,ty) = liftTy env ty
                              in  (env,tyList @ [ty])
                              end) (env,[]) tyList

        val tuplety = ConTy ([tyconid], tyList)
    in  
        (env, tuplety)
    end

  | liftTy env (ForallTy (tyvarList,ty)) = 
    let
        val (env,ty) = liftTy env ty
        val forallty = ForallTy (tyvarList, ty)
    in
        (env,forallty)
    end

and liftFol env fol =
  
    case fol of
      Assign (id,ty,exp) => 
        let
           val (env,liftedty) = liftTy env ty
           val (env,liftedexp) = liftExp env (exp,liftedty)
        in
           (env, Assign (id,liftedty,liftedexp))
        end

    | Unify (ty,exp1,exp2) =>
        let 
           val (env,liftedty) = liftTy env ty
           val (env,liftedexp1) = liftExp env (exp1,liftedty)
           val (env,liftedexp2) = liftExp env (exp2,liftedty)
        in  
           (env, Unify (liftedty,liftedexp1,liftedexp2))
        end 

    | Pred (path as [id],expList, negation) =>
        let 
          val idenv = #idenv env
          val tyInoutOptionList = 
                case SEnv.find(idenv,id) of
                  SOME (RelId tyInoutOptionList) => tyInoutOptionList
                | _ => raise (ErrLifting ("Not found: " ^ I.pr path))
          val _ = 
                if length expList=length tyInoutOptionList then ()
                else raise (ErrLifting ("Arity mismatch at liftFol"))
          val (tyList, inoutOptionList) = ListPair.unzip tyInoutOptionList
          val expTyList = ListPair.zip (expList,tyList)

          fun f ((exp,ty),(env,l)) = let val (env,liftedty) = liftTy env ty
                                      val (env,exp) = liftExp env (exp,liftedty)
                                  in  (env, l @ [exp])
                                  end
          val (env,expList) = foldl f (env,[]) expTyList
        in
          (env, Pred (path, expList, negation))
        end

    | Pred (path,expList,negation) =>
        raise (ErrLifting ("Not supported yet: " ^ I.pr path))

    | Conj folList =>
        let val (env,folList) = 
                 foldl (fn (fol,(env,l)) => 
                            let val (env,fol) = liftFol env fol
                            in  (env, l @ [fol])
                            end ) (env, []) folList
        in  (env, Conj folList)
        end

    | Disj folList =>
        let val (env,folList) = 
                 foldl (fn (fol,(env,l)) => 
                            let val (env,fol) = liftFol env fol
                            in  (env, l @ [fol])
                            end ) (env, []) folList
        in  (env, Disj folList)
        end

    | Exists (idTyList, fol) =>
        let 
           val {idenv=idenv,tyenv=tyenv,...} = env

           fun f ((id,ty),(env,l)) = 
               let 
                  val (env,liftedty) = liftTy env ty
                  val idenv = #idenv env
                  val varid = VarId(id,liftedty)
                  val idenv = SEnv.insert (idenv,id,varid)
                  val env = updidenv env idenv
               in
                  (env, l @ [(id,liftedty)])
               end

           val (env,liftedIdTyList) = foldl f (env,[]) idTyList
           val (env,fol) = liftFol env fol

           val env = updidenv env idenv
           val env = updtyenv env tyenv
        in
           (env, Exists (liftedIdTyList,fol))
        end

    | Forall (idTyList, fol) =>
        let 
           val {idenv=idenv,tyenv=tyenv,...} = env

           fun f ((id,ty),(env,l)) =
               let
                   val (env,liftedty) = liftTy env ty
                   val idenv = #idenv env
                   val varid = VarId (id,liftedty)
                   val idenv = SEnv.insert (idenv,id,varid)
                   val env = updidenv env idenv
               in
                   (env, l @ [(id,liftedty)])
               end

           val (env,liftedIdTyList) = foldl f (env, []) idTyList
           val (env,fol) = liftFol env fol

           val env = updidenv env idenv
           val env = updtyenv env tyenv
        in
           (env, Forall (liftedIdTyList, fol))
        end

and liftExp env (exp,ty) = 

    case exp of

      VarExp (path,ty) => 
        let val (env,liftedty) = liftTy env ty
        in  (env, VarExp (path,liftedty))
        end

    | ConExp (path as [id],expTyList) =>
        let 
            val id = liftConid id

            fun f ((exp,ty), (env,l)) = 
                   let val (env,liftedty) = liftTy env ty
                       val (env,liftedexp) = liftExp env (exp,liftedty)
                   in  (env, l @ [(liftedexp,liftedty)])
                   end

            val (env,expTyList) = foldl f (env,[]) expTyList
            
            val arity = length expTyList

            val (env,expTyList) =
                 case arity of
                   0 => (env, expTyList)
                 | 1 => (env, expTyList)
                 | n => let val env = addTupleTy env arity
                            val (tyconid,tyvarList,idTyOptionList,origin) = 
                                   case IEnv.find (#tuptyenv env, arity) of
                                     SOME x => x
                                   | NONE => raise (ErrLifting "Impossible to happen")
                            val (expList,tyList) = ListPair.unzip expTyList
                            val ty = ConTy ([tyconid],[TupleTy tyList])
                            val conid = 
                                   case idTyOptionList of
                                     [ _, (conid,_) ] => conid
                                   | _ => raise (ErrLifting "Unexpected tuple type")

                            val exp = ConExp ([conid], [(TupleExp expList, TupleTy tyList)])
                        in  
                            (env, [ (exp, ty) ])
                        end
            
        in  
            (env, ConExp ([id], expTyList))
        end

    | ConExp (path,expTyList) =>
        raise (ErrLifting ("Not supported: ConExp with " ^ I.pr path))

    | IntExp i => 
        let 
            val conid = getIndirCon (#3 (valOf (SEnv.find (#dtenv env, I.intTycon))))
        in  (env, ConExp ([conid], [(IntExp i,Type.intTy)]))
        end

    | WordExp i => 
        let val conid = getIndirCon (#3 (valOf (SEnv.find (#dtenv env, I.wordTycon))))
        in  (env, ConExp ([conid], [(WordExp i,Type.wordTy)]))
        end

    | RealExp s => 
        let val conid = getIndirCon (#3 (valOf (SEnv.find (#dtenv env, I.realTycon))))
        in  (env, ConExp ([conid], [(RealExp s,Type.realTy)]))
        end

    | StringExp s => 
        let val conid = getIndirCon (#3 (valOf (SEnv.find (#dtenv env, I.stringTycon ))))
        in  (env, ConExp ([conid], [(StringExp s,Type.stringTy)]))
        end

    | CharExp s => 
        let val conid = getIndirCon (#3 (valOf (SEnv.find (#dtenv env, I.charTycon ))))
        in  (env, ConExp ([conid], [(CharExp s,Type.charTy)]))
        end

    | RecordExp idExpList => 
      let
          val (labels,expList) = ListPair.unzip idExpList

          val (tyconid,tyList) =
                 case ty of
                   ConTy ([tyconid], tyList) => (tyconid, tyList)
                 | _ => raise (ErrLifting ("Not record type: " ^ Type.pr ty))

          val conid = 
                 case SLEnv.find (#rectyenv env, labels) of
                   SOME (tyconid',_,idtys,_)
                        => if tyconid=tyconid' then getIndirCon idtys else 
                           raise (ErrLifting ("Mismatch : "^tyconid^" <> "^tyconid'))
                 | NONE => raise (ErrLifting ("Record type not found for labels: " 
                             ^ Util.intersperse "," labels ^ Type.pr ty))

          val _ = if length expList=length tyList then ()
                  else raise (ErrLifting ("Arity mismatch: " ^ Type.pr ty))

          fun f (((id,exp),ty),(env,l)) = 
                let val (env,exp) = liftExp env (exp,ty)
                in  (env, l @ [(id,exp)])
                end

          val (env,idExpList) = foldl f (env,[]) (ListPair.zip (idExpList,tyList))
          val idTyList = ListPair.zip (labels,tyList)
      in
         (env, ConExp ([conid], [(RecordExp idExpList, RecordTy idTyList)]))
      end

    | TupleExp expList =>
        let
           val (tyconid, tyList) = 
                  case ty of
                    ConTy ([tyconid], tyList) => (tyconid, tyList)
                  | _ => raise (ErrLifting ("Not tuple type: " ^ Type.pr ty))

          val conid = 
                 case IEnv.find (#tuptyenv env, length expList) of
                   SOME (tyconid',_,idtys,_)
                        => if tyconid=tyconid' then getIndirCon idtys else
                           raise (ErrLifting ("Mismatch : "^tyconid^" <> "^tyconid'))
                 | NONE => raise (ErrLifting ("Not found: " ^ tyconid))

           val _ = if length expList=length tyList then ()
                   else raise (ErrLifting ("Arity mismatch: " ^ Type.pr ty))

           val expTyList = ListPair.zip (expList, tyList)

           fun f (expTy,(env,l)) = 
                 let val (env,exp) = liftExp env expTy
                 in (env, l @ [exp])
                 end

           val (env,expList) = foldl f (env,[]) expTyList
        in
           (env,ConExp ([conid], [(TupleExp expList, TupleTy tyList)]))
        end

    | SelectorExp label => raise (ErrLifting ("Unexpected expr: SelectorExp " ^ label))

    | ConstraintExp (exp,ty) => 
        let
           val (env,liftedty) = liftTy env ty
           val (env,liftedexp) = liftExp env (exp,liftedty) 
        in
           (env, ConstraintExp (liftedexp,liftedty))
        end

    | VectorExp expList => raise (ErrLifting ("VectorExp not supported yet"))

(*         let *)
(*            val elemty = case ty of *)
(*                  ConTy (path as [vecid],[elemty])  *)
(*                    => if I.vecTycon=vecid then elemty *)
(*                       else raise (ErrLifting ("Not vector type: " ^ Type.pr ty)) *)
(*                | ConTy (path,tyList)  *)
(*                    => raise (ErrLifting ("Not supported: " ^ I.pr path ^  *)
(*                                          " of type " ^ Type.pr ty)) *)
(*                | _ => raise (ErrLifting ("Not tuple type: " ^ Type.pr ty)) *)

(*            fun f (exp,(env,l)) =  *)
(*                   let val (env,exp) = liftExp env (exp,elemty) *)
(*                   in  (env, l @ [exp]) *)
(*                   end *)

(*            val (env,expList) = foldl f (env,[]) expList *)
(*         in *)
(*            (env, VectorExp expList) *)
(*         end *)

    | FlatAppExp (id,resty,expTyList) => 
        let
           fun f ((exp,ty),(env,l)) = 
                 let val (env,liftedty)  = liftTy env ty
                     val (env,liftedexp) = liftExp env (exp,liftedty)
                 in (env, l @ [(liftedexp,liftedty)])
                 end

           val (env,liftedresty) = liftTy env resty
           val (env,expTyList) = foldl f (env,[]) expTyList
        in
           (env,FlatAppExp (id,liftedresty,expTyList))
        end


end

end
