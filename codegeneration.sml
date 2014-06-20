
structure CodeGeneration =

struct

local

open Env Type TypedAst

in

structure A = Ast
structure I = Identifier
structure S = String
structure T = Type
structure U = Util

datatype world = FunWorld of (ty * inout option) list | RelWorld of I.id

type CodeGenerationEnv = 
     {tyenv:(tyinfo * origin option) SEnv.map, idenv:(idinfo * world option) SEnv.map}

exception ErrCodeGeneration of string

(* Identifiers *)

val prefix = ""

val smlogId = "SMLOG"

val stepId   = "STEP"
val conjId   = "CONJ"
val disjId   = "DISJ"
val existsId = "EXISTS"
val negationId  = "NOT"

val trueId   = "TRUE"
val falseId  = "FALSE"

val doUnifyId     = "doUnify"
val doAppUnderSubstId      = "doAppUnderSubst"
val extendTcId    = "extendTc"
val freshStringId = "freshString"
val mapStreamId   = "mapS"
val redoId = "Redo"
val undoId = "Undo"

val projExnId = "ProjectionError"

(* occur *)
fun occurId id = prefix ^ "OCCUR" ^ id
fun occurTupleId n = prefix ^ "TUPLEOCCUR" ^ Int.toString n
fun occurRecordId labels = prefix ^ "RECORDOCCUR" ^ S.translate I.tr (I.pr labels)

(* unify *)
fun unifyId id = prefix ^ "UNIFY" ^ id
fun unifyTupleId n = prefix ^ "TUPLEUNIFY" ^ Int.toString n
fun unifyRecordId labels = prefix ^ "RECORDUNIFY" ^ S.translate I.tr (I.pr labels)

(* projection/embedding *)
fun projId id  = prefix ^ "PROJ" ^ id
fun embedId id = prefix ^ "EMBED" ^ id

(* print lifted values *)
val doPrintId = "doPrint"
val outputId  = "print"
fun printId id  = prefix ^ "PRINT" ^ id

val smlogIntersperseExp = A.VarExp [smlogId, "intersperse"]
val stringConcatExp = A.VarExp ["String", "concat"]
val stringCaretExp = A.VarExp ["String", "^"]
val boolToStringExp = A.VarExp ["Bool", "toString"]
val intToStringExp = A.VarExp ["Int", "toString"]
val wordToStringExp = A.VarExp ["Word", "toString"]
val realToStringExp = A.VarExp ["Real", "toString"]
val charToStringExp = A.VarExp ["Char", "toString"]
val doPrintExpr = A.VarExp [ smlogId, doPrintId ]

(* formulae *)
val stepExpr = A.VarExp [ smlogId, stepId ]
val conjExpr = A.VarExp [ smlogId, conjId ]
val disjExpr = A.VarExp [ smlogId, disjId ]
val existsExpr = A.VarExp [ smlogId, existsId ]
val negationExpr = A.VarExp [ smlogId, negationId ]

val trueExpr = A.VarExp [ smlogId, trueId ]
val falseExpr = A.VarExp [ smlogId, falseId ]

(* etc *)
val doUnifyExpr     = A.VarExp [ smlogId, doUnifyId ]
val doAppUnderSubstExpr     = A.VarExp [ smlogId, doAppUnderSubstId ]
val outputExpr      = A.VarExp [ smlogId, outputId ]
val extendTcExpr    = A.VarExp [ smlogId, extendTcId ]
val freshStringExpr = A.VarExp [ smlogId, freshStringId ]
val mapStreamExpr   = A.VarExp [ smlogId, mapStreamId ]
val redoExpr        = A.VarExp [ smlogId, redoId ]
val undoExpr        = A.VarExp [ smlogId, undoId ]
val projExnExpr     = A.VarExp [ smlogId, projExnId ]

val listMapExpr     = A.VarExp [ "List", "map" ]

(* Utilities *)

fun idExp id = A.VarExp [id]
fun idTyExp (id,ty) = A.ConstraintExp {expr=idExp id,constraint=ty}

fun idPat id = A.VarPat [id]
fun idTyPat (id,ty) = A.ConstraintPat {pattern=idPat id,constraint=ty}

fun fnExp idTy exp = A.FnExp [ A.Rule {pat=idTyPat idTy,exp=exp} ]

(* occur function *)

fun mkOccurExp dict argvExp (VarTy v) e = 
    let val f = case SEnv.find (dict, v) of
            SOME f => f
          | NONE => raise (ErrCodeGeneration ("Not found: " ^ Type.pr (VarTy v)))
    in
        A.FlatAppExp [f, argvExp, e]
    end

  | mkOccurExp dict argvExp (ConTy (path as [id], tyList)) e = 
    let 
        val fnExp = A.VarExp [ occurId id ]

        val argvid = I.arg "v"
        val argvp = A.VarPat [argvid]
        val argve = A.VarExp [argvid]

        val argtid = I.arg "t"
        val argtp = A.VarPat [argtid]
        val argte = A.VarExp [argtid]

        fun mkTyarg ty = 
            A.FnExp [
              A.Rule {pat=argvp,
                      exp=A.FnExp [
                            A.Rule {pat=argtp,
                                    exp=mkOccurExp dict argve ty argte}] }]

        val tyargs = map mkTyarg tyList
        val fnArgListExp = case tyargs of
                             []      => fnExp
                           | [tyarg] => A.FlatAppExp [ fnExp, tyarg ]
                           | _       => A.FlatAppExp [ fnExp, A.TupleExp tyargs ]
    in
        A.FlatAppExp [fnArgListExp, argvExp, e]
    end

  | mkOccurExp dict argvExp (RecordTy idTyList) e =
    let 
        val argid = I.arg "v"
        val argvp = A.VarPat [argid]
        val argve = A.VarExp [argid]

        val (labels,tyList) = ListPair.unzip idTyList
        val pat = A.RecordPat {def=map (fn s => (s,A.VarPat [s])) labels,
                               flexibility=false}
        val exp = foldl (fn (e,b) => 
                    A.FlatAppExp 
                      [ A.FnExp [ 
                          A.Rule {pat=A.VarPat [I.trueCon],
                                  exp=A.VarExp [I.trueCon]},
                          A.Rule {pat=A.VarPat [I.falseCon],
                                  exp=e} ],
                        b
                      ] )
                    (A.VarExp [I.falseCon])
                    (map (fn (s,ty) => mkOccurExp dict argve ty (A.VarExp [s])) idTyList)
    in
        A.FlatAppExp [ A.FnExp [ A.Rule {
                           pat=argvp, 
                           exp=A.FnExp [A.Rule {pat=pat, exp=exp}] }], 
                       argvExp, e ]
    end

  | mkOccurExp dict argvExp (TupleTy tyList) e = 
    let 
        val argid = I.arg "v"
        val argvp = A.VarPat [argid]
        val argve = A.VarExp [argid]

        val n = length tyList
        val args = List.tabulate (n, fn n => I.arg (Int.toString n))
        val pat = A.TuplePat (map (fn s => A.VarPat [s]) args)
        val exp = foldl (fn (e,b) => 
                    A.FlatAppExp 
                      [ A.FnExp [ 
                          A.Rule {pat=A.VarPat [I.trueCon],
                                  exp=A.VarExp [I.trueCon]},
                          A.Rule {pat=A.VarPat [I.falseCon],
                                  exp=e} ],
                        b
                      ] )
                   (A.VarExp [I.falseCon])
                   (map (fn (ty,s) => mkOccurExp dict argve ty (A.VarExp [s]))
                                                (ListPair.zip (tyList,args)))
    in
        A.FlatAppExp [ A.FnExp [A.Rule {
                         pat=argvp,
                         exp=A.FnExp [A.Rule {pat=pat, exp=exp}] }],
                       argvExp, e ]
    end

  | mkOccurExp dict argvExp ty _ = raise (ErrCodeGeneration ("Unexpected type: " ^ Type.pr ty))

fun mkOccur env tyconid tyvarList conidTyOptionList location = 
    let
        val fnId = occurId tyconid
        val fnPat = A.VarPat [fnId]
        val fnExp = A.VarExp [fnId]
        val argList = map (S.translate I.tr) tyvarList
        val argListPat = map (A.VarPat o Util.list) argList
        val argListExp = map (A.VarExp o Util.list) argList

        val fnArgPatList = case argListPat of 
                             [] => [ fnPat ]
                           | _  => [ fnPat, A.TuplePat argListPat ]

        fun mkDict ((tyvar,arg),dict) = SEnv.insert (dict,tyvar,A.VarExp [arg])
        val dict = foldl mkDict SEnv.empty (ListPair.zip (tyvarList, argList))

        fun mkLogicVarClause (conid,_) = 
            let 
                val argv = I.arg "0"
                val argw = I.arg "1"
                val argt = I.arg "2"

                val argvExp = A.VarExp [argv]
                val argwExp = A.VarExp [argw]
                val argtExp = A.VarExp [argt]

                val eqExp = A.VarExp I.pathEq

                val argvPat = A.VarPat [argv]
                val argwPat = A.VarPat [argw]
                val argtPat = A.VarPat [argt]

                val pat1 = A.FlatAppPat 
                           [ A.VarPat [conid],
                             A.TuplePat
                              [ argwPat,
                                A.FlatAppPat 
                                 [ A.VarPat [I.refCon],
                                   A.VarPat I.pathOptionNONE
                                 ]
                              ]
                           ]

                val pat2 = A.FlatAppPat 
                           [ A.VarPat [conid], 
                             A.TuplePat
                              [ A.WildPat,
                                A.FlatAppPat 
                                 [ A.VarPat [I.refCon],
                                   A.FlatAppPat 
                                    [ A.VarPat I.pathOptionSOME,
                                      argtPat
                                    ]
                                 ]
                              ]
                           ]

                val fnExp = case argList of
                              [] => fnExp
                            | _  => A.FlatAppExp [fnExp, A.TupleExp argListExp]
            in
               [ A.Clause
                 { pats = fnArgPatList @ [argvPat, pat1], 
                   resultty=NONE,
                   exp=A.FlatAppExp [eqExp, A.TupleExp [argvExp,argwExp]] },
                 A.Clause 
                 { pats = fnArgPatList @ [argvPat, pat2],
                   resultty=NONE,
                   exp=A.FlatAppExp [fnExp, argvExp, argtExp] }
               ]
            end

        fun mkBoundaryClause (conid,_) = 
            let 
                val argv = I.arg "0"
                val argvPat = A.VarPat [argv]

                val pat = A.FlatAppPat [A.VarPat [conid], A.WildPat]
            in
               [
                 A.Clause
                 { pats = fnArgPatList @ [argvPat, pat], 
                   resultty=NONE,
                   exp=A.VarExp [I.falseCon] }
               ]
            end

        fun mkClause (conid,SOME ty) = 
            let 
                val argv = I.arg "0"
                val argvPat = A.VarPat [argv]
                val argvExp = A.VarExp [argv]

                val argw = I.arg "1"
                val argwPat = A.VarPat [argw]
                val argwExp = A.VarExp [argw]

                val pat = A.FlatAppPat [A.VarPat [conid], argwPat]
            in
                A.Clause
                { pats = fnArgPatList @ [argvPat, pat],
                  resultty = NONE,
                  exp = mkOccurExp dict argvExp ty argwExp }
            end

          | mkClause  (conid,NONE) = 
            let 
                val argv = I.arg "0"
                val argvPat = A.VarPat [argv]
                val pat = A.VarPat [conid]
            in
                A.Clause
                { pats = fnArgPatList @ [argvPat, pat], 
                  resultty = NONE,
                  exp = A.VarExp [I.falseCon] }
            end


        val (conidTyOption, conidTyOptionList) = case conidTyOptionList of
                  (x :: xs) => (x, xs)
                | [] => raise (ErrCodeGeneration ("Not lifted datatype: " ^ tyconid))

        val firstclause = mkLogicVarClause conidTyOption 

        val restclauses = 
            case (location,conidTyOptionList) of
              ((Boundary _ ,_),[conidTyOption]) => mkBoundaryClause conidTyOption
            | ((Boundary _ ,_), _) => raise (ErrCodeGeneration ("Unexpected number of constructors: "
                                          ^ Int.toString (length conidTyOptionList)))
            | ((Center,_),_) => map mkClause conidTyOptionList

    in
        A.Fb (firstclause @ restclauses)
    end

(* unify function *)

val sel1Exp = A.FnExp [ A.Rule {
                 pat=A.TuplePat 
                         [ 
                           A.VarPat [I.arg "1"], 
                           A.VarPat [I.arg "2"] 
                         ], 
                 exp=A.VarExp [I.arg "1"]} ]

val sel2Exp = A.FnExp [ A.Rule {
                 pat=A.TuplePat 
                         [ 
                           A.VarPat [I.arg "1"], 
                           A.VarPat [I.arg "2"] 
                         ], 
                 exp=A.VarExp [I.arg "2"]} ]

fun mkUnifyExp dict argtcExp (VarTy v) t1 t2 = 
    let val f = case SEnv.find (dict, v) of
                  SOME f => f
                | NONE => raise (ErrCodeGeneration ("Not found: " ^ Type.pr (VarTy v)))
        val fnExp = A.FlatAppExp [ sel1Exp, f ]
    in
        A.FlatAppExp [ fnExp, A.TupleExp [t1,t2], argtcExp ]
    end

  | mkUnifyExp dict argtcExp (ConTy (path as [id], tyList)) t1 t2 = 
    let
        val fnExp = A.VarExp [ unifyId id ]

        val arg1id = I.arg "t1"
        val arg2id = I.arg "t2"
        val arg3id = I.arg "tc"
        val argvid = I.arg "v"
        val argtid = I.arg "t"

        val arg1Pat = A.VarPat [arg1id]
        val arg2Pat = A.VarPat [arg2id]
        val arg3Pat = A.VarPat [arg3id]
        val argvPat = A.VarPat [argvid]
        val argtPat = A.VarPat [argtid]

        val arg1Exp = A.VarExp [arg1id]
        val arg2Exp = A.VarExp [arg2id]
        val arg3Exp = A.VarExp [arg3id]
        val argvExp = A.VarExp [argvid]
        val argtExp = A.VarExp [argtid]

        val dict' = SEnv.map (fn e => A.FlatAppExp [sel2Exp,e]) dict 

        fun mkTyarg ty = A.TupleExp
            [
              A.FnExp [
                A.Rule {
                 pat=A.TuplePat [ arg1Pat, arg2Pat ],
                 exp=A.FnExp [
                       A.Rule {
                        pat=arg3Pat,
                        exp=mkUnifyExp dict arg3Exp ty arg1Exp arg2Exp }] } ],
              A.FnExp [ 
                A.Rule {
                 pat=argvPat,
                 exp=A.FnExp [
                       A.Rule {
                        pat=argtPat,
                        exp=mkOccurExp dict' argvExp ty argtExp }] } ]
            ]

        val tyargs = map mkTyarg tyList

        val fnArgListExp = case tyargs of
                             [] => fnExp
                           | [tyarg] => A.FlatAppExp [ fnExp, tyarg ]
                           | _  => A.FlatAppExp [ fnExp, A.TupleExp tyargs ]
    in
        A.FlatAppExp [fnArgListExp, A.TupleExp [t1,t2], argtcExp]
    end 

  | mkUnifyExp dict argtcExp (RecordTy idTyList) t1 t2 =
    let
        val arg3id = I.arg "tc"
        val arg3Pat = A.VarPat [arg3id]
        val arg3Exp = A.VarExp [arg3id]

        val (labels,tyList) = ListPair.unzip idTyList
        val labels1 = map (fn s => s ^ "1") labels
        val labels2 = map (fn s => s ^ "2") labels

        val llabels1 = ListPair.zip (labels, labels1)
        val llabels2 = ListPair.zip (labels, labels2)

        val idIdTyList = Util.zip3 (labels1, labels2,tyList)

        val recPat1 = A.RecordPat { def=map (fn (l,s) => (l,A.VarPat [s])) llabels1,
                                    flexibility=false }
        val recPat2 = A.RecordPat { def=map (fn (l,s) => (l,A.VarPat [s])) llabels2,
                                    flexibility=false }
        val pat = A.TuplePat [recPat1, recPat2]

        val exp = foldl 
                    (fn (f,btc) => 
                     A.FlatAppExp 
                      [ A.FnExp [ 
                            A.Rule {
                               pat=A.TuplePat [A.VarPat [I.trueCon],arg3Pat],
                               exp=f arg3Exp },
                            A.Rule {
                               pat=A.TuplePat [A.VarPat [I.falseCon],arg3Pat],
                               exp=A.TupleExp [A.VarExp [I.falseCon],arg3Exp]} ],
                        btc ] )
                     (A.TupleExp [A.VarExp [I.trueCon],argtcExp])
                     (map (fn (s1,s2,ty) => 
                           let val t1 = A.VarExp [s1]
                               val t2 = A.VarExp [s2]
                               val f = fn tc => mkUnifyExp dict tc ty t1 t2
                           in
                               f
                           end ) idIdTyList)
    in
        A.FlatAppExp [ 
           A.FnExp [ A.Rule { pat=pat, exp=exp} ], 
           A.TupleExp [t1, t2] ]
    end

  | mkUnifyExp dict argtcExp (TupleTy tyList) t1 t2 = 
    let
        val arg3id = I.arg "tc"
        val arg3Pat = A.VarPat [arg3id]
        val arg3Exp = A.VarExp [arg3id]

        val n = length tyList
        val args1 = List.tabulate (n, fn n => I.arg ("1" ^ Int.toString n))
        val args2 = List.tabulate (n, fn n => I.arg ("2" ^ Int.toString n))

        val idIdTyList = Util.zip3 (args1, args2, tyList)

        val pat1 = A.TuplePat (map (fn s => A.VarPat [s]) args1) 
        val pat2 = A.TuplePat (map (fn s => A.VarPat [s]) args2)
        val pat = A.TuplePat [pat1, pat2]

        val exp = foldl 
                    (fn (f,btc) => 
                     A.FlatAppExp 
                      [ A.FnExp [ 
                            A.Rule {
                               pat=A.TuplePat [A.VarPat [I.trueCon],arg3Pat],
                               exp=f arg3Exp },
                            A.Rule {
                               pat=A.TuplePat [A.VarPat [I.falseCon],arg3Pat],
                               exp=A.TupleExp [A.VarExp [I.falseCon],arg3Exp]} ],
                        btc ] )
                     (A.TupleExp [A.VarExp [I.trueCon],argtcExp])
                     (map (fn (s1,s2,ty) => 
                           let val t1 = A.VarExp [s1]
                               val t2 = A.VarExp [s2]
                               val f = fn tc => mkUnifyExp dict tc ty t1 t2
                           in
                               f
                           end ) idIdTyList)
    in
        A.FlatAppExp [ 
           A.FnExp [ A.Rule { pat=pat, exp=exp} ], 
           A.TupleExp [t1, t2] ]
    end

  | mkUnifyExp dict argtcExp ty _ _ = 
        raise (ErrCodeGeneration ("Unexpected type: " ^ Type.pr ty))

fun mkDoUnify dict tc ty e1 e2 = 
    let
        val arg0 = I.arg "0"
        val arg1 = I.arg "1"

        val argpat0 = A.VarPat [arg0]
        val argpat1 = A.VarPat [arg1]

        val argexp0 = A.VarExp [arg0]
        val argexp1 = A.VarExp [arg1]

        val argtc = I.arg "tc"
        val tcpat = A.VarPat [argtc]
        val tcexp = A.VarExp [argtc]
    in
        A.FlatAppExp [
           doUnifyExpr, 
           A.FnExp [ A.Rule 
               {pat = A.TuplePat [argpat0, argpat1],
                exp = A.FnExp [ A.Rule { 
                      pat = tcpat,
                      exp = mkUnifyExp dict tcexp ty argexp0 argexp1 } ] } ],
           e1,
           e2,
           tc ]
    end

fun mkUnify env tyconid tyvarList conidTyOptionList locsrc = 
    let
        val fnId = unifyId tyconid
        val fnPat = A.VarPat [fnId]
        val fnExp = A.VarExp [fnId]
        val argList = map (S.translate I.tr) tyvarList
        val argListPat = map (A.VarPat o Util.list) argList
        val argListExp = map (A.VarExp o Util.list) argList

        val fnArgPatList = case argListPat of 
                             []    => [ fnPat ]
                           | [arg] => [ fnPat, arg ]
                           | _     => [ fnPat, A.TuplePat argListPat ]

        val occurFnId = occurId tyconid
        val occurFnExp = A.VarExp [occurFnId]
        val occurFnExp = case argListExp of 
                             []    => occurFnExp
                           | [arg] => A.FlatAppExp 
                                       [ occurFnExp, A.FlatAppExp [sel2Exp, arg] ]
                           | _     => A.FlatAppExp
                                       [ occurFnExp, 
                                         A.TupleExp 
                                          (map (fn e => A.FlatAppExp [sel2Exp, e]) 
                                               argListExp) ]

        fun mkDict ((tyvar,arg),dict) = SEnv.insert (dict,tyvar,A.VarExp [arg])
        val dict = foldl mkDict
                      SEnv.empty (ListPair.zip (tyvarList, argList))


        fun mkLogicVarClause (conid,_) = 
            let 
                val argv = I.arg "0"
                val argt1 = I.arg "t1"
                val argt2 = I.arg "t2"
                val argtc = I.arg "tc"
                val argr = I.arg "r"

                val argvExp = A.VarExp [argv]
                val argt1Exp = A.VarExp [argt1]
                val argt2Exp = A.VarExp [argt2]
                val argtcExp = A.VarExp [argtc]
                val argrExp = A.VarExp [argr]

                val argvPat = A.VarPat [argv]
                val argt1Pat = A.VarPat [argt1]
                val argt2Pat = A.VarPat [argt2]
                val argtcPat = A.VarPat [argtc]
                val argrPat = A.VarPat [argr]

                val eqExp = A.VarExp I.pathEq

                val pat1 = A.FlatAppPat 
                           [ A.VarPat [conid], 
                             A.TuplePat
                              [ argt1Pat,
                                A.FlatAppPat 
                                 [ A.VarPat [I.refCon],
                                   A.VarPat I.pathOptionNONE
                                 ]
                              ]
                           ]

                val pat2 = A.FlatAppPat 
                           [ A.VarPat [conid], 
                             A.TuplePat
                              [ A.WildPat,
                                A.FlatAppPat 
                                 [ A.VarPat [I.refCon],
                                   A.FlatAppPat 
                                    [ A.VarPat I.pathOptionSOME,
                                      argt2Pat
                                    ]
                                 ]
                              ]
                           ]

                val fnExp = case argList of
                              [] => fnExp
                            | _  => A.FlatAppExp [fnExp, A.TupleExp argListExp]
            in

               (* unify (t1, VAR (_, ref (SOME t2))) tc = unify (t1,t2) tc *)

               [ A.Clause
                 { pats = fnArgPatList 
                        @ [ 
                            A.TuplePat
                             [ argt1Pat, 
                               A.FlatAppPat 
                                [ A.VarPat [conid], 
                                   A.TuplePat
                                    [ A.WildPat,
                                       A.FlatAppPat 
                                       [ A.VarPat [I.refCon],
                                          A.FlatAppPat 
                                          [ A.VarPat I.pathOptionSOME,
                                            argt2Pat
                                          ]
                                       ]
                                    ]
                                ]
                            ],
                            argtcPat
                          ], 
                   resultty=NONE,
                   exp=A.FlatAppExp [fnExp, A.TupleExp [argt1Exp,argt2Exp], argtcExp] },

               (* unify (t1, VAR (v, r as ref NONE)) tc =        *)
               (*   if occurTY v t1                              *)
               (*   then (false, tc)                             *)
               (*   else (true, extendTc r t1)                   *)

                 A.Clause 
                 { pats = fnArgPatList 
                        @ [ A.TuplePat
                            [argt1Pat, 
                            A.FlatAppPat 
                            [ A.VarPat [conid], 
                              A.TuplePat
                               [ argvPat,
                                 A.LayeredPat
                                 { varPat = argrPat,
                                   expPat = A.FlatAppPat 
                                             [ A.VarPat [I.refCon],
                                               A.VarPat I.pathOptionNONE
                                             ]
                                 }
                               ]
                            ]],
                            argtcPat
                          ], 
                   resultty=NONE,
                   exp=A.FlatAppExp
                        [ A.FnExp
                           [ A.Rule 
                               {pat=A.VarPat [I.trueCon],
                                exp=A.TupleExp 
                                        [ A.VarExp [I.falseCon],
                                          argtcExp ] },
                             A.Rule 
                               {pat=A.VarPat [I.falseCon], 
                                exp=A.TupleExp
                                        [ A.VarExp [I.trueCon],
                                          A.FlatAppExp
                                           [ extendTcExpr,
                                             argtcExp,
                                             argrExp,
                                             argt1Exp 
                                           ] ] }
                           ],
                          A.FlatAppExp [ occurFnExp, argvExp, argt1Exp ]
                        ]
                   },

               (* unify (VAR (_, ref (SOME t1)), t2) tc = unify (t1,t2) tc *)

                 A.Clause
                 { pats = fnArgPatList 
                        @ [A.TuplePat
                           [A.FlatAppPat 
                            [ A.VarPat [conid], 
                              A.TuplePat
                               [ A.WildPat,
                                 A.FlatAppPat 
                                  [ A.VarPat [I.refCon],
                                    A.FlatAppPat 
                                     [ A.VarPat I.pathOptionSOME,
                                       argt1Pat
                                     ]
                                  ]
                               ]
                            ],
                            argt2Pat],
                            argtcPat
                          ],
                   resultty = NONE,
                   exp = A.FlatAppExp [fnExp, A.TupleExp [argt1Exp, argt2Exp], argtcExp] },

               (* unify (t1 as VAR (_, ref NONE), t2) tc = unify (t2,t1) tc *)

                 A.Clause
                 { pats = fnArgPatList 
                        @ [ A.TuplePat
                            [A.LayeredPat
                             {varPat=argt1Pat,
                              expPat=A.FlatAppPat 
                                      [ A.VarPat [conid],
                                        A.TuplePat
                                         [ A.WildPat,
                                           A.FlatAppPat 
                                            [ A.VarPat [I.refCon],
                                              A.VarPat I.pathOptionNONE
                                            ]
                                         ]
                                      ]},
                            argt2Pat], 
                            argtcPat
                          ],
                   resultty = NONE,
                   exp = A.FlatAppExp [fnExp, A.TupleExp [argt2Exp, argt1Exp], argtcExp] }
               ]
            end

        fun mkBoundaryClause (conid,_) equality = 
            let 
                val argt1 = I.arg "t1"
                val argt2 = I.arg "t2"
                val argtc = I.arg "tc"

                val argt1Pat = A.VarPat [argt1]
                val argt2Pat = A.VarPat [argt2]
                val argtcPat = A.VarPat [argtc]

                val argt1Exp = A.VarExp [argt1]
                val argt2Exp = A.VarExp [argt2]
                val argtcExp = A.VarExp [argtc]

                val eqExp = A.VarExp I.pathEq

                val pat1 = A.FlatAppPat [A.VarPat [conid], argt1Pat]
                val pat2 = A.FlatAppPat [A.VarPat [conid], argt2Pat]
                val pat  = A.TuplePat [pat1, pat2]

                val clauseExp = if equality=EQ
                                then A.FlatAppExp [eqExp, A.TupleExp [argt1Exp,argt2Exp]]
                                else A.VarExp [I.falseCon]
            in
               [
                 A.Clause
                 { pats = fnArgPatList @ [pat, argtcPat],
                   resultty=NONE,
                   exp=A.TupleExp [ clauseExp, argtcExp ]
                 }
               ]
            end

        fun mkClause (conid,SOME ty) = 
            let 
                val argtc = I.arg "tc"
                val argtcPat = A.VarPat [argtc]
                val argtcExp = A.VarExp [argtc]

                val argt1 = I.arg "t1"
                val argt1Pat = A.VarPat [argt1]
                val argt1Exp = A.VarExp [argt1]

                val argt2 = I.arg "t2"
                val argt2Pat = A.VarPat [argt2]
                val argt2Exp = A.VarExp [argt2]

                val pat1 = A.FlatAppPat [A.VarPat [conid], argt1Pat]
                val pat2 = A.FlatAppPat [A.VarPat [conid], argt2Pat]
                val pat  = A.TuplePat [pat1, pat2]
            in
                A.Clause
                { pats = fnArgPatList @ [pat, argtcPat],
                  resultty = NONE,
                  exp = mkUnifyExp dict argtcExp ty argt1Exp argt2Exp }
            end

          | mkClause  (conid,NONE) = 
            let 
                val argtc = I.arg "tc"
                val argtcPat = A.VarPat [argtc]
                val argtcExp = A.VarExp [argtc]

                val pat = A.VarPat [conid]
                val pat = A.TuplePat [ pat, pat ]
            in
                A.Clause
                { pats = fnArgPatList @ [pat, argtcPat], 
                  resultty = NONE,
                  exp = A.TupleExp [ A.VarExp [I.trueCon],argtcExp ] }
            end

        fun mkDefaultClause  () = 
            let 
                val argtc = I.arg "tc"
                val argtcPat = A.VarPat [argtc]
                val argtcExp = A.VarExp [argtc]

                val pat = A.TuplePat [ A.WildPat, A.WildPat ]
            in
                A.Clause
                { pats = fnArgPatList @ [pat, argtcPat], 
                  resultty = NONE,
                  exp = A.TupleExp [ A.VarExp [I.falseCon],argtcExp ] }
            end


        val (conidTyOption, conidTyOptionList) = case conidTyOptionList of
                  (x :: xs) => (x, xs)
                | [] => raise (ErrCodeGeneration ("Not lifted datatype: " ^ tyconid))

        val firstclause = mkLogicVarClause conidTyOption 

        val restclauses = 
            case (locsrc,conidTyOptionList) of
              ((Boundary equality,_), [conidTyOption]) => mkBoundaryClause conidTyOption equality
            | ((Boundary _ ,_), _) => raise (ErrCodeGeneration ("Unexpected number of constructors: "
                                          ^ Int.toString (length conidTyOptionList)))
            | ((Center,FromDatatype _),   _) => 
                  if length conidTyOptionList=1
                  then ( map mkClause conidTyOptionList )
                  else ( map mkClause conidTyOptionList @ [ mkDefaultClause () ] )
            | ((Center,_),   _) => map mkClause conidTyOptionList 

    in
        A.Fb (firstclause @ restclauses)
    end

(* projection/embedding functions *)

fun mkProjExp dict (VarTy v) e = 
    let val f = case SEnv.find (dict, v) of
            SOME f => f
          | NONE => raise (ErrCodeGeneration ("Not found: " ^ Type.pr (VarTy v)))
    in
        A.FlatAppExp [sel2Exp, f, e]
    end

  | mkProjExp dict (ty as ConTy (path as [id], tyList)) e =

    if id=I.liftTranslate I.arrowTycon andalso length tyList=2
    then

    let 
        val fnExp = A.VarExp [ projId id ]

        val (ty1,ty2) = case tyList of
                          [ty1,ty2] => (ty1,ty2)
                        | _ => raise (ErrCodeGeneration ("Impossible"))

        val argtid = I.arg "t"
        val argtp = A.VarPat [argtid]
        val argte = A.VarExp [argtid]
    in
        A.FlatAppExp 
           [ 
             fnExp,
             A.TupleExp 
             [
                A.TupleExp [A.FnExp [ A.Rule {pat = argtp,
                                              exp = mkEmbedExp dict ty1 argte} ],
                            A.FnExp [ A.Rule {pat = argtp,
                                              exp = mkProjExp dict ty1 argte} ]],
                A.TupleExp [A.FnExp [ A.Rule {pat = argtp,
                                              exp = mkEmbedExp dict ty2 argte} ], 
                            A.FnExp [ A.Rule {pat = argtp,
                                              exp = mkProjExp dict ty2 argte} ]]
             ],
             e 
           ]
    end

    else

    let 
        val fnExp = A.VarExp [ projId id ]

        val argtid = I.arg "t"
        val argtp = A.VarPat [argtid]
        val argte = A.VarExp [argtid]

        fun mkTyarg ty = A.TupleExp [
                           A.FnExp [ A.Rule {pat=argtp,
                                             exp= mkEmbedExp dict ty argte} ],
                           A.FnExp [ A.Rule {pat=argtp,
                                             exp= mkProjExp dict ty argte} ]
                         ]

        val tyargs = map mkTyarg tyList
        val fnArgListExp = case tyargs of
                             []      => fnExp
                           | [tyarg] => A.FlatAppExp [ fnExp, tyarg ]
                           | _       => A.FlatAppExp [ fnExp, A.TupleExp tyargs ]
    in
        A.FlatAppExp [fnArgListExp, e]
    end

  | mkProjExp dict (RecordTy idTyList) e =
    let 
        val pat = A.RecordPat { def=map (fn (s,_) => (s,A.VarPat [s])) idTyList,
                                flexibility=false }
        val exp = A.RecordExp (map (fn (s,ty) => (s,mkProjExp dict ty (A.VarExp [s]))) idTyList)
    in
        A.FlatAppExp [ A.FnExp [A.Rule {pat=pat, exp=exp}], e ]
    end

  | mkProjExp dict (TupleTy tyList) e = 
    let 
        val n = length tyList
        val args = List.tabulate (n, fn n => I.arg (Int.toString n))
        val pat = A.TuplePat (map (fn s => A.VarPat [s]) args)
        val idTyList = ListPair.zip (tyList,args)
        val exp = A.TupleExp (map (fn (ty,s) => mkProjExp dict ty (A.VarExp [s])) idTyList)
    in
        A.FlatAppExp [ A.FnExp [A.Rule {pat=pat, exp=exp}], e ]
    end

  | mkProjExp dict ty e = raise (ErrCodeGeneration ("Unexpected type: " ^ Type.pr ty))

and mkDoProj dict tc ty e = 
    let  
        val argt = I.arg "t"
        val argtpat = A.VarPat [argt]
        val argtexp = A.VarExp [argt]
    in
        A.FlatAppExp [
           doAppUnderSubstExpr,

           A.FnExp [
             A.Rule { pat=A.WildPat,
                      exp=A.FlatAppExp 
                          [
                           A.FnExp 
                           [ A.Rule
                             {pat = argtpat,
                              exp = mkProjExp dict ty argtexp} 
                           ],
                           e
                          ]
                    }
           ],

           tc ]
    end

and mkProj (env : CodeGenerationEnv) tyconid tyvarList conidTyOptionList locSrc = 
    let 
        val projFnId  = projId tyconid
        val projFnPat = A.VarPat [projFnId]
        val projFnExp = A.VarExp [projFnId]

        val argList = map (S.translate I.tr) tyvarList
        val argListPat = map (A.VarPat o Util.list) argList
        val argListExp = map (A.VarExp o Util.list) argList

        val projFnArgPatList = case argListPat of 
                             [] => [ projFnPat ]
                           | _  => [ projFnPat, A.TuplePat argListPat ]

        fun mkDict ((tyvar,arg),dict) = SEnv.insert (dict,tyvar,A.VarExp [arg])
        val dict = foldl mkDict SEnv.empty (ListPair.zip (tyvarList, argList))

        fun mkLogicVarClause (conid,_) = 
            let 
                val argt = I.arg "t"
                val argtExp = A.VarExp [argt]
                val argtPat = A.VarPat [argt]

                (* pat1: CONID (_, ref NONE) *)

                val pat1 = A.FlatAppPat 
                           [ A.VarPat [conid],
                             A.TuplePat
                              [ A.WildPat,
                                A.FlatAppPat 
                                 [ A.VarPat [I.refCon],
                                   A.VarPat I.pathOptionNONE
                                 ]
                              ]
                           ]

                (* pat2: CONID (_, ref (SOME t)) *)

                val pat2 = A.FlatAppPat 
                           [ A.VarPat [conid], 
                             A.TuplePat
                              [ A.WildPat,
                                A.FlatAppPat 
                                 [ A.VarPat [I.refCon],
                                   A.FlatAppPat 
                                    [ A.VarPat I.pathOptionSOME,
                                      argtPat
                                    ]
                                 ]
                              ]
                           ]


                val projFnDictExp = case argList of
                              [] => projFnExp
                            | _  => A.FlatAppExp [projFnExp, A.TupleExp argListExp]

                val proj = [ A.Clause
                               { pats     = projFnArgPatList @ [pat1],
                                 resultty = NONE,
                                 exp      = A.RaiseExp projExnExpr },

                             A.Clause 
                               { pats     = projFnArgPatList @ [pat2],
                                 resultty = NONE,
                                 exp      = A.FlatAppExp [projFnDictExp, argtExp] }
                           ]
            in
               proj
            end

(* 
  Idea:

     Given E1 : A => B,
           E2 : A,     
           E1 E2 : B

     Assume E1' = THE-> ( embedB o E1 o projA ) : O (OA => OB)
            E2' = embedA E2 : OA 

     THE->^-1 E1' E2' : OB

*)

        fun mkBoundaryClause (conid,_) = 
            let 
                val argt = I.arg "t"
                val argtPat = A.VarPat [argt]
                val argtExp = A.VarExp [argt]

                val argf = I.arg "f"
                val argfPat = A.VarPat [argf]
                val argfExp = A.VarExp [argf]

                val argtExp = if tyconid <> I.liftTranslate I.arrowTycon
                              then argtExp
                              else let val (tv1,tv2) = case argListExp of
                                                         [tv1,tv2] => (tv1,tv2)
                                                       | _ => raise (ErrCodeGeneration ("Impossbile"))
                                   in 
                                       A.FnExp
                                       [
                                         A.Rule { pat=argfPat,
                                                  exp= A.FlatAppExp
                                                       [ 
                                                         A.FlatAppExp [sel2Exp, tv2],

                                                         A.FlatAppExp [
                                                         argtExp,

                                                         A.FlatAppExp [
                                                         A.FlatAppExp [sel1Exp, tv1],
                                                         argfExp ] ]
                                                       ]
                                                }
                                       ]
                                   end

                val pat = A.FlatAppPat [A.VarPat [conid], argtPat]

                val projclauses = 
                    [
                       A.Clause
                        { pats = projFnArgPatList @ [pat], 
                          resultty=NONE,
                          exp=argtExp }
                    ]
            in
                projclauses
            end

        fun mkClause (conid, (liftedconid,SOME ty)) =
            let 
                val argt = I.arg "t"
                val argtPat = A.VarPat [argt]
                val argtExp = A.VarExp [argt]

                val pat = A.FlatAppPat [A.VarPat [liftedconid], argtPat]
                val exp = mkProjExp dict ty argtExp

                val proj = A.Clause
                           { pats     = projFnArgPatList @ [pat],
                             resultty = NONE,
                             exp      = A.FlatAppExp [A.VarExp [conid], exp] }
            in
                proj
            end

          | mkClause (conid, (liftedconid,NONE)) = 
            let 
                val proj = A.Clause
                           { pats = projFnArgPatList @ [A.VarPat [liftedconid]], 
                             resultty = NONE,
                             exp = A.VarExp [conid] }
            in
                proj
            end

        fun mkTupleClause (liftedconid,ty) =
            let 
                fun argt n = I.arg ("t" ^ Int.toString n)
                val (argtPat,argtExp) = 
                     case ty of
                       TupleTy tys => 
                         let val n = length tys
                             val argts = List.tabulate (n, argt)
                             val argtPat = A.TuplePat (map (A.VarPat o U.list) argts)
                             val argtExps = map (A.VarExp o U.list) argts
                             val tysArgtExps = ListPair.zip (tys,argtExps)
                             val argtExps = map (fn (ty,e) => mkProjExp dict ty e) tysArgtExps
                             val argtExp = A.TupleExp argtExps
                         in
                             (argtPat, argtExp)
                         end
                     | _ => 
                         let val argtPat = A.VarPat [argt 0]
                             val argtExp = mkProjExp dict ty (A.VarExp [argt 0])
                         in
                             (argtPat, argtExp)
                         end

                val pat = A.FlatAppPat [A.VarPat [liftedconid], argtPat]
                val exp = argtExp

                val proj = A.Clause
                           { pats     = projFnArgPatList @ [pat],
                             resultty = NONE,
                             exp      = exp }
            in
                proj
            end

        fun mkRecordClause labels (liftedconid,ty) =
            let 
                fun argt n = I.arg ("t" ^ Int.toString n)
                val (argtPat,argtExp) = 
                     case ty of
                       TupleTy tys => 
                         let val n = length tys
                             val _ = case n=length labels of 
                                       true => ()
                                     | false => raise (ErrCodeGeneration ("Something wrong"))
                             val argts = labels
                             val argtPats = map (A.VarPat o U.list) argts
                             val argtPat = A.RecordPat 
                                             {def=ListPair.zip (labels,argtPats),
                                              flexibility=false}
                             val argtExps = map (A.VarExp o U.list) argts
                             val tysArgtExps = ListPair.zip (tys,argtExps)
                             val argtExps = map (fn (ty,e) => mkProjExp dict ty e) tysArgtExps
                             val argtExp = A.RecordExp (ListPair.zip (labels,argtExps))
                         in
                             (argtPat, argtExp)
                         end
                     | _ => 
                         let val argtPat = A.VarPat [argt 0]
                             val argtExp = mkProjExp dict ty (A.VarExp [argt 0])
                         in
                             (argtPat, argtExp)
                         end

                val pat = A.FlatAppPat [A.VarPat [liftedconid], argtPat]
                val exp = argtExp

                val proj = A.Clause
                           { pats     = projFnArgPatList @ [pat],
                             resultty = NONE,
                             exp      = exp }
            in
                proj
            end


        val (conidTyOption, conidTyOptionList) = case conidTyOptionList of
                  (x :: xs) => (x, xs)
                | [] => raise (ErrCodeGeneration ("Not lifted datatype: " ^ tyconid))

        val firstprjclauses = mkLogicVarClause conidTyOption 

        val fb = 
            case (locSrc,conidTyOptionList) of
              ((Boundary _ ,_), [conidTyOption]) => 
               let val pclauses = mkBoundaryClause conidTyOption
               in
                   A.Fb (firstprjclauses @ pclauses)
               end
            | ((Boundary _ ,_), _) => raise (ErrCodeGeneration ("Unexpected number of constructors: "
                                          ^ Int.toString (length conidTyOptionList)))
            | ((Center,FromDatatype path), _) => 
               let 
                   val tyconid = 
                       case path of 
                         [tyconid] => tyconid
                       | _         => raise (ErrCodeGeneration ("Not supported yet:" ^ I.pr path))

                   val conidList = 
                       case SEnv.find (#tyenv env, tyconid) of
                         SOME (DtInfo (SOME (tyvarList,idTyList)),_) => #1 (ListPair.unzip idTyList)
                       | _ => raise (ErrCodeGeneration ("(mkProj) Unexpected type info for:" ^ I.pr path))

                   (* It is assumed that the order among original constructors is matched *)
                   (* with the order among their lifted constructors.                     *)

                   val conidConidTyOptionList = ListPair.zip (conidList, conidTyOptionList)

                   val pclauses = map mkClause conidConidTyOptionList
               in
                   A.Fb (firstprjclauses @ pclauses)
               end

            | ((Center,FromTupletype n), [(conid,SOME ty)]) => 
                   A.Fb ( firstprjclauses @ [ mkTupleClause (conid,ty) ] )

            | ((Center,FromTupletype n), _) => 
                        raise (ErrCodeGeneration ("Mismatched lifted tuple constructors"))

            | ((Center,FromRecordtype labels), [(conid, SOME ty)]) => 
                   A.Fb ( firstprjclauses @ [ mkRecordClause labels (conid,ty) ] )

            | ((Center,FromRecordtype labels), _) => 
                        raise (ErrCodeGeneration ("Mismatched lifted record constructors"))

        
    in
        fb
    end

and mkEmbedExp dict (VarTy v) e = 
    let val f = case SEnv.find (dict, v) of
            SOME f => f
          | NONE => raise (ErrCodeGeneration ("Not found: " ^ Type.pr (VarTy v)))
    in
        A.FlatAppExp [sel1Exp, f, e]
    end

  | mkEmbedExp dict (ConTy (path as [id], tyList)) e =
    if id = I.arrowTycon andalso length tyList=2
    then

    let
        val theConid = I.itselfTranslate id

        val (ty1,ty2) = case tyList of
                          [ty1,ty2] => (ty1,ty2)
                        | _ => raise (ErrCodeGeneration ("Impossible"))

        val argtid = I.arg "t"
        val argtp = A.VarPat [argtid]
        val argte = A.VarExp [argtid]

        val argfid = I.arg "f"
        val argfp = A.VarPat [argfid]
        val argfe = A.VarExp [argfid]

    in
        A.FlatAppExp [ 
           A.VarExp [theConid], 

           A.FlatAppExp [

           A.FnExp [ A.Rule
              {pat = argfp,
               exp = A.FnExp [ A.Rule
                        {pat = argtp,
                         exp = mkEmbedExp dict ty2 
                                   (A.FlatAppExp 
                                       [argfe,
                                        mkProjExp dict ty1 argte])} ]} ], e ] ]
    end

    else

    let 
        val fnExp = A.VarExp [ embedId id ]

        val argtid = I.arg "t"
        val argtp = A.VarPat [argtid]
        val argte = A.VarExp [argtid]

        fun mkTyarg ty = A.TupleExp [ 
                           A.FnExp [ 
                             A.Rule { pat=argtp,
                                      exp=mkEmbedExp dict ty argte } ],
                           A.FnExp [ 
                             A.Rule { pat=argtp,
                                      exp=mkProjExp dict ty argte } ]
                         ]

        val tyargs = map mkTyarg tyList
        val fnArgListExp = case tyargs of
                             []      => fnExp
                           | [tyarg] => A.FlatAppExp [ fnExp, tyarg ]
                           | _       => A.FlatAppExp [ fnExp, A.TupleExp tyargs ]
    in
        A.FlatAppExp [fnArgListExp, e]
    end

  | mkEmbedExp dict (RecordTy idTyList) e =
    let 
        val pat = A.RecordPat { def=map (fn (s,_) => (s,A.VarPat [s])) idTyList,
                                flexibility=false }
        val idExpList = map (fn (s,ty) => (s,mkEmbedExp dict ty (A.VarExp [s]))) idTyList
        val exp = A.RecordExp idExpList
       
        val recconid = I.recordconid (#1 (ListPair.unzip idTyList))
    in
        A.FlatAppExp [ 
          A.FnExp [A.Rule {pat=pat, 
                           exp=A.FlatAppExp [ A.VarExp [recconid], exp] }], e ]
    end

  | mkEmbedExp dict (TupleTy tyList) e = 
    let 
        val n = length tyList
        val args = List.tabulate (n, fn n => I.arg (Int.toString n))
        val pat = A.TuplePat (map (fn s => A.VarPat [s]) args)
        val idTyList = ListPair.zip (tyList,args)
        val expList = map (fn (ty,s) => mkEmbedExp dict ty (A.VarExp [s])) idTyList
        val exp = A.TupleExp expList

        val tupconid = I.tupleconid (length tyList)
    in
        A.FlatAppExp [ 
          A.FnExp [A.Rule {pat=pat, 
                           exp=A.FlatAppExp [ A.VarExp [tupconid], exp] }], e ]
    end

  | mkEmbedExp dict ty e = raise (ErrCodeGeneration ("Unexpected type: " ^ Type.pr ty))

and mkEmbed (env : CodeGenerationEnv) tyconid tyvarList conidTyOptionList locsrc = 
    let 
        val embedFnId  = embedId tyconid
        val embedFnPat = A.VarPat [embedFnId]
        val embedFnExp = A.VarExp [embedFnId]

        val argList = map (S.translate I.tr) tyvarList
        val argListPat = map (A.VarPat o Util.list) argList
        val argListExp = map (A.VarExp o Util.list) argList

        val embedFnArgPatList = case argListPat of 
                             [] => [ embedFnPat ]
                           | _  => [ embedFnPat, A.TuplePat argListPat ]

        fun mkDict ((tyvar,arg),dict) = SEnv.insert (dict,tyvar,A.VarExp [arg])
        val dict = foldl mkDict SEnv.empty (ListPair.zip (tyvarList, argList))

        fun mkBoundaryClause (conid,_) =
            let 

                val argt = I.arg "t"
                val argtPat = A.VarPat [argt]
                val argtExp = A.VarExp [argt]

                val argf = I.arg "f"
                val argfPat = A.VarPat [argf]
                val argfExp = A.VarExp [argf]

                val argtExp = if tyconid <> I.liftTranslate I.arrowTycon
                              then argtExp
                              else let val (tv1,tv2) = case argListExp of
                                                         [tv1,tv2] => (tv1,tv2)
                                                       | _ => raise (ErrCodeGeneration ("Impossbile"))
                                   in 
                                       A.FnExp
                                       [
                                         A.Rule { pat=argfPat,
                                                  exp= A.FlatAppExp
                                                       [ 
                                                         A.FlatAppExp [sel1Exp, tv2],

                                                       A.FlatAppExp [
                                                         argtExp,

                                                       A.FlatAppExp [
                                                         A.FlatAppExp [sel2Exp, tv1],
                                                         argfExp] ]

                                                       ]
                                                }
                                       ]
                                   end

                val pat = argtPat

                val embedclauses = 
                    [
                      A.Clause
                        { pats = embedFnArgPatList @ [pat],
                          resultty=NONE,
                          exp=A.FlatAppExp [A.VarExp [conid], argtExp] }
                     ]
            in
                embedclauses
            end

        fun mkClause (conid, (liftedconid,SOME ty)) =
            let 
                val argt = I.arg "t"
                val argtPat = A.VarPat [argt]
                val argtExp = A.VarExp [argt]

                val pat = A.FlatAppPat [A.VarPat [conid], argtPat]
                val exp = mkEmbedExp dict ty argtExp

                val embed = A.Clause
                            { pats     = embedFnArgPatList @ [pat],
                              resultty = NONE,
                              exp      = A.FlatAppExp [A.VarExp [liftedconid], exp] }
            in
                embed
            end

          | mkClause (conid, (liftedconid,NONE)) = 
            let 
                val embed = A.Clause
                           { pats = embedFnArgPatList @ [A.VarPat [conid]], 
                             resultty = NONE,
                             exp = A.VarExp [liftedconid] }
            in
                embed
            end

        fun mkTupleClause (liftedconid,ty) =
            let 
                fun argt n = I.arg ("t" ^ Int.toString n)
                val (argtPat,argtExp) = 
                     case ty of
                       TupleTy tys => 
                         let val n = length tys
                             val argts = List.tabulate (n, argt)
                             val argtPat = A.TuplePat (map (A.VarPat o U.list) argts)
                             val argtExps = map (A.VarExp o U.list) argts
                             val tysArgtExps = ListPair.zip (tys,argtExps)
                             val argtExps = map (fn (ty,e) => mkEmbedExp dict ty e) tysArgtExps
                             val argtExp = A.FlatAppExp [ A.VarExp [liftedconid], A.TupleExp argtExps ]
                         in
                             (argtPat, argtExp)
                         end
                     | _ => 
                         let val argtPat = A.VarPat [argt 0]
                             val argtExp = mkEmbedExp dict ty (A.VarExp [argt 0])
                         in
                             (argtPat, argtExp)
                         end

                val embed = A.Clause {pats     = embedFnArgPatList @ [argtPat],
                                      resultty = NONE,
                                      exp      = argtExp }
            in
                embed
            end

        fun mkRecordClause labels (liftedconid,ty) =
            let 
                fun argt n = I.arg ("t" ^ Int.toString n)
                val (argtPat,argtExp) = 
                     case ty of
                       TupleTy tys => 
                         let val n = length tys
                             val _ = case n=length labels of 
                                       true => ()
                                     | false => raise (ErrCodeGeneration ("Something wrong"))
                             val argts = labels
                             val argtPats = map (A.VarPat o U.list) argts
                             val argtPat = A.RecordPat 
                                             {def=ListPair.zip (labels,argtPats),
                                              flexibility=false}
                             val argtExps = map (A.VarExp o U.list) argts
                             val tysArgtExps = ListPair.zip (tys,argtExps)
                             val argtExps = map (fn (ty,e) => mkEmbedExp dict ty e) tysArgtExps
                             val argtExp = A.RecordExp (ListPair.zip (labels,argtExps))
                         in
                             (argtPat, A.FlatAppExp [A.VarExp [liftedconid], argtExp])
                         end
                     | _ => 
                         let val argtPat = A.VarPat [argt 0]
                             val argtExp = mkEmbedExp dict ty (A.VarExp [argt 0])
                         in
                             (argtPat, argtExp)
                         end

                val embed = A.Clause { pats     = embedFnArgPatList @ [argtPat],
                                      resultty = NONE,
                                      exp      = argtExp }
            in
                embed
            end

        val conidTyOptionList = case conidTyOptionList of
                  (_ :: xs) => xs
                | [] => raise (ErrCodeGeneration ("Not lifted datatype: " ^ tyconid))

        val fb =
            case (locsrc,conidTyOptionList) of
              ((Boundary _ ,_), [conidTyOption]) => 
               let val eclauses = mkBoundaryClause conidTyOption
               in  A.Fb eclauses
               end

            | ((Boundary _ ,_), _) => raise (ErrCodeGeneration ("Unexpected number of constructors: "
                                          ^ Int.toString (length conidTyOptionList)))

            | ((Center,FromDatatype path), _) => 
               let 
                   val tyconid = 
                       case path of 
                         [tyconid] => tyconid
                       | _         => raise (ErrCodeGeneration ("Not supported yet:" ^ I.pr path))

                   val conidList = 
                       case SEnv.find (#tyenv env, tyconid) of
                         SOME (DtInfo (SOME (tyvarList,idTyList)),_) => #1 (ListPair.unzip idTyList)
                       | _ => raise (ErrCodeGeneration ("(mkEmbed) Unexpected type info for:" ^ I.pr path))

                   (* It is assumed that the order among original constructors is matched *)
                   (* with the order among their lifted constructors.                     *)

                   val conidConidTyOptionList = ListPair.zip (conidList, conidTyOptionList)

                   val eclauses = map mkClause conidConidTyOptionList
               in
                   A.Fb eclauses
               end

            | ((Center,FromTupletype n), [(conid,SOME ty)]) => A.Fb [ mkTupleClause (conid, ty) ]

            | ((Center,FromTupletype n), _) => 
                   raise (ErrCodeGeneration ("Mismatched lifted tuple constructors"))

            | ((Center,FromRecordtype labels), [(conid,SOME ty)]) => 
                   A.Fb [ mkRecordClause labels (conid, ty) ]

            | ((Center,FromRecordtype labels), _) => 
                   raise (ErrCodeGeneration ("Mismatched lifted record constructors"))
    in
        fb
    end

(* printing *)

fun mkPrintExp dict (VarTy v) e = 
    let val f = case SEnv.find (dict, v) of
            SOME f => f
          | NONE => raise (ErrCodeGeneration ("Not found: " ^ Type.pr (VarTy v)))
    in
        A.FlatAppExp [f, e]
    end

  | mkPrintExp dict (ConTy (path as [id], tyList)) e =
    let 
        val fnExp = A.VarExp [ printId id ]

        val argtid = I.arg "t"
        val argtp = A.VarPat [argtid]
        val argte = A.VarExp [argtid]

        fun mkTyarg ty = A.FnExp [ A.Rule {pat=argtp,
                                           exp=mkPrintExp dict ty argte} ]

        val tyargs = map mkTyarg tyList
        val fnArgListExp = case tyargs of
                             []      => fnExp
                           | [tyarg] => A.FlatAppExp [ fnExp, tyarg ]
                           | _       => A.FlatAppExp [ fnExp, A.TupleExp tyargs ]
    in
        A.FlatAppExp [fnArgListExp, e]
    end

  | mkPrintExp dict (RecordTy idTyList) e =
    let 
        val pat = A.RecordPat { def=map (fn (s,_) => (s,A.VarPat [s])) idTyList, flexibility=false }
        val exp = A.FlatAppExp 
                  [
                    stringConcatExp,
                    A.ListExp
                    [
                      A.StringExp "{",

                      A.FlatAppExp
                        [
                          smlogIntersperseExp,
                          A.StringExp ",",
                          A.ListExp
                             (map (fn (s,ty) => 
                               A.FlatAppExp
                                 [ stringConcatExp,
                                    A.ListExp 
                                      [
                                        A.StringExp s,
                                        A.StringExp "=",
                                        mkPrintExp dict ty (A.VarExp [s])
                                      ]
                                 ]) idTyList)
                        ],

                      A.StringExp "}"
                    ]
                  ]
    in
        A.FlatAppExp [ A.FnExp [A.Rule {pat=pat, exp=exp}], e ]
    end

  | mkPrintExp dict (TupleTy tyList) e = 
    let 
        val n = length tyList
        val args = List.tabulate (n, fn n => I.arg (Int.toString n))
        val pat = A.TuplePat (map (fn s => A.VarPat [s]) args)
        val idTyList = ListPair.zip (tyList,args)
        val exp = A.FlatAppExp
                  [
                    stringConcatExp,
                    A.ListExp
                    [ 
                      A.StringExp "(",
                      A.FlatAppExp
                      [
                        smlogIntersperseExp,
                        A.StringExp ",", 
                        A.ListExp (map (fn (ty,s) => mkPrintExp dict ty (A.VarExp [s])) idTyList)
                      ],
                      A.StringExp ")"
                    ]
                  ]
    in
        A.FlatAppExp [ A.FnExp [A.Rule {pat=pat, exp=exp}], e ]
    end

  | mkPrintExp dict ty e = raise (ErrCodeGeneration ("Unexpected type: " ^ Type.pr ty))

fun mkPrint (env : CodeGenerationEnv) tyconid tyvarList conidTyOptionList locSrc = 
    let 
        val printFnId  = printId tyconid
        val printFnPat = A.VarPat [printFnId]
        val printFnExp = A.VarExp [printFnId]

        val argList = map (S.translate I.tr) tyvarList
        val argListPat = map (A.VarPat o Util.list) argList
        val argListExp = map (A.VarExp o Util.list) argList

        val printFnArgPatList = case argListPat of 
                             [] => [ printFnPat ]
                           | _  => [ printFnPat, A.TuplePat argListPat ]

        fun mkDict ((tyvar,arg),dict) = SEnv.insert (dict,tyvar,A.VarExp [arg])
        val dict = foldl mkDict SEnv.empty (ListPair.zip (tyvarList, argList))

        fun mkLogicVarClause (conid,_) = 
            let 
                val argt = I.arg "t"
                val argtExp = A.VarExp [argt]
                val argtPat = A.VarPat [argt]

                val args = I.arg "s"
                val argsExp = A.VarExp [args]
                val argsPat = A.VarPat [args]

                (* pat1: CONID (s, ref NONE) *)

                val pat1 = A.FlatAppPat 
                           [ A.VarPat [conid],
                             A.TuplePat
                              [ argsPat,
                                A.FlatAppPat 
                                 [ A.VarPat [I.refCon],
                                   A.VarPat I.pathOptionNONE
                                 ]
                              ]
                           ]

                (* pat2: CONID (_, ref (SOME t)) *)

                val pat2 = A.FlatAppPat 
                           [ A.VarPat [conid], 
                             A.TuplePat
                              [ A.WildPat,
                                A.FlatAppPat 
                                 [ A.VarPat [I.refCon],
                                   A.FlatAppPat 
                                    [ A.VarPat I.pathOptionSOME,
                                      argtPat
                                    ]
                                 ]
                              ]
                           ]


                val printFnDictExp = case argList of
                              [] => printFnExp
                            | _  => A.FlatAppExp [printFnExp, A.TupleExp argListExp]

                val printclause = [ 
                             A.Clause
                               { pats     = printFnArgPatList @ [pat1],
                                 resultty = NONE,
                                 exp      = argsExp },

                             A.Clause 
                               { pats     = printFnArgPatList @ [pat2],
                                 resultty = NONE,
                                 exp      = A.FlatAppExp [printFnDictExp, argtExp] }
                           ]
            in
               printclause
            end

        fun mkBoundaryClause (conid,tyOption) = 
            let 
                val tyOption = case tyOption of
                                 NONE => NONE
                               | SOME ty => SOME ((AstUtil.unfoldSynWith (#1) (#tyenv env) ty) : T.ty)

                val argt = I.arg "t"
                val argtPat = A.VarPat [argt]
                val argtExp = A.VarExp [argt]

                val pat = A.FlatAppPat [A.VarPat [conid], argtPat]
                val exp = case tyOption of
                            NONE => raise (ErrCodeGeneration ("Impossible: "))
                          | SOME (ty as ConTy ([tycon],_)) => 
                                 if tycon=I.unitTycon then A.StringExp "()" 
                            else if tycon=I.boolTycon then A.FlatAppExp [boolToStringExp, argtExp]
                            else if tycon=I.intTycon  then A.FlatAppExp [intToStringExp, argtExp] 
                            else if tycon=I.wordTycon then A.FlatAppExp [wordToStringExp, argtExp]
                            else if tycon=I.realTycon then A.FlatAppExp [realToStringExp, argtExp]
                            else if tycon=I.charTycon then A.FlatAppExp [charToStringExp, argtExp]
                            else if tycon=I.stringTycon then A.FlatAppExp [ stringConcatExp, 
				    A.ListExp [A.StringExp "\\\"", argtExp, A.StringExp "\\\"" ] ]
                            else A.StringExp ("<" ^ Type.pr ty ^ ">")
   
                          | SOME ty => A.StringExp ("<" ^ Type.pr ty ^ ">")

                val printclauses = 
                    [
                       A.Clause
                        { pats = printFnArgPatList @ [pat], 
                          resultty=NONE,
                          exp=exp }
                    ]
            in
                printclauses
            end

        fun mkClause (conid, (liftedconid,SOME ty)) =
            let 
                val argt = I.arg "t"
                val argtPat = A.VarPat [argt]
                val argtExp = A.VarExp [argt]

                val pat = A.FlatAppPat [A.VarPat [liftedconid], argtPat]
                val exp = mkPrintExp dict ty argtExp

                val printclause = A.Clause
                           { pats     = printFnArgPatList @ [pat],
                             resultty = NONE,
                             exp      = A.FlatAppExp 
                                          [
                                            stringConcatExp, 
                                            A.ListExp 
                                              [ 
                                                A.StringExp conid, 
                                                A.StringExp " ",
                                                exp
                                              ]
                                          ] }
            in
                printclause
            end

          | mkClause (conid, (liftedconid,NONE)) = 
            let 
                val printclause = A.Clause
                           { pats = printFnArgPatList @ [A.VarPat [liftedconid]], 
                             resultty = NONE,
                             exp = A.StringExp conid }
            in
                printclause
            end

        fun mkTupleClause (liftedconid,ty) =
            let 
                fun argt n = I.arg ("t" ^ Int.toString n)
                val (argtPat,argtExp) = 
                     case ty of
                       TupleTy tys => 
                         let val n = length tys
                             val argts = List.tabulate (n, argt)
                             val argtPat = A.TuplePat (map (A.VarPat o U.list) argts)
                             val argtExps = map (A.VarExp o U.list) argts
                             val tysArgtExps = ListPair.zip (tys,argtExps)
                             val argtExps = map (fn (ty,e) => mkPrintExp dict ty e) tysArgtExps
                             val argtExp = A.FlatAppExp
                                             [ 
                                               stringConcatExp,
                                               A.ListExp
                                                 [
                                                   A.StringExp "(",
                                                   A.FlatAppExp
                                                     [
                                                       smlogIntersperseExp,
                                                       A.StringExp ",",
                                                       A.ListExp argtExps 
                                                     ],
                                                   A.StringExp ")"
                                                 ]
                                             ]
                         in
                             (argtPat, argtExp)
                         end
                     | _ => 
                         let val argtPat = A.VarPat [argt 0]
                             val argtExp = mkPrintExp dict ty (A.VarExp [argt 0])
                         in
                             (argtPat, argtExp)
                         end

                val pat = A.FlatAppPat [A.VarPat [liftedconid], argtPat]
                val exp = argtExp

                val printclause = A.Clause
                           { pats     = printFnArgPatList @ [pat],
                             resultty = NONE,
                             exp      = exp }
            in
                printclause
            end

        fun mkRecordClause labels (liftedconid,ty) =
            let 
                fun argt n = I.arg ("t" ^ Int.toString n)
                val (argtPat,argtExp) = 
                     case ty of
                       TupleTy tys => 
                         let val n = length tys
                             val _ = case n=length labels of 
                                       true => ()
                                     | false => raise (ErrCodeGeneration ("Something wrong"))
                             val argts = labels
                             val argtPats = map (A.VarPat o U.list) argts
                             val argtPat = A.RecordPat 
                                             {def=ListPair.zip (labels,argtPats),
                                              flexibility=false}
                             val argtExps = map (A.VarExp o U.list) argts
                             val tysArgtExps = ListPair.zip (tys,argtExps)
                             val argtExps = map (fn (ty,e) => mkPrintExp dict ty e) tysArgtExps
                             val argtExp = A.FlatAppExp
                                             [ 
                                               stringConcatExp,
                                               A.ListExp
                                                 [
                                                   A.StringExp "{",
                                                   A.FlatAppExp
                                                     [
                                                       smlogIntersperseExp,
                                                       A.StringExp ",",
                                                       A.ListExp
                                                        (map (fn (l,e) => 
                                                           A.FlatAppExp 
                                                             [
                                                               A.StringExp l,
                                                               A.StringExp "=",
                                                               e
                                                             ] ) (ListPair.zip (labels,argtExps)))
                                                     ],
                                                   A.StringExp "}"
                                                 ]
                                             ]
                         in
                             (argtPat, argtExp)
                         end
                     | _ => 
                         let val argtPat = A.VarPat [argt 0]
                             val argtExp = mkPrintExp dict ty (A.VarExp [argt 0])
                         in
                             (argtPat, argtExp)
                         end

                val pat = A.FlatAppPat [A.VarPat [liftedconid], argtPat]
                val exp = argtExp

                val print = A.Clause
                           { pats     = printFnArgPatList @ [pat],
                             resultty = NONE,
                             exp      = exp }
            in
                print
            end


        val (conidTyOption, conidTyOptionList) = case conidTyOptionList of
                  (x :: xs) => (x, xs)
                | [] => raise (ErrCodeGeneration ("Not lifted datatype: " ^ tyconid))

        val firstprjclauses = mkLogicVarClause conidTyOption 

        val fb = 
            case (locSrc,conidTyOptionList) of
              ((Boundary _ ,_), [conidTyOption]) => 
               let val pclauses = mkBoundaryClause conidTyOption
               in
                   A.Fb (firstprjclauses @ pclauses)
               end
            | ((Boundary _ ,_), _) => raise (ErrCodeGeneration ("Unexpected number of constructors: "
                                          ^ Int.toString (length conidTyOptionList)))
            | ((Center,FromDatatype path), _) => 
               let 
                   val tyconid = 
                       case path of 
                         [tyconid] => tyconid
                       | _         => raise (ErrCodeGeneration ("Not supported yet:" ^ I.pr path))

                   val conidList = 
                       case SEnv.find (#tyenv env, tyconid) of
                         SOME (DtInfo (SOME (tyvarList,idTyList)),_) => #1 (ListPair.unzip idTyList)
                       | _ => raise (ErrCodeGeneration ("(mkPrint) Unexpected type info for:" ^ I.pr path))

                   (* It is assumed that the order among original constructors is matched *)
                   (* with the order among their lifted constructors.                     *)

                   val conidConidTyOptionList = ListPair.zip (conidList, conidTyOptionList)

                   val pclauses = map mkClause conidConidTyOptionList
               in
                   A.Fb (firstprjclauses @ pclauses)
               end

            | ((Center,FromTupletype n), [(conid,SOME ty)]) => 
                   A.Fb ( firstprjclauses @ [ mkTupleClause (conid,ty) ] )

            | ((Center,FromTupletype n), _) => 
                        raise (ErrCodeGeneration ("Mismatched lifted tuple constructors"))

            | ((Center,FromRecordtype labels), [(conid, SOME ty)]) => 
                   A.Fb ( firstprjclauses @ [ mkRecordClause labels (conid,ty) ] )

            | ((Center,FromRecordtype labels), _) => 
                        raise (ErrCodeGeneration ("Mismatched lifted record constructors"))
        
    in
        fb
    end

(* utility *)

(* Return the first constructor used for logic variable *)
fun varCon env (ConTy (path as [id], _)) = 
    let val {tyenv=tyenv,idenv=_} = env
        val conid = case SEnv.find (tyenv, id) of
                      SOME (DtInfo (SOME (_,(conid,_)::_)),_) => conid
                    | _ => raise (ErrCodeGeneration ("Unexpected type info for : " ^ I.pr path))
    in conid
    end
  | varCon env ty = raise (ErrCodeGeneration ("Unexpected type: " ^ T.pr ty))

(* Return the second constructor of a lifted datatype *)
fun theCon env (ConTy (path as [id], _)) = 
    let val {tyenv=tyenv,idenv=_} = env
        val conid = case SEnv.find (tyenv, id) of
                      SOME (DtInfo (SOME (_,_::(conid,_)::_)),_) => conid
                    | _ => raise (ErrCodeGeneration ("Unexpected type info for : " ^ I.pr path))
    in conid
    end
  | theCon env ty = raise (ErrCodeGeneration ("Unexpected type: " ^ T.pr ty))

(* Entry function *)
fun codegeneration declList = 
    let val declList = cgDeclList {idenv=SEnv.empty,tyenv=SEnv.empty} declList
    in  declList
    end

and cgDeclList env (RelationSigDec (id,tyInoutOptionList) :: declList) =
    let
       val {idenv=idenv,tyenv=tyenv} = env
       val w = FunWorld tyInoutOptionList
       val idenv = SEnv.insert (idenv, id, (RelId tyInoutOptionList, SOME w))
       val env = {idenv=idenv,tyenv=tyenv}
       val declList = cgDeclList env declList
    in
       declList
    end

  | cgDeclList env (DatatypeDec (dbOriginList,tbListOption) :: declList) =
    let 
        fun acc NONE origin = SOME origin
          | acc (SOME Prelude) Prelude = SOME Prelude
          | acc (SOME Prelude) _ = raise (ErrCodeGeneration ("Mixed datatype declarations"))
          | acc (SOME _) Prelude = raise (ErrCodeGeneration ("Mixed datatype declarations"))
          | acc (SOME origin) origin' = SOME origin  (* anyone should be fine *)

        val {idenv=idenv,tyenv=tyenv} = env
        val {idenv=idenv,tyenv=tyenv,
             occurs=occurs,unifys=unifys,embedprojs=embedprojs,prints=prints,
             accorigin=accorigin} = 
             foldl 
              (fn ((Db {tyc=id,tyvars=tyvarList,rhs=Constrs idTyOptionList},origin),
                    {idenv=idenv,tyenv=tyenv,
                     occurs=occurs,unifys=unifys,embedprojs=embedprojs,prints=prints,
                     accorigin=accorigin}) =>

                   let val ty = ConTy ([id], map VarTy tyvarList)
                       val idTyList = map (fn (cid,tyOption) => 
                                              (cid, Type.arrowty (tyOption,ty)))
                                               idTyOptionList
                   in  {idenv=foldl (fn ((cid,ty),idenv) => 
                                      SEnv.insert (idenv, cid, (ConId (ty,tyvarList,id),NONE)))
                                      idenv
                                      idTyList,

                        tyenv=SEnv.insert(tyenv,id,(DtInfo(SOME(tyvarList,idTyList)),SOME origin)),
                        occurs=case origin of
                                 Lift locsrc
                                   => occurs
                                      @ [ mkOccur env id tyvarList idTyOptionList locsrc ]
                               | _ => occurs,
                        unifys=case origin of
                                 Lift locsrc
                                   => unifys
                                      @ [ mkUnify env id tyvarList idTyOptionList locsrc ]
                               | _ => unifys,
                        prints=case origin of
                                 Lift locsrc
                                   => prints
                                      @ [ mkPrint env id tyvarList idTyOptionList locsrc ]
                               | _ => prints, 
                        embedprojs=case origin of
                                 Lift locsrc
                                   => embedprojs
                                      @ [ mkEmbed env id tyvarList idTyOptionList locsrc,
                                          mkProj env id tyvarList idTyOptionList locsrc
                                        ]
                               | _ => embedprojs,
                        accorigin=acc accorigin origin}
                   end 

                | ((Db {tyc=id,tyvars=tyvarList,rhs=Repl [defid]},origin),
                    {idenv=idenv,tyenv=tyenv,
                     occurs=occurs,unifys=unifys,embedprojs=embedprojs,prints=prints,
                     accorigin=accorigin}) =>
                      (case SEnv.find (tyenv, defid) of
                         SOME (dtinfo as (DtInfo _, _)) => 
                           {idenv=idenv,
                            tyenv=SEnv.insert(tyenv,id,dtinfo),
                            occurs=occurs,
                            unifys=unifys,
                            embedprojs=embedprojs,
                            prints=prints,
                            accorigin=acc accorigin origin}
                       | _ => raise (ErrCodeGeneration ("Undefined type: " ^ defid)))
          
                | _ => raise (ErrCodeGeneration ("Not implemented at cgDeclList")))

               {idenv=idenv,tyenv=tyenv,
                occurs=[],unifys=[],embedprojs=[],prints=[],
                accorigin=NONE} dbOriginList

        val (dbList,_) = ListPair.unzip dbOriginList

        val env = {idenv=idenv, tyenv=tyenv}
        
        val decl = A.DatatypeDec (dbList,NONE)

        val decls = [ decl ]

        val decls = case embedprojs of 
                      [] => decls
                    | _  => decls @ [A.FunDec (embedprojs, [])]

        val decls = case occurs of 
                      [] => decls
                    | _  => decls @ [A.FunDec (occurs, [])]

        val decls = case unifys of 
                      [] => decls
                    | _  => decls @ [A.FunDec (unifys, [])]

        val decls = case prints of 
                      [] => decls
                    | _  => decls @ [A.FunDec (prints, [])]

        val declList = cgDeclList env declList

        val declList = case accorigin of 
                         SOME Prelude => declList 
                       | _ => decls @ declList
    in 
        declList
    end

  | cgDeclList env (TypeDec tbList :: declList) =
      let val {idenv=idenv,tyenv=tyenv} = env
          
          val tyenv =
              foldl (fn ((id,tyinfo),tyenv) => SEnv.insert(tyenv,id,(tyinfo,NONE)))
                 tyenv
                 (map (fn tb as Tb {tyc=id,tyvars=tyvarList,def=ty} =>
                      (id, SynInfo (tyvarList, ty))) tbList)
          
          val env = {idenv=idenv,tyenv=tyenv}

          val decl = A.TypeDec tbList
          val declList = cgDeclList env declList
      in
          decl :: declList
      end

  | cgDeclList env (FolDec (idFolList, tyvarList) :: declList) =
    let 
        (* dictionary *)
        val dict = SEnv.empty  (* Need to be updated *)

        (* mark relation declarations *)
        val idList = #1 (ListPair.unzip idFolList)
        fun markRelWorld (id,env) = 
            let val {tyenv=tyenv,idenv=idenv} = env
            in  case SEnv.find (idenv, id) of
                  SOME (idinfo, SOME _) => 
                  let val liftedid =I.liftRel id
                      val idenv = SEnv.insert (idenv, id, (idinfo, SOME (RelWorld liftedid)))
                  in  
                      {tyenv=tyenv,idenv=idenv}
                  end
                | SOME (idinfo, NONE) 
                    => raise (ErrCodeGeneration ("Overridden signature for relation declaration: "^id))
                | _ => raise (ErrCodeGeneration ("No signature for relation declaration: " ^ id))
            end
        val env = foldl markRelWorld env idList

        (* code generation for functional interfaces *)

        (* For each relation declaration   

                   r : (T1, T2)

           we will generate:

               fun r (x1, x2) = 
                 let val z1 = case x1 of
                                SOME y1 => EMBED_T1 y1
                              | NONE    => LT1 (fresh (), ref NONE)

                     val z2 = case x2 of
                                SOME y2 => EMBED_T2 y2
                              | NONE    => LT2 (fresh (), ref NONE)

                     val inittc = fn _ => ()

                     val tcListStream = RELr (z1, z2) inittc

                     fun conv tc = (tc Redo; 
                                    (fn x => (tc Undo; x))
                                      ( SOME (PROJ_T1 z1) handle _ => NONE,
                                        SOME (PROJ_T2 z2) handle _ => NONE  );
                                    )
                 in
                     mapStream (map conv) tcListStream
                 end

           where RELr is the lifted id of r, LT1 is a constructor of T1,
                 and LT2 is a constructor of T2.
        *)

        val {tyenv=tyenv,idenv=idenv} = env

        fun typeSig (id,_) = 
             (case SEnv.find (idenv,id) of
                SOME (RelId tyInoutOptionList, SOME (RelWorld liftedid))
                  => let val tyList = #1 (ListPair.unzip tyInoutOptionList)
                     in  (id, liftedid, tyList)
                     end
              | _ => raise (ErrCodeGeneration ("No signature for relation declaration: " ^ id)))

        val idLiftedidTyListList = map typeSig idFolList

        val dict = SEnv.empty

        fun mkInterface (id,liftedId, tyList) =
            let val n = length tyList

                val args = List.tabulate (n, fn i => I.arg (Int.toString i))
                val argpats = map (A.VarPat o U.list) args
                val argexps = map (A.VarExp o U.list) args

                val argr = I.arg "res"
                val argrpat = A.VarPat [argr]
                val argrexp = A.VarExp [argr]

                val conids = map (varCon env) tyList

                val argConidTylist = U.zip3 (args, conids, tyList)

                fun embed (arg, conid, ty) = 
                        A.FlatAppExp [ 

                         A.FnExp [ 
                          A.Rule {pat=A.FlatAppPat [
                                       A.VarPat I.pathOptionSOME, 
                                       A.VarPat [arg] ],
                                  exp=mkEmbedExp dict ty (A.VarExp [arg])},

                          A.Rule {pat=A.VarPat I.pathOptionNONE,
                                  exp=A.FlatAppExp [
                                       A.VarExp [conid],
                                       A.TupleExp [ 
                                        A.FlatAppExp [
                                          freshStringExpr, 
                                          A.TupleExp []],
                                        A.FlatAppExp [
                                          A.VarExp [I.refCon], 
                                          A.VarExp I.pathOptionNONE ]  ]]} ],
                         A.VarExp [arg] ]

                val inittc = A.FnExp [ A.Rule {pat=A.WildPat, exp=A.TupleExp []} ]
                
                val tcListStream = A.FlatAppExp [
                                      A.VarExp [liftedId],
                                      A.TupleExp argexps,
                                      inittc ]

                val argtc = I.arg "tc"

                val tcRedo = A.FlatAppExp [ A.VarExp [argtc], redoExpr ]
                val tcUndo = A.FlatAppExp [ A.VarExp [argtc], undoExpr ]

                fun seq e1 e2 = A.FlatAppExp [
                                    A.FnExp [ A.Rule {pat=A.WildPat, exp=e2} ],
                                    e1 ]

                val argTylist = ListPair.zip (args, tyList)

                (* for projection *)

                fun proj (arg,ty) = A.HandleExp {
                       expr=A.FlatAppExp [A.VarExp I.pathOptionSOME,
                                          mkProjExp dict ty (A.VarExp [arg])], 
                       rules=[A.Rule {pat=A.WildPat,exp=A.VarExp I.pathOptionNONE}]}

                val mkprojresult = A.TupleExp (map proj argTylist)

                val projconv = A.FnExp [ A.Rule {
                                       pat=A.VarPat [argtc],
                                       exp=seq tcRedo 
                                          (A.FlatAppExp [
                                              A.FnExp [ A.Rule { 
                                                 pat=argrpat, 
                                                 exp=seq tcUndo argrexp } ],
                                              mkprojresult]) } ]

                (* for printing lifted values *)

                fun print (arg,ty) = mkPrintExp dict ty (A.VarExp [arg])

                val mkprintresult = A.TupleExp (map print argTylist)

                val printconv = A.FnExp [ A.Rule {
                                       pat=A.VarPat [argtc],
                                       exp=seq tcRedo 
                                          (A.FlatAppExp [
                                              A.FnExp [ A.Rule { 
                                                 pat=argrpat, 
                                                 exp=seq tcUndo argrexp } ],
                                              mkprintresult]) } ]

                (* bodyexp: either "printconv" or "projconv" *)

                val bodyexp = A.FlatAppExp [ 
                               mapStreamExpr,
                               A.FlatAppExp [ listMapExpr, printconv ],
                               tcListStream ]

                val body = 
                    A.FlatAppExp [

                    A.FnExp [
                     A.Rule {
                        pat=A.TuplePat argpats,
                        exp=bodyexp } ],
                    
                    A.TupleExp (map embed argConidTylist) ]
            in
                A.Fb [ A.Clause { pats=[A.VarPat [id], A.TuplePat argpats], 
                                  resultty=NONE, 
                                  exp=body} ]
            end

        val fbInterfaceList= map mkInterface idLiftedidTyListList

        (* code generation for relational declarations *)
        val fbList = cgFolDec env idFolList

        val declList = cgDeclList env declList
    in 
        A.FunDec (fbList,tyvarList) 
        :: A.FunDec (fbInterfaceList,tyvarList) 
        :: declList
    end

  | cgDeclList env (RelationDec _ :: declList) =
       raise (ErrCodeGeneration ("Unexpected declaration"))

  | cgDeclList env [] = []

and cgFolDec env idFolList =
    let 
        fun fbClause (id,fol) = A.Fb [A.Clause (cgIdFol env id fol)]
        val fbList = map fbClause idFolList
    in
        fbList
    end

and cgIdFol env id (Forall (idTyList,fol)) =
    let
        val patList = 
              map (fn (id,ty) => 
                   A.ConstraintPat 
                     {pattern=A.VarPat [id],constraint=ty}) idTyList

        val liftedid = case SEnv.find (#idenv env, id) of
                         SOME (_, SOME (RelWorld liftedid)) => liftedid
                       | _ => raise (ErrCodeGeneration ("No signature for relation declaration: " ^ id))

        val funPat  = A.VarPat [liftedid]
        val argsPat = A.TuplePat patList
        val patList = [ funPat, argsPat ]

        val resultty = NONE
        val exp = cgFol env fol
        val exp = A.FlatAppExp [stepExpr, exp]
    in
        {pats=patList, resultty=resultty, exp=exp}
    end

  | cgIdFol env id _ = 
        raise (ErrCodeGeneration "Expected Forall (...) at cgIdFol")

and cgFol env fol = 

    case fol of

      Assign (id,ty,(FlatAppExp (_,_,expTyList))) => 
      let 
          val {tyenv=tyenv,idenv=idenv} = env 

          val tcid  = I.arg "tc"
          val tcpat = A.VarPat [tcid]
          val tcexp = A.VarExp [tcid]

          val dict = SEnv.empty

          fun mkapp (expf,tyf) [] = expf
            | mkapp (expf,tyf) ((exp,ty)::expTyList) = 
              let 
                 val expa = cgExp env exp

                 val thearrowtycon = theCon env tyf
                 val (conid,ty1,ty2) = case tyf of
                             ConTy (conid, [ty1,ty2]) => (conid,ty1,ty2)
                           | _ => raise (ErrCodeGeneration ("Not function type: " ^ T.pr tyf))

                 val argt = I.arg "t"
                 val argtExp = A.VarExp [argt]
                 val argtPat = A.VarPat [argt]

                 val expg = mkEmbedExp dict ty2
                               (A.FlatAppExp
                                 [ 
                                   mkDoProj dict tcexp tyf expf,
                                   mkDoProj dict tcexp ty1 expa
                                 ])
              in
                 mkapp (expg,ty2) expTyList
              end

          val ((exp,tyf),expTyList) = case expTyList of
                  [] => raise (ErrCodeGeneration ("Empty FlatAppExp"))
                | (expTy :: expTyList) => (expTy, expTyList)

          val expf = cgExp env exp
          val tyf  = tyf

          val appExp = mkapp (expf,tyf) expTyList

          val appExp = A.FlatAppExp
                     [
                       doAppUnderSubstExpr,
                       A.FnExp
                       [
                         A.Rule 
                         { 
                           pat=A.WildPat,
                           exp=appExp
                         }
                       ]
                     ]

      in
          A.FnExp 
          [ 
            A.Rule 
            { 
              pat = tcpat,
              exp = A.HandleExp {
                     expr = mkDoUnify dict tcexp ty 
                              (A.VarExp [id]) 
                                 (A.FlatAppExp [appExp, tcexp]),
                     rules = [ A.Rule {pat=A.WildPat, exp=A.FlatAppExp [falseExpr,tcexp]} ] }
            }
          ]
      end

    | Assign _ => raise (ErrCodeGeneration ("Never reachable"))

    | Unify (ty,exp1,exp2) =>
      let 
          val tcid  = I.arg "tc"
          val tcpat = A.VarPat [tcid]
          val tcexp = A.VarExp [tcid]

          val exp1 = cgExp env exp1
          val exp2 = cgExp env exp2

          val dict = SEnv.empty
      in
          A.FnExp [ A.Rule { pat = tcpat,
                             exp = mkDoUnify dict tcexp ty exp1 exp2 } ]
      end

    | Pred (path as [id],expList,negation) =>
      let val expList = map (cgExp env) expList
          val vExp = A.VarExp path

          val {tyenv=tyenv,idenv=idenv} = env
          val w = case SEnv.find (idenv,id) of
                    SOME (_, SOME w) => w
                  | _ => raise (ErrCodeGeneration ("Neither a function nor a relation: " ^ I.pr path))

          val exp = case w of
                      RelWorld id => A.FlatAppExp [ A.VarExp [id], A.TupleExp expList ]

                    | FunWorld tyInoutOptionList => 
                      let val tyInoutOptionExpList = ListPair.zip (tyInoutOptionList,expList)

                          fun sort (((ty,SOME IN), exp),(ins,outs)) = ((ty,exp) :: ins, outs)
                            | sort (((ty,SOME OUT),exp),(ins,outs)) = (ins, (ty,exp) :: outs)
                            | sort (((ty,NONE),    exp),(ins,outs)) = 
                                   raise (ErrCodeGeneration ("Function signature without IN/OUT: " 
                                               ^ I.pr path))

                          val (inTyExpList,outTyExpList) = foldr sort ([], []) tyInoutOptionExpList

                      (*  f (E1, E2), where f : (T1 : in, T2 : out), is translated into
                        
                          UNIFY_T2 E2 (EMBED_T2 (f (PROJ_T1 E1))) handle exn => FALSE
                            
                          exn will be either ProjectionError or any other exception raised
                          during calling f.
                       *)
                          val dict = SEnv.empty

                          val argtc = I.arg "tc"
                          val argtcPat = A.VarPat [argtc]
                          val argtcExp = A.VarExp [argtc]

                          fun proj  (ty,e) = mkDoProj dict argtcExp ty e
                          fun embed (ty,e) = mkEmbedExp dict ty e

                          fun unify (ty,e1,e2) = A.FnExp [ A.Rule {pat=argtcPat,
                                                      exp=mkDoUnify dict argtcExp ty e1 e2} ]

                          val n = length outTyExpList
                          val args = List.tabulate (n, fn i => I.arg (Int.toString i))
                          val argPats = map (A.VarPat o U.list) args
                          val argExps = map (A.VarExp o U.list) args

                          val (outTyList,outExpList) = ListPair.unzip outTyExpList
                          val outTyArgList = ListPair.zip (outTyList, argExps)
                          val outTyArgExpList = U.zip3 (outTyList,argExps,outExpList)

                          fun mkConj (p1,p2) = A.FlatAppExp [conjExpr,A.TupleExp [p1,p2]]
                          fun mkSeq preds = foldl mkConj trueExpr preds

                      in
                          A.FnExp [ A.Rule {pat=argtcPat,

                          exp=
                          A.FlatAppExp [
                          A.HandleExp {
                            expr=A.FlatAppExp [
                                    A.FnExp [ A.Rule {pat=A.TuplePat argPats,
                                                      exp=mkSeq (map unify outTyArgExpList)} ],
                                    A.FlatAppExp [
                                       A.FnExp [ A.Rule {pat=A.TuplePat argPats,
                                                         exp=A.TupleExp (map embed outTyArgList)} ],
                                       A.FlatAppExp [ 
                                          A.VarExp path, 
                                          A.TupleExp (map proj inTyExpList) ] ] ],
                           rules= [A.Rule {pat=A.WildPat,exp=falseExpr}] },

                           argtcExp ]
                          } ]
                          
                      end

          val exp = case negation of
                      true => A.FlatAppExp [negationExpr, exp]
                    | false => exp
      in  
          exp
      end

    | Pred (path,expList,negation) =>
        raise (ErrCodeGeneration ("Not supported yet: " ^ I.pr path))

    | Conj [fol] => cgFol env fol

    | Conj folList =>
      let 
          val cexp = conjExpr

          val exp = foldl (fn (fol,exp1) => 
                     let val exp2 = cgFol env fol
                     in  A.FlatAppExp [cexp, A.TupleExp [exp1,exp2]]
                     end )
                     trueExpr folList
      in
          exp
      end

    | Disj [fol] => cgFol env fol

    | Disj folList =>
      let 
          val dexp = disjExpr

          val exp = 
              foldl (fn (fol,exp1) => 
                     let val exp2 = cgFol env fol
                     in  A.FlatAppExp [dexp, A.TupleExp [exp1,exp2]]
                     end ) 
                     falseExpr folList
      in
          exp
      end

    | Exists (idTyList, fol) =>
      let 
          val exp0 = cgFol env fol
          fun exists (idTy as (_,ty),exp) = 
              let 
                  val conid = varCon env ty
                  val vExp = A.VarExp [conid]
              in
                  A.FlatAppExp [existsExpr, vExp, fnExp idTy exp]
              end
          val exp = foldr exists exp0 idTyList
      in
          exp
      end

    | Forall (idTyList, fol) =>
        raise (ErrCodeGeneration "Unexpected Forall (...) at cgFol")

and cgExp env exp =

    case exp of

      VarExp (path,ty) => A.VarExp path

    | ConExp (path,expTyList) =>
      let val vExp = A.VarExp path
          val (expList,tyList) = ListPair.unzip expTyList
          val expList = map (cgExp env) expList
      in  
          A.FlatAppExp (vExp :: expList)
      end

    | IntExp i => A.IntExp i

    | WordExp i => A.WordExp i

    | RealExp s => A.RealExp s

    | StringExp s => A.StringExp s

    | CharExp s => A.CharExp s

    | RecordExp idExpList => 
      let fun f (id,exp) = (id, cgExp env exp)
          val idExpList = map f idExpList
      in
          A.RecordExp idExpList
      end

    | TupleExp expList =>
      let val expList = map (cgExp env) expList
      in
          A.TupleExp expList
      end

    | SelectorExp label => A.SelectorExp label

    | ConstraintExp (exp,ty) =>
      let val exp = cgExp env exp
      in
          A.ConstraintExp {expr=exp,constraint=ty}
      end

    | VectorExp expList =>
      let val expList = map (cgExp env) expList
      in
          A.VectorExp expList
      end

    | FlatAppExp _ => raise (ErrCodeGeneration ("Never reachable"))

end

end
