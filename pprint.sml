
structure PrettyPrint =

struct

open Ast

exception ErrPPrint of string

(* PP combinators *)

val CR = "\n"
val SP = " "
val COLON = ":"
val ARROW = "=>"
val WILD = "_"
val OPENPAREN = "("
val CLOSEPAREN = ")"
val OPENBRACKET = "["
val CLOSEBRACKET = "]"
val OPENBRACE = "{"
val CLOSEBRACE = "}"
val DOUBLEQUOTE = "\""
val COMMA = ","
val EQ = "="
val BAR = "|"
val SHARP = "#"
val AND = "and"
val AS = "as"
val OF = "of"
val DATATYPE = "datatype"
val TYPE = "type"
val FUN = "fun"
val FN = "fn"
val RAISE = "raise"
val HANDLE = "handle"

val outputFn = ref (fn s => TextIO.output (TextIO.stdOut,s))

fun pp s = !outputFn s

fun PAREN p s = (pp OPENPAREN; p s; pp CLOSEPAREN)

fun IF cond s = if cond then pp s else ()

fun REP p del []  = ()
  | REP p del [s] = p s
  | REP p del (s::ss)  = (p s; foldl (fn (s,_) => (pp del; p s)) () ss)

fun OPT p NONE     = ()
  | OPT p (SOME s) = p s

fun INT i = pp (Int.toString i)
fun STRING s = pp s

fun ID id = pp id
fun QID path = pp (Identifier.pr path)
fun TY ty = pp (Type.pr ty)

(* Entry function *)

fun pprint declList = REP ppDecl CR declList

and ppDecl (DatatypeDec (dbList,tbListOption)) = 
    ( pp DATATYPE; pp SP;
      REP ppDb (CR ^ AND ^ SP) dbList; pp CR;
      OPT (REP ppTb (CR ^ AND ^ SP)) tbListOption )

  | ppDecl (TypeDec tbList) = 
    ( pp TYPE; pp SP;
      REP ppTb (CR ^ AND ^ SP) tbList; pp CR )

  | ppDecl (FunDec (fbList,tyvars)) =
    ( pp FUN; pp SP;
      ppTyvars tyvars; IF (not (null tyvars)) SP;
      REP ppFb (CR ^ AND ^ SP) fbList; pp CR )

  | ppDecl _ = raise (ErrPPrint ("Not supported yet"))

and ppTyvars tyvars = 
    let val n = length tyvars
        val notzero = n <> 0
        val notone  = n <> 1
        val cond = notzero andalso notone
    in
    ( IF cond OPENPAREN; 
      REP STRING (COMMA ^ SP) tyvars;
      IF cond CLOSEPAREN;
      IF notzero SP)
    end

and ppDb (Db {tyc=id,tyvars=tyvars,rhs=dbrhs}) =
    ( ppTyvars tyvars; IF (not (null tyvars)) SP;
      ID id; pp SP; 
      pp EQ; pp SP;
      ppDbrhs dbrhs )

and ppDbrhs (Constrs idTyOptions) = ( REP ppConidTyOption (SP ^ BAR ^ SP) idTyOptions )
  | ppDbrhs (Repl path) = ( pp DATATYPE; pp SP; QID path )

and ppConidTyOption (id,NONE) =
    ( ID id )

  | ppConidTyOption (id,SOME ty) =
    ( ID id; pp SP;
      pp OF; pp SP;
      TY ty )

and ppTb (Tb {tyc=id,tyvars=tyvars,def=ty}) =
    ( ppTyvars tyvars; SP;
      ID id; pp SP;
      pp EQ; pp SP;
      TY ty )

and ppFb (Fb clauses) = REP ppClause (CR ^ BAR ^ SP) clauses

and ppClause (Clause {pats=pats,resultty=tyOption,exp=exp}) =
    ( REP ppPat SP pats; pp SP;
      OPT ppHastype tyOption; pp SP;
      pp EQ; pp SP;
      ppExp exp )

and ppHastype ty = (pp SP; pp COLON; pp SP; TY ty)

and ppPat (VarPat path) = QID path
  | ppPat (IntPat i) = INT i
  | ppPat (WordPat i) = INT i
  | ppPat (StringPat s) = STRING (DOUBLEQUOTE ^ s ^ DOUBLEQUOTE)
  | ppPat (CharPat s) = STRING s
  | ppPat (WildPat) = pp WILD
  | ppPat (RecordPat {def=idpats,flexibility=_}) =
      (pp OPENBRACE; REP (ppLabelPat ppPat) (COMMA ^ SP) idpats; pp CLOSEBRACE)
  | ppPat (ListPat pats) = (pp OPENBRACKET; REP ppPat (COMMA ^ SP) pats; pp CLOSEBRACKET)
  | ppPat (TuplePat pats) = (pp OPENPAREN; REP ppPat (COMMA ^ SP) pats; pp CLOSEPAREN)
  | ppPat (FlatAppPat pats) = (pp OPENPAREN; REP ppPat SP pats; pp CLOSEPAREN)
  | ppPat (ConstraintPat {pattern=pat,constraint=ty}) = (ppPat pat; SP; ppHastype ty)
  | ppPat (LayeredPat {varPat=pat,expPat=pat'}) = 
      (pp OPENPAREN; ppPat pat; pp (SP ^ AS ^ SP); ppPat pat'; pp CLOSEPAREN)
  | ppPat (VectorPat pats) = (pp SHARP; pp OPENBRACKET; REP ppPat (COMMA ^ SP) pats; pp CLOSEBRACKET)
  | ppPat (OrPat pats) = (PAREN (REP ppPat (SP ^ BAR ^ SP)) pats)

and ppLabelExp p (id,x) = (ID id; pp SP; pp EQ; pp SP; p x)

and ppLabelPat p (id,x) = (ID id; pp SP; pp EQ; pp SP; p x)

and ppExp (VarExp path) = QID path
  | ppExp (FlatAppExp exps) = (pp OPENPAREN; REP ppExp SP exps; pp CLOSEPAREN; pp CR)
  | ppExp (AppExp {function=exp,argument=exp'}) = (ppExp exp; pp SP; ppExp exp'; pp CR)
  | ppExp (IntExp i) = INT i
  | ppExp (WordExp i) = INT i
  | ppExp (RealExp s) = STRING s
  | ppExp (StringExp s) = STRING (DOUBLEQUOTE ^ s ^ DOUBLEQUOTE)
  | ppExp (CharExp s) = STRING s
  | ppExp (RecordExp idexps) =
      (pp OPENBRACE; REP (ppLabelExp ppExp) (COMMA ^ SP) idexps; pp CLOSEBRACE)
  | ppExp (ListExp exps) = (pp OPENBRACKET; REP ppExp (COMMA ^ SP) exps; pp CLOSEBRACKET)
  | ppExp (TupleExp exps) = (pp OPENPAREN; REP ppExp (COMMA ^ SP) exps; pp CLOSEPAREN)
  | ppExp (SelectorExp s) = (pp SHARP; STRING s)
  | ppExp (ConstraintExp {expr=exp,constraint=ty}) = (ppExp exp; pp SP; ppHastype ty)
  | ppExp (VectorExp exps) = (pp SHARP; pp OPENBRACKET; REP ppExp (COMMA ^ SP) exps; pp CLOSEBRACKET)
  | ppExp (FnExp rules) = (pp OPENPAREN; pp FN; pp SP; REP ppRule (SP ^ BAR ^ SP) rules; pp CLOSEPAREN)
  | ppExp (HandleExp {expr=exp,rules=rules}) = (pp OPENPAREN; pp OPENPAREN; ppExp exp; pp CLOSEPAREN; pp CR; pp SP; pp HANDLE; pp SP; REP ppRule (SP ^ BAR ^ SP) rules; pp CLOSEPAREN)
  | ppExp (RaiseExp exp) = (pp RAISE; pp SP; ppExp exp)

and ppRule (Rule {pat=pat,exp=exp}) = 
    ( ppPat pat; pp SP;
      pp ARROW; pp CR;
      ppExp exp )

fun prettyprint filename declList =

let

    val outfilename = 
        case String.tokens (fn x => x = #".") filename of
          [prefix,"smlog"] => prefix ^ ".sml"
        | _ => filename ^ ".sml"

    val outstream = TextIO.openOut outfilename

    val _ = outputFn := (fn s => TextIO.output (outstream, s))


in

( pprint declList; 
  TextIO.closeOut outstream )

end

end

