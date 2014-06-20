
structure TypedAst =
struct

local

open Identifier Type

in

datatype typedexp = 
    VarExp of path * ty
  | ConExp of path * (typedexp * ty) list
  | IntExp of int
  | WordExp of int
  | RealExp of string
  | StringExp of string
  | CharExp of string
  | RecordExp of (id * typedexp) list
  | TupleExp of typedexp list
  | SelectorExp of string
  | ConstraintExp of typedexp * ty
  | VectorExp of typedexp list

(*   | AppExp of {function:typedexp, argument:typedexp} *)
(*   | ListExp of typedexp list *)
  | FlatAppExp of id * ty * (typedexp * ty) list

datatype typedpat =
    VarPat of path * ty
  | IntPat of int
  | WordPat of int
  | StringPat of string
  | CharPat of string
  | WildPat of id * ty
  | RecordPat of {def:(string * typedpat) list, flexibility:bool}
  | TuplePat of typedpat list
  | ConPat of path * (typedpat * ty) list
  | LayeredPat of id * ty * typedpat
  | VectorPat of typedpat list
  | OrPat of id * ty * typedpat list
  | ConstraintPat of typedpat * ty

(*   | ListPat of typedpat list *)
  | FlatAppPat of id * ty * (typedpat * ty) list

type negation = bool

datatype fol =
    Unify  of ty  * typedexp * typedexp
  | Pred   of path * typedexp list * negation
  | Assign of id * ty * typedexp (* Unify after an application is performed *)
  | Conj   of fol list            
  | Disj   of fol list            
  | Exists of (id * ty) list * fol
  | Forall of (id * ty) list * fol

val folTrue  = Conj []
val folFalse = Disj []

datatype typedrelation = 
    Relation of {args:typedpat list, 
             body:(path * typedexp list * negation) list, 
             bvenv: ty SEnv.map}

datatype typedrb = Rb of typedrelation list * bool

datatype dbrhs = datatype Ast.dbrhs

datatype origin = Prelude | User | Lift of location * source

and location = Boundary of Equality | Center

and Equality = EQ | NONEQ

and source = FromDatatype   of path
           | FromTupletype  of int
           | FromRecordtype of id list

datatype db = datatype Ast.db

datatype tb = datatype Ast.tb

datatype typeddecl = 
    RelationDec    of id list * typedrb list * tyvar list
  | RelationSigDec of id * (ty * inout option) list
  | DatatypeDec    of (db * origin) list * (tb list) option
  | TypeDec        of tb list
  | FolDec         of (id * fol) list * tyvar list

end

end

