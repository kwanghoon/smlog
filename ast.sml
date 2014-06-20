
structure Ast =
struct

local

open Identifier Type

in

datatype exp = 
    VarExp of path
  | FlatAppExp of exp list
  | AppExp of {function:exp, argument:exp}
  | IntExp of int
  | WordExp of int
  | RealExp of string
  | StringExp of string
  | CharExp of string
  | RecordExp of (id * exp) list
  | ListExp of exp list
  | TupleExp of exp list
  | SelectorExp of string
  | ConstraintExp of {expr:exp,constraint:ty}
  | VectorExp of exp list
  | FnExp of rule list
  | HandleExp of {expr:exp, rules:rule list}
  | RaiseExp of exp

and pat =
    VarPat of path
  | IntPat of int
  | WordPat of int
  | StringPat of string
  | CharPat of string
  | WildPat
  | RecordPat of {def:(string * pat) list, flexibility:bool}
  | ListPat of pat list
  | TuplePat of pat list
  | FlatAppPat of pat list
  | ConstraintPat of {pattern:pat,constraint:ty}
  | LayeredPat of {varPat:pat,expPat:pat}
  | VectorPat of pat list
  | OrPat of pat list

and rule = Rule of {pat:pat, exp:exp}

datatype dbrhs =
    Constrs of ( id * ty option ) list
  | Repl of id list

datatype db = Db of {tyc:id, tyvars:tyvar list, rhs:dbrhs}

datatype tb = Tb of {tyc:id, tyvars:tyvar list, def:ty}

datatype fb = Fb of clause list

and clause = Clause of {pats:pat list, resultty: ty option, exp:exp}

datatype rb = Rb of id * relation list * bool

and relation = Relation of {name:id, args: pat list, body:(path * exp list * id option) list}

datatype decl = 
    RelationDec     of rb list * tyvar list
  | RelationSigDec  of id * (ty * inout option) list
  | DatatypeDec     of db list * (tb list) option
  | TypeDec         of tb list
  | FunDec          of fb list * tyvar list

(* printing *)

end

end

