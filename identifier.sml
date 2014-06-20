
structure Identifier =

struct

type id = string

type path = string list

fun pr []   = ""
  | pr [id] = id
  | pr (id::path) = id ^ foldl (fn (id,s) => "." ^ id) "" path

(* Type, constructor, function, and structure names *)

val opId = "op"

val trueCon = "true"
val falseCon = "false"
val nilCon = "nil"
val neutralConsCon = opId ^ "::"
val infixConsCon = "::"
val refCon = "ref"

val arrowTycon = "->"
val unitTycon = "unit"
val boolTycon = "bool"
val intTycon = "int"
val wordTycon = "word"
val realTycon = "real"
val stringTycon = "string"
val charTycon = "char"
val listTycon = "list"
val refTycon = "ref"
val exnTycon = "exn"
val vecTycon = "vector"
val optionTycon = "option"

val vectorStr = "Vector"
val listStr = "List"
val optionStr = "Option"

val pathListConcat = [listStr, "concat"]
val pathEq = [ "op=" ]
val pathOptionSOME = [optionStr, "SOME" ]
val pathOptionNONE = [optionStr, "NONE" ]

fun isListCons [con] = if con=infixConsCon 
                       then SOME ([neutralConsCon]) 
                       else NONE
  | isListCons [str,con] = if str=listStr andalso con=infixConsCon 
                           then SOME [str,con] 
                           else NONE
  | isListCons _ = NONE

(* This should be fixed *)
fun isSameId env path1 path2 = path1=path2

val symTrans = 
    foldl 
    (fn ((c,s),env) => CEnv.insert(env,c,s)) 
    CEnv.empty
    [
      (#" ", "sp"),
      (#"'", "qu"),
      (#"(", "bp"),
      (#")", "ep"),
      (#"{", "bb"),
      (#"}", "eb"),
      (#":", "co"),
      (#"*", "st"),
      (#".", "dot"),
      (#"!", "ex"),
      (#"%", "pe"),
      (#"&", "em"),
      (#"#", "sh"),
      (#"+", "pl"),
      (#"-", "mi"),
      (#"/", "sl"),
      (#"<", "lt"),
      (#"=", "eq"),
      (#">", "gt"),
      (#"?", "qm"),
      (#"@", "at"),
      (#"\\", "ba"),
      (#"~", "ti"),
      (#"`", "bq"),
      (#"^", "an"),
      (#"|", "vb"),
      (#",", "comma")
    ]

fun tr c = case CEnv.find (symTrans,c) of
             SOME s => s
           | NONE => String.str c

fun liftRel id   = "REL" ^ id
fun lift id   = "L" ^ id
fun replicate id   = "O" ^ id
fun itself id = "THE" ^ id
fun arg id = "ARG" ^ id

fun orpatId id   = "ORPAT_" ^ id
fun wildId id    = "WILD_" ^ id
fun flatappId id = "FLATAPP_" ^ id

fun liftTranslate id = lift (String.translate tr id)

fun itselfTranslate id = itself (String.translate tr id)

fun tupletyid arity = "TUPLETYPE" ^ Int.toString arity
fun tupleconid arity = "TUPLECON" ^ Int.toString arity

fun recordtyid labels = 
    let val prefix = "RECORDTYPE"
        val id = foldl (fn (label,s) => s ^ label) prefix labels
    in  id
    end

fun recordconid labels = 
    let val prefix = "RECORDCON"
        val id = foldl (fn (label,s) => s ^ label) prefix labels
    in  id
    end

end

