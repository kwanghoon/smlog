type Ostring = string

datatype Lstring = Lstring of (Ostring * Lstring Option.option ref) | THEstring of Ostring

fun PROJLstring (Lstring (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLstring (Lstring (_, (ref (Option.SOME ARGt))))  = (PROJLstring ARGt)

| PROJLstring (THEstring ARGt)  = ARGt

fun EMBEDLstring ARGt  = (THEstring ARGt)


fun OCCURLstring ARG0 (Lstring (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLstring ARG0 (Lstring (_, (ref (Option.SOME ARG2))))  = (OCCURLstring ARG0 ARG2)

| OCCURLstring ARG0 (THEstring _)  = false

fun UNIFYLstring (ARGt1, (Lstring (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLstring (ARGt1, ARGt2) ARGtc)

| UNIFYLstring (ARGt1, (Lstring (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLstring ARG0 ARGt1)
)

| UNIFYLstring ((Lstring (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLstring (ARGt1, ARGt2) ARGtc)

| UNIFYLstring ((ARGt1 as (Lstring (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLstring (ARGt2, ARGt1) ARGtc)

| UNIFYLstring ((THEstring ARGt1), (THEstring ARGt2)) ARGtc  = ((op= (ARGt1, ARGt2))
, ARGtc)

fun PRINTLstring (Lstring (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLstring (Lstring (_, (ref (Option.SOME ARGt))))  = (PRINTLstring ARGt)

| PRINTLstring (THEstring ARGt)  = (String.concat ["\"", ARGt, "\""])


type Ounit = unit

datatype Lunit = Lunit of (Ostring * Lunit Option.option ref) | THEunit of Ounit

fun PROJLunit (Lunit (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLunit (Lunit (_, (ref (Option.SOME ARGt))))  = (PROJLunit ARGt)

| PROJLunit (THEunit ARGt)  = ARGt

fun EMBEDLunit ARGt  = (THEunit ARGt)


fun OCCURLunit ARG0 (Lunit (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLunit ARG0 (Lunit (_, (ref (Option.SOME ARG2))))  = (OCCURLunit ARG0 ARG2)

| OCCURLunit ARG0 (THEunit _)  = false

fun UNIFYLunit (ARGt1, (Lunit (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLunit (ARGt1, ARGt2) ARGtc)

| UNIFYLunit (ARGt1, (Lunit (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLunit ARG0 ARGt1)
)

| UNIFYLunit ((Lunit (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLunit (ARGt1, ARGt2) ARGtc)

| UNIFYLunit ((ARGt1 as (Lunit (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLunit (ARGt2, ARGt1) ARGtc)

| UNIFYLunit ((THEunit ARGt1), (THEunit ARGt2)) ARGtc  = ((op= (ARGt1, ARGt2))
, ARGtc)

fun PRINTLunit (Lunit (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLunit (Lunit (_, (ref (Option.SOME ARGt))))  = (PRINTLunit ARGt)

| PRINTLunit (THEunit ARGt)  = "()"

datatype Obool = datatype bool

datatype Lbool = Lbool of (Ostring * Lbool Option.option ref) | Ltrue | Lfalse

fun PROJLbool (Lbool (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLbool (Lbool (_, (ref (Option.SOME ARGt))))  = (PROJLbool ARGt)

| PROJLbool Ltrue  = true
| PROJLbool Lfalse  = false

fun EMBEDLbool true  = Ltrue
| EMBEDLbool false  = Lfalse

fun OCCURLbool ARG0 (Lbool (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLbool ARG0 (Lbool (_, (ref (Option.SOME ARG2))))  = (OCCURLbool ARG0 ARG2)

| OCCURLbool ARG0 Ltrue  = false
| OCCURLbool ARG0 Lfalse  = false

fun UNIFYLbool (ARGt1, (Lbool (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLbool (ARGt1, ARGt2) ARGtc)

| UNIFYLbool (ARGt1, (Lbool (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLbool ARG0 ARGt1)
)

| UNIFYLbool ((Lbool (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLbool (ARGt1, ARGt2) ARGtc)

| UNIFYLbool ((ARGt1 as (Lbool (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLbool (ARGt2, ARGt1) ARGtc)

| UNIFYLbool (Ltrue, Ltrue) ARGtc  = (true, ARGtc)
| UNIFYLbool (Lfalse, Lfalse) ARGtc  = (true, ARGtc)
| UNIFYLbool (_, _) ARGtc  = (false, ARGtc)

fun PRINTLbool (Lbool (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLbool (Lbool (_, (ref (Option.SOME ARGt))))  = (PRINTLbool ARGt)

| PRINTLbool Ltrue  = "true"
| PRINTLbool Lfalse  = "false"

type Oint = int

datatype Lint = Lint of (Ostring * Lint Option.option ref) | THEint of Oint

fun PROJLint (Lint (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLint (Lint (_, (ref (Option.SOME ARGt))))  = (PROJLint ARGt)

| PROJLint (THEint ARGt)  = ARGt

fun EMBEDLint ARGt  = (THEint ARGt)


fun OCCURLint ARG0 (Lint (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLint ARG0 (Lint (_, (ref (Option.SOME ARG2))))  = (OCCURLint ARG0 ARG2)

| OCCURLint ARG0 (THEint _)  = false

fun UNIFYLint (ARGt1, (Lint (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLint (ARGt1, ARGt2) ARGtc)

| UNIFYLint (ARGt1, (Lint (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLint ARG0 ARGt1)
)

| UNIFYLint ((Lint (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLint (ARGt1, ARGt2) ARGtc)

| UNIFYLint ((ARGt1 as (Lint (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLint (ARGt2, ARGt1) ARGtc)

| UNIFYLint ((THEint ARGt1), (THEint ARGt2)) ARGtc  = ((op= (ARGt1, ARGt2))
, ARGtc)

fun PRINTLint (Lint (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLint (Lint (_, (ref (Option.SOME ARGt))))  = (PRINTLint ARGt)

| PRINTLint (THEint ARGt)  = (Int.toString ARGt)


type Oword = word

datatype Lword = Lword of (Ostring * Lword Option.option ref) | THEword of Oword

fun PROJLword (Lword (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLword (Lword (_, (ref (Option.SOME ARGt))))  = (PROJLword ARGt)

| PROJLword (THEword ARGt)  = ARGt

fun EMBEDLword ARGt  = (THEword ARGt)


fun OCCURLword ARG0 (Lword (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLword ARG0 (Lword (_, (ref (Option.SOME ARG2))))  = (OCCURLword ARG0 ARG2)

| OCCURLword ARG0 (THEword _)  = false

fun UNIFYLword (ARGt1, (Lword (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLword (ARGt1, ARGt2) ARGtc)

| UNIFYLword (ARGt1, (Lword (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLword ARG0 ARGt1)
)

| UNIFYLword ((Lword (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLword (ARGt1, ARGt2) ARGtc)

| UNIFYLword ((ARGt1 as (Lword (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLword (ARGt2, ARGt1) ARGtc)

| UNIFYLword ((THEword ARGt1), (THEword ARGt2)) ARGtc  = ((op= (ARGt1, ARGt2))
, ARGtc)

fun PRINTLword (Lword (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLword (Lword (_, (ref (Option.SOME ARGt))))  = (PRINTLword ARGt)

| PRINTLword (THEword ARGt)  = (Word.toString ARGt)


type Ochar = char

datatype Lchar = Lchar of (Ostring * Lchar Option.option ref) | THEchar of Ochar

fun PROJLchar (Lchar (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLchar (Lchar (_, (ref (Option.SOME ARGt))))  = (PROJLchar ARGt)

| PROJLchar (THEchar ARGt)  = ARGt

fun EMBEDLchar ARGt  = (THEchar ARGt)


fun OCCURLchar ARG0 (Lchar (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLchar ARG0 (Lchar (_, (ref (Option.SOME ARG2))))  = (OCCURLchar ARG0 ARG2)

| OCCURLchar ARG0 (THEchar _)  = false

fun UNIFYLchar (ARGt1, (Lchar (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLchar (ARGt1, ARGt2) ARGtc)

| UNIFYLchar (ARGt1, (Lchar (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLchar ARG0 ARGt1)
)

| UNIFYLchar ((Lchar (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLchar (ARGt1, ARGt2) ARGtc)

| UNIFYLchar ((ARGt1 as (Lchar (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLchar (ARGt2, ARGt1) ARGtc)

| UNIFYLchar ((THEchar ARGt1), (THEchar ARGt2)) ARGtc  = ((op= (ARGt1, ARGt2))
, ARGtc)

fun PRINTLchar (Lchar (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLchar (Lchar (_, (ref (Option.SOME ARGt))))  = (PRINTLchar ARGt)

| PRINTLchar (THEchar ARGt)  = (Char.toString ARGt)


datatype ('a0, 'a1) TUPLETYPE2 = TUPLETYPE2 of (Ostring * ('a0 , 'a1) TUPLETYPE2 Option.option ref) | TUPLECON2 of ('a0 * 'a1)

fun PROJTUPLETYPE2 (qua0, qua1) (TUPLETYPE2 (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJTUPLETYPE2 (qua0, qua1) (TUPLETYPE2 (_, (ref (Option.SOME ARGt))))  = ((PROJTUPLETYPE2 (qua0, qua1))
 ARGt)

| PROJTUPLETYPE2 (qua0, qua1) (TUPLECON2 (ARGt0, ARGt1))  = ((qua0 ARGt0)
, (qua1 ARGt1)
)

fun EMBEDTUPLETYPE2 (qua0, qua1) (ARGt0, ARGt1)  = (TUPLECON2 ((qua0 ARGt0)
, (qua1 ARGt1)
))


fun OCCURTUPLETYPE2 (qua0, qua1) ARG0 (TUPLETYPE2 (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURTUPLETYPE2 (qua0, qua1) ARG0 (TUPLETYPE2 (_, (ref (Option.SOME ARG2))))  = ((OCCURTUPLETYPE2 (qua0, qua1))
 ARG0 ARG2)

| OCCURTUPLETYPE2 (qua0, qua1) ARG0 (TUPLECON2 ARG1)  = ((fn ARGv =>
(fn (ARG0, ARG1) =>
((fn true =>
true | false =>
(qua1 ARGv ARG1)
) ((fn true =>
true | false =>
(qua0 ARGv ARG0)
) false)
)
)) ARG0 ARG1)


fun UNIFYTUPLETYPE2 (qua0, qua1) (ARGt1, (TUPLETYPE2 (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYTUPLETYPE2 (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYTUPLETYPE2 (qua0, qua1) (ARGt1, (TUPLETYPE2 (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURTUPLETYPE2 (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
))
 ARG0 ARGt1)
)

| UNIFYTUPLETYPE2 (qua0, qua1) ((TUPLETYPE2 (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYTUPLETYPE2 (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYTUPLETYPE2 (qua0, qua1) ((ARGt1 as (TUPLETYPE2 (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYTUPLETYPE2 (qua0, qua1))
 (ARGt2, ARGt1) ARGtc)

| UNIFYTUPLETYPE2 (qua0, qua1) ((TUPLECON2 ARGt1), (TUPLECON2 ARGt2)) ARGtc  = ((fn ((ARG10, ARG11), (ARG20, ARG21)) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (ARG11, ARG21) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (ARG10, ARG20) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
) (ARGt1, ARGt2))


fun PRINTTUPLETYPE2 (qua0, qua1) (TUPLETYPE2 (ARGs, (ref Option.NONE)))  = ARGs
| PRINTTUPLETYPE2 (qua0, qua1) (TUPLETYPE2 (_, (ref (Option.SOME ARGt))))  = ((PRINTTUPLETYPE2 (qua0, qua1))
 ARGt)

| PRINTTUPLETYPE2 (qua0, qua1) (TUPLECON2 (ARGt0, ARGt1))  = (String.concat ["(", (SMLOG.intersperse "," [(qua0 ARGt0)
, (qua1 ARGt1)
])
, ")"])


datatype Olist = datatype list

datatype 'a Llist = Llist of (Ostring * 'a Llist Option.option ref) | Lnil | Lopcoco of ('a , 'a Llist) TUPLETYPE2

fun PROJLlist (qua) (Llist (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLlist (qua) (Llist (_, (ref (Option.SOME ARGt))))  = ((PROJLlist (qua))
 ARGt)

| PROJLlist (qua) Lnil  = nil
| PROJLlist (qua) (Lopcoco ARGt)  = (op:: ((PROJTUPLETYPE2 ((fn ARGt =>
(qua ARGt)
), (fn ARGt =>
((PROJLlist (fn ARGt =>
(qua ARGt)
))
 ARGt)
)))
 ARGt)
)


fun EMBEDLlist (qua) nil  = Lnil
| EMBEDLlist (qua) (op:: ARGt)  = (Lopcoco ((EMBEDTUPLETYPE2 ((fn ARGt =>
(qua ARGt)
), (fn ARGt =>
((EMBEDLlist (fn ARGt =>
(qua ARGt)
))
 ARGt)
)))
 ARGt)
)


fun OCCURLlist (qua) ARG0 (Llist (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLlist (qua) ARG0 (Llist (_, (ref (Option.SOME ARG2))))  = ((OCCURLlist (qua))
 ARG0 ARG2)

| OCCURLlist (qua) ARG0 Lnil  = false
| OCCURLlist (qua) ARG0 (Lopcoco ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(qua ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(qua ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARG0 ARG1)


fun UNIFYLlist qua (ARGt1, (Llist (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYLlist (qua))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLlist qua (ARGt1, (Llist (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURLlist ((fn (ARG1, ARG2) =>
ARG2) qua)
)
 ARG0 ARGt1)
)

| UNIFYLlist qua ((Llist (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYLlist (qua))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLlist qua ((ARGt1 as (Llist (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYLlist (qua))
 (ARGt2, ARGt1) ARGtc)

| UNIFYLlist qua (Lnil, Lnil) ARGtc  = (true, ARGtc)
| UNIFYLlist qua ((Lopcoco ARGt1), (Lopcoco ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(((fn (ARG1, ARG2) =>
ARG1) qua)
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(((fn (ARG1, ARG2) =>
ARG2) qua)
 ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(((fn (ARG1, ARG2) =>
ARG1) qua)
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(((fn (ARG1, ARG2) =>
ARG2) qua)
 ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(((fn (ARG1, ARG2) =>
ARG2) qua)
 ARGv ARGt)
)))
 ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLlist qua (_, _) ARGtc  = (false, ARGtc)

fun PRINTLlist (qua) (Llist (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLlist (qua) (Llist (_, (ref (Option.SOME ARGt))))  = ((PRINTLlist (qua))
 ARGt)

| PRINTLlist (qua) Lnil  = "nil"
| PRINTLlist (qua) (Lopcoco ARGt)  = (String.concat ["op::", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(qua ARGt)
), (fn ARGt =>
((PRINTLlist (fn ARGt =>
(qua ARGt)
))
 ARGt)
)))
 ARGt)
])


datatype ty = Int | Fun of (ty list * ty)

datatype Oty = datatype ty

datatype Lty = Lty of (Ostring * Lty Option.option ref) | LInt | LFun of (Lty Llist , Lty) TUPLETYPE2

fun PROJLty (Lty (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLty (Lty (_, (ref (Option.SOME ARGt))))  = (PROJLty ARGt)

| PROJLty LInt  = Int
| PROJLty (LFun ARGt)  = (Fun ((PROJTUPLETYPE2 ((fn ARGt =>
((PROJLlist (fn ARGt =>
(PROJLty ARGt)
))
 ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
)


fun EMBEDLty Int  = LInt
| EMBEDLty (Fun ARGt)  = (LFun ((EMBEDTUPLETYPE2 ((fn ARGt =>
((EMBEDLlist (fn ARGt =>
(EMBEDLty ARGt)
))
 ARGt)
), (fn ARGt =>
(EMBEDLty ARGt)
)))
 ARGt)
)


fun OCCURLty ARG0 (Lty (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLty ARG0 (Lty (_, (ref (Option.SOME ARG2))))  = (OCCURLty ARG0 ARG2)

| OCCURLty ARG0 LInt  = false
| OCCURLty ARG0 (LFun ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))
 ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 ARG0 ARG1)


fun UNIFYLty (ARGt1, (Lty (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLty (ARGt1, ARGt2) ARGtc)

| UNIFYLty (ARGt1, (Lty (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLty ARG0 ARGt1)
)

| UNIFYLty ((Lty (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLty (ARGt1, ARGt2) ARGtc)

| UNIFYLty ((ARGt1 as (Lty (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLty (ARGt2, ARGt1) ARGtc)

| UNIFYLty (LInt, LInt) ARGtc  = (true, ARGtc)
| UNIFYLty ((LFun ARGt1), (LFun ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))
 ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLty (_, _) ARGtc  = (false, ARGtc)

fun PRINTLty (Lty (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLty (Lty (_, (ref (Option.SOME ARGt))))  = (PRINTLty ARGt)

| PRINTLty LInt  = "Int"
| PRINTLty (LFun ARGt)  = (String.concat ["Fun", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLty ARGt)
))
 ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
])


datatype term = Var of string | App of (term * term) | Lam of (string * term)

datatype Oterm = datatype term

datatype Lterm = Lterm of (Ostring * Lterm Option.option ref) | LVar of Lstring | LApp of (Lterm , Lterm) TUPLETYPE2 | LLam of (Lstring , Lterm) TUPLETYPE2

fun PROJLterm (Lterm (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLterm (Lterm (_, (ref (Option.SOME ARGt))))  = (PROJLterm ARGt)

| PROJLterm (LVar ARGt)  = (Var (PROJLstring ARGt)
)

| PROJLterm (LApp ARGt)  = (App ((PROJTUPLETYPE2 ((fn ARGt =>
(PROJLterm ARGt)
), (fn ARGt =>
(PROJLterm ARGt)
)))
 ARGt)
)

| PROJLterm (LLam ARGt)  = (Lam ((PROJTUPLETYPE2 ((fn ARGt =>
(PROJLstring ARGt)
), (fn ARGt =>
(PROJLterm ARGt)
)))
 ARGt)
)


fun EMBEDLterm (Var ARGt)  = (LVar (EMBEDLstring ARGt)
)

| EMBEDLterm (App ARGt)  = (LApp ((EMBEDTUPLETYPE2 ((fn ARGt =>
(EMBEDLterm ARGt)
), (fn ARGt =>
(EMBEDLterm ARGt)
)))
 ARGt)
)

| EMBEDLterm (Lam ARGt)  = (LLam ((EMBEDTUPLETYPE2 ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(EMBEDLterm ARGt)
)))
 ARGt)
)


fun OCCURLterm ARG0 (Lterm (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLterm ARG0 (Lterm (_, (ref (Option.SOME ARG2))))  = (OCCURLterm ARG0 ARG2)

| OCCURLterm ARG0 (LVar ARG1)  = (OCCURLstring ARG0 ARG1)

| OCCURLterm ARG0 (LApp ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLterm ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLterm ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLterm ARG0 (LLam ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLterm ARGv ARGt)
))))
 ARG0 ARG1)


fun UNIFYLterm (ARGt1, (Lterm (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLterm (ARGt1, ARGt2) ARGtc)

| UNIFYLterm (ARGt1, (Lterm (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLterm ARG0 ARGt1)
)

| UNIFYLterm ((Lterm (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLterm (ARGt1, ARGt2) ARGtc)

| UNIFYLterm ((ARGt1 as (Lterm (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLterm (ARGt2, ARGt1) ARGtc)

| UNIFYLterm ((LVar ARGt1), (LVar ARGt2)) ARGtc  = (UNIFYLstring (ARGt1, ARGt2) ARGtc)

| UNIFYLterm ((LApp ARGt1), (LApp ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLterm (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLterm ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLterm (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLterm ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLterm ((LLam ARGt1), (LLam ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLterm (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLterm ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLterm (_, _) ARGtc  = (false, ARGtc)

fun PRINTLterm (Lterm (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLterm (Lterm (_, (ref (Option.SOME ARGt))))  = (PRINTLterm ARGt)

| PRINTLterm (LVar ARGt)  = (String.concat ["Var", " ", (PRINTLstring ARGt)
])

| PRINTLterm (LApp ARGt)  = (String.concat ["App", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLterm ARGt)
), (fn ARGt =>
(PRINTLterm ARGt)
)))
 ARGt)
])

| PRINTLterm (LLam ARGt)  = (String.concat ["Lam", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLterm ARGt)
)))
 ARGt)
])


type typingenv = (string * ty) list

type delta = ty list

fun RELtoStr (ARG0 : Lterm, ARG1 : Lstring)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lstring (fn Y : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG0 (LVar Y)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG1 Y ARGtc)
)))
))
))
, (SMLOG.EXISTS Lterm (fn M : Lterm =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.EXISTS Lstring (fn Y : Lstring =>
(SMLOG.EXISTS Lstring (fn Y' : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG0 (LLam (TUPLECON2 (X, M))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG1 Y' ARGtc)
)))
, (RELtoStr (M, Y))
))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG0 Y' ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLstring ARG0)
)) (concat ((SMLOG.doProj (fn ARGt =>
((PROJLlist (fn ARGt =>
(PROJLstring ARGt)
))
 ARGt)
) (Lopcoco (TUPLECON2 ((THEstring "(")
, (Lopcoco (TUPLECON2 ((THEstring "lam ")
, (Lopcoco (TUPLECON2 (X, (Lopcoco (TUPLECON2 ((THEstring ".")
, (Lopcoco (TUPLECON2 (Y, (Lopcoco (TUPLECON2 ((THEstring ")")
, (Lnil)
))
)
))
)
))
)
))
)
))
)
))
)
 ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
))
))
))
))
))
, (SMLOG.EXISTS Lterm (fn M1 : Lterm =>
(SMLOG.EXISTS Lterm (fn M2 : Lterm =>
(SMLOG.EXISTS Lstring (fn S1 : Lstring =>
(SMLOG.EXISTS Lstring (fn S2 : Lstring =>
(SMLOG.EXISTS Lstring (fn Y' : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG0 (LApp (TUPLECON2 (M1, M2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG1 Y' ARGtc)
)))
, (RELtoStr (M1, S1))
))
, (RELtoStr (M2, S2))
))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG0 Y' ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLstring ARG0)
)) (concat ((SMLOG.doProj (fn ARGt =>
((PROJLlist (fn ARGt =>
(PROJLstring ARGt)
))
 ARGt)
) (Lopcoco (TUPLECON2 ((THEstring "(")
, (Lopcoco (TUPLECON2 (S1, (Lopcoco (TUPLECON2 ((THEstring " ")
, (Lopcoco (TUPLECON2 (S2, (Lopcoco (TUPLECON2 ((THEstring ")")
, (Lnil)
))
)
))
)
))
)
))
)
))
)
 ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
))
))
))
))
))
))
)


fun toStr (ARG0, ARG1)  = ((fn (ARG0, ARG1) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLterm ARG0)
, (PRINTLstring ARG1)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELtoStr (ARG0, ARG1) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLterm ARG0)
 | Option.NONE =>
(Lterm ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLstring ARG1)
 | Option.NONE =>
(Lstring ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
))


datatype ('a0, 'a1, 'a2, 'a3) TUPLETYPE4 = TUPLETYPE4 of (Ostring * ('a0 , 'a1 , 'a2 , 'a3) TUPLETYPE4 Option.option ref) | TUPLECON4 of ('a0 * 'a1 * 'a2 * 'a3)

fun PROJTUPLETYPE4 (qua0, qua1, qua2, qua3) (TUPLETYPE4 (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJTUPLETYPE4 (qua0, qua1, qua2, qua3) (TUPLETYPE4 (_, (ref (Option.SOME ARGt))))  = ((PROJTUPLETYPE4 (qua0, qua1, qua2, qua3))
 ARGt)

| PROJTUPLETYPE4 (qua0, qua1, qua2, qua3) (TUPLECON4 (ARGt0, ARGt1, ARGt2, ARGt3))  = ((qua0 ARGt0)
, (qua1 ARGt1)
, (qua2 ARGt2)
, (qua3 ARGt3)
)

fun EMBEDTUPLETYPE4 (qua0, qua1, qua2, qua3) (ARGt0, ARGt1, ARGt2, ARGt3)  = (TUPLECON4 ((qua0 ARGt0)
, (qua1 ARGt1)
, (qua2 ARGt2)
, (qua3 ARGt3)
))


fun OCCURTUPLETYPE4 (qua0, qua1, qua2, qua3) ARG0 (TUPLETYPE4 (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURTUPLETYPE4 (qua0, qua1, qua2, qua3) ARG0 (TUPLETYPE4 (_, (ref (Option.SOME ARG2))))  = ((OCCURTUPLETYPE4 (qua0, qua1, qua2, qua3))
 ARG0 ARG2)

| OCCURTUPLETYPE4 (qua0, qua1, qua2, qua3) ARG0 (TUPLECON4 ARG1)  = ((fn ARGv =>
(fn (ARG0, ARG1, ARG2, ARG3) =>
((fn true =>
true | false =>
(qua3 ARGv ARG3)
) ((fn true =>
true | false =>
(qua2 ARGv ARG2)
) ((fn true =>
true | false =>
(qua1 ARGv ARG1)
) ((fn true =>
true | false =>
(qua0 ARGv ARG0)
) false)
)
)
)
)) ARG0 ARG1)


fun UNIFYTUPLETYPE4 (qua0, qua1, qua2, qua3) (ARGt1, (TUPLETYPE4 (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYTUPLETYPE4 (qua0, qua1, qua2, qua3))
 (ARGt1, ARGt2) ARGtc)

| UNIFYTUPLETYPE4 (qua0, qua1, qua2, qua3) (ARGt1, (TUPLETYPE4 (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURTUPLETYPE4 (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
, ((fn (ARG1, ARG2) =>
ARG2) qua2)
, ((fn (ARG1, ARG2) =>
ARG2) qua3)
))
 ARG0 ARGt1)
)

| UNIFYTUPLETYPE4 (qua0, qua1, qua2, qua3) ((TUPLETYPE4 (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYTUPLETYPE4 (qua0, qua1, qua2, qua3))
 (ARGt1, ARGt2) ARGtc)

| UNIFYTUPLETYPE4 (qua0, qua1, qua2, qua3) ((ARGt1 as (TUPLETYPE4 (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYTUPLETYPE4 (qua0, qua1, qua2, qua3))
 (ARGt2, ARGt1) ARGtc)

| UNIFYTUPLETYPE4 (qua0, qua1, qua2, qua3) ((TUPLECON4 ARGt1), (TUPLECON4 ARGt2)) ARGtc  = ((fn ((ARG10, ARG11, ARG12, ARG13), (ARG20, ARG21, ARG22, ARG23)) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua3)
 (ARG13, ARG23) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua2)
 (ARG12, ARG22) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (ARG11, ARG21) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (ARG10, ARG20) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
)
)
) (ARGt1, ARGt2))


fun PRINTTUPLETYPE4 (qua0, qua1, qua2, qua3) (TUPLETYPE4 (ARGs, (ref Option.NONE)))  = ARGs
| PRINTTUPLETYPE4 (qua0, qua1, qua2, qua3) (TUPLETYPE4 (_, (ref (Option.SOME ARGt))))  = ((PRINTTUPLETYPE4 (qua0, qua1, qua2, qua3))
 ARGt)

| PRINTTUPLETYPE4 (qua0, qua1, qua2, qua3) (TUPLECON4 (ARGt0, ARGt1, ARGt2, ARGt3))  = (String.concat ["(", (SMLOG.intersperse "," [(qua0 ARGt0)
, (qua1 ARGt1)
, (qua2 ARGt2)
, (qua3 ARGt3)
])
, ")"])


datatype judgment = J of (typingenv * delta * term * ty)

datatype Ojudgment = datatype judgment

datatype Ljudgment = Ljudgment of (Ostring * Ljudgment Option.option ref) | LJ of ((Lstring , Lty) TUPLETYPE2 Llist , Lty Llist , Lterm , Lty) TUPLETYPE4

fun PROJLjudgment (Ljudgment (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLjudgment (Ljudgment (_, (ref (Option.SOME ARGt))))  = (PROJLjudgment ARGt)

| PROJLjudgment (LJ ARGt)  = (J ((PROJTUPLETYPE4 ((fn ARGt =>
((PROJLlist (fn ARGt =>
((PROJTUPLETYPE2 ((fn ARGt =>
(PROJLstring ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
))
 ARGt)
), (fn ARGt =>
((PROJLlist (fn ARGt =>
(PROJLty ARGt)
))
 ARGt)
), (fn ARGt =>
(PROJLterm ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
)


fun EMBEDLjudgment (J ARGt)  = (LJ ((EMBEDTUPLETYPE4 ((fn ARGt =>
((EMBEDLlist (fn ARGt =>
((EMBEDTUPLETYPE2 ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(EMBEDLty ARGt)
)))
 ARGt)
))
 ARGt)
), (fn ARGt =>
((EMBEDLlist (fn ARGt =>
(EMBEDLty ARGt)
))
 ARGt)
), (fn ARGt =>
(EMBEDLterm ARGt)
), (fn ARGt =>
(EMBEDLty ARGt)
)))
 ARGt)
)


fun OCCURLjudgment ARG0 (Ljudgment (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLjudgment ARG0 (Ljudgment (_, (ref (Option.SOME ARG2))))  = (OCCURLjudgment ARG0 ARG2)

| OCCURLjudgment ARG0 (LJ ARG1)  = ((OCCURTUPLETYPE4 ((fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 ARGv ARGt)
)))
 ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))
 ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLterm ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 ARG0 ARG1)


fun UNIFYLjudgment (ARGt1, (Ljudgment (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLjudgment (ARGt1, ARGt2) ARGtc)

| UNIFYLjudgment (ARGt1, (Ljudgment (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLjudgment ARG0 ARGt1)
)

| UNIFYLjudgment ((Ljudgment (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLjudgment (ARGt1, ARGt2) ARGtc)

| UNIFYLjudgment ((ARGt1 as (Ljudgment (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLjudgment (ARGt2, ARGt1) ARGtc)

| UNIFYLjudgment ((LJ ARGt1), (LJ ARGt2)) ARGtc  = ((UNIFYTUPLETYPE4 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 ARGv ARGt)
)))
 ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))
 ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLterm (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLterm ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)


fun PRINTLjudgment (Ljudgment (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLjudgment (Ljudgment (_, (ref (Option.SOME ARGt))))  = (PRINTLjudgment ARGt)

| PRINTLjudgment (LJ ARGt)  = (String.concat ["J", " ", ((PRINTTUPLETYPE4 ((fn ARGt =>
((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
))
 ARGt)
), (fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLty ARGt)
))
 ARGt)
), (fn ARGt =>
(PRINTLterm ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
])


datatype ('a0, 'a1, 'a2) TUPLETYPE3 = TUPLETYPE3 of (Ostring * ('a0 , 'a1 , 'a2) TUPLETYPE3 Option.option ref) | TUPLECON3 of ('a0 * 'a1 * 'a2)

fun PROJTUPLETYPE3 (qua0, qua1, qua2) (TUPLETYPE3 (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJTUPLETYPE3 (qua0, qua1, qua2) (TUPLETYPE3 (_, (ref (Option.SOME ARGt))))  = ((PROJTUPLETYPE3 (qua0, qua1, qua2))
 ARGt)

| PROJTUPLETYPE3 (qua0, qua1, qua2) (TUPLECON3 (ARGt0, ARGt1, ARGt2))  = ((qua0 ARGt0)
, (qua1 ARGt1)
, (qua2 ARGt2)
)

fun EMBEDTUPLETYPE3 (qua0, qua1, qua2) (ARGt0, ARGt1, ARGt2)  = (TUPLECON3 ((qua0 ARGt0)
, (qua1 ARGt1)
, (qua2 ARGt2)
))


fun OCCURTUPLETYPE3 (qua0, qua1, qua2) ARG0 (TUPLETYPE3 (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURTUPLETYPE3 (qua0, qua1, qua2) ARG0 (TUPLETYPE3 (_, (ref (Option.SOME ARG2))))  = ((OCCURTUPLETYPE3 (qua0, qua1, qua2))
 ARG0 ARG2)

| OCCURTUPLETYPE3 (qua0, qua1, qua2) ARG0 (TUPLECON3 ARG1)  = ((fn ARGv =>
(fn (ARG0, ARG1, ARG2) =>
((fn true =>
true | false =>
(qua2 ARGv ARG2)
) ((fn true =>
true | false =>
(qua1 ARGv ARG1)
) ((fn true =>
true | false =>
(qua0 ARGv ARG0)
) false)
)
)
)) ARG0 ARG1)


fun UNIFYTUPLETYPE3 (qua0, qua1, qua2) (ARGt1, (TUPLETYPE3 (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYTUPLETYPE3 (qua0, qua1, qua2))
 (ARGt1, ARGt2) ARGtc)

| UNIFYTUPLETYPE3 (qua0, qua1, qua2) (ARGt1, (TUPLETYPE3 (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURTUPLETYPE3 (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
, ((fn (ARG1, ARG2) =>
ARG2) qua2)
))
 ARG0 ARGt1)
)

| UNIFYTUPLETYPE3 (qua0, qua1, qua2) ((TUPLETYPE3 (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYTUPLETYPE3 (qua0, qua1, qua2))
 (ARGt1, ARGt2) ARGtc)

| UNIFYTUPLETYPE3 (qua0, qua1, qua2) ((ARGt1 as (TUPLETYPE3 (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYTUPLETYPE3 (qua0, qua1, qua2))
 (ARGt2, ARGt1) ARGtc)

| UNIFYTUPLETYPE3 (qua0, qua1, qua2) ((TUPLECON3 ARGt1), (TUPLECON3 ARGt2)) ARGtc  = ((fn ((ARG10, ARG11, ARG12), (ARG20, ARG21, ARG22)) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua2)
 (ARG12, ARG22) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (ARG11, ARG21) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (ARG10, ARG20) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
)
) (ARGt1, ARGt2))


fun PRINTTUPLETYPE3 (qua0, qua1, qua2) (TUPLETYPE3 (ARGs, (ref Option.NONE)))  = ARGs
| PRINTTUPLETYPE3 (qua0, qua1, qua2) (TUPLETYPE3 (_, (ref (Option.SOME ARGt))))  = ((PRINTTUPLETYPE3 (qua0, qua1, qua2))
 ARGt)

| PRINTTUPLETYPE3 (qua0, qua1, qua2) (TUPLECON3 (ARGt0, ARGt1, ARGt2))  = (String.concat ["(", (SMLOG.intersperse "," [(qua0 ARGt0)
, (qua1 ARGt1)
, (qua2 ARGt2)
])
, ")"])


datatype typingtree = VAR of judgment | LAM of (typingtree * judgment) | CLO of (typingtree * judgment) | APP of (typingtree * typingtree * judgment) | CODE of (typingtree * judgment)

datatype Otypingtree = datatype typingtree

datatype Ltypingtree = Ltypingtree of (Ostring * Ltypingtree Option.option ref) | LVAR of Ljudgment | LLAM of (Ltypingtree , Ljudgment) TUPLETYPE2 | LCLO of (Ltypingtree , Ljudgment) TUPLETYPE2 | LAPP of (Ltypingtree , Ltypingtree , Ljudgment) TUPLETYPE3 | LCODE of (Ltypingtree , Ljudgment) TUPLETYPE2

fun PROJLtypingtree (Ltypingtree (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLtypingtree (Ltypingtree (_, (ref (Option.SOME ARGt))))  = (PROJLtypingtree ARGt)

| PROJLtypingtree (LVAR ARGt)  = (VAR (PROJLjudgment ARGt)
)

| PROJLtypingtree (LLAM ARGt)  = (LAM ((PROJTUPLETYPE2 ((fn ARGt =>
(PROJLtypingtree ARGt)
), (fn ARGt =>
(PROJLjudgment ARGt)
)))
 ARGt)
)

| PROJLtypingtree (LCLO ARGt)  = (CLO ((PROJTUPLETYPE2 ((fn ARGt =>
(PROJLtypingtree ARGt)
), (fn ARGt =>
(PROJLjudgment ARGt)
)))
 ARGt)
)

| PROJLtypingtree (LAPP ARGt)  = (APP ((PROJTUPLETYPE3 ((fn ARGt =>
(PROJLtypingtree ARGt)
), (fn ARGt =>
(PROJLtypingtree ARGt)
), (fn ARGt =>
(PROJLjudgment ARGt)
)))
 ARGt)
)

| PROJLtypingtree (LCODE ARGt)  = (CODE ((PROJTUPLETYPE2 ((fn ARGt =>
(PROJLtypingtree ARGt)
), (fn ARGt =>
(PROJLjudgment ARGt)
)))
 ARGt)
)


fun EMBEDLtypingtree (VAR ARGt)  = (LVAR (EMBEDLjudgment ARGt)
)

| EMBEDLtypingtree (LAM ARGt)  = (LLAM ((EMBEDTUPLETYPE2 ((fn ARGt =>
(EMBEDLtypingtree ARGt)
), (fn ARGt =>
(EMBEDLjudgment ARGt)
)))
 ARGt)
)

| EMBEDLtypingtree (CLO ARGt)  = (LCLO ((EMBEDTUPLETYPE2 ((fn ARGt =>
(EMBEDLtypingtree ARGt)
), (fn ARGt =>
(EMBEDLjudgment ARGt)
)))
 ARGt)
)

| EMBEDLtypingtree (APP ARGt)  = (LAPP ((EMBEDTUPLETYPE3 ((fn ARGt =>
(EMBEDLtypingtree ARGt)
), (fn ARGt =>
(EMBEDLtypingtree ARGt)
), (fn ARGt =>
(EMBEDLjudgment ARGt)
)))
 ARGt)
)

| EMBEDLtypingtree (CODE ARGt)  = (LCODE ((EMBEDTUPLETYPE2 ((fn ARGt =>
(EMBEDLtypingtree ARGt)
), (fn ARGt =>
(EMBEDLjudgment ARGt)
)))
 ARGt)
)


fun OCCURLtypingtree ARG0 (Ltypingtree (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLtypingtree ARG0 (Ltypingtree (_, (ref (Option.SOME ARG2))))  = (OCCURLtypingtree ARG0 ARG2)

| OCCURLtypingtree ARG0 (LVAR ARG1)  = (OCCURLjudgment ARG0 ARG1)

| OCCURLtypingtree ARG0 (LLAM ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLtypingtree ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLjudgment ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLtypingtree ARG0 (LCLO ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLtypingtree ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLjudgment ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLtypingtree ARG0 (LAPP ARG1)  = ((OCCURTUPLETYPE3 ((fn ARGv =>
(fn ARGt =>
(OCCURLtypingtree ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtypingtree ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLjudgment ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLtypingtree ARG0 (LCODE ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLtypingtree ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLjudgment ARGv ARGt)
))))
 ARG0 ARG1)


fun UNIFYLtypingtree (ARGt1, (Ltypingtree (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLtypingtree (ARGt1, ARGt2) ARGtc)

| UNIFYLtypingtree (ARGt1, (Ltypingtree (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLtypingtree ARG0 ARGt1)
)

| UNIFYLtypingtree ((Ltypingtree (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLtypingtree (ARGt1, ARGt2) ARGtc)

| UNIFYLtypingtree ((ARGt1 as (Ltypingtree (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLtypingtree (ARGt2, ARGt1) ARGtc)

| UNIFYLtypingtree ((LVAR ARGt1), (LVAR ARGt2)) ARGtc  = (UNIFYLjudgment (ARGt1, ARGt2) ARGtc)

| UNIFYLtypingtree ((LLAM ARGt1), (LLAM ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtypingtree ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLjudgment (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLjudgment ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLtypingtree ((LCLO ARGt1), (LCLO ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtypingtree ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLjudgment (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLjudgment ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLtypingtree ((LAPP ARGt1), (LAPP ARGt2)) ARGtc  = ((UNIFYTUPLETYPE3 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtypingtree ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtypingtree ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLjudgment (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLjudgment ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLtypingtree ((LCODE ARGt1), (LCODE ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtypingtree ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLjudgment (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLjudgment ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLtypingtree (_, _) ARGtc  = (false, ARGtc)

fun PRINTLtypingtree (Ltypingtree (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLtypingtree (Ltypingtree (_, (ref (Option.SOME ARGt))))  = (PRINTLtypingtree ARGt)

| PRINTLtypingtree (LVAR ARGt)  = (String.concat ["VAR", " ", (PRINTLjudgment ARGt)
])

| PRINTLtypingtree (LLAM ARGt)  = (String.concat ["LAM", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLtypingtree ARGt)
), (fn ARGt =>
(PRINTLjudgment ARGt)
)))
 ARGt)
])

| PRINTLtypingtree (LCLO ARGt)  = (String.concat ["CLO", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLtypingtree ARGt)
), (fn ARGt =>
(PRINTLjudgment ARGt)
)))
 ARGt)
])

| PRINTLtypingtree (LAPP ARGt)  = (String.concat ["APP", " ", ((PRINTTUPLETYPE3 ((fn ARGt =>
(PRINTLtypingtree ARGt)
), (fn ARGt =>
(PRINTLtypingtree ARGt)
), (fn ARGt =>
(PRINTLjudgment ARGt)
)))
 ARGt)
])

| PRINTLtypingtree (LCODE ARGt)  = (String.concat ["CODE", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLtypingtree ARGt)
), (fn ARGt =>
(PRINTLjudgment ARGt)
)))
 ARGt)
])


fun RELttToStr (ARG0 : Ltypingtree, ARG1 : Lstring)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lstring (fn S : Lstring =>
(SMLOG.EXISTS Lty (fn WILD_169 : Lty =>
(SMLOG.EXISTS Lterm (fn WILD_170 : Lterm =>
(SMLOG.EXISTS Llist (fn WILD_171 : Lty Llist =>
(SMLOG.EXISTS Llist (fn WILD_172 : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARG0, ARG1) ARGtc)
)) ARG0 (LVAR (LJ (TUPLECON4 (WILD_172, WILD_171, WILD_170, WILD_169))
)
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG1 S ARGtc)
)))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG0 S ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLstring ARG0)
)) (concat ((SMLOG.doProj (fn ARGt =>
((PROJLlist (fn ARGt =>
(PROJLstring ARGt)
))
 ARGt)
) (Lopcoco (TUPLECON2 ((THEstring "VAR ")
, (Lnil)
))
)
 ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
))
))
))
))
))
))
, (SMLOG.EXISTS Lstring (fn S : Lstring =>
(SMLOG.EXISTS Lstring (fn S0 : Lstring =>
(SMLOG.EXISTS Ltypingtree (fn T : Ltypingtree =>
(SMLOG.EXISTS Lty (fn WILD_173 : Lty =>
(SMLOG.EXISTS Lterm (fn WILD_174 : Lterm =>
(SMLOG.EXISTS Llist (fn WILD_175 : Lty Llist =>
(SMLOG.EXISTS Llist (fn WILD_176 : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARG0, ARG1) ARGtc)
)) ARG0 (LLAM (TUPLECON2 (T, (LJ (TUPLECON4 (WILD_176, WILD_175, WILD_174, WILD_173))
)
))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG1 S ARGtc)
)))
, (RELttToStr (T, S0))
))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG0 S ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLstring ARG0)
)) (concat ((SMLOG.doProj (fn ARGt =>
((PROJLlist (fn ARGt =>
(PROJLstring ARGt)
))
 ARGt)
) (Lopcoco (TUPLECON2 ((THEstring "LAM ")
, (Lopcoco (TUPLECON2 ((THEstring "(")
, (Lopcoco (TUPLECON2 (S0, (Lopcoco (TUPLECON2 ((THEstring ")")
, (Lnil)
))
)
))
)
))
)
))
)
 ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lstring (fn S : Lstring =>
(SMLOG.EXISTS Lstring (fn S0 : Lstring =>
(SMLOG.EXISTS Ltypingtree (fn T : Ltypingtree =>
(SMLOG.EXISTS Lty (fn WILD_177 : Lty =>
(SMLOG.EXISTS Lterm (fn WILD_178 : Lterm =>
(SMLOG.EXISTS Llist (fn WILD_179 : Lty Llist =>
(SMLOG.EXISTS Llist (fn WILD_180 : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARG0, ARG1) ARGtc)
)) ARG0 (LCLO (TUPLECON2 (T, (LJ (TUPLECON4 (WILD_180, WILD_179, WILD_178, WILD_177))
)
))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG1 S ARGtc)
)))
, (RELttToStr (T, S0))
))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG0 S ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLstring ARG0)
)) (concat ((SMLOG.doProj (fn ARGt =>
((PROJLlist (fn ARGt =>
(PROJLstring ARGt)
))
 ARGt)
) (Lopcoco (TUPLECON2 ((THEstring "CLO ")
, (Lopcoco (TUPLECON2 ((THEstring "(")
, (Lopcoco (TUPLECON2 (S0, (Lopcoco (TUPLECON2 ((THEstring ")")
, (Lnil)
))
)
))
)
))
)
))
)
 ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lstring (fn S : Lstring =>
(SMLOG.EXISTS Lstring (fn S0 : Lstring =>
(SMLOG.EXISTS Ltypingtree (fn T : Ltypingtree =>
(SMLOG.EXISTS Lty (fn WILD_181 : Lty =>
(SMLOG.EXISTS Lterm (fn WILD_182 : Lterm =>
(SMLOG.EXISTS Llist (fn WILD_183 : Lty Llist =>
(SMLOG.EXISTS Llist (fn WILD_184 : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARG0, ARG1) ARGtc)
)) ARG0 (LCODE (TUPLECON2 (T, (LJ (TUPLECON4 (WILD_184, WILD_183, WILD_182, WILD_181))
)
))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG1 S ARGtc)
)))
, (RELttToStr (T, S0))
))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG0 S ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLstring ARG0)
)) (concat ((SMLOG.doProj (fn ARGt =>
((PROJLlist (fn ARGt =>
(PROJLstring ARGt)
))
 ARGt)
) (Lopcoco (TUPLECON2 ((THEstring "CODE ")
, (Lopcoco (TUPLECON2 ((THEstring "(")
, (Lopcoco (TUPLECON2 (S0, (Lopcoco (TUPLECON2 ((THEstring ")")
, (Lnil)
))
)
))
)
))
)
))
)
 ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lstring (fn S : Lstring =>
(SMLOG.EXISTS Lstring (fn S1 : Lstring =>
(SMLOG.EXISTS Lstring (fn S2 : Lstring =>
(SMLOG.EXISTS Ltypingtree (fn T1 : Ltypingtree =>
(SMLOG.EXISTS Ltypingtree (fn T2 : Ltypingtree =>
(SMLOG.EXISTS Lty (fn WILD_185 : Lty =>
(SMLOG.EXISTS Lterm (fn WILD_186 : Lterm =>
(SMLOG.EXISTS Llist (fn WILD_187 : Lty Llist =>
(SMLOG.EXISTS Llist (fn WILD_188 : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARG0, ARG1) ARGtc)
)) ARG0 (LAPP (TUPLECON3 (T1, T2, (LJ (TUPLECON4 (WILD_188, WILD_187, WILD_186, WILD_185))
)
))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG1 S ARGtc)
)))
, (RELttToStr (T1, S1))
))
, (RELttToStr (T2, S2))
))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG0 S ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLstring ARG0)
)) (concat ((SMLOG.doProj (fn ARGt =>
((PROJLlist (fn ARGt =>
(PROJLstring ARGt)
))
 ARGt)
) (Lopcoco (TUPLECON2 ((THEstring "APP ")
, (Lopcoco (TUPLECON2 ((THEstring "(")
, (Lopcoco (TUPLECON2 (S1, (Lopcoco (TUPLECON2 ((THEstring ", ")
, (Lopcoco (TUPLECON2 (S2, (Lopcoco (TUPLECON2 ((THEstring ")")
, (Lnil)
))
)
))
)
))
)
))
)
))
)
))
)
 ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
))
))
))
))
))
))
))
))
))
))
)


fun ttToStr (ARG0, ARG1)  = ((fn (ARG0, ARG1) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLtypingtree ARG0)
, (PRINTLstring ARG1)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELttToStr (ARG0, ARG1) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLtypingtree ARG0)
 | Option.NONE =>
(Ltypingtree ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLstring ARG1)
 | Option.NONE =>
(Lstring ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
))


fun RELmember (ARG0 : (Lstring , Lty) TUPLETYPE2 Llist, ARG1 : Lstring, ARG2 : Lty)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Llist (fn E : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 (Lopcoco (TUPLECON2 ((TUPLECON2 (X, T))
, E))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG1 X ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG2 T ARGtc)
)))
))
))
))
))
, (SMLOG.EXISTS Llist (fn E : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS TUPLETYPE2 (fn WILD_189 : (Lstring , Lty) TUPLETYPE2 =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 (Lopcoco (TUPLECON2 (WILD_189, E))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG1 X ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG2 T ARGtc)
)))
, (RELmember (E, X, T))
))
))
))
))
))
))
)


fun member (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) (((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
))
 ARG0)
, (PRINTLstring ARG1)
, (PRINTLty ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELmember (ARG0, ARG1, ARG2) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDLlist (fn ARGt =>
((EMBEDTUPLETYPE2 ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(EMBEDLty ARGt)
)))
 ARGt)
))
 ARG0)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLstring ARG1)
 | Option.NONE =>
(Lstring ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
(EMBEDLty ARG2)
 | Option.NONE =>
(Lty ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
))


fun RELnotEmpty (ARG0 : Lty Llist)  = (SMLOG.STEP (SMLOG.EXISTS Lty (fn WILD_190 : Lty =>
(SMLOG.EXISTS Llist (fn WILD_191 : Lty Llist =>
(fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 (Lopcoco (TUPLECON2 (WILD_190, WILD_191))
)
 ARGtc)
)))
))
)


fun notEmpty (ARG0)  = ((fn (ARG0) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) (((PRINTLlist (fn ARGt =>
(PRINTLty ARGt)
))
 ARG0)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELnotEmpty (ARG0) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDLlist (fn ARGt =>
(EMBEDLty ARGt)
))
 ARG0)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
))


fun RELtyping (ARG0 : (Lstring , Lty) TUPLETYPE2 Llist, ARG1 : Lty Llist, ARG2 : Lterm, ARG3 : Lty, ARG4 : Ltypingtree)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Llist (fn E : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lnil)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG2 (LVar X)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 T ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARG0, ARG1) ARGtc)
)) ARG4 (LVAR (LJ (TUPLECON4 (E, (Lnil)
, (LVar X)
, T))
)
)
 ARGtc)
)))
, (RELmember (E, X, T))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn D : Lty Llist =>
(SMLOG.EXISTS Llist (fn E : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lterm (fn M : Lterm =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Lty (fn T' : Lty =>
(SMLOG.EXISTS Ltypingtree (fn TT : Ltypingtree =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 (T, D))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG2 (LLam (TUPLECON2 (X, M))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 T' ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARG0, ARG1) ARGtc)
)) ARG4 (LLAM (TUPLECON2 (TT, (LJ (TUPLECON4 (E, (Lopcoco (TUPLECON2 (T, D))
)
, (LLam (TUPLECON2 (X, M))
)
, T'))
)
))
)
 ARGtc)
)))
, (RELtyping ((Lopcoco (TUPLECON2 ((TUPLECON2 (X, T))
, E))
)
, D, M, T', TT))
))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn D : Lty Llist =>
(SMLOG.EXISTS Llist (fn E : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lterm (fn M : Lterm =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Ltypingtree (fn TT : Ltypingtree =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lnil)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG2 (LLam (TUPLECON2 (X, M))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 (LFun (TUPLECON2 (D, T))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARG0, ARG1) ARGtc)
)) ARG4 (LCLO (TUPLECON2 (TT, (LJ (TUPLECON4 (E, (Lnil)
, (LLam (TUPLECON2 (X, M))
)
, (LFun (TUPLECON2 (D, T))
)
))
)
))
)
 ARGtc)
)))
, (RELnotEmpty (D))
))
, (RELtyping (E, D, (LLam (TUPLECON2 (X, M))
)
, T, TT))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn D : Lty Llist =>
(SMLOG.EXISTS Llist (fn E : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lterm (fn M1 : Lterm =>
(SMLOG.EXISTS Lterm (fn M2 : Lterm =>
(SMLOG.EXISTS Lty (fn T1 : Lty =>
(SMLOG.EXISTS Lty (fn T2 : Lty =>
(SMLOG.EXISTS Ltypingtree (fn TT1 : Ltypingtree =>
(SMLOG.EXISTS Ltypingtree (fn TT2 : Ltypingtree =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 D ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG2 (LApp (TUPLECON2 (M1, M2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 T1 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARG0, ARG1) ARGtc)
)) ARG4 (LAPP (TUPLECON3 (TT1, TT2, (LJ (TUPLECON4 (E, D, (LApp (TUPLECON2 (M1, M2))
)
, T1))
)
))
)
 ARGtc)
)))
, (RELtyping (E, (Lnil)
, M2, T2, TT2))
))
, (RELtyping (E, (Lopcoco (TUPLECON2 (T2, D))
)
, M1, T1, TT1))
))
))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn D : Lty Llist =>
(SMLOG.EXISTS Llist (fn E : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lterm (fn M : Lterm =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Ltypingtree (fn TT : Ltypingtree =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 D ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG2 M ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 T ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARG0, ARG1) ARGtc)
)) ARG4 (LCODE (TUPLECON2 (TT, (LJ (TUPLECON4 (E, D, M, T))
)
))
)
 ARGtc)
)))
, (RELnotEmpty (D))
))
, (RELtyping (E, (Lnil)
, M, (LFun (TUPLECON2 (D, T))
)
, TT))
))
))
))
))
))
))
))
)


fun typing (ARG0, ARG1, ARG2, ARG3, ARG4)  = ((fn (ARG0, ARG1, ARG2, ARG3, ARG4) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) (((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
))
 ARG0)
, ((PRINTLlist (fn ARGt =>
(PRINTLty ARGt)
))
 ARG1)
, (PRINTLterm ARG2)
, (PRINTLty ARG3)
, (PRINTLtypingtree ARG4)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELtyping (ARG0, ARG1, ARG2, ARG3, ARG4) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDLlist (fn ARGt =>
((EMBEDTUPLETYPE2 ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(EMBEDLty ARGt)
)))
 ARGt)
))
 ARG0)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
((EMBEDLlist (fn ARGt =>
(EMBEDLty ARGt)
))
 ARG1)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
(EMBEDLterm ARG2)
 | Option.NONE =>
(Lterm ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
, ((fn (Option.SOME ARG3) =>
(EMBEDLty ARG3)
 | Option.NONE =>
(Lty ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG3)
, ((fn (Option.SOME ARG4) =>
(EMBEDLtypingtree ARG4)
 | Option.NONE =>
(Ltypingtree ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG4)
))


fun RELcps (ARG0 : Ltypingtree, ARG1 : Lterm)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lty (fn WILD_192 : Lty =>
(SMLOG.EXISTS Llist (fn WILD_193 : Lty Llist =>
(SMLOG.EXISTS Llist (fn WILD_194 : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARG0, ARG1) ARGtc)
)) ARG0 (LVAR (LJ (TUPLECON4 (WILD_194, WILD_193, (LVar X)
, WILD_192))
)
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG1 (LVar X)
 ARGtc)
)))
))
))
))
))
))
, (SMLOG.EXISTS Lterm (fn M : Lterm =>
(SMLOG.EXISTS Lterm (fn M' : Lterm =>
(SMLOG.EXISTS Ltypingtree (fn T : Ltypingtree =>
(SMLOG.EXISTS Lty (fn WILD_195 : Lty =>
(SMLOG.EXISTS Llist (fn WILD_196 : Lty Llist =>
(SMLOG.EXISTS Llist (fn WILD_197 : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARG0, ARG1) ARGtc)
)) ARG0 (LLAM (TUPLECON2 (T, (LJ (TUPLECON4 (WILD_197, WILD_196, (LLam (TUPLECON2 (X, M))
)
, WILD_195))
)
))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG1 (LLam (TUPLECON2 (X, M'))
)
 ARGtc)
)))
, (RELcps (T, M'))
))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lterm (fn M : Lterm =>
(SMLOG.EXISTS Lterm (fn M' : Lterm =>
(SMLOG.EXISTS Ltypingtree (fn T : Ltypingtree =>
(SMLOG.EXISTS Lty (fn WILD_198 : Lty =>
(SMLOG.EXISTS Llist (fn WILD_199 : Lty Llist =>
(SMLOG.EXISTS Llist (fn WILD_200 : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARG0, ARG1) ARGtc)
)) ARG0 (LCLO (TUPLECON2 (T, (LJ (TUPLECON4 (WILD_200, WILD_199, (LLam (TUPLECON2 (X, M))
)
, WILD_198))
)
))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG1 (LLam (TUPLECON2 ((THEstring "k")
, (LApp (TUPLECON2 ((LVar (THEstring "k")
)
, M'))
)
))
)
 ARGtc)
)))
, (RELcps (T, M'))
))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lterm (fn M1 : Lterm =>
(SMLOG.EXISTS Lterm (fn M1' : Lterm =>
(SMLOG.EXISTS Lterm (fn M2 : Lterm =>
(SMLOG.EXISTS Lterm (fn M2' : Lterm =>
(SMLOG.EXISTS Ltypingtree (fn T1 : Ltypingtree =>
(SMLOG.EXISTS Ltypingtree (fn T2 : Ltypingtree =>
(SMLOG.EXISTS Lty (fn WILD_201 : Lty =>
(SMLOG.EXISTS Llist (fn WILD_202 : Lty Llist =>
(SMLOG.EXISTS Llist (fn WILD_203 : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARG0, ARG1) ARGtc)
)) ARG0 (LAPP (TUPLECON3 (T1, T2, (LJ (TUPLECON4 (WILD_203, WILD_202, (LApp (TUPLECON2 (M1, M2))
)
, WILD_201))
)
))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG1 (LApp (TUPLECON2 (M1', M2'))
)
 ARGtc)
)))
, (RELcps (T1, M1'))
))
, (RELcps (T2, M2'))
))
))
))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lterm (fn M : Lterm =>
(SMLOG.EXISTS Lterm (fn M' : Lterm =>
(SMLOG.EXISTS Ltypingtree (fn T : Ltypingtree =>
(SMLOG.EXISTS Lty (fn WILD_204 : Lty =>
(SMLOG.EXISTS Llist (fn WILD_205 : Lty Llist =>
(SMLOG.EXISTS Llist (fn WILD_206 : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtypingtree (ARG0, ARG1) ARGtc)
)) ARG0 (LCODE (TUPLECON2 (T, (LJ (TUPLECON4 (WILD_206, WILD_205, M, WILD_204))
)
))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG1 (LApp (TUPLECON2 (M', (LLam (TUPLECON2 ((THEstring "v")
, (LVar (THEstring "v")
)
))
)
))
)
 ARGtc)
)))
, (RELcps (T, M'))
))
))
))
))
))
))
))
))
)


fun cps (ARG0, ARG1)  = ((fn (ARG0, ARG1) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLtypingtree ARG0)
, (PRINTLterm ARG1)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELcps (ARG0, ARG1) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLtypingtree ARG0)
 | Option.NONE =>
(Ltypingtree ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLterm ARG1)
 | Option.NONE =>
(Lterm ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
))


fun RELtest (ARG0 : Lterm, ARG1 : Lstring, ARG2 : Lstring, ARG3 : Lstring)  = (SMLOG.STEP (SMLOG.EXISTS Lterm (fn K : Lterm =>
(SMLOG.EXISTS Lterm (fn M : Lterm =>
(SMLOG.EXISTS Lstring (fn S1 : Lstring =>
(SMLOG.EXISTS Lstring (fn S2 : Lstring =>
(SMLOG.EXISTS Lstring (fn S3 : Lstring =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Ltypingtree (fn TT : Ltypingtree =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG0 M ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG1 S1 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG2 S2 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG3 S3 ARGtc)
)))
, (RELtyping ((Lnil)
, (Lnil)
, M, T, TT))
))
, (RELcps (TT, K))
))
, (RELtoStr (M, S1))
))
, (RELttToStr (TT, S2))
))
, (RELtoStr (K, S3))
))
))
))
))
))
))
))
))
)


fun test (ARG0, ARG1, ARG2, ARG3)  = ((fn (ARG0, ARG1, ARG2, ARG3) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLterm ARG0)
, (PRINTLstring ARG1)
, (PRINTLstring ARG2)
, (PRINTLstring ARG3)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELtest (ARG0, ARG1, ARG2, ARG3) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLterm ARG0)
 | Option.NONE =>
(Lterm ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLstring ARG1)
 | Option.NONE =>
(Lstring ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
(EMBEDLstring ARG2)
 | Option.NONE =>
(Lstring ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
, ((fn (Option.SOME ARG3) =>
(EMBEDLstring ARG3)
 | Option.NONE =>
(Lstring ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG3)
))


fun RELrun (ARG0 : Lstring, ARG1 : Lstring, ARG2 : Lstring)  = (SMLOG.STEP (SMLOG.EXISTS Lstring (fn S1 : Lstring =>
(SMLOG.EXISTS Lstring (fn S2 : Lstring =>
(SMLOG.EXISTS Lstring (fn S3 : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG0 S1 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG1 S2 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG2 S3 ARGtc)
)))
, (RELtest ((LLam (TUPLECON2 ((THEstring "f")
, (LLam (TUPLECON2 ((THEstring "x")
, (LLam (TUPLECON2 ((THEstring "y")
, (LApp (TUPLECON2 ((LApp (TUPLECON2 ((LVar (THEstring "f")
)
, (LVar (THEstring "x")
)
))
)
, (LVar (THEstring "y")
)
))
)
))
)
))
)
))
)
, S1, S2, S3))
))
))
))
))
)


fun run (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLstring ARG0)
, (PRINTLstring ARG1)
, (PRINTLstring ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELrun (ARG0, ARG1, ARG2) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLstring ARG0)
 | Option.NONE =>
(Lstring ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLstring ARG1)
 | Option.NONE =>
(Lstring ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
(EMBEDLstring ARG2)
 | Option.NONE =>
(Lstring ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
))

