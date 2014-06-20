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


datatype ty = Int | Fun of (ty * ty) | O of ty

datatype Oty = datatype ty

datatype Lty = Lty of (Ostring * Lty Option.option ref) | LInt | LFun of (Lty , Lty) TUPLETYPE2 | LO of Lty

fun PROJLty (Lty (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLty (Lty (_, (ref (Option.SOME ARGt))))  = (PROJLty ARGt)

| PROJLty LInt  = Int
| PROJLty (LFun ARGt)  = (Fun ((PROJTUPLETYPE2 ((fn ARGt =>
(PROJLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
)

| PROJLty (LO ARGt)  = (O (PROJLty ARGt)
)


fun EMBEDLty Int  = LInt
| EMBEDLty (Fun ARGt)  = (LFun ((EMBEDTUPLETYPE2 ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(EMBEDLty ARGt)
)))
 ARGt)
)

| EMBEDLty (O ARGt)  = (LO (EMBEDLty ARGt)
)


fun OCCURLty ARG0 (Lty (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLty ARG0 (Lty (_, (ref (Option.SOME ARG2))))  = (OCCURLty ARG0 ARG2)

| OCCURLty ARG0 LInt  = false
| OCCURLty ARG0 (LFun ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLty ARG0 (LO ARG1)  = (OCCURLty ARG0 ARG1)


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
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLty ((LO ARGt1), (LO ARGt2)) ARGtc  = (UNIFYLty (ARGt1, ARGt2) ARGtc)

| UNIFYLty (_, _) ARGtc  = (false, ARGtc)

fun PRINTLty (Lty (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLty (Lty (_, (ref (Option.SOME ARGt))))  = (PRINTLty ARGt)

| PRINTLty LInt  = "Int"
| PRINTLty (LFun ARGt)  = (String.concat ["Fun", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLty ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
])

| PRINTLty (LO ARGt)  = (String.concat ["O", " ", (PRINTLty ARGt)
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


datatype term = Var of string | Lam of (string * ty * term) | App of (term * term) | Next of term | Prev of term

datatype Oterm = datatype term

datatype Lterm = Lterm of (Ostring * Lterm Option.option ref) | LVar of Lstring | LLam of (Lstring , Lty , Lterm) TUPLETYPE3 | LApp of (Lterm , Lterm) TUPLETYPE2 | LNext of Lterm | LPrev of Lterm

fun PROJLterm (Lterm (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLterm (Lterm (_, (ref (Option.SOME ARGt))))  = (PROJLterm ARGt)

| PROJLterm (LVar ARGt)  = (Var (PROJLstring ARGt)
)

| PROJLterm (LLam ARGt)  = (Lam ((PROJTUPLETYPE3 ((fn ARGt =>
(PROJLstring ARGt)
), (fn ARGt =>
(PROJLty ARGt)
), (fn ARGt =>
(PROJLterm ARGt)
)))
 ARGt)
)

| PROJLterm (LApp ARGt)  = (App ((PROJTUPLETYPE2 ((fn ARGt =>
(PROJLterm ARGt)
), (fn ARGt =>
(PROJLterm ARGt)
)))
 ARGt)
)

| PROJLterm (LNext ARGt)  = (Next (PROJLterm ARGt)
)

| PROJLterm (LPrev ARGt)  = (Prev (PROJLterm ARGt)
)


fun EMBEDLterm (Var ARGt)  = (LVar (EMBEDLstring ARGt)
)

| EMBEDLterm (Lam ARGt)  = (LLam ((EMBEDTUPLETYPE3 ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(EMBEDLterm ARGt)
)))
 ARGt)
)

| EMBEDLterm (App ARGt)  = (LApp ((EMBEDTUPLETYPE2 ((fn ARGt =>
(EMBEDLterm ARGt)
), (fn ARGt =>
(EMBEDLterm ARGt)
)))
 ARGt)
)

| EMBEDLterm (Next ARGt)  = (LNext (EMBEDLterm ARGt)
)

| EMBEDLterm (Prev ARGt)  = (LPrev (EMBEDLterm ARGt)
)


fun OCCURLterm ARG0 (Lterm (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLterm ARG0 (Lterm (_, (ref (Option.SOME ARG2))))  = (OCCURLterm ARG0 ARG2)

| OCCURLterm ARG0 (LVar ARG1)  = (OCCURLstring ARG0 ARG1)

| OCCURLterm ARG0 (LLam ARG1)  = ((OCCURTUPLETYPE3 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLterm ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLterm ARG0 (LApp ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLterm ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLterm ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLterm ARG0 (LNext ARG1)  = (OCCURLterm ARG0 ARG1)

| OCCURLterm ARG0 (LPrev ARG1)  = (OCCURLterm ARG0 ARG1)


fun UNIFYLterm (ARGt1, (Lterm (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLterm (ARGt1, ARGt2) ARGtc)

| UNIFYLterm (ARGt1, (Lterm (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLterm ARG0 ARGt1)
)

| UNIFYLterm ((Lterm (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLterm (ARGt1, ARGt2) ARGtc)

| UNIFYLterm ((ARGt1 as (Lterm (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLterm (ARGt2, ARGt1) ARGtc)

| UNIFYLterm ((LVar ARGt1), (LVar ARGt2)) ARGtc  = (UNIFYLstring (ARGt1, ARGt2) ARGtc)

| UNIFYLterm ((LLam ARGt1), (LLam ARGt2)) ARGtc  = ((UNIFYTUPLETYPE3 (((fn (ARGt1, ARGt2) =>
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
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLterm (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLterm ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

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

| UNIFYLterm ((LNext ARGt1), (LNext ARGt2)) ARGtc  = (UNIFYLterm (ARGt1, ARGt2) ARGtc)

| UNIFYLterm ((LPrev ARGt1), (LPrev ARGt2)) ARGtc  = (UNIFYLterm (ARGt1, ARGt2) ARGtc)

| UNIFYLterm (_, _) ARGtc  = (false, ARGtc)

fun PRINTLterm (Lterm (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLterm (Lterm (_, (ref (Option.SOME ARGt))))  = (PRINTLterm ARGt)

| PRINTLterm (LVar ARGt)  = (String.concat ["Var", " ", (PRINTLstring ARGt)
])

| PRINTLterm (LLam ARGt)  = (String.concat ["Lam", " ", ((PRINTTUPLETYPE3 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
), (fn ARGt =>
(PRINTLterm ARGt)
)))
 ARGt)
])

| PRINTLterm (LApp ARGt)  = (String.concat ["App", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLterm ARGt)
), (fn ARGt =>
(PRINTLterm ARGt)
)))
 ARGt)
])

| PRINTLterm (LNext ARGt)  = (String.concat ["Next", " ", (PRINTLterm ARGt)
])

| PRINTLterm (LPrev ARGt)  = (String.concat ["Prev", " ", (PRINTLterm ARGt)
])


datatype time = Z | S of time

datatype Otime = datatype time

datatype Ltime = Ltime of (Ostring * Ltime Option.option ref) | LZ | LS of Ltime

fun PROJLtime (Ltime (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLtime (Ltime (_, (ref (Option.SOME ARGt))))  = (PROJLtime ARGt)

| PROJLtime LZ  = Z
| PROJLtime (LS ARGt)  = (S (PROJLtime ARGt)
)


fun EMBEDLtime Z  = LZ
| EMBEDLtime (S ARGt)  = (LS (EMBEDLtime ARGt)
)


fun OCCURLtime ARG0 (Ltime (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLtime ARG0 (Ltime (_, (ref (Option.SOME ARG2))))  = (OCCURLtime ARG0 ARG2)

| OCCURLtime ARG0 LZ  = false
| OCCURLtime ARG0 (LS ARG1)  = (OCCURLtime ARG0 ARG1)


fun UNIFYLtime (ARGt1, (Ltime (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLtime (ARGt1, ARGt2) ARGtc)

| UNIFYLtime (ARGt1, (Ltime (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLtime ARG0 ARGt1)
)

| UNIFYLtime ((Ltime (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLtime (ARGt1, ARGt2) ARGtc)

| UNIFYLtime ((ARGt1 as (Ltime (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLtime (ARGt2, ARGt1) ARGtc)

| UNIFYLtime (LZ, LZ) ARGtc  = (true, ARGtc)
| UNIFYLtime ((LS ARGt1), (LS ARGt2)) ARGtc  = (UNIFYLtime (ARGt1, ARGt2) ARGtc)

| UNIFYLtime (_, _) ARGtc  = (false, ARGtc)

fun PRINTLtime (Ltime (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLtime (Ltime (_, (ref (Option.SOME ARGt))))  = (PRINTLtime ARGt)

| PRINTLtime LZ  = "Z"
| PRINTLtime (LS ARGt)  = (String.concat ["S", " ", (PRINTLtime ARGt)
])


type typingenv = (string * ty * time) list

fun RELmember (ARG0 : (Lstring , Lty , Ltime) TUPLETYPE3 Llist, ARG1 : Lstring, ARG2 : Lty, ARG3 : Ltime)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Llist (fn E : (Lstring , Lty , Ltime) TUPLETYPE3 Llist =>
(SMLOG.EXISTS Ltime (fn N : Ltime =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE3 (((fn (ARGt1, ARGt2) =>
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
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtime (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtime ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE3 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtime ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 (Lopcoco (TUPLECON2 ((TUPLECON3 (X, T, N))
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
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtime (ARG0, ARG1) ARGtc)
)) ARG3 N ARGtc)
)))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn E : (Lstring , Lty , Ltime) TUPLETYPE3 Llist =>
(SMLOG.EXISTS Ltime (fn N : Ltime =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS TUPLETYPE3 (fn WILD_4 : (Lstring , Lty , Ltime) TUPLETYPE3 =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE3 (((fn (ARGt1, ARGt2) =>
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
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtime (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtime ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE3 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtime ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 (Lopcoco (TUPLECON2 (WILD_4, E))
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
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtime (ARG0, ARG1) ARGtc)
)) ARG3 N ARGtc)
)))
, (RELmember (E, X, T, N))
))
))
))
))
))
))
))
)


fun member (ARG0, ARG1, ARG2, ARG3)  = ((fn (ARG0, ARG1, ARG2, ARG3) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) (((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE3 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
), (fn ARGt =>
(PRINTLtime ARGt)
)))
 ARGt)
))
 ARG0)
, (PRINTLstring ARG1)
, (PRINTLty ARG2)
, (PRINTLtime ARG3)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELmember (ARG0, ARG1, ARG2, ARG3) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDLlist (fn ARGt =>
((EMBEDTUPLETYPE3 ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(EMBEDLtime ARGt)
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
, ((fn (Option.SOME ARG3) =>
(EMBEDLtime ARG3)
 | Option.NONE =>
(Ltime ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG3)
))


fun RELtyping (ARG0 : (Lstring , Lty , Ltime) TUPLETYPE3 Llist, ARG1 : Lterm, ARG2 : Lty, ARG3 : Ltime)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Llist (fn E : (Lstring , Lty , Ltime) TUPLETYPE3 Llist =>
(SMLOG.EXISTS Ltime (fn N : Ltime =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE3 (((fn (ARGt1, ARGt2) =>
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
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtime (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtime ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE3 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtime ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG1 (LVar X)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG2 T ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtime (ARG0, ARG1) ARGtc)
)) ARG3 N ARGtc)
)))
, (RELmember (E, X, T, N))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn E : (Lstring , Lty , Ltime) TUPLETYPE3 Llist =>
(SMLOG.EXISTS Lterm (fn M : Lterm =>
(SMLOG.EXISTS Ltime (fn N : Ltime =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Lty (fn T' : Lty =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE3 (((fn (ARGt1, ARGt2) =>
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
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtime (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtime ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE3 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtime ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG1 (LLam (TUPLECON3 (X, T, M))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG2 (LFun (TUPLECON2 (T, T'))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtime (ARG0, ARG1) ARGtc)
)) ARG3 N ARGtc)
)))
, (RELtyping ((Lopcoco (TUPLECON2 ((TUPLECON3 (X, T, N))
, E))
)
, M, T', N))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn E : (Lstring , Lty , Ltime) TUPLETYPE3 Llist =>
(SMLOG.EXISTS Lterm (fn M1 : Lterm =>
(SMLOG.EXISTS Lterm (fn M2 : Lterm =>
(SMLOG.EXISTS Ltime (fn N : Ltime =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Lty (fn T2 : Lty =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE3 (((fn (ARGt1, ARGt2) =>
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
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtime (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtime ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE3 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtime ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG1 (LApp (TUPLECON2 (M1, M2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG2 T ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtime (ARG0, ARG1) ARGtc)
)) ARG3 N ARGtc)
)))
, (RELtyping (E, M1, (LFun (TUPLECON2 (T2, T))
)
, N))
))
, (RELtyping (E, M2, T2, N))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn E : (Lstring , Lty , Ltime) TUPLETYPE3 Llist =>
(SMLOG.EXISTS Lterm (fn M : Lterm =>
(SMLOG.EXISTS Ltime (fn N : Ltime =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE3 (((fn (ARGt1, ARGt2) =>
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
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtime (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtime ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE3 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtime ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG1 (LNext M)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG2 (LO T)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtime (ARG0, ARG1) ARGtc)
)) ARG3 N ARGtc)
)))
, (RELtyping (E, M, T, (LS N)
))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn E : (Lstring , Lty , Ltime) TUPLETYPE3 Llist =>
(SMLOG.EXISTS Lterm (fn M : Lterm =>
(SMLOG.EXISTS Ltime (fn N : Ltime =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE3 (((fn (ARGt1, ARGt2) =>
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
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtime (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtime ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE3 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtime ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG1 (LPrev M)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG2 T ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtime (ARG0, ARG1) ARGtc)
)) ARG3 (LS N)
 ARGtc)
)))
, (RELtyping (E, M, (LO T)
, N))
))
))
))
))
))
))
)


fun typing (ARG0, ARG1, ARG2, ARG3)  = ((fn (ARG0, ARG1, ARG2, ARG3) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) (((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE3 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
), (fn ARGt =>
(PRINTLtime ARGt)
)))
 ARGt)
))
 ARG0)
, (PRINTLterm ARG1)
, (PRINTLty ARG2)
, (PRINTLtime ARG3)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELtyping (ARG0, ARG1, ARG2, ARG3) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDLlist (fn ARGt =>
((EMBEDTUPLETYPE3 ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(EMBEDLtime ARGt)
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
(EMBEDLterm ARG1)
 | Option.NONE =>
(Lterm ((SMLOG.freshString ())
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
, ((fn (Option.SOME ARG3) =>
(EMBEDLtime ARG3)
 | Option.NONE =>
(Ltime ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG3)
))

