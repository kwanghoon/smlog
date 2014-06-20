type Ostring = string

datatype Lstring = Lstring of (Ostring * Lstring Option.option ref) | THEstring of Ostring

fun EMBEDLstring ARGt  = (THEstring ARGt)

and PROJLstring (Lstring (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLstring (Lstring (_, (ref (Option.SOME ARGt))))  = (PROJLstring ARGt)

| PROJLstring (THEstring ARGt)  = ARGt

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


datatype ('a, 'b) Lmigt = Lmigt of (Ostring * ('a , 'b) Lmigt Option.option ref) | THEmigt of ('a -> 'b)

fun EMBEDLmigt (qua, qub) ARGt  = (THEmigt (fn ARGf =>
(((fn (ARG1, ARG2) =>
ARG1) qub)
 (ARGt (((fn (ARG1, ARG2) =>
ARG2) qua)
 ARGf)
)
)
))

and PROJLmigt (qua, qub) (Lmigt (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLmigt (qua, qub) (Lmigt (_, (ref (Option.SOME ARGt))))  = ((PROJLmigt (qua, qub))
 ARGt)

| PROJLmigt (qua, qub) (THEmigt ARGt)  = (fn ARGf =>
(((fn (ARG1, ARG2) =>
ARG2) qub)
 (ARGt (((fn (ARG1, ARG2) =>
ARG1) qua)
 ARGf)
)
)
)

fun OCCURLmigt (qua, qub) ARG0 (Lmigt (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLmigt (qua, qub) ARG0 (Lmigt (_, (ref (Option.SOME ARG2))))  = ((OCCURLmigt (qua, qub))
 ARG0 ARG2)

| OCCURLmigt (qua, qub) ARG0 (THEmigt _)  = false

fun UNIFYLmigt (qua, qub) (ARGt1, (Lmigt (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYLmigt (qua, qub))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLmigt (qua, qub) (ARGt1, (Lmigt (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURLmigt (((fn (ARG1, ARG2) =>
ARG2) qua)
, ((fn (ARG1, ARG2) =>
ARG2) qub)
))
 ARG0 ARGt1)
)

| UNIFYLmigt (qua, qub) ((Lmigt (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYLmigt (qua, qub))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLmigt (qua, qub) ((ARGt1 as (Lmigt (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYLmigt (qua, qub))
 (ARGt2, ARGt1) ARGtc)

| UNIFYLmigt (qua, qub) ((THEmigt ARGt1), (THEmigt ARGt2)) ARGtc  = (false, ARGtc)

fun PRINTLmigt (qua, qub) (Lmigt (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLmigt (qua, qub) (Lmigt (_, (ref (Option.SOME ARGt))))  = ((PRINTLmigt (qua, qub))
 ARGt)

| PRINTLmigt (qua, qub) (THEmigt ARGt)  = "<('a -> 'b)>"

type Ounit = unit

datatype Lunit = Lunit of (Ostring * Lunit Option.option ref) | THEunit of Ounit

fun EMBEDLunit ARGt  = (THEunit ARGt)

and PROJLunit (Lunit (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLunit (Lunit (_, (ref (Option.SOME ARGt))))  = (PROJLunit ARGt)

| PROJLunit (THEunit ARGt)  = ARGt

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

fun EMBEDLbool true  = Ltrue
| EMBEDLbool false  = Lfalse
and PROJLbool (Lbool (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLbool (Lbool (_, (ref (Option.SOME ARGt))))  = (PROJLbool ARGt)

| PROJLbool Ltrue  = true
| PROJLbool Lfalse  = false

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

fun EMBEDLint ARGt  = (THEint ARGt)

and PROJLint (Lint (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLint (Lint (_, (ref (Option.SOME ARGt))))  = (PROJLint ARGt)

| PROJLint (THEint ARGt)  = ARGt

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

fun EMBEDLword ARGt  = (THEword ARGt)

and PROJLword (Lword (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLword (Lword (_, (ref (Option.SOME ARGt))))  = (PROJLword ARGt)

| PROJLword (THEword ARGt)  = ARGt

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

fun EMBEDLchar ARGt  = (THEchar ARGt)

and PROJLchar (Lchar (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLchar (Lchar (_, (ref (Option.SOME ARGt))))  = (PROJLchar ARGt)

| PROJLchar (THEchar ARGt)  = ARGt

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

fun EMBEDTUPLETYPE2 (qua0, qua1) (ARGt0, ARGt1)  = (TUPLECON2 (((fn (ARG1, ARG2) =>
ARG1) qua0 ARGt0)
, ((fn (ARG1, ARG2) =>
ARG1) qua1 ARGt1)
))

and PROJTUPLETYPE2 (qua0, qua1) (TUPLETYPE2 (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJTUPLETYPE2 (qua0, qua1) (TUPLETYPE2 (_, (ref (Option.SOME ARGt))))  = ((PROJTUPLETYPE2 (qua0, qua1))
 ARGt)

| PROJTUPLETYPE2 (qua0, qua1) (TUPLECON2 (ARGt0, ARGt1))  = (((fn (ARG1, ARG2) =>
ARG2) qua0 ARGt0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1 ARGt1)
)

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

fun EMBEDLlist (qua) nil  = Lnil
| EMBEDLlist (qua) (op:: ARGt)  = (Lopcoco ((EMBEDTUPLETYPE2 (((fn ARGt =>
((fn (ARG1, ARG2) =>
ARG1) qua ARGt)
), (fn ARGt =>
((fn (ARG1, ARG2) =>
ARG2) qua ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
((fn (ARG1, ARG2) =>
ARG1) qua ARGt)
), (fn ARGt =>
((fn (ARG1, ARG2) =>
ARG2) qua ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
((fn (ARG1, ARG2) =>
ARG1) qua ARGt)
), (fn ARGt =>
((fn (ARG1, ARG2) =>
ARG2) qua ARGt)
)))
 ARGt)
))))
 ARGt)
)

and PROJLlist (qua) (Llist (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLlist (qua) (Llist (_, (ref (Option.SOME ARGt))))  = ((PROJLlist (qua))
 ARGt)

| PROJLlist (qua) Lnil  = nil
| PROJLlist (qua) (Lopcoco ARGt)  = (op:: ((PROJTUPLETYPE2 (((fn ARGt =>
((fn (ARG1, ARG2) =>
ARG1) qua ARGt)
), (fn ARGt =>
((fn (ARG1, ARG2) =>
ARG2) qua ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
((fn (ARG1, ARG2) =>
ARG1) qua ARGt)
), (fn ARGt =>
((fn (ARG1, ARG2) =>
ARG2) qua ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
((fn (ARG1, ARG2) =>
ARG1) qua ARGt)
), (fn ARGt =>
((fn (ARG1, ARG2) =>
ARG2) qua ARGt)
)))
 ARGt)
))))
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


type id = string

type label = int

type flowset = label list

datatype ('a0, 'a1, 'a2) TUPLETYPE3 = TUPLETYPE3 of (Ostring * ('a0 , 'a1 , 'a2) TUPLETYPE3 Option.option ref) | TUPLECON3 of ('a0 * 'a1 * 'a2)

fun EMBEDTUPLETYPE3 (qua0, qua1, qua2) (ARGt0, ARGt1, ARGt2)  = (TUPLECON3 (((fn (ARG1, ARG2) =>
ARG1) qua0 ARGt0)
, ((fn (ARG1, ARG2) =>
ARG1) qua1 ARGt1)
, ((fn (ARG1, ARG2) =>
ARG1) qua2 ARGt2)
))

and PROJTUPLETYPE3 (qua0, qua1, qua2) (TUPLETYPE3 (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJTUPLETYPE3 (qua0, qua1, qua2) (TUPLETYPE3 (_, (ref (Option.SOME ARGt))))  = ((PROJTUPLETYPE3 (qua0, qua1, qua2))
 ARGt)

| PROJTUPLETYPE3 (qua0, qua1, qua2) (TUPLECON3 (ARGt0, ARGt1, ARGt2))  = (((fn (ARG1, ARG2) =>
ARG2) qua0 ARGt0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1 ARGt1)
, ((fn (ARG1, ARG2) =>
ARG2) qua2 ARGt2)
)

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


datatype exp = Var of (id * label) | Lam of (label * id * exp) | App of (exp * exp * label)

datatype Oexp = datatype exp

datatype Lexp = Lexp of (Ostring * Lexp Option.option ref) | LVar of (Lstring , Lint) TUPLETYPE2 | LLam of (Lint , Lstring , Lexp) TUPLETYPE3 | LApp of (Lexp , Lexp , Lint) TUPLETYPE3

fun EMBEDLexp (Var ARGt)  = (LVar ((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
))))
 ARGt)
)

| EMBEDLexp (Lam ARGt)  = (LLam ((EMBEDTUPLETYPE3 (((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
))))
 ARGt)
)

| EMBEDLexp (App ARGt)  = (LApp ((EMBEDTUPLETYPE3 (((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
))))
 ARGt)
)

and PROJLexp (Lexp (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLexp (Lexp (_, (ref (Option.SOME ARGt))))  = (PROJLexp ARGt)

| PROJLexp (LVar ARGt)  = (Var ((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
))))
 ARGt)
)

| PROJLexp (LLam ARGt)  = (Lam ((PROJTUPLETYPE3 (((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
))))
 ARGt)
)

| PROJLexp (LApp ARGt)  = (App ((PROJTUPLETYPE3 (((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
))))
 ARGt)
)


fun OCCURLexp ARG0 (Lexp (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLexp ARG0 (Lexp (_, (ref (Option.SOME ARG2))))  = (OCCURLexp ARG0 ARG2)

| OCCURLexp ARG0 (LVar ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLexp ARG0 (LLam ARG1)  = ((OCCURTUPLETYPE3 ((fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLexp ARG0 (LApp ARG1)  = ((OCCURTUPLETYPE3 ((fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
))))
 ARG0 ARG1)


fun UNIFYLexp (ARGt1, (Lexp (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLexp (ARGt1, ARGt2) ARGtc)

| UNIFYLexp (ARGt1, (Lexp (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLexp ARG0 ARGt1)
)

| UNIFYLexp ((Lexp (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLexp (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((ARGt1 as (Lexp (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLexp (ARGt2, ARGt1) ARGtc)

| UNIFYLexp ((LVar ARGt1), (LVar ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLint (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LLam ARGt1), (LLam ARGt2)) ARGtc  = ((UNIFYTUPLETYPE3 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLint (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLexp (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LApp ARGt1), (LApp ARGt2)) ARGtc  = ((UNIFYTUPLETYPE3 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLexp (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLexp (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLint (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLexp (_, _) ARGtc  = (false, ARGtc)

fun PRINTLexp (Lexp (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLexp (Lexp (_, (ref (Option.SOME ARGt))))  = (PRINTLexp ARGt)

| PRINTLexp (LVar ARGt)  = (String.concat ["Var", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLint ARGt)
)))
 ARGt)
])

| PRINTLexp (LLam ARGt)  = (String.concat ["Lam", " ", ((PRINTTUPLETYPE3 ((fn ARGt =>
(PRINTLint ARGt)
), (fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLexp ARGt)
)))
 ARGt)
])

| PRINTLexp (LApp ARGt)  = (String.concat ["App", " ", ((PRINTTUPLETYPE3 ((fn ARGt =>
(PRINTLexp ARGt)
), (fn ARGt =>
(PRINTLexp ARGt)
), (fn ARGt =>
(PRINTLint ARGt)
)))
 ARGt)
])


fun RELlabel (ARG0 : Lexp, ARG1 : Lint)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lstring (fn WILD_187 : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG0 (LVar (TUPLECON2 (WILD_187, L))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG1 L ARGtc)
)))
))
))
))
, (SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lexp (fn WILD_188 : Lexp =>
(SMLOG.EXISTS Lstring (fn WILD_189 : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG0 (LLam (TUPLECON3 (L, WILD_189, WILD_188))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG1 L ARGtc)
)))
))
))
))
))
, (SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lexp (fn WILD_190 : Lexp =>
(SMLOG.EXISTS Lexp (fn WILD_191 : Lexp =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG0 (LApp (TUPLECON3 (WILD_191, WILD_190, L))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG1 L ARGtc)
)))
))
))
))
))
)


fun label (ARG0, ARG1)  = ((fn (ARG0, ARG1) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLexp ARG0)
, (PRINTLint ARG1)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELlabel (ARG0, ARG1) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLexp ARG0)
 | Option.NONE =>
(Lexp ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLint ARG1)
 | Option.NONE =>
(Lint ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
))


datatype edge = NN of (label * label) | VN of (id * label)

datatype Oedge = datatype edge

datatype Ledge = Ledge of (Ostring * Ledge Option.option ref) | LNN of (Lint , Lint) TUPLETYPE2 | LVN of (Lstring , Lint) TUPLETYPE2

fun EMBEDLedge (NN ARGt)  = (LNN ((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
))))
 ARGt)
)

| EMBEDLedge (VN ARGt)  = (LVN ((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
))))
 ARGt)
)

and PROJLedge (Ledge (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLedge (Ledge (_, (ref (Option.SOME ARGt))))  = (PROJLedge ARGt)

| PROJLedge (LNN ARGt)  = (NN ((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
))))
 ARGt)
)

| PROJLedge (LVN ARGt)  = (VN ((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
))))
 ARGt)
)


fun OCCURLedge ARG0 (Ledge (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLedge ARG0 (Ledge (_, (ref (Option.SOME ARG2))))  = (OCCURLedge ARG0 ARG2)

| OCCURLedge ARG0 (LNN ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLedge ARG0 (LVN ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
))))
 ARG0 ARG1)


fun UNIFYLedge (ARGt1, (Ledge (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLedge (ARGt1, ARGt2) ARGtc)

| UNIFYLedge (ARGt1, (Ledge (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLedge ARG0 ARGt1)
)

| UNIFYLedge ((Ledge (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLedge (ARGt1, ARGt2) ARGtc)

| UNIFYLedge ((ARGt1 as (Ledge (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLedge (ARGt2, ARGt1) ARGtc)

| UNIFYLedge ((LNN ARGt1), (LNN ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLint (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLint (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLedge ((LVN ARGt1), (LVN ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLint (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLedge (_, _) ARGtc  = (false, ARGtc)

fun PRINTLedge (Ledge (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLedge (Ledge (_, (ref (Option.SOME ARGt))))  = (PRINTLedge ARGt)

| PRINTLedge (LNN ARGt)  = (String.concat ["NN", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLint ARGt)
), (fn ARGt =>
(PRINTLint ARGt)
)))
 ARGt)
])

| PRINTLedge (LVN ARGt)  = (String.concat ["VN", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLint ARGt)
)))
 ARGt)
])


fun RELisin (ARG0 : Ledge, ARG1 : Ledge Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lint (fn L1 : Lint =>
(SMLOG.EXISTS Lint (fn L2 : Lint =>
(SMLOG.EXISTS Llist (fn XS : Ledge Llist =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLedge (ARG0, ARG1) ARGtc)
)) ARG0 (LNN (TUPLECON2 (L1, L2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 ((LNN (TUPLECON2 (L1, L2))
)
, XS))
)
 ARGtc)
)))
))
))
))
))
, (SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.EXISTS Llist (fn XS : Ledge Llist =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLedge (ARG0, ARG1) ARGtc)
)) ARG0 (LVN (TUPLECON2 (X, L))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 ((LVN (TUPLECON2 (X, L))
)
, XS))
)
 ARGtc)
)))
))
))
))
))
, (SMLOG.EXISTS Ledge (fn X : Ledge =>
(SMLOG.EXISTS Llist (fn XS : Ledge Llist =>
(SMLOG.EXISTS Ledge (fn Y : Ledge =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLedge (ARG0, ARG1) ARGtc)
)) ARG0 X ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 (Y, XS))
)
 ARGtc)
)))
, (RELisin (X, XS))
))
))
))
))
))
)


fun isin (ARG0, ARG1)  = ((fn (ARG0, ARG1) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLedge ARG0)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG1)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELisin (ARG0, ARG1) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLedge ARG0)
 | Option.NONE =>
(Ledge ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG1)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
))


fun RELappendEdge (ARG0 : Ledge Llist, ARG1 : Ledge Llist, ARG2 : Ledge Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Llist (fn XS : Ledge Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 (Lnil)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 XS ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 XS ARGtc)
)))
))
))
, (SMLOG.EXISTS Ledge (fn X : Ledge =>
(SMLOG.EXISTS Llist (fn XS : Ledge Llist =>
(SMLOG.EXISTS Llist (fn YS : Ledge Llist =>
(SMLOG.EXISTS Llist (fn ZS : Ledge Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 (Lopcoco (TUPLECON2 (X, XS))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 YS ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 (Lopcoco (TUPLECON2 (X, ZS))
)
 ARGtc)
)))
, (SMLOG.NOT (RELisin (X, YS))
)
))
, (RELappendEdge (XS, YS, ZS))
))
))
))
))
))
))
, (SMLOG.EXISTS Ledge (fn X : Ledge =>
(SMLOG.EXISTS Llist (fn XS : Ledge Llist =>
(SMLOG.EXISTS Llist (fn YS : Ledge Llist =>
(SMLOG.EXISTS Llist (fn ZS : Ledge Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 (Lopcoco (TUPLECON2 (X, XS))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 YS ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 ZS ARGtc)
)))
, (RELisin (X, YS))
))
, (RELappendEdge (XS, YS, ZS))
))
))
))
))
))
))
)


fun appendEdge (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) (((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG0)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG1)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELappendEdge (ARG0, ARG1, ARG2) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG0)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG1)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG2)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
))


datatype LamInfo = Label of label | Bv of (label * id) | Body of (label * label)

datatype OLamInfo = datatype LamInfo

datatype LLamInfo = LLamInfo of (Ostring * LLamInfo Option.option ref) | LLabel of Lint | LBv of (Lint , Lstring) TUPLETYPE2 | LBody of (Lint , Lint) TUPLETYPE2

fun EMBEDLLamInfo (Label ARGt)  = (LLabel (EMBEDLint ARGt)
)

| EMBEDLLamInfo (Bv ARGt)  = (LBv ((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
))))
 ARGt)
)

| EMBEDLLamInfo (Body ARGt)  = (LBody ((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
))))
 ARGt)
)

and PROJLLamInfo (LLamInfo (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLLamInfo (LLamInfo (_, (ref (Option.SOME ARGt))))  = (PROJLLamInfo ARGt)

| PROJLLamInfo (LLabel ARGt)  = (Label (PROJLint ARGt)
)

| PROJLLamInfo (LBv ARGt)  = (Bv ((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
))))
 ARGt)
)

| PROJLLamInfo (LBody ARGt)  = (Body ((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
))))
 ARGt)
)


fun OCCURLLamInfo ARG0 (LLamInfo (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLLamInfo ARG0 (LLamInfo (_, (ref (Option.SOME ARG2))))  = (OCCURLLamInfo ARG0 ARG2)

| OCCURLLamInfo ARG0 (LLabel ARG1)  = (OCCURLint ARG0 ARG1)

| OCCURLLamInfo ARG0 (LBv ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLLamInfo ARG0 (LBody ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
))))
 ARG0 ARG1)


fun UNIFYLLamInfo (ARGt1, (LLamInfo (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)

| UNIFYLLamInfo (ARGt1, (LLamInfo (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLLamInfo ARG0 ARGt1)
)

| UNIFYLLamInfo ((LLamInfo (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)

| UNIFYLLamInfo ((ARGt1 as (LLamInfo (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLLamInfo (ARGt2, ARGt1) ARGtc)

| UNIFYLLamInfo ((LLabel ARGt1), (LLabel ARGt2)) ARGtc  = (UNIFYLint (ARGt1, ARGt2) ARGtc)

| UNIFYLLamInfo ((LBv ARGt1), (LBv ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLint (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLLamInfo ((LBody ARGt1), (LBody ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLint (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLint (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLLamInfo (_, _) ARGtc  = (false, ARGtc)

fun PRINTLLamInfo (LLamInfo (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLLamInfo (LLamInfo (_, (ref (Option.SOME ARGt))))  = (PRINTLLamInfo ARGt)

| PRINTLLamInfo (LLabel ARGt)  = (String.concat ["Label", " ", (PRINTLint ARGt)
])

| PRINTLLamInfo (LBv ARGt)  = (String.concat ["Bv", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLint ARGt)
), (fn ARGt =>
(PRINTLstring ARGt)
)))
 ARGt)
])

| PRINTLLamInfo (LBody ARGt)  = (String.concat ["Body", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLint ARGt)
), (fn ARGt =>
(PRINTLint ARGt)
)))
 ARGt)
])


fun RELisinLamInfo (ARG0 : LLamInfo, ARG1 : LLamInfo Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Llist (fn XS : LLamInfo Llist =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARG0, ARG1) ARGtc)
)) ARG0 (LLabel L)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 ((LLabel L)
, XS))
)
 ARGtc)
)))
))
))
))
, (SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.EXISTS Llist (fn XS : LLamInfo Llist =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARG0, ARG1) ARGtc)
)) ARG0 (LBv (TUPLECON2 (L, X))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 ((LBv (TUPLECON2 (L, X))
)
, XS))
)
 ARGtc)
)))
))
))
))
))
, (SMLOG.EXISTS Lint (fn L1 : Lint =>
(SMLOG.EXISTS Lint (fn L2 : Lint =>
(SMLOG.EXISTS Llist (fn XS : LLamInfo Llist =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARG0, ARG1) ARGtc)
)) ARG0 (LBody (TUPLECON2 (L1, L2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 ((LBody (TUPLECON2 (L1, L2))
)
, XS))
)
 ARGtc)
)))
))
))
))
))
, (SMLOG.EXISTS LLamInfo (fn X : LLamInfo =>
(SMLOG.EXISTS Llist (fn XS : LLamInfo Llist =>
(SMLOG.EXISTS LLamInfo (fn Y : LLamInfo =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARG0, ARG1) ARGtc)
)) ARG0 X ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 (Y, XS))
)
 ARGtc)
)))
, (RELisinLamInfo (X, XS))
))
))
))
))
))
)


fun isinLamInfo (ARG0, ARG1)  = ((fn (ARG0, ARG1) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLLamInfo ARG0)
, ((PRINTLlist (fn ARGt =>
(PRINTLLamInfo ARGt)
))
 ARG1)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELisinLamInfo (ARG0, ARG1) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLLamInfo ARG0)
 | Option.NONE =>
(LLamInfo ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLLamInfo ARGt)
), (fn ARGt =>
(PROJLLamInfo ARGt)
)))
 ARG1)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
))


fun RELappendLamInfo (ARG0 : LLamInfo Llist, ARG1 : LLamInfo Llist, ARG2 : LLamInfo Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Llist (fn XS : LLamInfo Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 (Lnil)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 XS ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 XS ARGtc)
)))
))
))
, (SMLOG.EXISTS LLamInfo (fn X : LLamInfo =>
(SMLOG.EXISTS Llist (fn XS : LLamInfo Llist =>
(SMLOG.EXISTS Llist (fn YS : LLamInfo Llist =>
(SMLOG.EXISTS Llist (fn ZS : LLamInfo Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 (Lopcoco (TUPLECON2 (X, XS))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 YS ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 (Lopcoco (TUPLECON2 (X, ZS))
)
 ARGtc)
)))
, (RELappendLamInfo (XS, YS, ZS))
))
))
))
))
))
))
)


fun appendLamInfo (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) (((PRINTLlist (fn ARGt =>
(PRINTLLamInfo ARGt)
))
 ARG0)
, ((PRINTLlist (fn ARGt =>
(PRINTLLamInfo ARGt)
))
 ARG1)
, ((PRINTLlist (fn ARGt =>
(PRINTLLamInfo ARGt)
))
 ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELappendLamInfo (ARG0, ARG1, ARG2) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLLamInfo ARGt)
), (fn ARGt =>
(PROJLLamInfo ARGt)
)))
 ARG0)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLLamInfo ARGt)
), (fn ARGt =>
(PROJLLamInfo ARGt)
)))
 ARG1)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLLamInfo ARGt)
), (fn ARGt =>
(PROJLLamInfo ARGt)
)))
 ARG2)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
))


fun RELgatherLam (ARG0 : Lexp, ARG1 : LLamInfo Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG0 (LVar (TUPLECON2 (X, L))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lnil)
 ARGtc)
)))
))
))
))
, (SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Llist (fn G : LLamInfo Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lint (fn LE : Lint =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG0 (LLam (TUPLECON3 (L, X, E))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 ((LLabel L)
, (Lopcoco (TUPLECON2 ((LBv (TUPLECON2 (L, X))
)
, (Lopcoco (TUPLECON2 ((LBody (TUPLECON2 (L, LE))
)
, G))
)
))
)
))
)
 ARGtc)
)))
, (RELlabel (E, LE))
))
, (RELgatherLam (E, G))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lexp (fn E1 : Lexp =>
(SMLOG.EXISTS Lexp (fn E2 : Lexp =>
(SMLOG.EXISTS Llist (fn G1 : LLamInfo Llist =>
(SMLOG.EXISTS Llist (fn G2 : LLamInfo Llist =>
(SMLOG.EXISTS Llist (fn G3 : LLamInfo Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG0 (LApp (TUPLECON3 (E1, E2, L))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 G3 ARGtc)
)))
, (RELgatherLam (E1, G1))
))
, (RELgatherLam (E2, G2))
))
, (RELappendLamInfo (G1, G2, G3))
))
))
))
))
))
))
))
))
)


fun gatherLam (ARG0, ARG1)  = ((fn (ARG0, ARG1) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLexp ARG0)
, ((PRINTLlist (fn ARGt =>
(PRINTLLamInfo ARGt)
))
 ARG1)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELgatherLam (ARG0, ARG1) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLexp ARG0)
 | Option.NONE =>
(Lexp ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLLamInfo ARGt)
), (fn ARGt =>
(PROJLLamInfo ARGt)
)))
 ARG1)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
))


fun RELgather (ARG0 : LLamInfo Llist, ARG1 : Lexp, ARG2 : Ledge Llist, ARG3 : Ledge Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Llist (fn G1 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G12 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G2 : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Llist (fn LamInfos : LLamInfo Llist =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 LamInfos ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG1 (LVar (TUPLECON2 (X, L))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 G1 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG3 G12 ARGtc)
)))
, (RELaddFv (LamInfos, X, L, G1, G2))
))
, (RELappendEdge (G1, G2, G12))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Llist (fn G1 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G2 : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Llist (fn LamInfos : LLamInfo Llist =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 LamInfos ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG1 (LLam (TUPLECON3 (L, X, E))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 G1 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG3 (Lopcoco (TUPLECON2 ((LNN (TUPLECON2 (L, L))
)
, G2))
)
 ARGtc)
)))
, (SMLOG.NOT (RELisin ((LNN (TUPLECON2 (L, L))
)
, G1))
)
))
, (RELgather (LamInfos, E, G1, G2))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Llist (fn G1 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G2 : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Llist (fn LamInfos : LLamInfo Llist =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 LamInfos ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG1 (LLam (TUPLECON3 (L, X, E))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 G1 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG3 G2 ARGtc)
)))
, (RELisin ((LNN (TUPLECON2 (L, L))
)
, G1))
))
, (RELgather (LamInfos, E, G1, G2))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lexp (fn E1 : Lexp =>
(SMLOG.EXISTS Lexp (fn E2 : Lexp =>
(SMLOG.EXISTS Llist (fn G1 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G12 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G123 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G1234 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G12345 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G2 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G3 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G4 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G5 : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lint (fn L1 : Lint =>
(SMLOG.EXISTS Lint (fn L2 : Lint =>
(SMLOG.EXISTS Llist (fn LamInfos : LLamInfo Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 LamInfos ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG1 (LApp (TUPLECON3 (E1, E2, L))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 G1 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG3 G12345 ARGtc)
)))
, (RELgather (LamInfos, E1, G1, G2))
))
, (RELappendEdge (G1, G2, G12))
))
, (RELgather (LamInfos, E2, G12, G3))
))
, (RELappendEdge (G12, G3, G123))
))
, (RELlabel (E1, L1))
))
, (RELlabel (E2, L2))
))
, (RELaddBv (LamInfos, L1, G123, L2, G4))
))
, (RELappendEdge (G123, G4, G1234))
))
, (RELaddBody (LamInfos, L1, G1234, L, G5))
))
, (RELappendEdge (G1234, G5, G12345))
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
))
))
))
))
))
))
)

and RELaddFv (ARG0 : LLamInfo Llist, ARG1 : Lstring, ARG2 : Lint, ARG3 : Ledge Llist, ARG4 : Ledge Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Llist (fn G : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lint (fn L' : Lint =>
(SMLOG.EXISTS Llist (fn LamInfos : LLamInfo Llist =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 LamInfos ARGtc)
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
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG2 L ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG3 G ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG4 (Lopcoco (TUPLECON2 ((LNN (TUPLECON2 (L, L'))
)
, (Lnil)
))
)
 ARGtc)
)))
, (RELisin ((LVN (TUPLECON2 (X, L'))
)
, G))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn G : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lint (fn L' : Lint =>
(SMLOG.EXISTS Llist (fn LamInfos : LLamInfo Llist =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 LamInfos ARGtc)
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
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG2 L ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG3 G ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG4 (Lnil)
 ARGtc)
)))
, (SMLOG.NOT (RELisin ((LVN (TUPLECON2 (X, L'))
)
, G))
)
))
))
))
))
))
))
))
)

and RELaddBv (ARG0 : LLamInfo Llist, ARG1 : Lint, ARG2 : Ledge Llist, ARG3 : Lint, ARG4 : Ledge Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Llist (fn G : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lint (fn L' : Lint =>
(SMLOG.EXISTS Llist (fn LamInfos : LLamInfo Llist =>
(SMLOG.EXISTS Lint (fn Lapparg : Lint =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 LamInfos ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG1 L ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 G ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG3 Lapparg ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG4 (Lopcoco (TUPLECON2 ((LVN (TUPLECON2 (X, Lapparg))
)
, (Lnil)
))
)
 ARGtc)
)))
, (RELisinLamInfo ((LBv (TUPLECON2 (L', X))
)
, LamInfos))
))
, (RELisin ((LNN (TUPLECON2 (L, L'))
)
, G))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn G : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lint (fn L' : Lint =>
(SMLOG.EXISTS Llist (fn LamInfos : LLamInfo Llist =>
(SMLOG.EXISTS Lint (fn Lapparg : Lint =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 LamInfos ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG1 L ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 G ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG3 Lapparg ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG4 (Lnil)
 ARGtc)
)))
, (SMLOG.NOT (RELisinLamInfo ((LBv (TUPLECON2 (L', X))
)
, LamInfos))
)
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn G : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lint (fn L' : Lint =>
(SMLOG.EXISTS Llist (fn LamInfos : LLamInfo Llist =>
(SMLOG.EXISTS Lint (fn Lapparg : Lint =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 LamInfos ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG1 L ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 G ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG3 Lapparg ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG4 (Lnil)
 ARGtc)
)))
, (RELisinLamInfo ((LBv (TUPLECON2 (L', X))
)
, LamInfos))
))
, (SMLOG.NOT (RELisin ((LNN (TUPLECON2 (L, L'))
)
, G))
)
))
))
))
))
))
))
))
))
)

and RELaddBody (ARG0 : LLamInfo Llist, ARG1 : Lint, ARG2 : Ledge Llist, ARG3 : Lint, ARG4 : Ledge Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Llist (fn G : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lint (fn L' : Lint =>
(SMLOG.EXISTS Llist (fn LamInfos : LLamInfo Llist =>
(SMLOG.EXISTS Lint (fn Lapp : Lint =>
(SMLOG.EXISTS Lint (fn Lbody : Lint =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 LamInfos ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG1 L ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 G ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG3 Lapp ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG4 (Lopcoco (TUPLECON2 ((LNN (TUPLECON2 (Lapp, Lbody))
)
, (Lnil)
))
)
 ARGtc)
)))
, (RELisinLamInfo ((LBody (TUPLECON2 (L', Lbody))
)
, LamInfos))
))
, (RELisin ((LNN (TUPLECON2 (L, L'))
)
, G))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn G : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lint (fn L' : Lint =>
(SMLOG.EXISTS Llist (fn LamInfos : LLamInfo Llist =>
(SMLOG.EXISTS Lint (fn Lapp : Lint =>
(SMLOG.EXISTS Lint (fn Lbody : Lint =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 LamInfos ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG1 L ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 G ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG3 Lapp ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG4 (Lnil)
 ARGtc)
)))
, (SMLOG.NOT (RELisinLamInfo ((LBody (TUPLECON2 (L', Lbody))
)
, LamInfos))
)
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn G : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lint (fn L' : Lint =>
(SMLOG.EXISTS Llist (fn LamInfos : LLamInfo Llist =>
(SMLOG.EXISTS Lint (fn Lapp : Lint =>
(SMLOG.EXISTS Lint (fn Lbody : Lint =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 LamInfos ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG1 L ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 G ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG3 Lapp ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG4 (Lnil)
 ARGtc)
)))
, (RELisinLamInfo ((LBody (TUPLECON2 (L', Lbody))
)
, LamInfos))
))
, (SMLOG.NOT (RELisin ((LNN (TUPLECON2 (L, L'))
)
, G))
)
))
))
))
))
))
))
))
))
)


fun gather (ARG0, ARG1, ARG2, ARG3)  = ((fn (ARG0, ARG1, ARG2, ARG3) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) (((PRINTLlist (fn ARGt =>
(PRINTLLamInfo ARGt)
))
 ARG0)
, (PRINTLexp ARG1)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG2)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG3)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELgather (ARG0, ARG1, ARG2, ARG3) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLLamInfo ARGt)
), (fn ARGt =>
(PROJLLamInfo ARGt)
)))
 ARG0)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLexp ARG1)
 | Option.NONE =>
(Lexp ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG2)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
, ((fn (Option.SOME ARG3) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG3)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG3)
))

and addFv (ARG0, ARG1, ARG2, ARG3, ARG4)  = ((fn (ARG0, ARG1, ARG2, ARG3, ARG4) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) (((PRINTLlist (fn ARGt =>
(PRINTLLamInfo ARGt)
))
 ARG0)
, (PRINTLstring ARG1)
, (PRINTLint ARG2)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG3)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG4)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELaddFv (ARG0, ARG1, ARG2, ARG3, ARG4) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLLamInfo ARGt)
), (fn ARGt =>
(PROJLLamInfo ARGt)
)))
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
(EMBEDLint ARG2)
 | Option.NONE =>
(Lint ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
, ((fn (Option.SOME ARG3) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG3)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG3)
, ((fn (Option.SOME ARG4) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG4)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG4)
))

and addBv (ARG0, ARG1, ARG2, ARG3, ARG4)  = ((fn (ARG0, ARG1, ARG2, ARG3, ARG4) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) (((PRINTLlist (fn ARGt =>
(PRINTLLamInfo ARGt)
))
 ARG0)
, (PRINTLint ARG1)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG2)
, (PRINTLint ARG3)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG4)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELaddBv (ARG0, ARG1, ARG2, ARG3, ARG4) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLLamInfo ARGt)
), (fn ARGt =>
(PROJLLamInfo ARGt)
)))
 ARG0)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLint ARG1)
 | Option.NONE =>
(Lint ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG2)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
, ((fn (Option.SOME ARG3) =>
(EMBEDLint ARG3)
 | Option.NONE =>
(Lint ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG3)
, ((fn (Option.SOME ARG4) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG4)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG4)
))

and addBody (ARG0, ARG1, ARG2, ARG3, ARG4)  = ((fn (ARG0, ARG1, ARG2, ARG3, ARG4) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) (((PRINTLlist (fn ARGt =>
(PRINTLLamInfo ARGt)
))
 ARG0)
, (PRINTLint ARG1)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG2)
, (PRINTLint ARG3)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG4)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELaddBody (ARG0, ARG1, ARG2, ARG3, ARG4) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLLamInfo ARGt)
), (fn ARGt =>
(PROJLLamInfo ARGt)
)))
 ARG0)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLint ARG1)
 | Option.NONE =>
(Lint ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG2)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
, ((fn (Option.SOME ARG3) =>
(EMBEDLint ARG3)
 | Option.NONE =>
(Lint ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG3)
, ((fn (Option.SOME ARG4) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG4)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG4)
))


datatype num = Zero | Succ of num

datatype Onum = datatype num

datatype Lnum = Lnum of (Ostring * Lnum Option.option ref) | LZero | LSucc of Lnum

fun EMBEDLnum Zero  = LZero
| EMBEDLnum (Succ ARGt)  = (LSucc (EMBEDLnum ARGt)
)

and PROJLnum (Lnum (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLnum (Lnum (_, (ref (Option.SOME ARGt))))  = (PROJLnum ARGt)

| PROJLnum LZero  = Zero
| PROJLnum (LSucc ARGt)  = (Succ (PROJLnum ARGt)
)


fun OCCURLnum ARG0 (Lnum (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLnum ARG0 (Lnum (_, (ref (Option.SOME ARG2))))  = (OCCURLnum ARG0 ARG2)

| OCCURLnum ARG0 LZero  = false
| OCCURLnum ARG0 (LSucc ARG1)  = (OCCURLnum ARG0 ARG1)


fun UNIFYLnum (ARGt1, (Lnum (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLnum (ARGt1, ARGt2) ARGtc)

| UNIFYLnum (ARGt1, (Lnum (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLnum ARG0 ARGt1)
)

| UNIFYLnum ((Lnum (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLnum (ARGt1, ARGt2) ARGtc)

| UNIFYLnum ((ARGt1 as (Lnum (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLnum (ARGt2, ARGt1) ARGtc)

| UNIFYLnum (LZero, LZero) ARGtc  = (true, ARGtc)
| UNIFYLnum ((LSucc ARGt1), (LSucc ARGt2)) ARGtc  = (UNIFYLnum (ARGt1, ARGt2) ARGtc)

| UNIFYLnum (_, _) ARGtc  = (false, ARGtc)

fun PRINTLnum (Lnum (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLnum (Lnum (_, (ref (Option.SOME ARGt))))  = (PRINTLnum ARGt)

| PRINTLnum LZero  = "Zero"
| PRINTLnum (LSucc ARGt)  = (String.concat ["Succ", " ", (PRINTLnum ARGt)
])


fun RELtransitive (ARG0 : Ledge Llist, ARG1 : Ledge Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 (Lnil)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lnil)
 ARGtc)
)))
))
, (SMLOG.EXISTS Ledge (fn X : Ledge =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 (Lopcoco (TUPLECON2 (X, (Lnil)
))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 (X, (Lnil)
))
)
 ARGtc)
)))
))
))
, (SMLOG.EXISTS Llist (fn G : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G' : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G'' : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L1 : Lint =>
(SMLOG.EXISTS Lint (fn L2 : Lint =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 (Lopcoco (TUPLECON2 ((LNN (TUPLECON2 (L1, L2))
)
, G))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 ((LNN (TUPLECON2 (L1, L2))
)
, G''))
)
 ARGtc)
)))
, (RELtransitive (G, G'))
))
, (RELtran ((LNN (TUPLECON2 (L1, L2))
)
, G', G''))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn G : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G' : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 (Lopcoco (TUPLECON2 ((LVN (TUPLECON2 (X, L))
)
, G))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 ((LVN (TUPLECON2 (X, L))
)
, G'))
)
 ARGtc)
)))
, (RELtransitive (G, G'))
))
))
))
))
))
))
)

and RELtran (ARG0 : Ledge, ARG1 : Ledge Llist, ARG2 : Ledge Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lint (fn L1 : Lint =>
(SMLOG.EXISTS Lint (fn L2 : Lint =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLedge (ARG0, ARG1) ARGtc)
)) ARG0 (LNN (TUPLECON2 (L1, L2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lnil)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 (Lnil)
 ARGtc)
)))
))
))
))
, (SMLOG.EXISTS Llist (fn G : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G' : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L1 : Lint =>
(SMLOG.EXISTS Lint (fn L2 : Lint =>
(SMLOG.EXISTS Lint (fn L3 : Lint =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLedge (ARG0, ARG1) ARGtc)
)) ARG0 (LNN (TUPLECON2 (L1, L2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 ((LNN (TUPLECON2 (L2, L3))
)
, G))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 (Lopcoco (TUPLECON2 ((LNN (TUPLECON2 (L2, L3))
)
, (Lopcoco (TUPLECON2 ((LNN (TUPLECON2 (L1, L3))
)
, G'))
)
))
)
 ARGtc)
)))
, (RELtran ((LNN (TUPLECON2 (L1, L2))
)
, G, G'))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn G : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G' : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L1 : Lint =>
(SMLOG.EXISTS Lint (fn L2 : Lint =>
(SMLOG.EXISTS Lint (fn L3 : Lint =>
(SMLOG.EXISTS Lint (fn L4 : Lint =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLedge (ARG0, ARG1) ARGtc)
)) ARG0 (LNN (TUPLECON2 (L1, L2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 ((LNN (TUPLECON2 (L3, L4))
)
, G))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 (Lopcoco (TUPLECON2 ((LNN (TUPLECON2 (L3, L4))
)
, G'))
)
 ARGtc)
)))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLbool (ARG0, ARG1) ARGtc)
)) ARG0 (Lfalse)
 ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLbool ARG0)
)) (eqLabel ((SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
(PROJLint ARGt)
) L2)
) ARGtc)
, (SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
(PROJLint ARGt)
) L3)
) ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
, (RELtran ((LNN (TUPLECON2 (L1, L2))
)
, G, G'))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn G : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G' : Ledge Llist =>
(SMLOG.EXISTS Lint (fn L : Lint =>
(SMLOG.EXISTS Lint (fn L1 : Lint =>
(SMLOG.EXISTS Lint (fn L2 : Lint =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLedge (ARG0, ARG1) ARGtc)
)) ARG0 (LNN (TUPLECON2 (L1, L2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 ((LVN (TUPLECON2 (X, L))
)
, G))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 (Lopcoco (TUPLECON2 ((LVN (TUPLECON2 (X, L))
)
, G'))
)
 ARGtc)
)))
, (RELtran ((LNN (TUPLECON2 (L1, L2))
)
, G, G'))
))
))
))
))
))
))
))
))
)


fun transitive (ARG0, ARG1)  = ((fn (ARG0, ARG1) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) (((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG0)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG1)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELtransitive (ARG0, ARG1) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG0)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG1)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
))

and tran (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLedge ARG0)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG1)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELtran (ARG0, ARG1, ARG2) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLedge ARG0)
 | Option.NONE =>
(Ledge ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG1)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG2)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
))


fun RELloopGather (ARG0 : LLamInfo Llist, ARG1 : Lexp, ARG2 : Ledge Llist, ARG3 : Ledge Llist, ARG4 : Lbool)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Llist (fn G : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G1 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G2 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G3 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn LamInfos : LLamInfo Llist =>
(SMLOG.EXISTS Lbool (fn flag : Lbool =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 LamInfos ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG1 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 G1 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG3 G ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLbool (ARG0, ARG1) ARGtc)
)) ARG4 (Lfalse)
 ARGtc)
)))
, (RELgather (LamInfos, E, G1, G2))
))
, (RELtransitive (G2, G3))
))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLbool (ARG0, ARG1) ARGtc)
)) ARG0 flag ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLbool ARG0)
)) (eq ((SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARGt)
) G1)
) ARGtc)
, (SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARGt)
) G3)
) ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
, (fn ARGtc =>
(((((fn () =>
SMLOG.TRUE) ((fn () =>
()) (print ((SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
(PROJLstring ARGt)
) (THEstring "loop ")
)
) ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
, (RELloopGather (LamInfos, E, G3, G, flag))
))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Llist (fn G1 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn LamInfos : LLamInfo Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLLamInfo (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLLamInfo ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 LamInfos ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG1 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 G1 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG3 G1 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLbool (ARG0, ARG1) ARGtc)
)) ARG4 (Ltrue)
 ARGtc)
)))
))
))
))
))
)


fun loopGather (ARG0, ARG1, ARG2, ARG3, ARG4)  = ((fn (ARG0, ARG1, ARG2, ARG3, ARG4) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) (((PRINTLlist (fn ARGt =>
(PRINTLLamInfo ARGt)
))
 ARG0)
, (PRINTLexp ARG1)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG2)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG3)
, (PRINTLbool ARG4)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELloopGather (ARG0, ARG1, ARG2, ARG3, ARG4) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLLamInfo ARGt)
), (fn ARGt =>
(PROJLLamInfo ARGt)
)))
 ARG0)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLexp ARG1)
 | Option.NONE =>
(Lexp ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG2)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
, ((fn (Option.SOME ARG3) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG3)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG3)
, ((fn (Option.SOME ARG4) =>
(EMBEDLbool ARG4)
 | Option.NONE =>
(Lbool ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG4)
))


fun RELcfa (ARG0 : Lexp, ARG1 : Ledge Llist, ARG2 : Ledge Llist)  = (SMLOG.STEP (SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Llist (fn G1 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn G2 : Ledge Llist =>
(SMLOG.EXISTS Llist (fn LamInfos : LLamInfo Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG0 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 G1 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLedge (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLedge ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG2 G2 ARGtc)
)))
, (RELgatherLam (E, LamInfos))
))
, (RELloopGather (LamInfos, E, G1, G2, (Lfalse)
))
))
))
))
))
))
)


fun cfa (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLexp ARG0)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG1)
, ((PRINTLlist (fn ARGt =>
(PRINTLedge ARGt)
))
 ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELcfa (ARG0, ARG1, ARG2) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLexp ARG0)
 | Option.NONE =>
(Lexp ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG1)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLedge ARGt)
), (fn ARGt =>
(PROJLedge ARGt)
)))
 ARG2)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
))

