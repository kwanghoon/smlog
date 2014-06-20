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


datatype depth = Z | S of depth

datatype Odepth = datatype depth

datatype Ldepth = Ldepth of (Ostring * Ldepth Option.option ref) | LZ | LS of Ldepth

fun EMBEDLdepth Z  = LZ
| EMBEDLdepth (S ARGt)  = (LS (EMBEDLdepth ARGt)
)

and PROJLdepth (Ldepth (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLdepth (Ldepth (_, (ref (Option.SOME ARGt))))  = (PROJLdepth ARGt)

| PROJLdepth LZ  = Z
| PROJLdepth (LS ARGt)  = (S (PROJLdepth ARGt)
)


fun OCCURLdepth ARG0 (Ldepth (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLdepth ARG0 (Ldepth (_, (ref (Option.SOME ARG2))))  = (OCCURLdepth ARG0 ARG2)

| OCCURLdepth ARG0 LZ  = false
| OCCURLdepth ARG0 (LS ARG1)  = (OCCURLdepth ARG0 ARG1)


fun UNIFYLdepth (ARGt1, (Ldepth (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLdepth (ARGt1, ARGt2) ARGtc)

| UNIFYLdepth (ARGt1, (Ldepth (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLdepth ARG0 ARGt1)
)

| UNIFYLdepth ((Ldepth (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLdepth (ARGt1, ARGt2) ARGtc)

| UNIFYLdepth ((ARGt1 as (Ldepth (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLdepth (ARGt2, ARGt1) ARGtc)

| UNIFYLdepth (LZ, LZ) ARGtc  = (true, ARGtc)
| UNIFYLdepth ((LS ARGt1), (LS ARGt2)) ARGtc  = (UNIFYLdepth (ARGt1, ARGt2) ARGtc)

| UNIFYLdepth (_, _) ARGtc  = (false, ARGtc)

fun PRINTLdepth (Ldepth (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLdepth (Ldepth (_, (ref (Option.SOME ARGt))))  = (PRINTLdepth ARGt)

| PRINTLdepth LZ  = "Z"
| PRINTLdepth (LS ARGt)  = (String.concat ["S", " ", (PRINTLdepth ARGt)
])


fun RELmember (ARG0 : Lint, ARG1 : Lint Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lint (fn X : Lint =>
(SMLOG.EXISTS Llist (fn XS : Lint Llist =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG0 X ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLint (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 (X, XS))
)
 ARGtc)
)))
))
))
))
, (SMLOG.EXISTS Lint (fn X : Lint =>
(SMLOG.EXISTS Llist (fn XS : Lint Llist =>
(SMLOG.EXISTS Lint (fn Y : Lint =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG0 X ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLint (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 (Y, XS))
)
 ARGtc)
)))
, (RELmember (X, XS))
))
))
))
))
))
)


fun member (ARG0, ARG1)  = ((fn (ARG0, ARG1) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLint ARG0)
, ((PRINTLlist (fn ARGt =>
(PRINTLint ARGt)
))
 ARG1)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELmember (ARG0, ARG1) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLint ARG0)
 | Option.NONE =>
(Lint ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)))
 ARG1)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
))


fun RELInt (ARG0 : Lint)  = (SMLOG.STEP (SMLOG.EXISTS Lint (fn X : Lint =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG0 X ARGtc)
)))
, (RELmember (X, (Lopcoco (TUPLECON2 ((THEint 3)
, (Lnil)
))
)
))
))
))
)


fun Int (ARG0)  = ((fn (ARG0) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLint ARG0)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELInt (ARG0) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLint ARG0)
 | Option.NONE =>
(Lint ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
))


fun RELList (ARG0 : Ldepth, ARG1 : Lint Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Ldepth (fn D : Ldepth =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLdepth (ARG0, ARG1) ARGtc)
)) ARG0 D ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLint (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lnil)
 ARGtc)
)))
))
))
, (SMLOG.EXISTS Ldepth (fn D : Ldepth =>
(SMLOG.EXISTS Lint (fn X : Lint =>
(SMLOG.EXISTS Llist (fn XS : Lint Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLdepth (ARG0, ARG1) ARGtc)
)) ARG0 (LS D)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLint (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 (X, XS))
)
 ARGtc)
)))
, (RELInt (X))
))
, (RELList (D, XS))
))
))
))
))
))
)


fun List (ARG0, ARG1)  = ((fn (ARG0, ARG1) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLdepth ARG0)
, ((PRINTLlist (fn ARGt =>
(PRINTLint ARGt)
))
 ARG1)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELList (ARG0, ARG1) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLdepth ARG0)
 | Option.NONE =>
(Ldepth ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)))
 ARG1)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
))


fun RELgenList (ARG0 : Lint Llist)  = (SMLOG.STEP (SMLOG.EXISTS Llist (fn X : Lint Llist =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLint (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG0 X ARGtc)
)))
, (RELList ((LS (LS (LS (LZ)
)
)
)
, X))
))
))
)


fun genList (ARG0)  = ((fn (ARG0) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) (((PRINTLlist (fn ARGt =>
(PRINTLint ARGt)
))
 ARG0)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELgenList (ARG0) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)))
 ARG0)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
))


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


datatype tree = Leaf of int | Node of (tree * int * tree)

datatype Otree = datatype tree

datatype Ltree = Ltree of (Ostring * Ltree Option.option ref) | LLeaf of Lint | LNode of (Ltree , Lint , Ltree) TUPLETYPE3

fun EMBEDLtree (Leaf ARGt)  = (LLeaf (EMBEDLint ARGt)
)

| EMBEDLtree (Node ARGt)  = (LNode ((EMBEDTUPLETYPE3 (((fn ARGt =>
(EMBEDLtree ARGt)
), (fn ARGt =>
(PROJLtree ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLtree ARGt)
), (fn ARGt =>
(PROJLtree ARGt)
))))
 ARGt)
)

and PROJLtree (Ltree (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLtree (Ltree (_, (ref (Option.SOME ARGt))))  = (PROJLtree ARGt)

| PROJLtree (LLeaf ARGt)  = (Leaf (PROJLint ARGt)
)

| PROJLtree (LNode ARGt)  = (Node ((PROJTUPLETYPE3 (((fn ARGt =>
(EMBEDLtree ARGt)
), (fn ARGt =>
(PROJLtree ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLtree ARGt)
), (fn ARGt =>
(PROJLtree ARGt)
))))
 ARGt)
)


fun OCCURLtree ARG0 (Ltree (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLtree ARG0 (Ltree (_, (ref (Option.SOME ARG2))))  = (OCCURLtree ARG0 ARG2)

| OCCURLtree ARG0 (LLeaf ARG1)  = (OCCURLint ARG0 ARG1)

| OCCURLtree ARG0 (LNode ARG1)  = ((OCCURTUPLETYPE3 ((fn ARGv =>
(fn ARGt =>
(OCCURLtree ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtree ARGv ARGt)
))))
 ARG0 ARG1)


fun UNIFYLtree (ARGt1, (Ltree (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLtree (ARGt1, ARGt2) ARGtc)

| UNIFYLtree (ARGt1, (Ltree (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLtree ARG0 ARGt1)
)

| UNIFYLtree ((Ltree (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLtree (ARGt1, ARGt2) ARGtc)

| UNIFYLtree ((ARGt1 as (Ltree (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLtree (ARGt2, ARGt1) ARGtc)

| UNIFYLtree ((LLeaf ARGt1), (LLeaf ARGt2)) ARGtc  = (UNIFYLint (ARGt1, ARGt2) ARGtc)

| UNIFYLtree ((LNode ARGt1), (LNode ARGt2)) ARGtc  = ((UNIFYTUPLETYPE3 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtree (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtree ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLint (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtree (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtree ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLtree (_, _) ARGtc  = (false, ARGtc)

fun PRINTLtree (Ltree (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLtree (Ltree (_, (ref (Option.SOME ARGt))))  = (PRINTLtree ARGt)

| PRINTLtree (LLeaf ARGt)  = (String.concat ["Leaf", " ", (PRINTLint ARGt)
])

| PRINTLtree (LNode ARGt)  = (String.concat ["Node", " ", ((PRINTTUPLETYPE3 ((fn ARGt =>
(PRINTLtree ARGt)
), (fn ARGt =>
(PRINTLint ARGt)
), (fn ARGt =>
(PRINTLtree ARGt)
)))
 ARGt)
])


fun RELTree (ARG0 : Ldepth, ARG1 : Ltree)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Ldepth (fn D : Ldepth =>
(SMLOG.EXISTS Lint (fn X : Lint =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLdepth (ARG0, ARG1) ARGtc)
)) ARG0 D ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtree (ARG0, ARG1) ARGtc)
)) ARG1 (LLeaf X)
 ARGtc)
)))
, (RELInt (X))
))
))
))
))
, (SMLOG.EXISTS Ldepth (fn D : Ldepth =>
(SMLOG.EXISTS Ltree (fn T1 : Ltree =>
(SMLOG.EXISTS Ltree (fn T2 : Ltree =>
(SMLOG.EXISTS Lint (fn X : Lint =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLdepth (ARG0, ARG1) ARGtc)
)) ARG0 (LS D)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtree (ARG0, ARG1) ARGtc)
)) ARG1 (LNode (TUPLECON3 (T1, X, T2))
)
 ARGtc)
)))
, (RELTree (D, T1))
))
, (RELInt (X))
))
, (RELTree (D, T2))
))
))
))
))
))
))
)


fun Tree (ARG0, ARG1)  = ((fn (ARG0, ARG1) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLdepth ARG0)
, (PRINTLtree ARG1)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELTree (ARG0, ARG1) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLdepth ARG0)
 | Option.NONE =>
(Ldepth ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLtree ARG1)
 | Option.NONE =>
(Ltree ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
))


fun RELgenTree (ARG0 : Ltree)  = (SMLOG.STEP (SMLOG.EXISTS Ltree (fn T : Ltree =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLtree (ARG0, ARG1) ARGtc)
)) ARG0 T ARGtc)
)))
, (RELTree ((LS (LS (LS (LZ)
)
)
)
, T))
))
))
)


fun genTree (ARG0)  = ((fn (ARG0) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLtree ARG0)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELgenTree (ARG0) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLtree ARG0)
 | Option.NONE =>
(Ltree ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
))


type id = string

datatype ty = Int | Fun of (ty * ty)

datatype Oty = datatype ty

datatype Lty = Lty of (Ostring * Lty Option.option ref) | LInt | LFun of (Lty , Lty) TUPLETYPE2

fun EMBEDLty Int  = LInt
| EMBEDLty (Fun ARGt)  = (LFun ((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
))))
 ARGt)
)

and PROJLty (Lty (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLty (Lty (_, (ref (Option.SOME ARGt))))  = (PROJLty ARGt)

| PROJLty LInt  = Int
| PROJLty (LFun ARGt)  = (Fun ((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
))))
 ARGt)
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


datatype term = Var of id | Const of int | Lam of (id * ty * term) | App of (term * term)

datatype Oterm = datatype term

datatype Lterm = Lterm of (Ostring * Lterm Option.option ref) | LVar of Lstring | LConst of Lint | LLam of (Lstring , Lty , Lterm) TUPLETYPE3 | LApp of (Lterm , Lterm) TUPLETYPE2

fun EMBEDLterm (Var ARGt)  = (LVar (EMBEDLstring ARGt)
)

| EMBEDLterm (Const ARGt)  = (LConst (EMBEDLint ARGt)
)

| EMBEDLterm (Lam ARGt)  = (LLam ((EMBEDTUPLETYPE3 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
(EMBEDLterm ARGt)
), (fn ARGt =>
(PROJLterm ARGt)
))))
 ARGt)
)

| EMBEDLterm (App ARGt)  = (LApp ((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLterm ARGt)
), (fn ARGt =>
(PROJLterm ARGt)
)), ((fn ARGt =>
(EMBEDLterm ARGt)
), (fn ARGt =>
(PROJLterm ARGt)
))))
 ARGt)
)

and PROJLterm (Lterm (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLterm (Lterm (_, (ref (Option.SOME ARGt))))  = (PROJLterm ARGt)

| PROJLterm (LVar ARGt)  = (Var (PROJLstring ARGt)
)

| PROJLterm (LConst ARGt)  = (Const (PROJLint ARGt)
)

| PROJLterm (LLam ARGt)  = (Lam ((PROJTUPLETYPE3 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
(EMBEDLterm ARGt)
), (fn ARGt =>
(PROJLterm ARGt)
))))
 ARGt)
)

| PROJLterm (LApp ARGt)  = (App ((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLterm ARGt)
), (fn ARGt =>
(PROJLterm ARGt)
)), ((fn ARGt =>
(EMBEDLterm ARGt)
), (fn ARGt =>
(PROJLterm ARGt)
))))
 ARGt)
)


fun OCCURLterm ARG0 (Lterm (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLterm ARG0 (Lterm (_, (ref (Option.SOME ARG2))))  = (OCCURLterm ARG0 ARG2)

| OCCURLterm ARG0 (LVar ARG1)  = (OCCURLstring ARG0 ARG1)

| OCCURLterm ARG0 (LConst ARG1)  = (OCCURLint ARG0 ARG1)

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


fun UNIFYLterm (ARGt1, (Lterm (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLterm (ARGt1, ARGt2) ARGtc)

| UNIFYLterm (ARGt1, (Lterm (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLterm ARG0 ARGt1)
)

| UNIFYLterm ((Lterm (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLterm (ARGt1, ARGt2) ARGtc)

| UNIFYLterm ((ARGt1 as (Lterm (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLterm (ARGt2, ARGt1) ARGtc)

| UNIFYLterm ((LVar ARGt1), (LVar ARGt2)) ARGtc  = (UNIFYLstring (ARGt1, ARGt2) ARGtc)

| UNIFYLterm ((LConst ARGt1), (LConst ARGt2)) ARGtc  = (UNIFYLint (ARGt1, ARGt2) ARGtc)

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

| UNIFYLterm (_, _) ARGtc  = (false, ARGtc)

fun PRINTLterm (Lterm (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLterm (Lterm (_, (ref (Option.SOME ARGt))))  = (PRINTLterm ARGt)

| PRINTLterm (LVar ARGt)  = (String.concat ["Var", " ", (PRINTLstring ARGt)
])

| PRINTLterm (LConst ARGt)  = (String.concat ["Const", " ", (PRINTLint ARGt)
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


type env = (id * ty) list

fun RELlookup (ARG0 : Lstring, ARG1 : (Lstring , Lty) TUPLETYPE2 Llist, ARG2 : Lty)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Llist (fn E : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG0 X ARGtc)
)))
, (fn ARGtc =>
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
)) ARG1 (Lopcoco (TUPLECON2 ((TUPLECON2 (X, T))
, E))
)
 ARGtc)
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
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.EXISTS Lstring (fn Y : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLstring (ARG0, ARG1) ARGtc)
)) ARG0 X ARGtc)
)))
, (fn ARGtc =>
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
)) ARG1 (Lopcoco (TUPLECON2 ((TUPLECON2 (Y, T))
, E))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG2 T ARGtc)
)))
, (RELlookup (X, E, T))
))
))
))
))
))
))
)


fun lookup (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLstring ARG0)
, ((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
))
 ARG1)
, (PRINTLty ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELlookup (ARG0, ARG1, ARG2) (fn _ =>
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
((EMBEDLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
))))
 ARGt)
)))
 ARG1)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
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


fun RELtyping (ARG0 : Ldepth, ARG1 : (Lstring , Lty) TUPLETYPE2 Llist, ARG2 : Lterm, ARG3 : Lty)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Ldepth (fn D : Ldepth =>
(SMLOG.EXISTS Llist (fn E : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLdepth (ARG0, ARG1) ARGtc)
)) ARG0 D ARGtc)
)))
, (fn ARGtc =>
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
)) ARG1 E ARGtc)
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
, (RELlookup (X, E, T))
))
))
))
))
))
))
, (SMLOG.EXISTS Ldepth (fn D : Ldepth =>
(SMLOG.EXISTS Llist (fn E : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lint (fn X : Lint =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLdepth (ARG0, ARG1) ARGtc)
)) ARG0 D ARGtc)
)))
, (fn ARGtc =>
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
)) ARG1 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG2 (LConst X)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 (LInt)
 ARGtc)
)))
))
))
))
))
, (SMLOG.EXISTS Ldepth (fn D : Ldepth =>
(SMLOG.EXISTS Llist (fn E : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lterm (fn EX : Lterm =>
(SMLOG.EXISTS Lty (fn T1 : Lty =>
(SMLOG.EXISTS Lty (fn T2 : Lty =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLdepth (ARG0, ARG1) ARGtc)
)) ARG0 (LS D)
 ARGtc)
)))
, (fn ARGtc =>
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
)) ARG1 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG2 (LLam (TUPLECON3 (X, T1, EX))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 (LFun (TUPLECON2 (T1, T2))
)
 ARGtc)
)))
, (RELtyping (D, (Lopcoco (TUPLECON2 ((TUPLECON2 (X, T1))
, E))
)
, EX, T2))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Ldepth (fn D : Ldepth =>
(SMLOG.EXISTS Llist (fn E : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lterm (fn EX_arg : Lterm =>
(SMLOG.EXISTS Lterm (fn EX_fn : Lterm =>
(SMLOG.EXISTS Lty (fn T1 : Lty =>
(SMLOG.EXISTS Lty (fn T2 : Lty =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLdepth (ARG0, ARG1) ARGtc)
)) ARG0 (LS D)
 ARGtc)
)))
, (fn ARGtc =>
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
)) ARG1 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG2 (LApp (TUPLECON2 (EX_fn, EX_arg))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 T2 ARGtc)
)))
, (RELtyping (D, E, EX_fn, (LFun (TUPLECON2 (T1, T2))
)
))
))
, (RELtyping (D, E, EX_arg, T1))
))
))
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
) ((PRINTLdepth ARG0)
, ((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
))
 ARG1)
, (PRINTLterm ARG2)
, (PRINTLty ARG3)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELtyping (ARG0, ARG1, ARG2, ARG3) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLdepth ARG0)
 | Option.NONE =>
(Ldepth ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
((EMBEDLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
))))
 ARGt)
)))
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
))


fun RELgenTerm (ARG0 : Lterm, ARG1 : Lty)  = (SMLOG.STEP (SMLOG.EXISTS Lterm (fn TERM : Lterm =>
(SMLOG.EXISTS Lty (fn TYPE : Lty =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLterm (ARG0, ARG1) ARGtc)
)) ARG0 TERM ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG1 TYPE ARGtc)
)))
, (RELtyping ((LS (LS (LZ)
)
)
, (Lnil)
, TERM, TYPE))
))
))
))
)


fun genTerm (ARG0, ARG1)  = ((fn (ARG0, ARG1) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLterm ARG0)
, (PRINTLty ARG1)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELgenTerm (ARG0, ARG1) (fn _ =>
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
(EMBEDLty ARG1)
 | Option.NONE =>
(Lty ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
))

