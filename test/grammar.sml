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


datatype 'a option = NONE | SOME of 'a

datatype Ooption = datatype option

datatype 'a Loption = Loption of (Ostring * 'a Loption Option.option ref) | LNONE | LSOME of 'a

fun EMBEDLoption (qua) NONE  = LNONE
| EMBEDLoption (qua) (SOME ARGt)  = (LSOME ((fn (ARG1, ARG2) =>
ARG1) qua ARGt)
)

and PROJLoption (qua) (Loption (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLoption (qua) (Loption (_, (ref (Option.SOME ARGt))))  = ((PROJLoption (qua))
 ARGt)

| PROJLoption (qua) LNONE  = NONE
| PROJLoption (qua) (LSOME ARGt)  = (SOME ((fn (ARG1, ARG2) =>
ARG2) qua ARGt)
)


fun OCCURLoption (qua) ARG0 (Loption (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLoption (qua) ARG0 (Loption (_, (ref (Option.SOME ARG2))))  = ((OCCURLoption (qua))
 ARG0 ARG2)

| OCCURLoption (qua) ARG0 LNONE  = false
| OCCURLoption (qua) ARG0 (LSOME ARG1)  = (qua ARG0 ARG1)


fun UNIFYLoption qua (ARGt1, (Loption (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYLoption (qua))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLoption qua (ARGt1, (Loption (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURLoption ((fn (ARG1, ARG2) =>
ARG2) qua)
)
 ARG0 ARGt1)
)

| UNIFYLoption qua ((Loption (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYLoption (qua))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLoption qua ((ARGt1 as (Loption (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYLoption (qua))
 (ARGt2, ARGt1) ARGtc)

| UNIFYLoption qua (LNONE, LNONE) ARGtc  = (true, ARGtc)
| UNIFYLoption qua ((LSOME ARGt1), (LSOME ARGt2)) ARGtc  = (((fn (ARG1, ARG2) =>
ARG1) qua)
 (ARGt1, ARGt2) ARGtc)

| UNIFYLoption qua (_, _) ARGtc  = (false, ARGtc)

fun PRINTLoption (qua) (Loption (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLoption (qua) (Loption (_, (ref (Option.SOME ARGt))))  = ((PRINTLoption (qua))
 ARGt)

| PRINTLoption (qua) LNONE  = "NONE"
| PRINTLoption (qua) (LSOME ARGt)  = (String.concat ["SOME", " ", (qua ARGt)
])


type id = string

type path = id list

type tyvar = string

datatype inout = IN | OUT

datatype Oinout = datatype inout

datatype Linout = Linout of (Ostring * Linout Option.option ref) | LIN | LOUT

fun EMBEDLinout IN  = LIN
| EMBEDLinout OUT  = LOUT
and PROJLinout (Linout (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLinout (Linout (_, (ref (Option.SOME ARGt))))  = (PROJLinout ARGt)

| PROJLinout LIN  = IN
| PROJLinout LOUT  = OUT

fun OCCURLinout ARG0 (Linout (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLinout ARG0 (Linout (_, (ref (Option.SOME ARG2))))  = (OCCURLinout ARG0 ARG2)

| OCCURLinout ARG0 LIN  = false
| OCCURLinout ARG0 LOUT  = false

fun UNIFYLinout (ARGt1, (Linout (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLinout (ARGt1, ARGt2) ARGtc)

| UNIFYLinout (ARGt1, (Linout (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLinout ARG0 ARGt1)
)

| UNIFYLinout ((Linout (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLinout (ARGt1, ARGt2) ARGtc)

| UNIFYLinout ((ARGt1 as (Linout (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLinout (ARGt2, ARGt1) ARGtc)

| UNIFYLinout (LIN, LIN) ARGtc  = (true, ARGtc)
| UNIFYLinout (LOUT, LOUT) ARGtc  = (true, ARGtc)
| UNIFYLinout (_, _) ARGtc  = (false, ARGtc)

fun PRINTLinout (Linout (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLinout (Linout (_, (ref (Option.SOME ARGt))))  = (PRINTLinout ARGt)

| PRINTLinout LIN  = "IN"
| PRINTLinout LOUT  = "OUT"

datatype ty = VarTy of tyvar | ConTy of (path * ty list) | RecordTy of (id * ty) list | TupleTy of ty list | ForallTy of (tyvar list * ty)

datatype Oty = datatype ty

datatype Lty = Lty of (Ostring * Lty Option.option ref) | LVarTy of Lstring | LConTy of (Lstring Llist , Lty Llist) TUPLETYPE2 | LRecordTy of (Lstring , Lty) TUPLETYPE2 Llist | LTupleTy of Lty Llist | LForallTy of (Lstring Llist , Lty) TUPLETYPE2

fun EMBEDLty (VarTy ARGt)  = (LVarTy (EMBEDLstring ARGt)
)

| EMBEDLty (ConTy ARGt)  = (LConTy ((EMBEDTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
))))
 ARGt)
)

| EMBEDLty (RecordTy ARGt)  = (LRecordTy ((EMBEDLlist ((fn ARGt =>
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
 ARGt)
)

| EMBEDLty (TupleTy ARGt)  = (LTupleTy ((EMBEDLlist ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
)

| EMBEDLty (ForallTy ARGt)  = (LForallTy ((EMBEDTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
))))
 ARGt)
)

and PROJLty (Lty (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLty (Lty (_, (ref (Option.SOME ARGt))))  = (PROJLty ARGt)

| PROJLty (LVarTy ARGt)  = (VarTy (PROJLstring ARGt)
)

| PROJLty (LConTy ARGt)  = (ConTy ((PROJTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
))))
 ARGt)
)

| PROJLty (LRecordTy ARGt)  = (RecordTy ((PROJLlist ((fn ARGt =>
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
 ARGt)
)

| PROJLty (LTupleTy ARGt)  = (TupleTy ((PROJLlist ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
)

| PROJLty (LForallTy ARGt)  = (ForallTy ((PROJTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
))))
 ARGt)
)


fun OCCURLty ARG0 (Lty (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLty ARG0 (Lty (_, (ref (Option.SOME ARG2))))  = (OCCURLty ARG0 ARG2)

| OCCURLty ARG0 (LVarTy ARG1)  = (OCCURLstring ARG0 ARG1)

| OCCURLty ARG0 (LConTy ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLty ARG0 (LRecordTy ARG1)  = ((OCCURLlist (fn ARGv =>
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
 ARG0 ARG1)

| OCCURLty ARG0 (LTupleTy ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))
 ARG0 ARG1)

| OCCURLty ARG0 (LForallTy ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
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

| UNIFYLty ((LVarTy ARGt1), (LVarTy ARGt2)) ARGtc  = (UNIFYLstring (ARGt1, ARGt2) ARGtc)

| UNIFYLty ((LConTy ARGt1), (LConTy ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
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
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLty ((LRecordTy ARGt1), (LRecordTy ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
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

| UNIFYLty ((LTupleTy ARGt1), (LTupleTy ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLty ((LForallTy ARGt1), (LForallTy ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
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

| PRINTLty (LVarTy ARGt)  = (String.concat ["VarTy", " ", (PRINTLstring ARGt)
])

| PRINTLty (LConTy ARGt)  = (String.concat ["ConTy", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLstring ARGt)
))
 ARGt)
), (fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLty ARGt)
))
 ARGt)
)))
 ARGt)
])

| PRINTLty (LRecordTy ARGt)  = (String.concat ["RecordTy", " ", ((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
))
 ARGt)
])

| PRINTLty (LTupleTy ARGt)  = (String.concat ["TupleTy", " ", ((PRINTLlist (fn ARGt =>
(PRINTLty ARGt)
))
 ARGt)
])

| PRINTLty (LForallTy ARGt)  = (String.concat ["ForallTy", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLstring ARGt)
))
 ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
])


datatype ('a0, 'a1) RECORDTYPEexppat = RECORDTYPEexppat of (Ostring * ('a0 , 'a1) RECORDTYPEexppat Option.option ref) | RECORDCONexppat of {exp:'a0, pat:'a1}
and ('a0, 'a1) RECORDTYPEexpPatvarPat = RECORDTYPEexpPatvarPat of (Ostring * ('a0 , 'a1) RECORDTYPEexpPatvarPat Option.option ref) | RECORDCONexpPatvarPat of {expPat:'a0, varPat:'a1}
and ('a0, 'a1) RECORDTYPEconstraintpattern = RECORDTYPEconstraintpattern of (Ostring * ('a0 , 'a1) RECORDTYPEconstraintpattern Option.option ref) | RECORDCONconstraintpattern of {constraint:'a0, pattern:'a1}
and ('a0, 'a1) RECORDTYPEdefflexibility = RECORDTYPEdefflexibility of (Ostring * ('a0 , 'a1) RECORDTYPEdefflexibility Option.option ref) | RECORDCONdefflexibility of {def:'a0, flexibility:'a1}
and ('a0, 'a1) RECORDTYPEexprrules = RECORDTYPEexprrules of (Ostring * ('a0 , 'a1) RECORDTYPEexprrules Option.option ref) | RECORDCONexprrules of {expr:'a0, rules:'a1}
and ('a0, 'a1) RECORDTYPEconstraintexpr = RECORDTYPEconstraintexpr of (Ostring * ('a0 , 'a1) RECORDTYPEconstraintexpr Option.option ref) | RECORDCONconstraintexpr of {constraint:'a0, expr:'a1}
and ('a0, 'a1) RECORDTYPEargumentfunction = RECORDTYPEargumentfunction of (Ostring * ('a0 , 'a1) RECORDTYPEargumentfunction Option.option ref) | RECORDCONargumentfunction of {argument:'a0, function:'a1}

fun EMBEDRECORDTYPEexppat (qua0, qua1) ARGt0  = ((fn {exp = exp, pat = pat} =>
(RECORDCONexppat {exp = ((fn (ARG1, ARG2) =>
ARG1) qua0 exp)
, pat = ((fn (ARG1, ARG2) =>
ARG1) qua1 pat)
})
) ARGt0)

and PROJRECORDTYPEexppat (qua0, qua1) (RECORDTYPEexppat (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJRECORDTYPEexppat (qua0, qua1) (RECORDTYPEexppat (_, (ref (Option.SOME ARGt))))  = ((PROJRECORDTYPEexppat (qua0, qua1))
 ARGt)

| PROJRECORDTYPEexppat (qua0, qua1) (RECORDCONexppat ARGt0)  = ((fn {exp = exp, pat = pat} =>
{exp = ((fn (ARG1, ARG2) =>
ARG2) qua0 exp)
, pat = ((fn (ARG1, ARG2) =>
ARG2) qua1 pat)
}) ARGt0)

and EMBEDRECORDTYPEexpPatvarPat (qua0, qua1) ARGt0  = ((fn {expPat = expPat, varPat = varPat} =>
(RECORDCONexpPatvarPat {expPat = ((fn (ARG1, ARG2) =>
ARG1) qua0 expPat)
, varPat = ((fn (ARG1, ARG2) =>
ARG1) qua1 varPat)
})
) ARGt0)

and PROJRECORDTYPEexpPatvarPat (qua0, qua1) (RECORDTYPEexpPatvarPat (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJRECORDTYPEexpPatvarPat (qua0, qua1) (RECORDTYPEexpPatvarPat (_, (ref (Option.SOME ARGt))))  = ((PROJRECORDTYPEexpPatvarPat (qua0, qua1))
 ARGt)

| PROJRECORDTYPEexpPatvarPat (qua0, qua1) (RECORDCONexpPatvarPat ARGt0)  = ((fn {expPat = expPat, varPat = varPat} =>
{expPat = ((fn (ARG1, ARG2) =>
ARG2) qua0 expPat)
, varPat = ((fn (ARG1, ARG2) =>
ARG2) qua1 varPat)
}) ARGt0)

and EMBEDRECORDTYPEconstraintpattern (qua0, qua1) ARGt0  = ((fn {constraint = constraint, pattern = pattern} =>
(RECORDCONconstraintpattern {constraint = ((fn (ARG1, ARG2) =>
ARG1) qua0 constraint)
, pattern = ((fn (ARG1, ARG2) =>
ARG1) qua1 pattern)
})
) ARGt0)

and PROJRECORDTYPEconstraintpattern (qua0, qua1) (RECORDTYPEconstraintpattern (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJRECORDTYPEconstraintpattern (qua0, qua1) (RECORDTYPEconstraintpattern (_, (ref (Option.SOME ARGt))))  = ((PROJRECORDTYPEconstraintpattern (qua0, qua1))
 ARGt)

| PROJRECORDTYPEconstraintpattern (qua0, qua1) (RECORDCONconstraintpattern ARGt0)  = ((fn {constraint = constraint, pattern = pattern} =>
{constraint = ((fn (ARG1, ARG2) =>
ARG2) qua0 constraint)
, pattern = ((fn (ARG1, ARG2) =>
ARG2) qua1 pattern)
}) ARGt0)

and EMBEDRECORDTYPEdefflexibility (qua0, qua1) ARGt0  = ((fn {def = def, flexibility = flexibility} =>
(RECORDCONdefflexibility {def = ((fn (ARG1, ARG2) =>
ARG1) qua0 def)
, flexibility = ((fn (ARG1, ARG2) =>
ARG1) qua1 flexibility)
})
) ARGt0)

and PROJRECORDTYPEdefflexibility (qua0, qua1) (RECORDTYPEdefflexibility (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJRECORDTYPEdefflexibility (qua0, qua1) (RECORDTYPEdefflexibility (_, (ref (Option.SOME ARGt))))  = ((PROJRECORDTYPEdefflexibility (qua0, qua1))
 ARGt)

| PROJRECORDTYPEdefflexibility (qua0, qua1) (RECORDCONdefflexibility ARGt0)  = ((fn {def = def, flexibility = flexibility} =>
{def = ((fn (ARG1, ARG2) =>
ARG2) qua0 def)
, flexibility = ((fn (ARG1, ARG2) =>
ARG2) qua1 flexibility)
}) ARGt0)

and EMBEDRECORDTYPEexprrules (qua0, qua1) ARGt0  = ((fn {expr = expr, rules = rules} =>
(RECORDCONexprrules {expr = ((fn (ARG1, ARG2) =>
ARG1) qua0 expr)
, rules = ((fn (ARG1, ARG2) =>
ARG1) qua1 rules)
})
) ARGt0)

and PROJRECORDTYPEexprrules (qua0, qua1) (RECORDTYPEexprrules (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJRECORDTYPEexprrules (qua0, qua1) (RECORDTYPEexprrules (_, (ref (Option.SOME ARGt))))  = ((PROJRECORDTYPEexprrules (qua0, qua1))
 ARGt)

| PROJRECORDTYPEexprrules (qua0, qua1) (RECORDCONexprrules ARGt0)  = ((fn {expr = expr, rules = rules} =>
{expr = ((fn (ARG1, ARG2) =>
ARG2) qua0 expr)
, rules = ((fn (ARG1, ARG2) =>
ARG2) qua1 rules)
}) ARGt0)

and EMBEDRECORDTYPEconstraintexpr (qua0, qua1) ARGt0  = ((fn {constraint = constraint, expr = expr} =>
(RECORDCONconstraintexpr {constraint = ((fn (ARG1, ARG2) =>
ARG1) qua0 constraint)
, expr = ((fn (ARG1, ARG2) =>
ARG1) qua1 expr)
})
) ARGt0)

and PROJRECORDTYPEconstraintexpr (qua0, qua1) (RECORDTYPEconstraintexpr (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJRECORDTYPEconstraintexpr (qua0, qua1) (RECORDTYPEconstraintexpr (_, (ref (Option.SOME ARGt))))  = ((PROJRECORDTYPEconstraintexpr (qua0, qua1))
 ARGt)

| PROJRECORDTYPEconstraintexpr (qua0, qua1) (RECORDCONconstraintexpr ARGt0)  = ((fn {constraint = constraint, expr = expr} =>
{constraint = ((fn (ARG1, ARG2) =>
ARG2) qua0 constraint)
, expr = ((fn (ARG1, ARG2) =>
ARG2) qua1 expr)
}) ARGt0)

and EMBEDRECORDTYPEargumentfunction (qua0, qua1) ARGt0  = ((fn {argument = argument, function = function} =>
(RECORDCONargumentfunction {argument = ((fn (ARG1, ARG2) =>
ARG1) qua0 argument)
, function = ((fn (ARG1, ARG2) =>
ARG1) qua1 function)
})
) ARGt0)

and PROJRECORDTYPEargumentfunction (qua0, qua1) (RECORDTYPEargumentfunction (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJRECORDTYPEargumentfunction (qua0, qua1) (RECORDTYPEargumentfunction (_, (ref (Option.SOME ARGt))))  = ((PROJRECORDTYPEargumentfunction (qua0, qua1))
 ARGt)

| PROJRECORDTYPEargumentfunction (qua0, qua1) (RECORDCONargumentfunction ARGt0)  = ((fn {argument = argument, function = function} =>
{argument = ((fn (ARG1, ARG2) =>
ARG2) qua0 argument)
, function = ((fn (ARG1, ARG2) =>
ARG2) qua1 function)
}) ARGt0)


fun OCCURRECORDTYPEexppat (qua0, qua1) ARG0 (RECORDTYPEexppat (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURRECORDTYPEexppat (qua0, qua1) ARG0 (RECORDTYPEexppat (_, (ref (Option.SOME ARG2))))  = ((OCCURRECORDTYPEexppat (qua0, qua1))
 ARG0 ARG2)

| OCCURRECORDTYPEexppat (qua0, qua1) ARG0 (RECORDCONexppat ARG1)  = ((fn ARGv =>
(fn {exp = exp, pat = pat} =>
((fn true =>
true | false =>
(qua1 ARGv pat)
) ((fn true =>
true | false =>
(qua0 ARGv exp)
) false)
)
)) ARG0 ARG1)

and OCCURRECORDTYPEexpPatvarPat (qua0, qua1) ARG0 (RECORDTYPEexpPatvarPat (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURRECORDTYPEexpPatvarPat (qua0, qua1) ARG0 (RECORDTYPEexpPatvarPat (_, (ref (Option.SOME ARG2))))  = ((OCCURRECORDTYPEexpPatvarPat (qua0, qua1))
 ARG0 ARG2)

| OCCURRECORDTYPEexpPatvarPat (qua0, qua1) ARG0 (RECORDCONexpPatvarPat ARG1)  = ((fn ARGv =>
(fn {expPat = expPat, varPat = varPat} =>
((fn true =>
true | false =>
(qua1 ARGv varPat)
) ((fn true =>
true | false =>
(qua0 ARGv expPat)
) false)
)
)) ARG0 ARG1)

and OCCURRECORDTYPEconstraintpattern (qua0, qua1) ARG0 (RECORDTYPEconstraintpattern (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURRECORDTYPEconstraintpattern (qua0, qua1) ARG0 (RECORDTYPEconstraintpattern (_, (ref (Option.SOME ARG2))))  = ((OCCURRECORDTYPEconstraintpattern (qua0, qua1))
 ARG0 ARG2)

| OCCURRECORDTYPEconstraintpattern (qua0, qua1) ARG0 (RECORDCONconstraintpattern ARG1)  = ((fn ARGv =>
(fn {constraint = constraint, pattern = pattern} =>
((fn true =>
true | false =>
(qua1 ARGv pattern)
) ((fn true =>
true | false =>
(qua0 ARGv constraint)
) false)
)
)) ARG0 ARG1)

and OCCURRECORDTYPEdefflexibility (qua0, qua1) ARG0 (RECORDTYPEdefflexibility (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURRECORDTYPEdefflexibility (qua0, qua1) ARG0 (RECORDTYPEdefflexibility (_, (ref (Option.SOME ARG2))))  = ((OCCURRECORDTYPEdefflexibility (qua0, qua1))
 ARG0 ARG2)

| OCCURRECORDTYPEdefflexibility (qua0, qua1) ARG0 (RECORDCONdefflexibility ARG1)  = ((fn ARGv =>
(fn {def = def, flexibility = flexibility} =>
((fn true =>
true | false =>
(qua1 ARGv flexibility)
) ((fn true =>
true | false =>
(qua0 ARGv def)
) false)
)
)) ARG0 ARG1)

and OCCURRECORDTYPEexprrules (qua0, qua1) ARG0 (RECORDTYPEexprrules (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURRECORDTYPEexprrules (qua0, qua1) ARG0 (RECORDTYPEexprrules (_, (ref (Option.SOME ARG2))))  = ((OCCURRECORDTYPEexprrules (qua0, qua1))
 ARG0 ARG2)

| OCCURRECORDTYPEexprrules (qua0, qua1) ARG0 (RECORDCONexprrules ARG1)  = ((fn ARGv =>
(fn {expr = expr, rules = rules} =>
((fn true =>
true | false =>
(qua1 ARGv rules)
) ((fn true =>
true | false =>
(qua0 ARGv expr)
) false)
)
)) ARG0 ARG1)

and OCCURRECORDTYPEconstraintexpr (qua0, qua1) ARG0 (RECORDTYPEconstraintexpr (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURRECORDTYPEconstraintexpr (qua0, qua1) ARG0 (RECORDTYPEconstraintexpr (_, (ref (Option.SOME ARG2))))  = ((OCCURRECORDTYPEconstraintexpr (qua0, qua1))
 ARG0 ARG2)

| OCCURRECORDTYPEconstraintexpr (qua0, qua1) ARG0 (RECORDCONconstraintexpr ARG1)  = ((fn ARGv =>
(fn {constraint = constraint, expr = expr} =>
((fn true =>
true | false =>
(qua1 ARGv expr)
) ((fn true =>
true | false =>
(qua0 ARGv constraint)
) false)
)
)) ARG0 ARG1)

and OCCURRECORDTYPEargumentfunction (qua0, qua1) ARG0 (RECORDTYPEargumentfunction (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURRECORDTYPEargumentfunction (qua0, qua1) ARG0 (RECORDTYPEargumentfunction (_, (ref (Option.SOME ARG2))))  = ((OCCURRECORDTYPEargumentfunction (qua0, qua1))
 ARG0 ARG2)

| OCCURRECORDTYPEargumentfunction (qua0, qua1) ARG0 (RECORDCONargumentfunction ARG1)  = ((fn ARGv =>
(fn {argument = argument, function = function} =>
((fn true =>
true | false =>
(qua1 ARGv function)
) ((fn true =>
true | false =>
(qua0 ARGv argument)
) false)
)
)) ARG0 ARG1)


fun UNIFYRECORDTYPEexppat (qua0, qua1) (ARGt1, (RECORDTYPEexppat (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYRECORDTYPEexppat (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEexppat (qua0, qua1) (ARGt1, (RECORDTYPEexppat (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURRECORDTYPEexppat (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
))
 ARG0 ARGt1)
)

| UNIFYRECORDTYPEexppat (qua0, qua1) ((RECORDTYPEexppat (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEexppat (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEexppat (qua0, qua1) ((ARGt1 as (RECORDTYPEexppat (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEexppat (qua0, qua1))
 (ARGt2, ARGt1) ARGtc)

| UNIFYRECORDTYPEexppat (qua0, qua1) ((RECORDCONexppat ARGt1), (RECORDCONexppat ARGt2)) ARGtc  = ((fn ({exp = exp1, pat = pat1}, {exp = exp2, pat = pat2}) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (pat1, pat2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (exp1, exp2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
) (ARGt1, ARGt2))

and UNIFYRECORDTYPEexpPatvarPat (qua0, qua1) (ARGt1, (RECORDTYPEexpPatvarPat (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYRECORDTYPEexpPatvarPat (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEexpPatvarPat (qua0, qua1) (ARGt1, (RECORDTYPEexpPatvarPat (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURRECORDTYPEexpPatvarPat (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
))
 ARG0 ARGt1)
)

| UNIFYRECORDTYPEexpPatvarPat (qua0, qua1) ((RECORDTYPEexpPatvarPat (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEexpPatvarPat (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEexpPatvarPat (qua0, qua1) ((ARGt1 as (RECORDTYPEexpPatvarPat (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEexpPatvarPat (qua0, qua1))
 (ARGt2, ARGt1) ARGtc)

| UNIFYRECORDTYPEexpPatvarPat (qua0, qua1) ((RECORDCONexpPatvarPat ARGt1), (RECORDCONexpPatvarPat ARGt2)) ARGtc  = ((fn ({expPat = expPat1, varPat = varPat1}, {expPat = expPat2, varPat = varPat2}) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (varPat1, varPat2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (expPat1, expPat2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
) (ARGt1, ARGt2))

and UNIFYRECORDTYPEconstraintpattern (qua0, qua1) (ARGt1, (RECORDTYPEconstraintpattern (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYRECORDTYPEconstraintpattern (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEconstraintpattern (qua0, qua1) (ARGt1, (RECORDTYPEconstraintpattern (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURRECORDTYPEconstraintpattern (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
))
 ARG0 ARGt1)
)

| UNIFYRECORDTYPEconstraintpattern (qua0, qua1) ((RECORDTYPEconstraintpattern (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEconstraintpattern (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEconstraintpattern (qua0, qua1) ((ARGt1 as (RECORDTYPEconstraintpattern (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEconstraintpattern (qua0, qua1))
 (ARGt2, ARGt1) ARGtc)

| UNIFYRECORDTYPEconstraintpattern (qua0, qua1) ((RECORDCONconstraintpattern ARGt1), (RECORDCONconstraintpattern ARGt2)) ARGtc  = ((fn ({constraint = constraint1, pattern = pattern1}, {constraint = constraint2, pattern = pattern2}) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (pattern1, pattern2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (constraint1, constraint2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
) (ARGt1, ARGt2))

and UNIFYRECORDTYPEdefflexibility (qua0, qua1) (ARGt1, (RECORDTYPEdefflexibility (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYRECORDTYPEdefflexibility (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEdefflexibility (qua0, qua1) (ARGt1, (RECORDTYPEdefflexibility (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURRECORDTYPEdefflexibility (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
))
 ARG0 ARGt1)
)

| UNIFYRECORDTYPEdefflexibility (qua0, qua1) ((RECORDTYPEdefflexibility (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEdefflexibility (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEdefflexibility (qua0, qua1) ((ARGt1 as (RECORDTYPEdefflexibility (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEdefflexibility (qua0, qua1))
 (ARGt2, ARGt1) ARGtc)

| UNIFYRECORDTYPEdefflexibility (qua0, qua1) ((RECORDCONdefflexibility ARGt1), (RECORDCONdefflexibility ARGt2)) ARGtc  = ((fn ({def = def1, flexibility = flexibility1}, {def = def2, flexibility = flexibility2}) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (flexibility1, flexibility2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (def1, def2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
) (ARGt1, ARGt2))

and UNIFYRECORDTYPEexprrules (qua0, qua1) (ARGt1, (RECORDTYPEexprrules (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYRECORDTYPEexprrules (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEexprrules (qua0, qua1) (ARGt1, (RECORDTYPEexprrules (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURRECORDTYPEexprrules (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
))
 ARG0 ARGt1)
)

| UNIFYRECORDTYPEexprrules (qua0, qua1) ((RECORDTYPEexprrules (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEexprrules (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEexprrules (qua0, qua1) ((ARGt1 as (RECORDTYPEexprrules (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEexprrules (qua0, qua1))
 (ARGt2, ARGt1) ARGtc)

| UNIFYRECORDTYPEexprrules (qua0, qua1) ((RECORDCONexprrules ARGt1), (RECORDCONexprrules ARGt2)) ARGtc  = ((fn ({expr = expr1, rules = rules1}, {expr = expr2, rules = rules2}) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (rules1, rules2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (expr1, expr2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
) (ARGt1, ARGt2))

and UNIFYRECORDTYPEconstraintexpr (qua0, qua1) (ARGt1, (RECORDTYPEconstraintexpr (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYRECORDTYPEconstraintexpr (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEconstraintexpr (qua0, qua1) (ARGt1, (RECORDTYPEconstraintexpr (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURRECORDTYPEconstraintexpr (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
))
 ARG0 ARGt1)
)

| UNIFYRECORDTYPEconstraintexpr (qua0, qua1) ((RECORDTYPEconstraintexpr (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEconstraintexpr (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEconstraintexpr (qua0, qua1) ((ARGt1 as (RECORDTYPEconstraintexpr (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEconstraintexpr (qua0, qua1))
 (ARGt2, ARGt1) ARGtc)

| UNIFYRECORDTYPEconstraintexpr (qua0, qua1) ((RECORDCONconstraintexpr ARGt1), (RECORDCONconstraintexpr ARGt2)) ARGtc  = ((fn ({constraint = constraint1, expr = expr1}, {constraint = constraint2, expr = expr2}) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (expr1, expr2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (constraint1, constraint2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
) (ARGt1, ARGt2))

and UNIFYRECORDTYPEargumentfunction (qua0, qua1) (ARGt1, (RECORDTYPEargumentfunction (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYRECORDTYPEargumentfunction (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEargumentfunction (qua0, qua1) (ARGt1, (RECORDTYPEargumentfunction (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURRECORDTYPEargumentfunction (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
))
 ARG0 ARGt1)
)

| UNIFYRECORDTYPEargumentfunction (qua0, qua1) ((RECORDTYPEargumentfunction (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEargumentfunction (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEargumentfunction (qua0, qua1) ((ARGt1 as (RECORDTYPEargumentfunction (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEargumentfunction (qua0, qua1))
 (ARGt2, ARGt1) ARGtc)

| UNIFYRECORDTYPEargumentfunction (qua0, qua1) ((RECORDCONargumentfunction ARGt1), (RECORDCONargumentfunction ARGt2)) ARGtc  = ((fn ({argument = argument1, function = function1}, {argument = argument2, function = function2}) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (function1, function2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (argument1, argument2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
) (ARGt1, ARGt2))


fun PRINTRECORDTYPEexppat (qua0, qua1) (RECORDTYPEexppat (ARGs, (ref Option.NONE)))  = ARGs
| PRINTRECORDTYPEexppat (qua0, qua1) (RECORDTYPEexppat (_, (ref (Option.SOME ARGt))))  = ((PRINTRECORDTYPEexppat (qua0, qua1))
 ARGt)

| PRINTRECORDTYPEexppat (qua0, qua1) (RECORDCONexppat ARGt0)  = ((fn {exp = exp, pat = pat} =>
(String.concat ["{", (SMLOG.intersperse "," [(String.concat ["exp", "=", (qua0 exp)
])
, (String.concat ["pat", "=", (qua1 pat)
])
])
, "}"])
) ARGt0)

and PRINTRECORDTYPEexpPatvarPat (qua0, qua1) (RECORDTYPEexpPatvarPat (ARGs, (ref Option.NONE)))  = ARGs
| PRINTRECORDTYPEexpPatvarPat (qua0, qua1) (RECORDTYPEexpPatvarPat (_, (ref (Option.SOME ARGt))))  = ((PRINTRECORDTYPEexpPatvarPat (qua0, qua1))
 ARGt)

| PRINTRECORDTYPEexpPatvarPat (qua0, qua1) (RECORDCONexpPatvarPat ARGt0)  = ((fn {expPat = expPat, varPat = varPat} =>
(String.concat ["{", (SMLOG.intersperse "," [(String.concat ["expPat", "=", (qua0 expPat)
])
, (String.concat ["varPat", "=", (qua1 varPat)
])
])
, "}"])
) ARGt0)

and PRINTRECORDTYPEconstraintpattern (qua0, qua1) (RECORDTYPEconstraintpattern (ARGs, (ref Option.NONE)))  = ARGs
| PRINTRECORDTYPEconstraintpattern (qua0, qua1) (RECORDTYPEconstraintpattern (_, (ref (Option.SOME ARGt))))  = ((PRINTRECORDTYPEconstraintpattern (qua0, qua1))
 ARGt)

| PRINTRECORDTYPEconstraintpattern (qua0, qua1) (RECORDCONconstraintpattern ARGt0)  = ((fn {constraint = constraint, pattern = pattern} =>
(String.concat ["{", (SMLOG.intersperse "," [(String.concat ["constraint", "=", (qua0 constraint)
])
, (String.concat ["pattern", "=", (qua1 pattern)
])
])
, "}"])
) ARGt0)

and PRINTRECORDTYPEdefflexibility (qua0, qua1) (RECORDTYPEdefflexibility (ARGs, (ref Option.NONE)))  = ARGs
| PRINTRECORDTYPEdefflexibility (qua0, qua1) (RECORDTYPEdefflexibility (_, (ref (Option.SOME ARGt))))  = ((PRINTRECORDTYPEdefflexibility (qua0, qua1))
 ARGt)

| PRINTRECORDTYPEdefflexibility (qua0, qua1) (RECORDCONdefflexibility ARGt0)  = ((fn {def = def, flexibility = flexibility} =>
(String.concat ["{", (SMLOG.intersperse "," [(String.concat ["def", "=", (qua0 def)
])
, (String.concat ["flexibility", "=", (qua1 flexibility)
])
])
, "}"])
) ARGt0)

and PRINTRECORDTYPEexprrules (qua0, qua1) (RECORDTYPEexprrules (ARGs, (ref Option.NONE)))  = ARGs
| PRINTRECORDTYPEexprrules (qua0, qua1) (RECORDTYPEexprrules (_, (ref (Option.SOME ARGt))))  = ((PRINTRECORDTYPEexprrules (qua0, qua1))
 ARGt)

| PRINTRECORDTYPEexprrules (qua0, qua1) (RECORDCONexprrules ARGt0)  = ((fn {expr = expr, rules = rules} =>
(String.concat ["{", (SMLOG.intersperse "," [(String.concat ["expr", "=", (qua0 expr)
])
, (String.concat ["rules", "=", (qua1 rules)
])
])
, "}"])
) ARGt0)

and PRINTRECORDTYPEconstraintexpr (qua0, qua1) (RECORDTYPEconstraintexpr (ARGs, (ref Option.NONE)))  = ARGs
| PRINTRECORDTYPEconstraintexpr (qua0, qua1) (RECORDTYPEconstraintexpr (_, (ref (Option.SOME ARGt))))  = ((PRINTRECORDTYPEconstraintexpr (qua0, qua1))
 ARGt)

| PRINTRECORDTYPEconstraintexpr (qua0, qua1) (RECORDCONconstraintexpr ARGt0)  = ((fn {constraint = constraint, expr = expr} =>
(String.concat ["{", (SMLOG.intersperse "," [(String.concat ["constraint", "=", (qua0 constraint)
])
, (String.concat ["expr", "=", (qua1 expr)
])
])
, "}"])
) ARGt0)

and PRINTRECORDTYPEargumentfunction (qua0, qua1) (RECORDTYPEargumentfunction (ARGs, (ref Option.NONE)))  = ARGs
| PRINTRECORDTYPEargumentfunction (qua0, qua1) (RECORDTYPEargumentfunction (_, (ref (Option.SOME ARGt))))  = ((PRINTRECORDTYPEargumentfunction (qua0, qua1))
 ARGt)

| PRINTRECORDTYPEargumentfunction (qua0, qua1) (RECORDCONargumentfunction ARGt0)  = ((fn {argument = argument, function = function} =>
(String.concat ["{", (SMLOG.intersperse "," [(String.concat ["argument", "=", (qua0 argument)
])
, (String.concat ["function", "=", (qua1 function)
])
])
, "}"])
) ARGt0)


datatype exp = VarExp of path | FlatAppExp of exp list | AppExp of {argument:exp, function:exp} | IntExp of int | WordExp of int | RealExp of string | StringExp of string | CharExp of string | RecordExp of (id * exp) list | ListExp of exp list | TupleExp of exp list | SelectorExp of string | ConstraintExp of {constraint:ty, expr:exp} | VectorExp of exp list | FnExp of rule list | HandleExp of {expr:exp, rules:rule list} | RaiseExp of exp
and pat = VarPat of path | IntPat of int | WordPat of int | StringPat of string | CharPat of string | WildPat | RecordPat of {def:(string * pat) list, flexibility:bool} | ListPat of pat list | TuplePat of pat list | FlatAppPat of pat list | ConstraintPat of {constraint:ty, pattern:pat} | LayeredPat of {expPat:pat, varPat:pat} | VectorPat of pat list | OrPat of pat list
and rule = Rule of {exp:exp, pat:pat}

datatype Oexp = datatype exp

datatype Opat = datatype pat

datatype Orule = datatype rule

datatype Lexp = Lexp of (Ostring * Lexp Option.option ref) | LVarExp of Lstring Llist | LFlatAppExp of Lexp Llist | LAppExp of (Lexp , Lexp) RECORDTYPEargumentfunction | LIntExp of Lint | LWordExp of Lint | LRealExp of Lstring | LStringExp of Lstring | LCharExp of Lstring | LRecordExp of (Lstring , Lexp) TUPLETYPE2 Llist | LListExp of Lexp Llist | LTupleExp of Lexp Llist | LSelectorExp of Lstring | LConstraintExp of (Lty , Lexp) RECORDTYPEconstraintexpr | LVectorExp of Lexp Llist | LFnExp of Lrule Llist | LHandleExp of (Lexp , Lrule Llist) RECORDTYPEexprrules | LRaiseExp of Lexp
and Lpat = Lpat of (Ostring * Lpat Option.option ref) | LVarPat of Lstring Llist | LIntPat of Lint | LWordPat of Lint | LStringPat of Lstring | LCharPat of Lstring | LWildPat | LRecordPat of ((Lstring , Lpat) TUPLETYPE2 Llist , Lbool) RECORDTYPEdefflexibility | LListPat of Lpat Llist | LTuplePat of Lpat Llist | LFlatAppPat of Lpat Llist | LConstraintPat of (Lty , Lpat) RECORDTYPEconstraintpattern | LLayeredPat of (Lpat , Lpat) RECORDTYPEexpPatvarPat | LVectorPat of Lpat Llist | LOrPat of Lpat Llist
and Lrule = Lrule of (Ostring * Lrule Option.option ref) | LRule of (Lexp , Lpat) RECORDTYPEexppat

fun EMBEDLexp (VarExp ARGt)  = (LVarExp ((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)

| EMBEDLexp (FlatAppExp ARGt)  = (LFlatAppExp ((EMBEDLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
)

| EMBEDLexp (AppExp ARGt)  = (LAppExp ((EMBEDRECORDTYPEargumentfunction (((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
))))
 ARGt)
)

| EMBEDLexp (IntExp ARGt)  = (LIntExp (EMBEDLint ARGt)
)

| EMBEDLexp (WordExp ARGt)  = (LWordExp (EMBEDLint ARGt)
)

| EMBEDLexp (RealExp ARGt)  = (LRealExp (EMBEDLstring ARGt)
)

| EMBEDLexp (StringExp ARGt)  = (LStringExp (EMBEDLstring ARGt)
)

| EMBEDLexp (CharExp ARGt)  = (LCharExp (EMBEDLstring ARGt)
)

| EMBEDLexp (RecordExp ARGt)  = (LRecordExp ((EMBEDLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
))))
 ARGt)
)))
 ARGt)
)

| EMBEDLexp (ListExp ARGt)  = (LListExp ((EMBEDLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
)

| EMBEDLexp (TupleExp ARGt)  = (LTupleExp ((EMBEDLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
)

| EMBEDLexp (SelectorExp ARGt)  = (LSelectorExp (EMBEDLstring ARGt)
)

| EMBEDLexp (ConstraintExp ARGt)  = (LConstraintExp ((EMBEDRECORDTYPEconstraintexpr (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
))))
 ARGt)
)

| EMBEDLexp (VectorExp ARGt)  = (LVectorExp ((EMBEDLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
)

| EMBEDLexp (FnExp ARGt)  = (LFnExp ((EMBEDLlist ((fn ARGt =>
(EMBEDLrule ARGt)
), (fn ARGt =>
(PROJLrule ARGt)
)))
 ARGt)
)

| EMBEDLexp (HandleExp ARGt)  = (LHandleExp ((EMBEDRECORDTYPEexprrules (((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLrule ARGt)
), (fn ARGt =>
(PROJLrule ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLrule ARGt)
), (fn ARGt =>
(PROJLrule ARGt)
)))
 ARGt)
))))
 ARGt)
)

| EMBEDLexp (RaiseExp ARGt)  = (LRaiseExp (EMBEDLexp ARGt)
)

and PROJLexp (Lexp (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLexp (Lexp (_, (ref (Option.SOME ARGt))))  = (PROJLexp ARGt)

| PROJLexp (LVarExp ARGt)  = (VarExp ((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)

| PROJLexp (LFlatAppExp ARGt)  = (FlatAppExp ((PROJLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
)

| PROJLexp (LAppExp ARGt)  = (AppExp ((PROJRECORDTYPEargumentfunction (((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
))))
 ARGt)
)

| PROJLexp (LIntExp ARGt)  = (IntExp (PROJLint ARGt)
)

| PROJLexp (LWordExp ARGt)  = (WordExp (PROJLint ARGt)
)

| PROJLexp (LRealExp ARGt)  = (RealExp (PROJLstring ARGt)
)

| PROJLexp (LStringExp ARGt)  = (StringExp (PROJLstring ARGt)
)

| PROJLexp (LCharExp ARGt)  = (CharExp (PROJLstring ARGt)
)

| PROJLexp (LRecordExp ARGt)  = (RecordExp ((PROJLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
))))
 ARGt)
)))
 ARGt)
)

| PROJLexp (LListExp ARGt)  = (ListExp ((PROJLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
)

| PROJLexp (LTupleExp ARGt)  = (TupleExp ((PROJLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
)

| PROJLexp (LSelectorExp ARGt)  = (SelectorExp (PROJLstring ARGt)
)

| PROJLexp (LConstraintExp ARGt)  = (ConstraintExp ((PROJRECORDTYPEconstraintexpr (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
))))
 ARGt)
)

| PROJLexp (LVectorExp ARGt)  = (VectorExp ((PROJLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
)

| PROJLexp (LFnExp ARGt)  = (FnExp ((PROJLlist ((fn ARGt =>
(EMBEDLrule ARGt)
), (fn ARGt =>
(PROJLrule ARGt)
)))
 ARGt)
)

| PROJLexp (LHandleExp ARGt)  = (HandleExp ((PROJRECORDTYPEexprrules (((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLrule ARGt)
), (fn ARGt =>
(PROJLrule ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLrule ARGt)
), (fn ARGt =>
(PROJLrule ARGt)
)))
 ARGt)
))))
 ARGt)
)

| PROJLexp (LRaiseExp ARGt)  = (RaiseExp (PROJLexp ARGt)
)

and EMBEDLpat (VarPat ARGt)  = (LVarPat ((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)

| EMBEDLpat (IntPat ARGt)  = (LIntPat (EMBEDLint ARGt)
)

| EMBEDLpat (WordPat ARGt)  = (LWordPat (EMBEDLint ARGt)
)

| EMBEDLpat (StringPat ARGt)  = (LStringPat (EMBEDLstring ARGt)
)

| EMBEDLpat (CharPat ARGt)  = (LCharPat (EMBEDLstring ARGt)
)

| EMBEDLpat WildPat  = LWildPat
| EMBEDLpat (RecordPat ARGt)  = (LRecordPat ((EMBEDRECORDTYPEdefflexibility (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
))))
 ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
))))
 ARGt)
)))
 ARGt)
)), ((fn ARGt =>
(EMBEDLbool ARGt)
), (fn ARGt =>
(PROJLbool ARGt)
))))
 ARGt)
)

| EMBEDLpat (ListPat ARGt)  = (LListPat ((EMBEDLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
)

| EMBEDLpat (TuplePat ARGt)  = (LTuplePat ((EMBEDLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
)

| EMBEDLpat (FlatAppPat ARGt)  = (LFlatAppPat ((EMBEDLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
)

| EMBEDLpat (ConstraintPat ARGt)  = (LConstraintPat ((EMBEDRECORDTYPEconstraintpattern (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
))))
 ARGt)
)

| EMBEDLpat (LayeredPat ARGt)  = (LLayeredPat ((EMBEDRECORDTYPEexpPatvarPat (((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)), ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
))))
 ARGt)
)

| EMBEDLpat (VectorPat ARGt)  = (LVectorPat ((EMBEDLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
)

| EMBEDLpat (OrPat ARGt)  = (LOrPat ((EMBEDLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
)

and PROJLpat (Lpat (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLpat (Lpat (_, (ref (Option.SOME ARGt))))  = (PROJLpat ARGt)

| PROJLpat (LVarPat ARGt)  = (VarPat ((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)

| PROJLpat (LIntPat ARGt)  = (IntPat (PROJLint ARGt)
)

| PROJLpat (LWordPat ARGt)  = (WordPat (PROJLint ARGt)
)

| PROJLpat (LStringPat ARGt)  = (StringPat (PROJLstring ARGt)
)

| PROJLpat (LCharPat ARGt)  = (CharPat (PROJLstring ARGt)
)

| PROJLpat LWildPat  = WildPat
| PROJLpat (LRecordPat ARGt)  = (RecordPat ((PROJRECORDTYPEdefflexibility (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
))))
 ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
))))
 ARGt)
)))
 ARGt)
)), ((fn ARGt =>
(EMBEDLbool ARGt)
), (fn ARGt =>
(PROJLbool ARGt)
))))
 ARGt)
)

| PROJLpat (LListPat ARGt)  = (ListPat ((PROJLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
)

| PROJLpat (LTuplePat ARGt)  = (TuplePat ((PROJLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
)

| PROJLpat (LFlatAppPat ARGt)  = (FlatAppPat ((PROJLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
)

| PROJLpat (LConstraintPat ARGt)  = (ConstraintPat ((PROJRECORDTYPEconstraintpattern (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
))))
 ARGt)
)

| PROJLpat (LLayeredPat ARGt)  = (LayeredPat ((PROJRECORDTYPEexpPatvarPat (((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)), ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
))))
 ARGt)
)

| PROJLpat (LVectorPat ARGt)  = (VectorPat ((PROJLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
)

| PROJLpat (LOrPat ARGt)  = (OrPat ((PROJLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
)

and EMBEDLrule (Rule ARGt)  = (LRule ((EMBEDRECORDTYPEexppat (((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
))))
 ARGt)
)

and PROJLrule (Lrule (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLrule (Lrule (_, (ref (Option.SOME ARGt))))  = (PROJLrule ARGt)

| PROJLrule (LRule ARGt)  = (Rule ((PROJRECORDTYPEexppat (((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
))))
 ARGt)
)


fun OCCURLexp ARG0 (Lexp (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLexp ARG0 (Lexp (_, (ref (Option.SOME ARG2))))  = (OCCURLexp ARG0 ARG2)

| OCCURLexp ARG0 (LVarExp ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARG0 ARG1)

| OCCURLexp ARG0 (LFlatAppExp ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)))
 ARG0 ARG1)

| OCCURLexp ARG0 (LAppExp ARG1)  = ((OCCURRECORDTYPEargumentfunction ((fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLexp ARG0 (LIntExp ARG1)  = (OCCURLint ARG0 ARG1)

| OCCURLexp ARG0 (LWordExp ARG1)  = (OCCURLint ARG0 ARG1)

| OCCURLexp ARG0 (LRealExp ARG1)  = (OCCURLstring ARG0 ARG1)

| OCCURLexp ARG0 (LStringExp ARG1)  = (OCCURLstring ARG0 ARG1)

| OCCURLexp ARG0 (LCharExp ARG1)  = (OCCURLstring ARG0 ARG1)

| OCCURLexp ARG0 (LRecordExp ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 ARGv ARGt)
)))
 ARG0 ARG1)

| OCCURLexp ARG0 (LListExp ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)))
 ARG0 ARG1)

| OCCURLexp ARG0 (LTupleExp ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)))
 ARG0 ARG1)

| OCCURLexp ARG0 (LSelectorExp ARG1)  = (OCCURLstring ARG0 ARG1)

| OCCURLexp ARG0 (LConstraintExp ARG1)  = ((OCCURRECORDTYPEconstraintexpr ((fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLexp ARG0 (LVectorExp ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)))
 ARG0 ARG1)

| OCCURLexp ARG0 (LFnExp ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLrule ARGv ARGt)
)))
 ARG0 ARG1)

| OCCURLexp ARG0 (LHandleExp ARG1)  = ((OCCURRECORDTYPEexprrules ((fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLrule ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLexp ARG0 (LRaiseExp ARG1)  = (OCCURLexp ARG0 ARG1)

and OCCURLpat ARG0 (Lpat (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLpat ARG0 (Lpat (_, (ref (Option.SOME ARG2))))  = (OCCURLpat ARG0 ARG2)

| OCCURLpat ARG0 (LVarPat ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARG0 ARG1)

| OCCURLpat ARG0 (LIntPat ARG1)  = (OCCURLint ARG0 ARG1)

| OCCURLpat ARG0 (LWordPat ARG1)  = (OCCURLint ARG0 ARG1)

| OCCURLpat ARG0 (LStringPat ARG1)  = (OCCURLstring ARG0 ARG1)

| OCCURLpat ARG0 (LCharPat ARG1)  = (OCCURLstring ARG0 ARG1)

| OCCURLpat ARG0 LWildPat  = false
| OCCURLpat ARG0 (LRecordPat ARG1)  = ((OCCURRECORDTYPEdefflexibility ((fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
))))
 ARGv ARGt)
)))
 ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLbool ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLpat ARG0 (LListPat ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
)))
 ARG0 ARG1)

| OCCURLpat ARG0 (LTuplePat ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
)))
 ARG0 ARG1)

| OCCURLpat ARG0 (LFlatAppPat ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
)))
 ARG0 ARG1)

| OCCURLpat ARG0 (LConstraintPat ARG1)  = ((OCCURRECORDTYPEconstraintpattern ((fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLpat ARG0 (LLayeredPat ARG1)  = ((OCCURRECORDTYPEexpPatvarPat ((fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLpat ARG0 (LVectorPat ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
)))
 ARG0 ARG1)

| OCCURLpat ARG0 (LOrPat ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
)))
 ARG0 ARG1)

and OCCURLrule ARG0 (Lrule (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLrule ARG0 (Lrule (_, (ref (Option.SOME ARG2))))  = (OCCURLrule ARG0 ARG2)

| OCCURLrule ARG0 (LRule ARG1)  = ((OCCURRECORDTYPEexppat ((fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
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

| UNIFYLexp ((LVarExp ARGt1), (LVarExp ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LFlatAppExp ARGt1), (LFlatAppExp ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLexp (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LAppExp ARGt1), (LAppExp ARGt2)) ARGtc  = ((UNIFYRECORDTYPEargumentfunction (((fn (ARGt1, ARGt2) =>
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
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LIntExp ARGt1), (LIntExp ARGt2)) ARGtc  = (UNIFYLint (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LWordExp ARGt1), (LWordExp ARGt2)) ARGtc  = (UNIFYLint (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LRealExp ARGt1), (LRealExp ARGt2)) ARGtc  = (UNIFYLstring (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LStringExp ARGt1), (LStringExp ARGt2)) ARGtc  = (UNIFYLstring (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LCharExp ARGt1), (LCharExp ARGt2)) ARGtc  = (UNIFYLstring (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LRecordExp ARGt1), (LRecordExp ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
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
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LListExp ARGt1), (LListExp ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLexp (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LTupleExp ARGt1), (LTupleExp ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLexp (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LSelectorExp ARGt1), (LSelectorExp ARGt2)) ARGtc  = (UNIFYLstring (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LConstraintExp ARGt1), (LConstraintExp ARGt2)) ARGtc  = ((UNIFYRECORDTYPEconstraintexpr (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLexp (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LVectorExp ARGt1), (LVectorExp ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLexp (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LFnExp ARGt1), (LFnExp ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLrule (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLrule ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LHandleExp ARGt1), (LHandleExp ARGt2)) ARGtc  = ((UNIFYRECORDTYPEexprrules (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLexp (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLrule (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLrule ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLrule ARGv ARGt)
)))
 ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LRaiseExp ARGt1), (LRaiseExp ARGt2)) ARGtc  = (UNIFYLexp (ARGt1, ARGt2) ARGtc)

| UNIFYLexp (_, _) ARGtc  = (false, ARGtc)
and UNIFYLpat (ARGt1, (Lpat (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLpat (ARGt1, ARGt2) ARGtc)

| UNIFYLpat (ARGt1, (Lpat (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLpat ARG0 ARGt1)
)

| UNIFYLpat ((Lpat (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLpat (ARGt1, ARGt2) ARGtc)

| UNIFYLpat ((ARGt1 as (Lpat (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLpat (ARGt2, ARGt1) ARGtc)

| UNIFYLpat ((LVarPat ARGt1), (LVarPat ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLpat ((LIntPat ARGt1), (LIntPat ARGt2)) ARGtc  = (UNIFYLint (ARGt1, ARGt2) ARGtc)

| UNIFYLpat ((LWordPat ARGt1), (LWordPat ARGt2)) ARGtc  = (UNIFYLint (ARGt1, ARGt2) ARGtc)

| UNIFYLpat ((LStringPat ARGt1), (LStringPat ARGt2)) ARGtc  = (UNIFYLstring (ARGt1, ARGt2) ARGtc)

| UNIFYLpat ((LCharPat ARGt1), (LCharPat ARGt2)) ARGtc  = (UNIFYLstring (ARGt1, ARGt2) ARGtc)

| UNIFYLpat (LWildPat, LWildPat) ARGtc  = (true, ARGtc)
| UNIFYLpat ((LRecordPat ARGt1), (LRecordPat ARGt2)) ARGtc  = ((UNIFYRECORDTYPEdefflexibility (((fn (ARGt1, ARGt2) =>
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
(UNIFYLpat (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
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
(OCCURLpat ARGv ARGt)
))))
 ARGv ARGt)
)))
 ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLbool (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLbool ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLpat ((LListPat ARGt1), (LListPat ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLpat (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLpat ((LTuplePat ARGt1), (LTuplePat ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLpat (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLpat ((LFlatAppPat ARGt1), (LFlatAppPat ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLpat (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLpat ((LConstraintPat ARGt1), (LConstraintPat ARGt2)) ARGtc  = ((UNIFYRECORDTYPEconstraintpattern (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLpat (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLpat ((LLayeredPat ARGt1), (LLayeredPat ARGt2)) ARGtc  = ((UNIFYRECORDTYPEexpPatvarPat (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLpat (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLpat (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLpat ((LVectorPat ARGt1), (LVectorPat ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLpat (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLpat ((LOrPat ARGt1), (LOrPat ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLpat (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLpat (_, _) ARGtc  = (false, ARGtc)
and UNIFYLrule (ARGt1, (Lrule (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLrule (ARGt1, ARGt2) ARGtc)

| UNIFYLrule (ARGt1, (Lrule (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLrule ARG0 ARGt1)
)

| UNIFYLrule ((Lrule (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLrule (ARGt1, ARGt2) ARGtc)

| UNIFYLrule ((ARGt1 as (Lrule (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLrule (ARGt2, ARGt1) ARGtc)

| UNIFYLrule ((LRule ARGt1), (LRule ARGt2)) ARGtc  = ((UNIFYRECORDTYPEexppat (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLexp (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLpat (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)


fun PRINTLexp (Lexp (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLexp (Lexp (_, (ref (Option.SOME ARGt))))  = (PRINTLexp ARGt)

| PRINTLexp (LVarExp ARGt)  = (String.concat ["VarExp", " ", ((PRINTLlist (fn ARGt =>
(PRINTLstring ARGt)
))
 ARGt)
])

| PRINTLexp (LFlatAppExp ARGt)  = (String.concat ["FlatAppExp", " ", ((PRINTLlist (fn ARGt =>
(PRINTLexp ARGt)
))
 ARGt)
])

| PRINTLexp (LAppExp ARGt)  = (String.concat ["AppExp", " ", ((PRINTRECORDTYPEargumentfunction ((fn ARGt =>
(PRINTLexp ARGt)
), (fn ARGt =>
(PRINTLexp ARGt)
)))
 ARGt)
])

| PRINTLexp (LIntExp ARGt)  = (String.concat ["IntExp", " ", (PRINTLint ARGt)
])

| PRINTLexp (LWordExp ARGt)  = (String.concat ["WordExp", " ", (PRINTLint ARGt)
])

| PRINTLexp (LRealExp ARGt)  = (String.concat ["RealExp", " ", (PRINTLstring ARGt)
])

| PRINTLexp (LStringExp ARGt)  = (String.concat ["StringExp", " ", (PRINTLstring ARGt)
])

| PRINTLexp (LCharExp ARGt)  = (String.concat ["CharExp", " ", (PRINTLstring ARGt)
])

| PRINTLexp (LRecordExp ARGt)  = (String.concat ["RecordExp", " ", ((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLexp ARGt)
)))
 ARGt)
))
 ARGt)
])

| PRINTLexp (LListExp ARGt)  = (String.concat ["ListExp", " ", ((PRINTLlist (fn ARGt =>
(PRINTLexp ARGt)
))
 ARGt)
])

| PRINTLexp (LTupleExp ARGt)  = (String.concat ["TupleExp", " ", ((PRINTLlist (fn ARGt =>
(PRINTLexp ARGt)
))
 ARGt)
])

| PRINTLexp (LSelectorExp ARGt)  = (String.concat ["SelectorExp", " ", (PRINTLstring ARGt)
])

| PRINTLexp (LConstraintExp ARGt)  = (String.concat ["ConstraintExp", " ", ((PRINTRECORDTYPEconstraintexpr ((fn ARGt =>
(PRINTLty ARGt)
), (fn ARGt =>
(PRINTLexp ARGt)
)))
 ARGt)
])

| PRINTLexp (LVectorExp ARGt)  = (String.concat ["VectorExp", " ", ((PRINTLlist (fn ARGt =>
(PRINTLexp ARGt)
))
 ARGt)
])

| PRINTLexp (LFnExp ARGt)  = (String.concat ["FnExp", " ", ((PRINTLlist (fn ARGt =>
(PRINTLrule ARGt)
))
 ARGt)
])

| PRINTLexp (LHandleExp ARGt)  = (String.concat ["HandleExp", " ", ((PRINTRECORDTYPEexprrules ((fn ARGt =>
(PRINTLexp ARGt)
), (fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLrule ARGt)
))
 ARGt)
)))
 ARGt)
])

| PRINTLexp (LRaiseExp ARGt)  = (String.concat ["RaiseExp", " ", (PRINTLexp ARGt)
])

and PRINTLpat (Lpat (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLpat (Lpat (_, (ref (Option.SOME ARGt))))  = (PRINTLpat ARGt)

| PRINTLpat (LVarPat ARGt)  = (String.concat ["VarPat", " ", ((PRINTLlist (fn ARGt =>
(PRINTLstring ARGt)
))
 ARGt)
])

| PRINTLpat (LIntPat ARGt)  = (String.concat ["IntPat", " ", (PRINTLint ARGt)
])

| PRINTLpat (LWordPat ARGt)  = (String.concat ["WordPat", " ", (PRINTLint ARGt)
])

| PRINTLpat (LStringPat ARGt)  = (String.concat ["StringPat", " ", (PRINTLstring ARGt)
])

| PRINTLpat (LCharPat ARGt)  = (String.concat ["CharPat", " ", (PRINTLstring ARGt)
])

| PRINTLpat LWildPat  = "WildPat"
| PRINTLpat (LRecordPat ARGt)  = (String.concat ["RecordPat", " ", ((PRINTRECORDTYPEdefflexibility ((fn ARGt =>
((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLpat ARGt)
)))
 ARGt)
))
 ARGt)
), (fn ARGt =>
(PRINTLbool ARGt)
)))
 ARGt)
])

| PRINTLpat (LListPat ARGt)  = (String.concat ["ListPat", " ", ((PRINTLlist (fn ARGt =>
(PRINTLpat ARGt)
))
 ARGt)
])

| PRINTLpat (LTuplePat ARGt)  = (String.concat ["TuplePat", " ", ((PRINTLlist (fn ARGt =>
(PRINTLpat ARGt)
))
 ARGt)
])

| PRINTLpat (LFlatAppPat ARGt)  = (String.concat ["FlatAppPat", " ", ((PRINTLlist (fn ARGt =>
(PRINTLpat ARGt)
))
 ARGt)
])

| PRINTLpat (LConstraintPat ARGt)  = (String.concat ["ConstraintPat", " ", ((PRINTRECORDTYPEconstraintpattern ((fn ARGt =>
(PRINTLty ARGt)
), (fn ARGt =>
(PRINTLpat ARGt)
)))
 ARGt)
])

| PRINTLpat (LLayeredPat ARGt)  = (String.concat ["LayeredPat", " ", ((PRINTRECORDTYPEexpPatvarPat ((fn ARGt =>
(PRINTLpat ARGt)
), (fn ARGt =>
(PRINTLpat ARGt)
)))
 ARGt)
])

| PRINTLpat (LVectorPat ARGt)  = (String.concat ["VectorPat", " ", ((PRINTLlist (fn ARGt =>
(PRINTLpat ARGt)
))
 ARGt)
])

| PRINTLpat (LOrPat ARGt)  = (String.concat ["OrPat", " ", ((PRINTLlist (fn ARGt =>
(PRINTLpat ARGt)
))
 ARGt)
])

and PRINTLrule (Lrule (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLrule (Lrule (_, (ref (Option.SOME ARGt))))  = (PRINTLrule ARGt)

| PRINTLrule (LRule ARGt)  = (String.concat ["Rule", " ", ((PRINTRECORDTYPEexppat ((fn ARGt =>
(PRINTLexp ARGt)
), (fn ARGt =>
(PRINTLpat ARGt)
)))
 ARGt)
])


datatype dbrhs = Constrs of (id * ty option) list | Repl of id list

datatype Odbrhs = datatype dbrhs

datatype Ldbrhs = Ldbrhs of (Ostring * Ldbrhs Option.option ref) | LConstrs of (Lstring , Lty Loption) TUPLETYPE2 Llist | LRepl of Lstring Llist

fun EMBEDLdbrhs (Constrs ARGt)  = (LConstrs ((EMBEDLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
))))
 ARGt)
)))
 ARGt)
)

| EMBEDLdbrhs (Repl ARGt)  = (LRepl ((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)

and PROJLdbrhs (Ldbrhs (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLdbrhs (Ldbrhs (_, (ref (Option.SOME ARGt))))  = (PROJLdbrhs ARGt)

| PROJLdbrhs (LConstrs ARGt)  = (Constrs ((PROJLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
))))
 ARGt)
)))
 ARGt)
)

| PROJLdbrhs (LRepl ARGt)  = (Repl ((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)


fun OCCURLdbrhs ARG0 (Ldbrhs (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLdbrhs ARG0 (Ldbrhs (_, (ref (Option.SOME ARG2))))  = (OCCURLdbrhs ARG0 ARG2)

| OCCURLdbrhs ARG0 (LConstrs ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLoption (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARGv ARGt)
)))
 ARG0 ARG1)

| OCCURLdbrhs ARG0 (LRepl ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARG0 ARG1)


fun UNIFYLdbrhs (ARGt1, (Ldbrhs (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLdbrhs (ARGt1, ARGt2) ARGtc)

| UNIFYLdbrhs (ARGt1, (Ldbrhs (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLdbrhs ARG0 ARGt1)
)

| UNIFYLdbrhs ((Ldbrhs (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLdbrhs (ARGt1, ARGt2) ARGtc)

| UNIFYLdbrhs ((ARGt1 as (Ldbrhs (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLdbrhs (ARGt2, ARGt1) ARGtc)

| UNIFYLdbrhs ((LConstrs ARGt1), (LConstrs ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLoption ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLoption (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))
 ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLoption (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLdbrhs ((LRepl ARGt1), (LRepl ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLdbrhs (_, _) ARGtc  = (false, ARGtc)

fun PRINTLdbrhs (Ldbrhs (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLdbrhs (Ldbrhs (_, (ref (Option.SOME ARGt))))  = (PRINTLdbrhs ARGt)

| PRINTLdbrhs (LConstrs ARGt)  = (String.concat ["Constrs", " ", ((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
((PRINTLoption (fn ARGt =>
(PRINTLty ARGt)
))
 ARGt)
)))
 ARGt)
))
 ARGt)
])

| PRINTLdbrhs (LRepl ARGt)  = (String.concat ["Repl", " ", ((PRINTLlist (fn ARGt =>
(PRINTLstring ARGt)
))
 ARGt)
])


datatype ('a0, 'a1, 'a2) RECORDTYPErhstyctyvars = RECORDTYPErhstyctyvars of (Ostring * ('a0 , 'a1 , 'a2) RECORDTYPErhstyctyvars Option.option ref) | RECORDCONrhstyctyvars of {rhs:'a0, tyc:'a1, tyvars:'a2}

fun EMBEDRECORDTYPErhstyctyvars (qua0, qua1, qua2) ARGt0  = ((fn {rhs = rhs, tyc = tyc, tyvars = tyvars} =>
(RECORDCONrhstyctyvars {rhs = ((fn (ARG1, ARG2) =>
ARG1) qua0 rhs)
, tyc = ((fn (ARG1, ARG2) =>
ARG1) qua1 tyc)
, tyvars = ((fn (ARG1, ARG2) =>
ARG1) qua2 tyvars)
})
) ARGt0)

and PROJRECORDTYPErhstyctyvars (qua0, qua1, qua2) (RECORDTYPErhstyctyvars (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJRECORDTYPErhstyctyvars (qua0, qua1, qua2) (RECORDTYPErhstyctyvars (_, (ref (Option.SOME ARGt))))  = ((PROJRECORDTYPErhstyctyvars (qua0, qua1, qua2))
 ARGt)

| PROJRECORDTYPErhstyctyvars (qua0, qua1, qua2) (RECORDCONrhstyctyvars ARGt0)  = ((fn {rhs = rhs, tyc = tyc, tyvars = tyvars} =>
{rhs = ((fn (ARG1, ARG2) =>
ARG2) qua0 rhs)
, tyc = ((fn (ARG1, ARG2) =>
ARG2) qua1 tyc)
, tyvars = ((fn (ARG1, ARG2) =>
ARG2) qua2 tyvars)
}) ARGt0)


fun OCCURRECORDTYPErhstyctyvars (qua0, qua1, qua2) ARG0 (RECORDTYPErhstyctyvars (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURRECORDTYPErhstyctyvars (qua0, qua1, qua2) ARG0 (RECORDTYPErhstyctyvars (_, (ref (Option.SOME ARG2))))  = ((OCCURRECORDTYPErhstyctyvars (qua0, qua1, qua2))
 ARG0 ARG2)

| OCCURRECORDTYPErhstyctyvars (qua0, qua1, qua2) ARG0 (RECORDCONrhstyctyvars ARG1)  = ((fn ARGv =>
(fn {rhs = rhs, tyc = tyc, tyvars = tyvars} =>
((fn true =>
true | false =>
(qua2 ARGv tyvars)
) ((fn true =>
true | false =>
(qua1 ARGv tyc)
) ((fn true =>
true | false =>
(qua0 ARGv rhs)
) false)
)
)
)) ARG0 ARG1)


fun UNIFYRECORDTYPErhstyctyvars (qua0, qua1, qua2) (ARGt1, (RECORDTYPErhstyctyvars (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYRECORDTYPErhstyctyvars (qua0, qua1, qua2))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPErhstyctyvars (qua0, qua1, qua2) (ARGt1, (RECORDTYPErhstyctyvars (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURRECORDTYPErhstyctyvars (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
, ((fn (ARG1, ARG2) =>
ARG2) qua2)
))
 ARG0 ARGt1)
)

| UNIFYRECORDTYPErhstyctyvars (qua0, qua1, qua2) ((RECORDTYPErhstyctyvars (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPErhstyctyvars (qua0, qua1, qua2))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPErhstyctyvars (qua0, qua1, qua2) ((ARGt1 as (RECORDTYPErhstyctyvars (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPErhstyctyvars (qua0, qua1, qua2))
 (ARGt2, ARGt1) ARGtc)

| UNIFYRECORDTYPErhstyctyvars (qua0, qua1, qua2) ((RECORDCONrhstyctyvars ARGt1), (RECORDCONrhstyctyvars ARGt2)) ARGtc  = ((fn ({rhs = rhs1, tyc = tyc1, tyvars = tyvars1}, {rhs = rhs2, tyc = tyc2, tyvars = tyvars2}) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua2)
 (tyvars1, tyvars2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (tyc1, tyc2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (rhs1, rhs2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
)
) (ARGt1, ARGt2))


fun PRINTRECORDTYPErhstyctyvars (qua0, qua1, qua2) (RECORDTYPErhstyctyvars (ARGs, (ref Option.NONE)))  = ARGs
| PRINTRECORDTYPErhstyctyvars (qua0, qua1, qua2) (RECORDTYPErhstyctyvars (_, (ref (Option.SOME ARGt))))  = ((PRINTRECORDTYPErhstyctyvars (qua0, qua1, qua2))
 ARGt)

| PRINTRECORDTYPErhstyctyvars (qua0, qua1, qua2) (RECORDCONrhstyctyvars ARGt0)  = ((fn {rhs = rhs, tyc = tyc, tyvars = tyvars} =>
(String.concat ["{", (SMLOG.intersperse "," [(String.concat ["rhs", "=", (qua0 rhs)
])
, (String.concat ["tyc", "=", (qua1 tyc)
])
, (String.concat ["tyvars", "=", (qua2 tyvars)
])
])
, "}"])
) ARGt0)


datatype db = Db of {rhs:dbrhs, tyc:id, tyvars:tyvar list}

datatype Odb = datatype db

datatype Ldb = Ldb of (Ostring * Ldb Option.option ref) | LDb of (Ldbrhs , Lstring , Lstring Llist) RECORDTYPErhstyctyvars

fun EMBEDLdb (Db ARGt)  = (LDb ((EMBEDRECORDTYPErhstyctyvars (((fn ARGt =>
(EMBEDLdbrhs ARGt)
), (fn ARGt =>
(PROJLdbrhs ARGt)
)), ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
))))
 ARGt)
)

and PROJLdb (Ldb (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLdb (Ldb (_, (ref (Option.SOME ARGt))))  = (PROJLdb ARGt)

| PROJLdb (LDb ARGt)  = (Db ((PROJRECORDTYPErhstyctyvars (((fn ARGt =>
(EMBEDLdbrhs ARGt)
), (fn ARGt =>
(PROJLdbrhs ARGt)
)), ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
))))
 ARGt)
)


fun OCCURLdb ARG0 (Ldb (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLdb ARG0 (Ldb (_, (ref (Option.SOME ARG2))))  = (OCCURLdb ARG0 ARG2)

| OCCURLdb ARG0 (LDb ARG1)  = ((OCCURRECORDTYPErhstyctyvars ((fn ARGv =>
(fn ARGt =>
(OCCURLdbrhs ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARG0 ARG1)


fun UNIFYLdb (ARGt1, (Ldb (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLdb (ARGt1, ARGt2) ARGtc)

| UNIFYLdb (ARGt1, (Ldb (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLdb ARG0 ARGt1)
)

| UNIFYLdb ((Ldb (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLdb (ARGt1, ARGt2) ARGtc)

| UNIFYLdb ((ARGt1 as (Ldb (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLdb (ARGt2, ARGt1) ARGtc)

| UNIFYLdb ((LDb ARGt1), (LDb ARGt2)) ARGtc  = ((UNIFYRECORDTYPErhstyctyvars (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLdbrhs (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLdbrhs ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)


fun PRINTLdb (Ldb (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLdb (Ldb (_, (ref (Option.SOME ARGt))))  = (PRINTLdb ARGt)

| PRINTLdb (LDb ARGt)  = (String.concat ["Db", " ", ((PRINTRECORDTYPErhstyctyvars ((fn ARGt =>
(PRINTLdbrhs ARGt)
), (fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLstring ARGt)
))
 ARGt)
)))
 ARGt)
])


datatype ('a0, 'a1, 'a2) RECORDTYPEdeftyctyvars = RECORDTYPEdeftyctyvars of (Ostring * ('a0 , 'a1 , 'a2) RECORDTYPEdeftyctyvars Option.option ref) | RECORDCONdeftyctyvars of {def:'a0, tyc:'a1, tyvars:'a2}

fun EMBEDRECORDTYPEdeftyctyvars (qua0, qua1, qua2) ARGt0  = ((fn {def = def, tyc = tyc, tyvars = tyvars} =>
(RECORDCONdeftyctyvars {def = ((fn (ARG1, ARG2) =>
ARG1) qua0 def)
, tyc = ((fn (ARG1, ARG2) =>
ARG1) qua1 tyc)
, tyvars = ((fn (ARG1, ARG2) =>
ARG1) qua2 tyvars)
})
) ARGt0)

and PROJRECORDTYPEdeftyctyvars (qua0, qua1, qua2) (RECORDTYPEdeftyctyvars (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJRECORDTYPEdeftyctyvars (qua0, qua1, qua2) (RECORDTYPEdeftyctyvars (_, (ref (Option.SOME ARGt))))  = ((PROJRECORDTYPEdeftyctyvars (qua0, qua1, qua2))
 ARGt)

| PROJRECORDTYPEdeftyctyvars (qua0, qua1, qua2) (RECORDCONdeftyctyvars ARGt0)  = ((fn {def = def, tyc = tyc, tyvars = tyvars} =>
{def = ((fn (ARG1, ARG2) =>
ARG2) qua0 def)
, tyc = ((fn (ARG1, ARG2) =>
ARG2) qua1 tyc)
, tyvars = ((fn (ARG1, ARG2) =>
ARG2) qua2 tyvars)
}) ARGt0)


fun OCCURRECORDTYPEdeftyctyvars (qua0, qua1, qua2) ARG0 (RECORDTYPEdeftyctyvars (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURRECORDTYPEdeftyctyvars (qua0, qua1, qua2) ARG0 (RECORDTYPEdeftyctyvars (_, (ref (Option.SOME ARG2))))  = ((OCCURRECORDTYPEdeftyctyvars (qua0, qua1, qua2))
 ARG0 ARG2)

| OCCURRECORDTYPEdeftyctyvars (qua0, qua1, qua2) ARG0 (RECORDCONdeftyctyvars ARG1)  = ((fn ARGv =>
(fn {def = def, tyc = tyc, tyvars = tyvars} =>
((fn true =>
true | false =>
(qua2 ARGv tyvars)
) ((fn true =>
true | false =>
(qua1 ARGv tyc)
) ((fn true =>
true | false =>
(qua0 ARGv def)
) false)
)
)
)) ARG0 ARG1)


fun UNIFYRECORDTYPEdeftyctyvars (qua0, qua1, qua2) (ARGt1, (RECORDTYPEdeftyctyvars (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYRECORDTYPEdeftyctyvars (qua0, qua1, qua2))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEdeftyctyvars (qua0, qua1, qua2) (ARGt1, (RECORDTYPEdeftyctyvars (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURRECORDTYPEdeftyctyvars (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
, ((fn (ARG1, ARG2) =>
ARG2) qua2)
))
 ARG0 ARGt1)
)

| UNIFYRECORDTYPEdeftyctyvars (qua0, qua1, qua2) ((RECORDTYPEdeftyctyvars (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEdeftyctyvars (qua0, qua1, qua2))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEdeftyctyvars (qua0, qua1, qua2) ((ARGt1 as (RECORDTYPEdeftyctyvars (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEdeftyctyvars (qua0, qua1, qua2))
 (ARGt2, ARGt1) ARGtc)

| UNIFYRECORDTYPEdeftyctyvars (qua0, qua1, qua2) ((RECORDCONdeftyctyvars ARGt1), (RECORDCONdeftyctyvars ARGt2)) ARGtc  = ((fn ({def = def1, tyc = tyc1, tyvars = tyvars1}, {def = def2, tyc = tyc2, tyvars = tyvars2}) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua2)
 (tyvars1, tyvars2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (tyc1, tyc2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (def1, def2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
)
) (ARGt1, ARGt2))


fun PRINTRECORDTYPEdeftyctyvars (qua0, qua1, qua2) (RECORDTYPEdeftyctyvars (ARGs, (ref Option.NONE)))  = ARGs
| PRINTRECORDTYPEdeftyctyvars (qua0, qua1, qua2) (RECORDTYPEdeftyctyvars (_, (ref (Option.SOME ARGt))))  = ((PRINTRECORDTYPEdeftyctyvars (qua0, qua1, qua2))
 ARGt)

| PRINTRECORDTYPEdeftyctyvars (qua0, qua1, qua2) (RECORDCONdeftyctyvars ARGt0)  = ((fn {def = def, tyc = tyc, tyvars = tyvars} =>
(String.concat ["{", (SMLOG.intersperse "," [(String.concat ["def", "=", (qua0 def)
])
, (String.concat ["tyc", "=", (qua1 tyc)
])
, (String.concat ["tyvars", "=", (qua2 tyvars)
])
])
, "}"])
) ARGt0)


datatype tb = Tb of {def:ty, tyc:id, tyvars:tyvar list}

datatype Otb = datatype tb

datatype Ltb = Ltb of (Ostring * Ltb Option.option ref) | LTb of (Lty , Lstring , Lstring Llist) RECORDTYPEdeftyctyvars

fun EMBEDLtb (Tb ARGt)  = (LTb ((EMBEDRECORDTYPEdeftyctyvars (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
))))
 ARGt)
)

and PROJLtb (Ltb (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLtb (Ltb (_, (ref (Option.SOME ARGt))))  = (PROJLtb ARGt)

| PROJLtb (LTb ARGt)  = (Tb ((PROJRECORDTYPEdeftyctyvars (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
))))
 ARGt)
)


fun OCCURLtb ARG0 (Ltb (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLtb ARG0 (Ltb (_, (ref (Option.SOME ARG2))))  = (OCCURLtb ARG0 ARG2)

| OCCURLtb ARG0 (LTb ARG1)  = ((OCCURRECORDTYPEdeftyctyvars ((fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARG0 ARG1)


fun UNIFYLtb (ARGt1, (Ltb (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLtb (ARGt1, ARGt2) ARGtc)

| UNIFYLtb (ARGt1, (Ltb (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLtb ARG0 ARGt1)
)

| UNIFYLtb ((Ltb (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLtb (ARGt1, ARGt2) ARGtc)

| UNIFYLtb ((ARGt1 as (Ltb (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLtb (ARGt2, ARGt1) ARGtc)

| UNIFYLtb ((LTb ARGt1), (LTb ARGt2)) ARGtc  = ((UNIFYRECORDTYPEdeftyctyvars (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)


fun PRINTLtb (Ltb (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLtb (Ltb (_, (ref (Option.SOME ARGt))))  = (PRINTLtb ARGt)

| PRINTLtb (LTb ARGt)  = (String.concat ["Tb", " ", ((PRINTRECORDTYPEdeftyctyvars ((fn ARGt =>
(PRINTLty ARGt)
), (fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLstring ARGt)
))
 ARGt)
)))
 ARGt)
])


datatype ('a0, 'a1, 'a2) RECORDTYPEexppatsresultty = RECORDTYPEexppatsresultty of (Ostring * ('a0 , 'a1 , 'a2) RECORDTYPEexppatsresultty Option.option ref) | RECORDCONexppatsresultty of {exp:'a0, pats:'a1, resultty:'a2}

fun EMBEDRECORDTYPEexppatsresultty (qua0, qua1, qua2) ARGt0  = ((fn {exp = exp, pats = pats, resultty = resultty} =>
(RECORDCONexppatsresultty {exp = ((fn (ARG1, ARG2) =>
ARG1) qua0 exp)
, pats = ((fn (ARG1, ARG2) =>
ARG1) qua1 pats)
, resultty = ((fn (ARG1, ARG2) =>
ARG1) qua2 resultty)
})
) ARGt0)

and PROJRECORDTYPEexppatsresultty (qua0, qua1, qua2) (RECORDTYPEexppatsresultty (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJRECORDTYPEexppatsresultty (qua0, qua1, qua2) (RECORDTYPEexppatsresultty (_, (ref (Option.SOME ARGt))))  = ((PROJRECORDTYPEexppatsresultty (qua0, qua1, qua2))
 ARGt)

| PROJRECORDTYPEexppatsresultty (qua0, qua1, qua2) (RECORDCONexppatsresultty ARGt0)  = ((fn {exp = exp, pats = pats, resultty = resultty} =>
{exp = ((fn (ARG1, ARG2) =>
ARG2) qua0 exp)
, pats = ((fn (ARG1, ARG2) =>
ARG2) qua1 pats)
, resultty = ((fn (ARG1, ARG2) =>
ARG2) qua2 resultty)
}) ARGt0)


fun OCCURRECORDTYPEexppatsresultty (qua0, qua1, qua2) ARG0 (RECORDTYPEexppatsresultty (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURRECORDTYPEexppatsresultty (qua0, qua1, qua2) ARG0 (RECORDTYPEexppatsresultty (_, (ref (Option.SOME ARG2))))  = ((OCCURRECORDTYPEexppatsresultty (qua0, qua1, qua2))
 ARG0 ARG2)

| OCCURRECORDTYPEexppatsresultty (qua0, qua1, qua2) ARG0 (RECORDCONexppatsresultty ARG1)  = ((fn ARGv =>
(fn {exp = exp, pats = pats, resultty = resultty} =>
((fn true =>
true | false =>
(qua2 ARGv resultty)
) ((fn true =>
true | false =>
(qua1 ARGv pats)
) ((fn true =>
true | false =>
(qua0 ARGv exp)
) false)
)
)
)) ARG0 ARG1)


fun UNIFYRECORDTYPEexppatsresultty (qua0, qua1, qua2) (ARGt1, (RECORDTYPEexppatsresultty (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYRECORDTYPEexppatsresultty (qua0, qua1, qua2))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEexppatsresultty (qua0, qua1, qua2) (ARGt1, (RECORDTYPEexppatsresultty (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURRECORDTYPEexppatsresultty (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
, ((fn (ARG1, ARG2) =>
ARG2) qua2)
))
 ARG0 ARGt1)
)

| UNIFYRECORDTYPEexppatsresultty (qua0, qua1, qua2) ((RECORDTYPEexppatsresultty (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEexppatsresultty (qua0, qua1, qua2))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEexppatsresultty (qua0, qua1, qua2) ((ARGt1 as (RECORDTYPEexppatsresultty (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEexppatsresultty (qua0, qua1, qua2))
 (ARGt2, ARGt1) ARGtc)

| UNIFYRECORDTYPEexppatsresultty (qua0, qua1, qua2) ((RECORDCONexppatsresultty ARGt1), (RECORDCONexppatsresultty ARGt2)) ARGtc  = ((fn ({exp = exp1, pats = pats1, resultty = resultty1}, {exp = exp2, pats = pats2, resultty = resultty2}) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua2)
 (resultty1, resultty2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (pats1, pats2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (exp1, exp2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
)
) (ARGt1, ARGt2))


fun PRINTRECORDTYPEexppatsresultty (qua0, qua1, qua2) (RECORDTYPEexppatsresultty (ARGs, (ref Option.NONE)))  = ARGs
| PRINTRECORDTYPEexppatsresultty (qua0, qua1, qua2) (RECORDTYPEexppatsresultty (_, (ref (Option.SOME ARGt))))  = ((PRINTRECORDTYPEexppatsresultty (qua0, qua1, qua2))
 ARGt)

| PRINTRECORDTYPEexppatsresultty (qua0, qua1, qua2) (RECORDCONexppatsresultty ARGt0)  = ((fn {exp = exp, pats = pats, resultty = resultty} =>
(String.concat ["{", (SMLOG.intersperse "," [(String.concat ["exp", "=", (qua0 exp)
])
, (String.concat ["pats", "=", (qua1 pats)
])
, (String.concat ["resultty", "=", (qua2 resultty)
])
])
, "}"])
) ARGt0)


datatype fb = Fb of clause list
and clause = Clause of {exp:exp, pats:pat list, resultty:ty option}

datatype Ofb = datatype fb

datatype Oclause = datatype clause

datatype Lfb = Lfb of (Ostring * Lfb Option.option ref) | LFb of Lclause Llist
and Lclause = Lclause of (Ostring * Lclause Option.option ref) | LClause of (Lexp , Lpat Llist , Lty Loption) RECORDTYPEexppatsresultty

fun EMBEDLfb (Fb ARGt)  = (LFb ((EMBEDLlist ((fn ARGt =>
(EMBEDLclause ARGt)
), (fn ARGt =>
(PROJLclause ARGt)
)))
 ARGt)
)

and PROJLfb (Lfb (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLfb (Lfb (_, (ref (Option.SOME ARGt))))  = (PROJLfb ARGt)

| PROJLfb (LFb ARGt)  = (Fb ((PROJLlist ((fn ARGt =>
(EMBEDLclause ARGt)
), (fn ARGt =>
(PROJLclause ARGt)
)))
 ARGt)
)

and EMBEDLclause (Clause ARGt)  = (LClause ((EMBEDRECORDTYPEexppatsresultty (((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
))))
 ARGt)
)

and PROJLclause (Lclause (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLclause (Lclause (_, (ref (Option.SOME ARGt))))  = (PROJLclause ARGt)

| PROJLclause (LClause ARGt)  = (Clause ((PROJRECORDTYPEexppatsresultty (((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)))
 ARGt)
))))
 ARGt)
)


fun OCCURLfb ARG0 (Lfb (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLfb ARG0 (Lfb (_, (ref (Option.SOME ARG2))))  = (OCCURLfb ARG0 ARG2)

| OCCURLfb ARG0 (LFb ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLclause ARGv ARGt)
)))
 ARG0 ARG1)

and OCCURLclause ARG0 (Lclause (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLclause ARG0 (Lclause (_, (ref (Option.SOME ARG2))))  = (OCCURLclause ARG0 ARG2)

| OCCURLclause ARG0 (LClause ARG1)  = ((OCCURRECORDTYPEexppatsresultty ((fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
)))
 ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLoption (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARG0 ARG1)


fun UNIFYLfb (ARGt1, (Lfb (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLfb (ARGt1, ARGt2) ARGtc)

| UNIFYLfb (ARGt1, (Lfb (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLfb ARG0 ARGt1)
)

| UNIFYLfb ((Lfb (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLfb (ARGt1, ARGt2) ARGtc)

| UNIFYLfb ((ARGt1 as (Lfb (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLfb (ARGt2, ARGt1) ARGtc)

| UNIFYLfb ((LFb ARGt1), (LFb ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLclause (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLclause ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

and UNIFYLclause (ARGt1, (Lclause (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLclause (ARGt1, ARGt2) ARGtc)

| UNIFYLclause (ARGt1, (Lclause (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLclause ARG0 ARGt1)
)

| UNIFYLclause ((Lclause (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLclause (ARGt1, ARGt2) ARGtc)

| UNIFYLclause ((ARGt1 as (Lclause (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLclause (ARGt2, ARGt1) ARGtc)

| UNIFYLclause ((LClause ARGt1), (LClause ARGt2)) ARGtc  = ((UNIFYRECORDTYPEexppatsresultty (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLexp (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLpat (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
)))
 ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLoption ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLoption (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)))
 ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)


fun PRINTLfb (Lfb (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLfb (Lfb (_, (ref (Option.SOME ARGt))))  = (PRINTLfb ARGt)

| PRINTLfb (LFb ARGt)  = (String.concat ["Fb", " ", ((PRINTLlist (fn ARGt =>
(PRINTLclause ARGt)
))
 ARGt)
])

and PRINTLclause (Lclause (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLclause (Lclause (_, (ref (Option.SOME ARGt))))  = (PRINTLclause ARGt)

| PRINTLclause (LClause ARGt)  = (String.concat ["Clause", " ", ((PRINTRECORDTYPEexppatsresultty ((fn ARGt =>
(PRINTLexp ARGt)
), (fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLpat ARGt)
))
 ARGt)
), (fn ARGt =>
((PRINTLoption (fn ARGt =>
(PRINTLty ARGt)
))
 ARGt)
)))
 ARGt)
])


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


datatype ('a0, 'a1, 'a2) RECORDTYPEargsbodyname = RECORDTYPEargsbodyname of (Ostring * ('a0 , 'a1 , 'a2) RECORDTYPEargsbodyname Option.option ref) | RECORDCONargsbodyname of {args:'a0, body:'a1, name:'a2}

fun EMBEDRECORDTYPEargsbodyname (qua0, qua1, qua2) ARGt0  = ((fn {args = args, body = body, name = name} =>
(RECORDCONargsbodyname {args = ((fn (ARG1, ARG2) =>
ARG1) qua0 args)
, body = ((fn (ARG1, ARG2) =>
ARG1) qua1 body)
, name = ((fn (ARG1, ARG2) =>
ARG1) qua2 name)
})
) ARGt0)

and PROJRECORDTYPEargsbodyname (qua0, qua1, qua2) (RECORDTYPEargsbodyname (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJRECORDTYPEargsbodyname (qua0, qua1, qua2) (RECORDTYPEargsbodyname (_, (ref (Option.SOME ARGt))))  = ((PROJRECORDTYPEargsbodyname (qua0, qua1, qua2))
 ARGt)

| PROJRECORDTYPEargsbodyname (qua0, qua1, qua2) (RECORDCONargsbodyname ARGt0)  = ((fn {args = args, body = body, name = name} =>
{args = ((fn (ARG1, ARG2) =>
ARG2) qua0 args)
, body = ((fn (ARG1, ARG2) =>
ARG2) qua1 body)
, name = ((fn (ARG1, ARG2) =>
ARG2) qua2 name)
}) ARGt0)


fun OCCURRECORDTYPEargsbodyname (qua0, qua1, qua2) ARG0 (RECORDTYPEargsbodyname (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURRECORDTYPEargsbodyname (qua0, qua1, qua2) ARG0 (RECORDTYPEargsbodyname (_, (ref (Option.SOME ARG2))))  = ((OCCURRECORDTYPEargsbodyname (qua0, qua1, qua2))
 ARG0 ARG2)

| OCCURRECORDTYPEargsbodyname (qua0, qua1, qua2) ARG0 (RECORDCONargsbodyname ARG1)  = ((fn ARGv =>
(fn {args = args, body = body, name = name} =>
((fn true =>
true | false =>
(qua2 ARGv name)
) ((fn true =>
true | false =>
(qua1 ARGv body)
) ((fn true =>
true | false =>
(qua0 ARGv args)
) false)
)
)
)) ARG0 ARG1)


fun UNIFYRECORDTYPEargsbodyname (qua0, qua1, qua2) (ARGt1, (RECORDTYPEargsbodyname (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYRECORDTYPEargsbodyname (qua0, qua1, qua2))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEargsbodyname (qua0, qua1, qua2) (ARGt1, (RECORDTYPEargsbodyname (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURRECORDTYPEargsbodyname (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
, ((fn (ARG1, ARG2) =>
ARG2) qua2)
))
 ARG0 ARGt1)
)

| UNIFYRECORDTYPEargsbodyname (qua0, qua1, qua2) ((RECORDTYPEargsbodyname (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEargsbodyname (qua0, qua1, qua2))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEargsbodyname (qua0, qua1, qua2) ((ARGt1 as (RECORDTYPEargsbodyname (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEargsbodyname (qua0, qua1, qua2))
 (ARGt2, ARGt1) ARGtc)

| UNIFYRECORDTYPEargsbodyname (qua0, qua1, qua2) ((RECORDCONargsbodyname ARGt1), (RECORDCONargsbodyname ARGt2)) ARGtc  = ((fn ({args = args1, body = body1, name = name1}, {args = args2, body = body2, name = name2}) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua2)
 (name1, name2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (body1, body2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (args1, args2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
)
) (ARGt1, ARGt2))


fun PRINTRECORDTYPEargsbodyname (qua0, qua1, qua2) (RECORDTYPEargsbodyname (ARGs, (ref Option.NONE)))  = ARGs
| PRINTRECORDTYPEargsbodyname (qua0, qua1, qua2) (RECORDTYPEargsbodyname (_, (ref (Option.SOME ARGt))))  = ((PRINTRECORDTYPEargsbodyname (qua0, qua1, qua2))
 ARGt)

| PRINTRECORDTYPEargsbodyname (qua0, qua1, qua2) (RECORDCONargsbodyname ARGt0)  = ((fn {args = args, body = body, name = name} =>
(String.concat ["{", (SMLOG.intersperse "," [(String.concat ["args", "=", (qua0 args)
])
, (String.concat ["body", "=", (qua1 body)
])
, (String.concat ["name", "=", (qua2 name)
])
])
, "}"])
) ARGt0)


datatype rb = Rb of (id * relation list * bool)
and relation = Relation of {args:pat list, body:(path * exp list) list, name:id}

datatype Orb = datatype rb

datatype Orelation = datatype relation

datatype Lrb = Lrb of (Ostring * Lrb Option.option ref) | LRb of (Lstring , Lrelation Llist , Lbool) TUPLETYPE3
and Lrelation = Lrelation of (Ostring * Lrelation Option.option ref) | LRelation of (Lpat Llist , (Lstring Llist , Lexp Llist) TUPLETYPE2 Llist , Lstring) RECORDTYPEargsbodyname

fun EMBEDLrb (Rb ARGt)  = (LRb ((EMBEDTUPLETYPE3 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLrelation ARGt)
), (fn ARGt =>
(PROJLrelation ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLrelation ARGt)
), (fn ARGt =>
(PROJLrelation ARGt)
)))
 ARGt)
)), ((fn ARGt =>
(EMBEDLbool ARGt)
), (fn ARGt =>
(PROJLbool ARGt)
))))
 ARGt)
)

and PROJLrb (Lrb (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLrb (Lrb (_, (ref (Option.SOME ARGt))))  = (PROJLrb ARGt)

| PROJLrb (LRb ARGt)  = (Rb ((PROJTUPLETYPE3 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLrelation ARGt)
), (fn ARGt =>
(PROJLrelation ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLrelation ARGt)
), (fn ARGt =>
(PROJLrelation ARGt)
)))
 ARGt)
)), ((fn ARGt =>
(EMBEDLbool ARGt)
), (fn ARGt =>
(PROJLbool ARGt)
))))
 ARGt)
)

and EMBEDLrelation (Relation ARGt)  = (LRelation ((EMBEDRECORDTYPEargsbodyname (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
))))
 ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
))))
 ARGt)
)))
 ARGt)
)), ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
))))
 ARGt)
)

and PROJLrelation (Lrelation (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLrelation (Lrelation (_, (ref (Option.SOME ARGt))))  = (PROJLrelation ARGt)

| PROJLrelation (LRelation ARGt)  = (Relation ((PROJRECORDTYPEargsbodyname (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLpat ARGt)
), (fn ARGt =>
(PROJLpat ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
))))
 ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)))
 ARGt)
))))
 ARGt)
)))
 ARGt)
)), ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
))))
 ARGt)
)


fun OCCURLrb ARG0 (Lrb (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLrb ARG0 (Lrb (_, (ref (Option.SOME ARG2))))  = (OCCURLrb ARG0 ARG2)

| OCCURLrb ARG0 (LRb ARG1)  = ((OCCURTUPLETYPE3 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLrelation ARGv ARGt)
)))
 ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLbool ARGv ARGt)
))))
 ARG0 ARG1)

and OCCURLrelation ARG0 (Lrelation (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLrelation ARG0 (Lrelation (_, (ref (Option.SOME ARG2))))  = (OCCURLrelation ARG0 ARG2)

| OCCURLrelation ARG0 (LRelation ARG1)  = ((OCCURRECORDTYPEargsbodyname ((fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
)))
 ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARGv ARGt)
)))
 ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))))
 ARG0 ARG1)


fun UNIFYLrb (ARGt1, (Lrb (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLrb (ARGt1, ARGt2) ARGtc)

| UNIFYLrb (ARGt1, (Lrb (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLrb ARG0 ARGt1)
)

| UNIFYLrb ((Lrb (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLrb (ARGt1, ARGt2) ARGtc)

| UNIFYLrb ((ARGt1 as (Lrb (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLrb (ARGt2, ARGt1) ARGtc)

| UNIFYLrb ((LRb ARGt1), (LRb ARGt2)) ARGtc  = ((UNIFYTUPLETYPE3 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLrelation (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLrelation ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLrelation ARGv ARGt)
)))
 ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLbool (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLbool ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

and UNIFYLrelation (ARGt1, (Lrelation (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLrelation (ARGt1, ARGt2) ARGtc)

| UNIFYLrelation (ARGt1, (Lrelation (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLrelation ARG0 ARGt1)
)

| UNIFYLrelation ((Lrelation (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLrelation (ARGt1, ARGt2) ARGtc)

| UNIFYLrelation ((ARGt1 as (Lrelation (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLrelation (ARGt2, ARGt1) ARGtc)

| UNIFYLrelation ((LRelation ARGt1), (LRelation ARGt2)) ARGtc  = ((UNIFYRECORDTYPEargsbodyname (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLpat (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLpat ARGv ARGt)
)))
 ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLexp (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)))
 ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)))
 ARGv ARGt)
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
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARGv ARGt)
)))
 ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)


fun PRINTLrb (Lrb (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLrb (Lrb (_, (ref (Option.SOME ARGt))))  = (PRINTLrb ARGt)

| PRINTLrb (LRb ARGt)  = (String.concat ["Rb", " ", ((PRINTTUPLETYPE3 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLrelation ARGt)
))
 ARGt)
), (fn ARGt =>
(PRINTLbool ARGt)
)))
 ARGt)
])

and PRINTLrelation (Lrelation (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLrelation (Lrelation (_, (ref (Option.SOME ARGt))))  = (PRINTLrelation ARGt)

| PRINTLrelation (LRelation ARGt)  = (String.concat ["Relation", " ", ((PRINTRECORDTYPEargsbodyname ((fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLpat ARGt)
))
 ARGt)
), (fn ARGt =>
((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLstring ARGt)
))
 ARGt)
), (fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLexp ARGt)
))
 ARGt)
)))
 ARGt)
))
 ARGt)
), (fn ARGt =>
(PRINTLstring ARGt)
)))
 ARGt)
])


datatype decl = RelationDec of (rb list * tyvar list) | RelationSigDec of (id * (ty * inout option) list) | DatatypeDec of (db list * tb list option) | TypeDec of tb list | FunDec of (fb list * tyvar list)

datatype Odecl = datatype decl

datatype Ldecl = Ldecl of (Ostring * Ldecl Option.option ref) | LRelationDec of (Lrb Llist , Lstring Llist) TUPLETYPE2 | LRelationSigDec of (Lstring , (Lty , Linout Loption) TUPLETYPE2 Llist) TUPLETYPE2 | LDatatypeDec of (Ldb Llist , Ltb Llist Loption) TUPLETYPE2 | LTypeDec of Ltb Llist | LFunDec of (Lfb Llist , Lstring Llist) TUPLETYPE2

fun EMBEDLdecl (RelationDec ARGt)  = (LRelationDec ((EMBEDTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLrb ARGt)
), (fn ARGt =>
(PROJLrb ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLrb ARGt)
), (fn ARGt =>
(PROJLrb ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
))))
 ARGt)
)

| EMBEDLdecl (RelationSigDec ARGt)  = (LRelationSigDec ((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
))))
 ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
))))
 ARGt)
)))
 ARGt)
))))
 ARGt)
)

| EMBEDLdecl (DatatypeDec ARGt)  = (LDatatypeDec ((EMBEDTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLdb ARGt)
), (fn ARGt =>
(PROJLdb ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLdb ARGt)
), (fn ARGt =>
(PROJLdb ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLtb ARGt)
), (fn ARGt =>
(PROJLtb ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLtb ARGt)
), (fn ARGt =>
(PROJLtb ARGt)
)))
 ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLtb ARGt)
), (fn ARGt =>
(PROJLtb ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLtb ARGt)
), (fn ARGt =>
(PROJLtb ARGt)
)))
 ARGt)
)))
 ARGt)
))))
 ARGt)
)

| EMBEDLdecl (TypeDec ARGt)  = (LTypeDec ((EMBEDLlist ((fn ARGt =>
(EMBEDLtb ARGt)
), (fn ARGt =>
(PROJLtb ARGt)
)))
 ARGt)
)

| EMBEDLdecl (FunDec ARGt)  = (LFunDec ((EMBEDTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLfb ARGt)
), (fn ARGt =>
(PROJLfb ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLfb ARGt)
), (fn ARGt =>
(PROJLfb ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
))))
 ARGt)
)

and PROJLdecl (Ldecl (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLdecl (Ldecl (_, (ref (Option.SOME ARGt))))  = (PROJLdecl ARGt)

| PROJLdecl (LRelationDec ARGt)  = (RelationDec ((PROJTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLrb ARGt)
), (fn ARGt =>
(PROJLrb ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLrb ARGt)
), (fn ARGt =>
(PROJLrb ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
))))
 ARGt)
)

| PROJLdecl (LRelationSigDec ARGt)  = (RelationSigDec ((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
))))
 ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
(EMBEDLinout ARGt)
), (fn ARGt =>
(PROJLinout ARGt)
)))
 ARGt)
))))
 ARGt)
)))
 ARGt)
))))
 ARGt)
)

| PROJLdecl (LDatatypeDec ARGt)  = (DatatypeDec ((PROJTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLdb ARGt)
), (fn ARGt =>
(PROJLdb ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLdb ARGt)
), (fn ARGt =>
(PROJLdb ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLoption ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLtb ARGt)
), (fn ARGt =>
(PROJLtb ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLtb ARGt)
), (fn ARGt =>
(PROJLtb ARGt)
)))
 ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLoption ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLtb ARGt)
), (fn ARGt =>
(PROJLtb ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLtb ARGt)
), (fn ARGt =>
(PROJLtb ARGt)
)))
 ARGt)
)))
 ARGt)
))))
 ARGt)
)

| PROJLdecl (LTypeDec ARGt)  = (TypeDec ((PROJLlist ((fn ARGt =>
(EMBEDLtb ARGt)
), (fn ARGt =>
(PROJLtb ARGt)
)))
 ARGt)
)

| PROJLdecl (LFunDec ARGt)  = (FunDec ((PROJTUPLETYPE2 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLfb ARGt)
), (fn ARGt =>
(PROJLfb ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLfb ARGt)
), (fn ARGt =>
(PROJLfb ARGt)
)))
 ARGt)
)), ((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)))
 ARGt)
))))
 ARGt)
)


fun OCCURLdecl ARG0 (Ldecl (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLdecl ARG0 (Ldecl (_, (ref (Option.SOME ARG2))))  = (OCCURLdecl ARG0 ARG2)

| OCCURLdecl ARG0 (LRelationDec ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLrb ARGv ARGt)
)))
 ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLdecl ARG0 (LRelationSigDec ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLoption (fn ARGv =>
(fn ARGt =>
(OCCURLinout ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLdecl ARG0 (LDatatypeDec ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLdb ARGv ARGt)
)))
 ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLoption (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLtb ARGv ARGt)
)))
 ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLdecl ARG0 (LTypeDec ARG1)  = ((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLtb ARGv ARGt)
)))
 ARG0 ARG1)

| OCCURLdecl ARG0 (LFunDec ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLfb ARGv ARGt)
)))
 ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARG0 ARG1)


fun UNIFYLdecl (ARGt1, (Ldecl (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLdecl (ARGt1, ARGt2) ARGtc)

| UNIFYLdecl (ARGt1, (Ldecl (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLdecl ARG0 ARGt1)
)

| UNIFYLdecl ((Ldecl (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLdecl (ARGt1, ARGt2) ARGtc)

| UNIFYLdecl ((ARGt1 as (Ldecl (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLdecl (ARGt2, ARGt1) ARGtc)

| UNIFYLdecl ((LRelationDec ARGt1), (LRelationDec ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLrb (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLrb ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLrb ARGv ARGt)
)))
 ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLdecl ((LRelationSigDec ARGt1), (LRelationSigDec ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLty (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLoption ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLinout (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLinout ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLoption (fn ARGv =>
(fn ARGt =>
(OCCURLinout ARGv ARGt)
)))
 ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLoption (fn ARGv =>
(fn ARGt =>
(OCCURLinout ARGv ARGt)
)))
 ARGv ARGt)
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
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLoption (fn ARGv =>
(fn ARGt =>
(OCCURLinout ARGv ARGt)
)))
 ARGv ARGt)
))))
 ARGv ARGt)
)))
 ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLdecl ((LDatatypeDec ARGt1), (LDatatypeDec ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLdb (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLdb ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLdb ARGv ARGt)
)))
 ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLoption ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtb (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtb ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLtb ARGv ARGt)
)))
 ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLoption (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLtb ARGv ARGt)
)))
 ARGv ARGt)
)))
 ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLdecl ((LTypeDec ARGt1), (LTypeDec ARGt2)) ARGtc  = ((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLtb (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLtb ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLdecl ((LFunDec ARGt1), (LFunDec ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLfb (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLfb ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLfb ARGv ARGt)
)))
 ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
((UNIFYLlist ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)))
 ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLdecl (_, _) ARGtc  = (false, ARGtc)

fun PRINTLdecl (Ldecl (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLdecl (Ldecl (_, (ref (Option.SOME ARGt))))  = (PRINTLdecl ARGt)

| PRINTLdecl (LRelationDec ARGt)  = (String.concat ["RelationDec", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLrb ARGt)
))
 ARGt)
), (fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLstring ARGt)
))
 ARGt)
)))
 ARGt)
])

| PRINTLdecl (LRelationSigDec ARGt)  = (String.concat ["RelationSigDec", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLty ARGt)
), (fn ARGt =>
((PRINTLoption (fn ARGt =>
(PRINTLinout ARGt)
))
 ARGt)
)))
 ARGt)
))
 ARGt)
)))
 ARGt)
])

| PRINTLdecl (LDatatypeDec ARGt)  = (String.concat ["DatatypeDec", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLdb ARGt)
))
 ARGt)
), (fn ARGt =>
((PRINTLoption (fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLtb ARGt)
))
 ARGt)
))
 ARGt)
)))
 ARGt)
])

| PRINTLdecl (LTypeDec ARGt)  = (String.concat ["TypeDec", " ", ((PRINTLlist (fn ARGt =>
(PRINTLtb ARGt)
))
 ARGt)
])

| PRINTLdecl (LFunDec ARGt)  = (String.concat ["FunDec", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLfb ARGt)
))
 ARGt)
), (fn ARGt =>
((PRINTLlist (fn ARGt =>
(PRINTLstring ARGt)
))
 ARGt)
)))
 ARGt)
])

