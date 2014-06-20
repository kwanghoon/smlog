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


fun RELselect (ARG0 : Lint Llist, ARG1 : Lint, ARG2 : Lint Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lint (fn X : Lint =>
(SMLOG.EXISTS Llist (fn XS : Lint Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
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
)) ARG0 (Lopcoco (TUPLECON2 (X, XS))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG1 X ARGtc)
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
)) ARG2 XS ARGtc)
)))
))
))
))
, (SMLOG.EXISTS Lint (fn X : Lint =>
(SMLOG.EXISTS Llist (fn XS : Lint Llist =>
(SMLOG.EXISTS Lint (fn Z : Lint =>
(SMLOG.EXISTS Llist (fn ZS : Lint Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
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
)) ARG0 (Lopcoco (TUPLECON2 (X, XS))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG1 Z ARGtc)
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
)) ARG2 (Lopcoco (TUPLECON2 (X, ZS))
)
 ARGtc)
)))
, (RELselect (XS, Z, ZS))
))
))
))
))
))
))
)


fun select (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
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
, (PRINTLint ARG1)
, ((PRINTLlist (fn ARGt =>
(PRINTLint ARGt)
))
 ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELselect (ARG0, ARG1, ARG2) (fn _ =>
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
, ((fn (Option.SOME ARG1) =>
(EMBEDLint ARG1)
 | Option.NONE =>
(Lint ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
((EMBEDLlist ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)))
 ARG2)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
))


fun RELperm (ARG0 : Lint Llist, ARG1 : Lint Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
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
)) ARG0 (Lnil)
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
)) ARG1 (Lnil)
 ARGtc)
)))
))
, (SMLOG.EXISTS Llist (fn XS : Lint Llist =>
(SMLOG.EXISTS Lint (fn Y : Lint =>
(SMLOG.EXISTS Llist (fn YS : Lint Llist =>
(SMLOG.EXISTS Llist (fn ZS : Lint Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
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
)) ARG0 XS ARGtc)
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
)) ARG1 (Lopcoco (TUPLECON2 (Y, ZS))
)
 ARGtc)
)))
, (RELselect (XS, Y, YS))
))
, (RELperm (YS, ZS))
))
))
))
))
))
))
)


fun perm (ARG0, ARG1)  = ((fn (ARG0, ARG1) =>
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
, ((PRINTLlist (fn ARGt =>
(PRINTLint ARGt)
))
 ARG1)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELperm (ARG0, ARG1) (fn _ =>
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


type Map = {D:int, E:int, M:int, N:int, O:int, R:int, S:int, Y:int}

datatype ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) RECORDTYPEDEMNORSY = RECORDTYPEDEMNORSY of (Ostring * ('a0 , 'a1 , 'a2 , 'a3 , 'a4 , 'a5 , 'a6 , 'a7) RECORDTYPEDEMNORSY Option.option ref) | RECORDCONDEMNORSY of {D:'a0, E:'a1, M:'a2, N:'a3, O:'a4, R:'a5, S:'a6, Y:'a7}

fun EMBEDRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7) ARGt0  = ((fn {D = D, E = E, M = M, N = N, O = O, R = R, S = S, Y = Y} =>
(RECORDCONDEMNORSY {D = ((fn (ARG1, ARG2) =>
ARG1) qua0 D)
, E = ((fn (ARG1, ARG2) =>
ARG1) qua1 E)
, M = ((fn (ARG1, ARG2) =>
ARG1) qua2 M)
, N = ((fn (ARG1, ARG2) =>
ARG1) qua3 N)
, O = ((fn (ARG1, ARG2) =>
ARG1) qua4 O)
, R = ((fn (ARG1, ARG2) =>
ARG1) qua5 R)
, S = ((fn (ARG1, ARG2) =>
ARG1) qua6 S)
, Y = ((fn (ARG1, ARG2) =>
ARG1) qua7 Y)
})
) ARGt0)

and PROJRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7) (RECORDTYPEDEMNORSY (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7) (RECORDTYPEDEMNORSY (_, (ref (Option.SOME ARGt))))  = ((PROJRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7))
 ARGt)

| PROJRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7) (RECORDCONDEMNORSY ARGt0)  = ((fn {D = D, E = E, M = M, N = N, O = O, R = R, S = S, Y = Y} =>
{D = ((fn (ARG1, ARG2) =>
ARG2) qua0 D)
, E = ((fn (ARG1, ARG2) =>
ARG2) qua1 E)
, M = ((fn (ARG1, ARG2) =>
ARG2) qua2 M)
, N = ((fn (ARG1, ARG2) =>
ARG2) qua3 N)
, O = ((fn (ARG1, ARG2) =>
ARG2) qua4 O)
, R = ((fn (ARG1, ARG2) =>
ARG2) qua5 R)
, S = ((fn (ARG1, ARG2) =>
ARG2) qua6 S)
, Y = ((fn (ARG1, ARG2) =>
ARG2) qua7 Y)
}) ARGt0)


fun OCCURRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7) ARG0 (RECORDTYPEDEMNORSY (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7) ARG0 (RECORDTYPEDEMNORSY (_, (ref (Option.SOME ARG2))))  = ((OCCURRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7))
 ARG0 ARG2)

| OCCURRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7) ARG0 (RECORDCONDEMNORSY ARG1)  = ((fn ARGv =>
(fn {D = D, E = E, M = M, N = N, O = O, R = R, S = S, Y = Y} =>
((fn true =>
true | false =>
(qua7 ARGv Y)
) ((fn true =>
true | false =>
(qua6 ARGv S)
) ((fn true =>
true | false =>
(qua5 ARGv R)
) ((fn true =>
true | false =>
(qua4 ARGv O)
) ((fn true =>
true | false =>
(qua3 ARGv N)
) ((fn true =>
true | false =>
(qua2 ARGv M)
) ((fn true =>
true | false =>
(qua1 ARGv E)
) ((fn true =>
true | false =>
(qua0 ARGv D)
) false)
)
)
)
)
)
)
)
)) ARG0 ARG1)


fun UNIFYRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7) (ARGt1, (RECORDTYPEDEMNORSY (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7) (ARGt1, (RECORDTYPEDEMNORSY (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURRECORDTYPEDEMNORSY (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
, ((fn (ARG1, ARG2) =>
ARG2) qua2)
, ((fn (ARG1, ARG2) =>
ARG2) qua3)
, ((fn (ARG1, ARG2) =>
ARG2) qua4)
, ((fn (ARG1, ARG2) =>
ARG2) qua5)
, ((fn (ARG1, ARG2) =>
ARG2) qua6)
, ((fn (ARG1, ARG2) =>
ARG2) qua7)
))
 ARG0 ARGt1)
)

| UNIFYRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7) ((RECORDTYPEDEMNORSY (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7) ((ARGt1 as (RECORDTYPEDEMNORSY (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7))
 (ARGt2, ARGt1) ARGtc)

| UNIFYRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7) ((RECORDCONDEMNORSY ARGt1), (RECORDCONDEMNORSY ARGt2)) ARGtc  = ((fn ({D = D1, E = E1, M = M1, N = N1, O = O1, R = R1, S = S1, Y = Y1}, {D = D2, E = E2, M = M2, N = N2, O = O2, R = R2, S = S2, Y = Y2}) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua7)
 (Y1, Y2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua6)
 (S1, S2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua5)
 (R1, R2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua4)
 (O1, O2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua3)
 (N1, N2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua2)
 (M1, M2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (E1, E2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (D1, D2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
)
)
)
)
)
)
) (ARGt1, ARGt2))


fun PRINTRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7) (RECORDTYPEDEMNORSY (ARGs, (ref Option.NONE)))  = ARGs
| PRINTRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7) (RECORDTYPEDEMNORSY (_, (ref (Option.SOME ARGt))))  = ((PRINTRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7))
 ARGt)

| PRINTRECORDTYPEDEMNORSY (qua0, qua1, qua2, qua3, qua4, qua5, qua6, qua7) (RECORDCONDEMNORSY ARGt0)  = ((fn {D = D, E = E, M = M, N = N, O = O, R = R, S = S, Y = Y} =>
(String.concat ["{", (SMLOG.intersperse "," [(String.concat ["D", "=", (qua0 D)
])
, (String.concat ["E", "=", (qua1 E)
])
, (String.concat ["M", "=", (qua2 M)
])
, (String.concat ["N", "=", (qua3 N)
])
, (String.concat ["O", "=", (qua4 O)
])
, (String.concat ["R", "=", (qua5 R)
])
, (String.concat ["S", "=", (qua6 S)
])
, (String.concat ["Y", "=", (qua7 Y)
])
])
, "}"])
) ARGt0)


fun RELsolve (ARG0 : (Lint , Lint , Lint , Lint , Lint , Lint , Lint , Lint) RECORDTYPEDEMNORSY)  = (SMLOG.STEP (SMLOG.EXISTS Lint (fn A1 : Lint =>
(SMLOG.EXISTS Lint (fn A2 : Lint =>
(SMLOG.EXISTS Lint (fn A3 : Lint =>
(SMLOG.EXISTS Lint (fn A4 : Lint =>
(SMLOG.EXISTS Lint (fn D : Lint =>
(SMLOG.EXISTS Lint (fn E : Lint =>
(SMLOG.EXISTS Llist (fn LIST : Lint Llist =>
(SMLOG.EXISTS Lint (fn M : Lint =>
(SMLOG.EXISTS Lint (fn N : Lint =>
(SMLOG.EXISTS Lint (fn O : Lint =>
(SMLOG.EXISTS Lint (fn R : Lint =>
(SMLOG.EXISTS Lint (fn S : Lint =>
(SMLOG.EXISTS Lint (fn Y : Lint =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
((UNIFYRECORDTYPEDEMNORSY (((fn (ARGt1, ARGt2) =>
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
))), ((fn (ARGt1, ARGt2) =>
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
))), ((fn (ARGt1, ARGt2) =>
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
))), ((fn (ARGt1, ARGt2) =>
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
 (ARG0, ARG1) ARGtc)
)) ARG0 (RECORDCONDEMNORSY {D = D, E = E, M = M, N = N, O = O, R = R, S = S, Y = Y})
 ARGtc)
)))
, (RELperm ((Lopcoco (TUPLECON2 ((THEint 0)
, (Lopcoco (TUPLECON2 ((THEint 1)
, (Lopcoco (TUPLECON2 ((THEint 2)
, (Lopcoco (TUPLECON2 ((THEint 3)
, (Lopcoco (TUPLECON2 ((THEint 4)
, (Lopcoco (TUPLECON2 ((THEint 5)
, (Lopcoco (TUPLECON2 ((THEint 6)
, (Lopcoco (TUPLECON2 ((THEint 7)
, (Lopcoco (TUPLECON2 ((THEint 8)
, (Lopcoco (TUPLECON2 ((THEint 9)
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
))
)
))
)
))
)
))
)
, (Lopcoco (TUPLECON2 (S, (Lopcoco (TUPLECON2 (E, (Lopcoco (TUPLECON2 (N, (Lopcoco (TUPLECON2 (D, (Lopcoco (TUPLECON2 (M, (Lopcoco (TUPLECON2 (O, (Lopcoco (TUPLECON2 (R, (Lopcoco (TUPLECON2 (Y, LIST))
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
))
)
))
)
))
))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLbool (ARG0, ARG1) ARGtc)
)) ARG0 (Ltrue)
 ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLbool ARG0)
)) (nonzero ((SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
(PROJLint ARGt)
) S)
) ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLbool (ARG0, ARG1) ARGtc)
)) ARG0 (Ltrue)
 ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLbool ARG0)
)) (nonzero ((SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
(PROJLint ARGt)
) M)
) ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG0 A1 ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLint ARG0)
)) (toInt ((SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)))
 ARGt)
) (Lopcoco (TUPLECON2 (S, (Lopcoco (TUPLECON2 (E, (Lopcoco (TUPLECON2 (N, (Lopcoco (TUPLECON2 (D, (Lnil)
))
)
))
)
))
)
))
)
)
) ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG0 A2 ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLint ARG0)
)) (toInt ((SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)))
 ARGt)
) (Lopcoco (TUPLECON2 (M, (Lopcoco (TUPLECON2 (O, (Lopcoco (TUPLECON2 (R, (Lopcoco (TUPLECON2 (E, (Lnil)
))
)
))
)
))
)
))
)
)
) ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG0 A3 ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLint ARG0)
)) (toInt ((SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
((PROJLlist ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)))
 ARGt)
) (Lopcoco (TUPLECON2 (M, (Lopcoco (TUPLECON2 (O, (Lopcoco (TUPLECON2 (N, (Lopcoco (TUPLECON2 (E, (Lopcoco (TUPLECON2 (Y, (Lnil)
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
)
) ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG0 A4 ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLint ARG0)
)) (add ((SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
(PROJLint ARGt)
) A1)
) ARGtc)
, (SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
(PROJLint ARGt)
) A2)
) ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
, (fn ARGtc =>
(((((fn (ARG0) =>
(SMLOG.CONJ ((fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLbool (ARG0, ARG1) ARGtc)
)) ARG0 (Ltrue)
 ARGtc)
), SMLOG.TRUE))
) ((fn (ARG0) =>
((EMBEDLbool ARG0)
)) (eq ((SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
(PROJLint ARGt)
) A3)
) ARGtc)
, (SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
(PROJLint ARGt)
) A4)
) ARGtc)
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
))
))
))
)


fun solve (ARG0)  = ((fn (ARG0) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) (((PRINTRECORDTYPEDEMNORSY ((fn ARGt =>
(PRINTLint ARGt)
), (fn ARGt =>
(PRINTLint ARGt)
), (fn ARGt =>
(PRINTLint ARGt)
), (fn ARGt =>
(PRINTLint ARGt)
), (fn ARGt =>
(PRINTLint ARGt)
), (fn ARGt =>
(PRINTLint ARGt)
), (fn ARGt =>
(PRINTLint ARGt)
), (fn ARGt =>
(PRINTLint ARGt)
)))
 ARG0)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELsolve (ARG0) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDRECORDTYPEDEMNORSY (((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
)), ((fn ARGt =>
(EMBEDLint ARGt)
), (fn ARGt =>
(PROJLint ARGt)
))))
 ARG0)
 | Option.NONE =>
(RECORDTYPEDEMNORSY ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
))

