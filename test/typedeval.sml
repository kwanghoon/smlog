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


fun RELadd (ARG0 : Lnum, ARG1 : Lnum, ARG2 : Lnum)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lnum (fn Y : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LZero)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 Y ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG2 Y ARGtc)
)))
))
))
, (SMLOG.EXISTS Lnum (fn X : Lnum =>
(SMLOG.EXISTS Lnum (fn Y : Lnum =>
(SMLOG.EXISTS Lnum (fn Z : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc X)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 Y ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG2 (LSucc Z)
 ARGtc)
)))
, (RELadd (X, Y, Z))
))
))
))
))
))
)


fun add (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLnum ARG0)
, (PRINTLnum ARG1)
, (PRINTLnum ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELadd (ARG0, ARG1, ARG2) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLnum ARG0)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLnum ARG1)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
(EMBEDLnum ARG2)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
))


fun RELadd3 (ARG0 : Lnum, ARG1 : Lnum, ARG2 : Lnum, ARG3 : Lnum)  = (SMLOG.STEP (SMLOG.EXISTS Lnum (fn V : Lnum =>
(SMLOG.EXISTS Lnum (fn W : Lnum =>
(SMLOG.EXISTS Lnum (fn X : Lnum =>
(SMLOG.EXISTS Lnum (fn Y : Lnum =>
(SMLOG.EXISTS Lnum (fn Z : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 W ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 X ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG2 Y ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG3 Z ARGtc)
)))
, (RELadd (V, Y, Z))
))
, (RELadd (W, X, V))
))
))
))
))
))
))
)


fun add3 (ARG0, ARG1, ARG2, ARG3)  = ((fn (ARG0, ARG1, ARG2, ARG3) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLnum ARG0)
, (PRINTLnum ARG1)
, (PRINTLnum ARG2)
, (PRINTLnum ARG3)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELadd3 (ARG0, ARG1, ARG2, ARG3) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLnum ARG0)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLnum ARG1)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
(EMBEDLnum ARG2)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
, ((fn (Option.SOME ARG3) =>
(EMBEDLnum ARG3)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG3)
))


fun RELprnum (ARG0 : Lnum)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LZero)
 ARGtc)
)))
, (fn ARGtc =>
(((((fn () =>
SMLOG.TRUE) ((fn () =>
()) (print ((SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
(PROJLstring ARGt)
) (THEstring "*\n")
)
) ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
))
, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc D)
 ARGtc)
)))
, (fn ARGtc =>
(((((fn () =>
SMLOG.TRUE) ((fn () =>
()) (print ((SMLOG.doAppUnderSubst (fn _ =>
((fn ARGt =>
(PROJLstring ARGt)
) (THEstring "*")
)
) ARGtc)
))
)
)
)
 handle _ =>
SMLOG.FALSE) ARGtc)
)))
, (RELprnum (D))
))
))
))
)


fun prnum (ARG0)  = ((fn (ARG0) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLnum ARG0)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELprnum (ARG0) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLnum ARG0)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
))


fun RELmax (ARG0 : Lnum, ARG1 : Lnum, ARG2 : Lnum)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LZero)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 (LZero)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG2 (LZero)
 ARGtc)
)))
))
, (SMLOG.EXISTS Lnum (fn Y : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LZero)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 (LSucc Y)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG2 (LSucc Y)
 ARGtc)
)))
))
))
, (SMLOG.EXISTS Lnum (fn X : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc X)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 (LZero)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG2 (LSucc X)
 ARGtc)
)))
))
))
, (SMLOG.EXISTS Lnum (fn X : Lnum =>
(SMLOG.EXISTS Lnum (fn Y : Lnum =>
(SMLOG.EXISTS Lnum (fn Z : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc X)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 (LSucc Y)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG2 (LSucc Z)
 ARGtc)
)))
, (RELmax (X, Y, Z))
))
))
))
))
))
)


fun max (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLnum ARG0)
, (PRINTLnum ARG1)
, (PRINTLnum ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELmax (ARG0, ARG1, ARG2) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLnum ARG0)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLnum ARG1)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
(EMBEDLnum ARG2)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
))


fun RELle (ARG0 : Lnum, ARG1 : Lnum)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LZero)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 (LZero)
 ARGtc)
)))
))
, (SMLOG.EXISTS Lnum (fn X : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LZero)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 (LSucc X)
 ARGtc)
)))
))
))
, (SMLOG.EXISTS Lnum (fn X : Lnum =>
(SMLOG.EXISTS Lnum (fn Y : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc X)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 (LSucc Y)
 ARGtc)
)))
, (RELle (X, Y))
))
))
))
))
)


fun le (ARG0, ARG1)  = ((fn (ARG0, ARG1) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLnum ARG0)
, (PRINTLnum ARG1)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELle (ARG0, ARG1) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLnum ARG0)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLnum ARG1)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
))


type id = string

datatype ty = TyFun of (ty * ty) | TyUnit | TyTup of (ty * ty) | TyList of ty | TyExn

datatype Oty = datatype ty

datatype Lty = Lty of (Ostring * Lty Option.option ref) | LTyFun of (Lty , Lty) TUPLETYPE2 | LTyUnit | LTyTup of (Lty , Lty) TUPLETYPE2 | LTyList of Lty | LTyExn

fun EMBEDLty (TyFun ARGt)  = (LTyFun ((EMBEDTUPLETYPE2 (((fn ARGt =>
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

| EMBEDLty TyUnit  = LTyUnit
| EMBEDLty (TyTup ARGt)  = (LTyTup ((EMBEDTUPLETYPE2 (((fn ARGt =>
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

| EMBEDLty (TyList ARGt)  = (LTyList (EMBEDLty ARGt)
)

| EMBEDLty TyExn  = LTyExn
and PROJLty (Lty (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLty (Lty (_, (ref (Option.SOME ARGt))))  = (PROJLty ARGt)

| PROJLty (LTyFun ARGt)  = (TyFun ((PROJTUPLETYPE2 (((fn ARGt =>
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

| PROJLty LTyUnit  = TyUnit
| PROJLty (LTyTup ARGt)  = (TyTup ((PROJTUPLETYPE2 (((fn ARGt =>
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

| PROJLty (LTyList ARGt)  = (TyList (PROJLty ARGt)
)

| PROJLty LTyExn  = TyExn

fun OCCURLty ARG0 (Lty (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLty ARG0 (Lty (_, (ref (Option.SOME ARG2))))  = (OCCURLty ARG0 ARG2)

| OCCURLty ARG0 (LTyFun ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLty ARG0 LTyUnit  = false
| OCCURLty ARG0 (LTyTup ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLty ARG0 (LTyList ARG1)  = (OCCURLty ARG0 ARG1)

| OCCURLty ARG0 LTyExn  = false

fun UNIFYLty (ARGt1, (Lty (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLty (ARGt1, ARGt2) ARGtc)

| UNIFYLty (ARGt1, (Lty (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLty ARG0 ARGt1)
)

| UNIFYLty ((Lty (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLty (ARGt1, ARGt2) ARGtc)

| UNIFYLty ((ARGt1 as (Lty (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLty (ARGt2, ARGt1) ARGtc)

| UNIFYLty ((LTyFun ARGt1), (LTyFun ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
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

| UNIFYLty (LTyUnit, LTyUnit) ARGtc  = (true, ARGtc)
| UNIFYLty ((LTyTup ARGt1), (LTyTup ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
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

| UNIFYLty ((LTyList ARGt1), (LTyList ARGt2)) ARGtc  = (UNIFYLty (ARGt1, ARGt2) ARGtc)

| UNIFYLty (LTyExn, LTyExn) ARGtc  = (true, ARGtc)
| UNIFYLty (_, _) ARGtc  = (false, ARGtc)

fun PRINTLty (Lty (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLty (Lty (_, (ref (Option.SOME ARGt))))  = (PRINTLty ARGt)

| PRINTLty (LTyFun ARGt)  = (String.concat ["TyFun", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLty ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
])

| PRINTLty LTyUnit  = "TyUnit"
| PRINTLty (LTyTup ARGt)  = (String.concat ["TyTup", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLty ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
])

| PRINTLty (LTyList ARGt)  = (String.concat ["TyList", " ", (PRINTLty ARGt)
])

| PRINTLty LTyExn  = "TyExn"

type tyenv = (id * ty) list

datatype ('a0, 'a1, 'a2) TUPLETYPE3 = TUPLETYPE3 of (Ostring * ('a0 , 'a1 , 'a2) TUPLETYPE3 Option.option ref) | TUPLECON3 of ('a0 * 'a1 * 'a2)
and ('a0, 'a1, 'a2, 'a3) TUPLETYPE4 = TUPLETYPE4 of (Ostring * ('a0 , 'a1 , 'a2 , 'a3) TUPLETYPE4 Option.option ref) | TUPLECON4 of ('a0 * 'a1 * 'a2 * 'a3)

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
and EMBEDTUPLETYPE4 (qua0, qua1, qua2, qua3) (ARGt0, ARGt1, ARGt2, ARGt3)  = (TUPLECON4 (((fn (ARG1, ARG2) =>
ARG1) qua0 ARGt0)
, ((fn (ARG1, ARG2) =>
ARG1) qua1 ARGt1)
, ((fn (ARG1, ARG2) =>
ARG1) qua2 ARGt2)
, ((fn (ARG1, ARG2) =>
ARG1) qua3 ARGt3)
))

and PROJTUPLETYPE4 (qua0, qua1, qua2, qua3) (TUPLETYPE4 (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJTUPLETYPE4 (qua0, qua1, qua2, qua3) (TUPLETYPE4 (_, (ref (Option.SOME ARGt))))  = ((PROJTUPLETYPE4 (qua0, qua1, qua2, qua3))
 ARGt)

| PROJTUPLETYPE4 (qua0, qua1, qua2, qua3) (TUPLECON4 (ARGt0, ARGt1, ARGt2, ARGt3))  = (((fn (ARG1, ARG2) =>
ARG2) qua0 ARGt0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1 ARGt1)
, ((fn (ARG1, ARG2) =>
ARG2) qua2 ARGt2)
, ((fn (ARG1, ARG2) =>
ARG2) qua3 ARGt3)
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

and OCCURTUPLETYPE4 (qua0, qua1, qua2, qua3) ARG0 (TUPLETYPE4 (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

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

and UNIFYTUPLETYPE4 (qua0, qua1, qua2, qua3) (ARGt1, (TUPLETYPE4 (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYTUPLETYPE4 (qua0, qua1, qua2, qua3))
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


fun PRINTTUPLETYPE3 (qua0, qua1, qua2) (TUPLETYPE3 (ARGs, (ref Option.NONE)))  = ARGs
| PRINTTUPLETYPE3 (qua0, qua1, qua2) (TUPLETYPE3 (_, (ref (Option.SOME ARGt))))  = ((PRINTTUPLETYPE3 (qua0, qua1, qua2))
 ARGt)

| PRINTTUPLETYPE3 (qua0, qua1, qua2) (TUPLECON3 (ARGt0, ARGt1, ARGt2))  = (String.concat ["(", (SMLOG.intersperse "," [(qua0 ARGt0)
, (qua1 ARGt1)
, (qua2 ARGt2)
])
, ")"])

and PRINTTUPLETYPE4 (qua0, qua1, qua2, qua3) (TUPLETYPE4 (ARGs, (ref Option.NONE)))  = ARGs
| PRINTTUPLETYPE4 (qua0, qua1, qua2, qua3) (TUPLETYPE4 (_, (ref (Option.SOME ARGt))))  = ((PRINTTUPLETYPE4 (qua0, qua1, qua2, qua3))
 ARGt)

| PRINTTUPLETYPE4 (qua0, qua1, qua2, qua3) (TUPLECON4 (ARGt0, ARGt1, ARGt2, ARGt3))  = (String.concat ["(", (SMLOG.intersperse "," [(qua0 ARGt0)
, (qua1 ARGt1)
, (qua2 ARGt2)
, (qua3 ARGt3)
])
, ")"])


datatype exp = Var of id | Fix of (id * ty * id * exp) | App of (exp * exp) | Unit | Tup of (exp * exp) | Nil | Cons of (exp * exp) | CaseList of (exp * exp * (id * id * ty * exp)) | Exn | Raise of exp | Handle of (exp * exp)

datatype Oexp = datatype exp

datatype Lexp = Lexp of (Ostring * Lexp Option.option ref) | LVar of Lstring | LFix of (Lstring , Lty , Lstring , Lexp) TUPLETYPE4 | LApp of (Lexp , Lexp) TUPLETYPE2 | LUnit | LTup of (Lexp , Lexp) TUPLETYPE2 | LNil | LCons of (Lexp , Lexp) TUPLETYPE2 | LCaseList of (Lexp , Lexp , (Lstring , Lstring , Lty , Lexp) TUPLETYPE4) TUPLETYPE3 | LExn | LRaise of Lexp | LHandle of (Lexp , Lexp) TUPLETYPE2

fun EMBEDLexp (Var ARGt)  = (LVar (EMBEDLstring ARGt)
)

| EMBEDLexp (Fix ARGt)  = (LFix ((EMBEDTUPLETYPE4 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
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

| EMBEDLexp (App ARGt)  = (LApp ((EMBEDTUPLETYPE2 (((fn ARGt =>
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

| EMBEDLexp Unit  = LUnit
| EMBEDLexp (Tup ARGt)  = (LTup ((EMBEDTUPLETYPE2 (((fn ARGt =>
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

| EMBEDLexp Nil  = LNil
| EMBEDLexp (Cons ARGt)  = (LCons ((EMBEDTUPLETYPE2 (((fn ARGt =>
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

| EMBEDLexp (CaseList ARGt)  = (LCaseList ((EMBEDTUPLETYPE3 (((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
((EMBEDTUPLETYPE4 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE4 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
))))
 ARGt)
))))
 ARGt)
)

| EMBEDLexp Exn  = LExn
| EMBEDLexp (Raise ARGt)  = (LRaise (EMBEDLexp ARGt)
)

| EMBEDLexp (Handle ARGt)  = (LHandle ((EMBEDTUPLETYPE2 (((fn ARGt =>
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

and PROJLexp (Lexp (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLexp (Lexp (_, (ref (Option.SOME ARGt))))  = (PROJLexp ARGt)

| PROJLexp (LVar ARGt)  = (Var (PROJLstring ARGt)
)

| PROJLexp (LFix ARGt)  = (Fix ((PROJTUPLETYPE4 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
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

| PROJLexp (LApp ARGt)  = (App ((PROJTUPLETYPE2 (((fn ARGt =>
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

| PROJLexp LUnit  = Unit
| PROJLexp (LTup ARGt)  = (Tup ((PROJTUPLETYPE2 (((fn ARGt =>
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

| PROJLexp LNil  = Nil
| PROJLexp (LCons ARGt)  = (Cons ((PROJTUPLETYPE2 (((fn ARGt =>
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

| PROJLexp (LCaseList ARGt)  = (CaseList ((PROJTUPLETYPE3 (((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
)), ((fn ARGt =>
((EMBEDTUPLETYPE4 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE4 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
)), ((fn ARGt =>
(EMBEDLexp ARGt)
), (fn ARGt =>
(PROJLexp ARGt)
))))
 ARGt)
))))
 ARGt)
)

| PROJLexp LExn  = Exn
| PROJLexp (LRaise ARGt)  = (Raise (PROJLexp ARGt)
)

| PROJLexp (LHandle ARGt)  = (Handle ((PROJTUPLETYPE2 (((fn ARGt =>
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


fun OCCURLexp ARG0 (Lexp (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLexp ARG0 (Lexp (_, (ref (Option.SOME ARG2))))  = (OCCURLexp ARG0 ARG2)

| OCCURLexp ARG0 (LVar ARG1)  = (OCCURLstring ARG0 ARG1)

| OCCURLexp ARG0 (LFix ARG1)  = ((OCCURTUPLETYPE4 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLexp ARG0 (LApp ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLexp ARG0 LUnit  = false
| OCCURLexp ARG0 (LTup ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLexp ARG0 LNil  = false
| OCCURLexp ARG0 (LCons ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLexp ARG0 (LCaseList ARG1)  = ((OCCURTUPLETYPE3 ((fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE4 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLexp ARG0 LExn  = false
| OCCURLexp ARG0 (LRaise ARG1)  = (OCCURLexp ARG0 ARG1)

| OCCURLexp ARG0 (LHandle ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
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

| UNIFYLexp ((LVar ARGt1), (LVar ARGt2)) ARGtc  = (UNIFYLstring (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LFix ARGt1), (LFix ARGt2)) ARGtc  = ((UNIFYTUPLETYPE4 (((fn (ARGt1, ARGt2) =>
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

| UNIFYLexp ((LApp ARGt1), (LApp ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
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

| UNIFYLexp (LUnit, LUnit) ARGtc  = (true, ARGtc)
| UNIFYLexp ((LTup ARGt1), (LTup ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
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

| UNIFYLexp (LNil, LNil) ARGtc  = (true, ARGtc)
| UNIFYLexp ((LCons ARGt1), (LCons ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
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

| UNIFYLexp ((LCaseList ARGt1), (LCaseList ARGt2)) ARGtc  = ((UNIFYTUPLETYPE3 (((fn (ARGt1, ARGt2) =>
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
((UNIFYTUPLETYPE4 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLstring (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
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
(UNIFYLexp (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE4 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLexp (LExn, LExn) ARGtc  = (true, ARGtc)
| UNIFYLexp ((LRaise ARGt1), (LRaise ARGt2)) ARGtc  = (UNIFYLexp (ARGt1, ARGt2) ARGtc)

| UNIFYLexp ((LHandle ARGt1), (LHandle ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
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

| UNIFYLexp (_, _) ARGtc  = (false, ARGtc)

fun PRINTLexp (Lexp (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLexp (Lexp (_, (ref (Option.SOME ARGt))))  = (PRINTLexp ARGt)

| PRINTLexp (LVar ARGt)  = (String.concat ["Var", " ", (PRINTLstring ARGt)
])

| PRINTLexp (LFix ARGt)  = (String.concat ["Fix", " ", ((PRINTTUPLETYPE4 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
), (fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLexp ARGt)
)))
 ARGt)
])

| PRINTLexp (LApp ARGt)  = (String.concat ["App", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLexp ARGt)
), (fn ARGt =>
(PRINTLexp ARGt)
)))
 ARGt)
])

| PRINTLexp LUnit  = "Unit"
| PRINTLexp (LTup ARGt)  = (String.concat ["Tup", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLexp ARGt)
), (fn ARGt =>
(PRINTLexp ARGt)
)))
 ARGt)
])

| PRINTLexp LNil  = "Nil"
| PRINTLexp (LCons ARGt)  = (String.concat ["Cons", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLexp ARGt)
), (fn ARGt =>
(PRINTLexp ARGt)
)))
 ARGt)
])

| PRINTLexp (LCaseList ARGt)  = (String.concat ["CaseList", " ", ((PRINTTUPLETYPE3 ((fn ARGt =>
(PRINTLexp ARGt)
), (fn ARGt =>
(PRINTLexp ARGt)
), (fn ARGt =>
((PRINTTUPLETYPE4 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
), (fn ARGt =>
(PRINTLexp ARGt)
)))
 ARGt)
)))
 ARGt)
])

| PRINTLexp LExn  = "Exn"
| PRINTLexp (LRaise ARGt)  = (String.concat ["Raise", " ", (PRINTLexp ARGt)
])

| PRINTLexp (LHandle ARGt)  = (String.concat ["Handle", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLexp ARGt)
), (fn ARGt =>
(PRINTLexp ARGt)
)))
 ARGt)
])


datatype value = ValClo of ((id * value) list * tyenv * (id * ty * id * exp)) | ValUnit | ValTup of (value * value) | ValNil | ValCons of (value * value) | ValExn

datatype Ovalue = datatype value

datatype Lvalue = Lvalue of (Ostring * Lvalue Option.option ref) | LValClo of ((Lstring , Lvalue) TUPLETYPE2 Llist , (Lstring , Lty) TUPLETYPE2 Llist , (Lstring , Lty , Lstring , Lexp) TUPLETYPE4) TUPLETYPE3 | LValUnit | LValTup of (Lvalue , Lvalue) TUPLETYPE2 | LValNil | LValCons of (Lvalue , Lvalue) TUPLETYPE2 | LValExn

fun EMBEDLvalue (ValClo ARGt)  = (LValClo ((EMBEDTUPLETYPE3 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
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
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
)))
 ARGt)
)), ((fn ARGt =>
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
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
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
)), ((fn ARGt =>
((EMBEDTUPLETYPE4 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
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
), (fn ARGt =>
((PROJTUPLETYPE4 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
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
))))
 ARGt)
)

| EMBEDLvalue ValUnit  = LValUnit
| EMBEDLvalue (ValTup ARGt)  = (LValTup ((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
)

| EMBEDLvalue ValNil  = LValNil
| EMBEDLvalue (ValCons ARGt)  = (LValCons ((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
)

| EMBEDLvalue ValExn  = LValExn
and PROJLvalue (Lvalue (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLvalue (Lvalue (_, (ref (Option.SOME ARGt))))  = (PROJLvalue ARGt)

| PROJLvalue (LValClo ARGt)  = (ValClo ((PROJTUPLETYPE3 (((fn ARGt =>
((EMBEDLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
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
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
)))
 ARGt)
)), ((fn ARGt =>
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
 ARGt)
), (fn ARGt =>
((PROJLlist ((fn ARGt =>
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
)), ((fn ARGt =>
((EMBEDTUPLETYPE4 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
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
), (fn ARGt =>
((PROJTUPLETYPE4 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLty ARGt)
), (fn ARGt =>
(PROJLty ARGt)
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
))))
 ARGt)
)

| PROJLvalue LValUnit  = ValUnit
| PROJLvalue (LValTup ARGt)  = (ValTup ((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
)

| PROJLvalue LValNil  = ValNil
| PROJLvalue (LValCons ARGt)  = (ValCons ((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
)

| PROJLvalue LValExn  = ValExn

fun OCCURLvalue ARG0 (Lvalue (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLvalue ARG0 (Lvalue (_, (ref (Option.SOME ARG2))))  = (OCCURLvalue ARG0 ARG2)

| OCCURLvalue ARG0 (LValClo ARG1)  = ((OCCURTUPLETYPE3 ((fn ARGv =>
(fn ARGt =>
((OCCURLlist (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
)))
 ARGv ARGt)
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
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE4 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLvalue ARG0 LValUnit  = false
| OCCURLvalue ARG0 (LValTup ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLvalue ARG0 LValNil  = false
| OCCURLvalue ARG0 (LValCons ARG1)  = ((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARG0 ARG1)

| OCCURLvalue ARG0 LValExn  = false

fun UNIFYLvalue (ARGt1, (Lvalue (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLvalue (ARGt1, ARGt2) ARGtc)

| UNIFYLvalue (ARGt1, (Lvalue (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLvalue ARG0 ARGt1)
)

| UNIFYLvalue ((Lvalue (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLvalue (ARGt1, ARGt2) ARGtc)

| UNIFYLvalue ((ARGt1 as (Lvalue (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLvalue (ARGt2, ARGt1) ARGtc)

| UNIFYLvalue ((LValClo ARGt1), (LValClo ARGt2)) ARGtc  = ((UNIFYTUPLETYPE3 (((fn (ARGt1, ARGt2) =>
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
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
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
)))
 ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
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
((UNIFYTUPLETYPE4 (((fn (ARGt1, ARGt2) =>
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
((OCCURTUPLETYPE4 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLty ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLexp ARGv ARGt)
))))
 ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLvalue (LValUnit, LValUnit) ARGtc  = (true, ARGtc)
| UNIFYLvalue ((LValTup ARGt1), (LValTup ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLvalue (LValNil, LValNil) ARGtc  = (true, ARGtc)
| UNIFYLvalue ((LValCons ARGt1), (LValCons ARGt2)) ARGtc  = ((UNIFYTUPLETYPE2 (((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))), ((fn (ARGt1, ARGt2) =>
(fn ARGtc =>
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)

| UNIFYLvalue (LValExn, LValExn) ARGtc  = (true, ARGtc)
| UNIFYLvalue (_, _) ARGtc  = (false, ARGtc)

fun PRINTLvalue (Lvalue (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLvalue (Lvalue (_, (ref (Option.SOME ARGt))))  = (PRINTLvalue ARGt)

| PRINTLvalue (LValClo ARGt)  = (String.concat ["ValClo", " ", ((PRINTTUPLETYPE3 ((fn ARGt =>
((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLvalue ARGt)
)))
 ARGt)
))
 ARGt)
), (fn ARGt =>
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
((PRINTTUPLETYPE4 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
), (fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLexp ARGt)
)))
 ARGt)
)))
 ARGt)
])

| PRINTLvalue LValUnit  = "ValUnit"
| PRINTLvalue (LValTup ARGt)  = (String.concat ["ValTup", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLvalue ARGt)
), (fn ARGt =>
(PRINTLvalue ARGt)
)))
 ARGt)
])

| PRINTLvalue LValNil  = "ValNil"
| PRINTLvalue (LValCons ARGt)  = (String.concat ["ValCons", " ", ((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLvalue ARGt)
), (fn ARGt =>
(PRINTLvalue ARGt)
)))
 ARGt)
])

| PRINTLvalue LValExn  = "ValExn"

type env = (id * value) list

fun RELlookupEnv (ARG0 : Lstring, ARG1 : (Lstring , Lvalue) TUPLETYPE2 Llist, ARG2 : Lvalue)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lvalue (fn V : Lvalue =>
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 ((TUPLECON2 (X, V))
, ENV))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG2 V ARGtc)
)))
))
))
))
))
, (SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lvalue (fn V : Lvalue =>
(SMLOG.EXISTS Lvalue (fn W : Lvalue =>
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 ((TUPLECON2 (Y, W))
, ENV))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG2 V ARGtc)
)))
, (RELlookupEnv (X, ENV, V))
))
))
))
))
))
))
))
)


fun lookupEnv (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
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
(PRINTLvalue ARGt)
)))
 ARGt)
))
 ARG1)
, (PRINTLvalue ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELlookupEnv (ARG0, ARG1, ARG2) (fn _ =>
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
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
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
(EMBEDLvalue ARG2)
 | Option.NONE =>
(Lvalue ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
))


fun RELlookupTyEnv (ARG0 : Lstring, ARG1 : (Lstring , Lty) TUPLETYPE2 Llist, ARG2 : Lty)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Llist (fn ENV : (Lstring , Lty) TUPLETYPE2 Llist =>
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
, ENV))
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
, (SMLOG.EXISTS Llist (fn ENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lty (fn S : Lty =>
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
)) ARG1 (Lopcoco (TUPLECON2 ((TUPLECON2 (Y, S))
, ENV))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG2 T ARGtc)
)))
, (RELlookupTyEnv (X, ENV, T))
))
))
))
))
))
))
))
)


fun lookupTyEnv (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
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
 (RELlookupTyEnv (ARG0, ARG1, ARG2) (fn _ =>
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


type size = num

type height = num

fun RELtypExp (ARG0 : Lnum, ARG1 : (Lstring , Lty) TUPLETYPE2 Llist, ARG2 : Lexp, ARG3 : Lty)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LZero)
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
)) ARG1 TENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LVar X)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 T ARGtc)
)))
, (RELlookupTyEnv (X, TENV, T))
))
))
))
))
))
, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Lstring (fn F : Lstring =>
(SMLOG.EXISTS Lty (fn T1 : Lty =>
(SMLOG.EXISTS Lty (fn T2 : Lty =>
(SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc D)
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
)) ARG1 TENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LFix (TUPLECON4 (F, (LTyFun (TUPLECON2 (T1, T2))
)
, X, E))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 (LTyFun (TUPLECON2 (T1, T2))
)
 ARGtc)
)))
, (RELtypExp (D, (Lopcoco (TUPLECON2 ((TUPLECON2 (X, T1))
, (Lopcoco (TUPLECON2 ((TUPLECON2 (F, (LTyFun (TUPLECON2 (T1, T2))
)
))
, TENV))
)
))
)
, E, T2))
))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lnum (fn D1 : Lnum =>
(SMLOG.EXISTS Lnum (fn D2 : Lnum =>
(SMLOG.EXISTS Lexp (fn E1 : Lexp =>
(SMLOG.EXISTS Lexp (fn E2 : Lexp =>
(SMLOG.EXISTS Lty (fn T1 : Lty =>
(SMLOG.EXISTS Lty (fn T2 : Lty =>
(SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc D)
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
)) ARG1 TENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LApp (TUPLECON2 (E1, E2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 T2 ARGtc)
)))
, (RELmax (D1, D2, D))
))
, (RELtypExp (D1, TENV, E1, (LTyFun (TUPLECON2 (T1, T2))
)
))
))
, (RELtypExp (D2, TENV, E2, T1))
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
, (SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LZero)
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
)) ARG1 TENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LUnit)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 (LTyUnit)
 ARGtc)
)))
))
))
, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lnum (fn D1 : Lnum =>
(SMLOG.EXISTS Lnum (fn D2 : Lnum =>
(SMLOG.EXISTS Lexp (fn E1 : Lexp =>
(SMLOG.EXISTS Lexp (fn E2 : Lexp =>
(SMLOG.EXISTS Lty (fn T1 : Lty =>
(SMLOG.EXISTS Lty (fn T2 : Lty =>
(SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc D)
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
)) ARG1 TENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LTup (TUPLECON2 (E1, E2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 (LTyTup (TUPLECON2 (T1, T2))
)
 ARGtc)
)))
, (RELmax (D1, D2, D))
))
, (RELtypExp (D1, TENV, E1, T1))
))
, (RELtypExp (D2, TENV, E2, T2))
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
, (SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LZero)
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
)) ARG1 TENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LNil)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 (LTyList T)
 ARGtc)
)))
))
))
))
, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lnum (fn D1 : Lnum =>
(SMLOG.EXISTS Lnum (fn D2 : Lnum =>
(SMLOG.EXISTS Lexp (fn E1 : Lexp =>
(SMLOG.EXISTS Lexp (fn E2 : Lexp =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc D)
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
)) ARG1 TENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LCons (TUPLECON2 (E1, E2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 (LTyList T)
 ARGtc)
)))
, (RELmax (D1, D2, D))
))
, (RELtypExp (D1, TENV, E1, T))
))
, (RELtypExp (D2, TENV, E2, (LTyList T)
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
, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lnum (fn D1 : Lnum =>
(SMLOG.EXISTS Lnum (fn D2 : Lnum =>
(SMLOG.EXISTS Lnum (fn D3 : Lnum =>
(SMLOG.EXISTS Lnum (fn D4 : Lnum =>
(SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Lexp (fn E1 : Lexp =>
(SMLOG.EXISTS Lexp (fn E2 : Lexp =>
(SMLOG.EXISTS Lty (fn T1 : Lty =>
(SMLOG.EXISTS Lty (fn T2 : Lty =>
(SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.EXISTS Lstring (fn Y : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc D)
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
)) ARG1 TENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LCaseList (TUPLECON3 (E, E1, (TUPLECON4 (X, Y, (LTyList T1)
, E2))
))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 T2 ARGtc)
)))
, (RELmax (D1, D2, D4))
))
, (RELmax (D3, D4, D))
))
, (RELtypExp (D1, TENV, E, (LTyList T1)
))
))
, (RELtypExp (D2, TENV, E1, T2))
))
, (RELtypExp (D3, (Lopcoco (TUPLECON2 ((TUPLECON2 (Y, (LTyList T1)
))
, (Lopcoco (TUPLECON2 ((TUPLECON2 (X, T1))
, TENV))
)
))
)
, E2, T2))
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
, (SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LZero)
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
)) ARG1 TENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LExn)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 (LTyExn)
 ARGtc)
)))
))
))
, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc D)
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
)) ARG1 TENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LRaise E)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 T ARGtc)
)))
, (RELtypExp (D, TENV, E, (LTyExn)
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lnum (fn D1 : Lnum =>
(SMLOG.EXISTS Lnum (fn D2 : Lnum =>
(SMLOG.EXISTS Lexp (fn E1 : Lexp =>
(SMLOG.EXISTS Lexp (fn E2 : Lexp =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc D)
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
)) ARG1 TENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LHandle (TUPLECON2 (E1, E2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 T ARGtc)
)))
, (RELmax (D1, D2, D))
))
, (RELtypExp (D1, TENV, E1, T))
))
, (RELtypExp (D2, TENV, E2, T))
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

and RELtypVal (ARG0 : Lnum, ARG1 : Lvalue, ARG2 : Lty)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lnum (fn D1 : Lnum =>
(SMLOG.EXISTS Lnum (fn D2 : Lnum =>
(SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lstring (fn F : Lstring =>
(SMLOG.EXISTS Lty (fn T1 : Lty =>
(SMLOG.EXISTS Lty (fn T2 : Lty =>
(SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc D)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG1 (LValClo (TUPLECON3 (ENV, TENV, (TUPLECON4 (F, (LTyFun (TUPLECON2 (T1, T2))
)
, X, E))
))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG2 (LTyFun (TUPLECON2 (T1, T2))
)
 ARGtc)
)))
, (RELmax (D1, D2, D))
))
, (RELtypEnv (D1, ENV, TENV))
))
, (RELtypExp (D2, (Lopcoco (TUPLECON2 ((TUPLECON2 (X, T1))
, (Lopcoco (TUPLECON2 ((TUPLECON2 (F, (LTyFun (TUPLECON2 (T1, T2))
)
))
, TENV))
)
))
)
, E, T2))
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
, (SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LZero)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG1 (LValUnit)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG2 (LTyUnit)
 ARGtc)
)))
))
, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lnum (fn D1 : Lnum =>
(SMLOG.EXISTS Lnum (fn D2 : Lnum =>
(SMLOG.EXISTS Lty (fn T1 : Lty =>
(SMLOG.EXISTS Lty (fn T2 : Lty =>
(SMLOG.EXISTS Lvalue (fn V1 : Lvalue =>
(SMLOG.EXISTS Lvalue (fn V2 : Lvalue =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc D)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG1 (LValTup (TUPLECON2 (V1, V2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG2 (LTyTup (TUPLECON2 (T1, T2))
)
 ARGtc)
)))
, (RELmax (D1, D2, D))
))
, (RELtypVal (D1, V1, T1))
))
, (RELtypVal (D2, V2, T2))
))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LZero)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG1 (LValNil)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG2 (LTyList T)
 ARGtc)
)))
))
))
, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lnum (fn D1 : Lnum =>
(SMLOG.EXISTS Lnum (fn D2 : Lnum =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Lvalue (fn V1 : Lvalue =>
(SMLOG.EXISTS Lvalue (fn V2 : Lvalue =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc D)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG1 (LValCons (TUPLECON2 (V1, V2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG2 (LTyList T)
 ARGtc)
)))
, (RELmax (D1, D2, D))
))
, (RELtypVal (D1, V1, T))
))
, (RELtypVal (D2, V2, (LTyList T)
))
))
))
))
))
))
))
))
))
, (SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LZero)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG1 (LValExn)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG2 (LTyExn)
 ARGtc)
)))
))
)

and RELtypEnv (ARG0 : Lnum, ARG1 : (Lstring , Lvalue) TUPLETYPE2 Llist, ARG2 : (Lstring , Lty) TUPLETYPE2 Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LZero)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
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
)) ARG2 (Lnil)
 ARGtc)
)))
))
, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lnum (fn D1 : Lnum =>
(SMLOG.EXISTS Lnum (fn D2 : Lnum =>
(SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lvalue (fn V : Lvalue =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc D)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 ((TUPLECON2 (X, V))
, ENV))
)
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
)) ARG2 (Lopcoco (TUPLECON2 ((TUPLECON2 (X, T))
, TENV))
)
 ARGtc)
)))
, (RELmax (D1, D2, D))
))
, (RELtypVal (D1, V, T))
))
, (RELtypEnv (D2, ENV, TENV))
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


fun typExp (ARG0, ARG1, ARG2, ARG3)  = ((fn (ARG0, ARG1, ARG2, ARG3) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLnum ARG0)
, ((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
))
 ARG1)
, (PRINTLexp ARG2)
, (PRINTLty ARG3)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELtypExp (ARG0, ARG1, ARG2, ARG3) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLnum ARG0)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
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
(EMBEDLexp ARG2)
 | Option.NONE =>
(Lexp ((SMLOG.freshString ())
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

and typVal (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLnum ARG0)
, (PRINTLvalue ARG1)
, (PRINTLty ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELtypVal (ARG0, ARG1, ARG2) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLnum ARG0)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLvalue ARG1)
 | Option.NONE =>
(Lvalue ((SMLOG.freshString ())
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

and typEnv (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLnum ARG0)
, ((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLvalue ARGt)
)))
 ARGt)
))
 ARG1)
, ((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
))
 ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELtypEnv (ARG0, ARG1, ARG2) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLnum ARG0)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
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
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
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
 ARG2)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
))


fun RELgenEnv (ARG0 : Lnum, ARG1 : (Lstring , Lvalue) TUPLETYPE2 Llist, ARG2 : (Lstring , Lty) TUPLETYPE2 Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lnum (fn S : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 S ARGtc)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
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
)) ARG2 (Lnil)
 ARGtc)
)))
))
))
, (SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lnum (fn S : Lnum =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lvalue (fn V : Lvalue =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 S ARGtc)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 (Lopcoco (TUPLECON2 ((TUPLECON2 (X, V))
, ENV))
)
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
)) ARG2 (Lopcoco (TUPLECON2 ((TUPLECON2 (X, T))
, TENV))
)
 ARGtc)
)))
, (RELenumTypVal (S, (LZero)
, V, T))
))
, (RELgenEnv (S, ENV, TENV))
))
))
))
))
))
))
))
))
)

and RELenumTypVal (ARG0 : Lnum, ARG1 : Lnum, ARG2 : Lvalue, ARG3 : Lty)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lnum (fn S : Lnum =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Lvalue (fn V : Lvalue =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 S ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 D ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG2 V ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 T ARGtc)
)))
, (RELmaxsize (T, D))
))
, (RELle (D, S))
))
, (RELtypVal (D, V, T))
))
))
))
))
))
))
, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lnum (fn S : Lnum =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Lvalue (fn V : Lvalue =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 S ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 D ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG2 V ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG3 T ARGtc)
)))
, (RELmaxsize (T, D))
))
, (RELle (D, S))
))
, (RELenumTypVal (S, (LSucc D)
, V, T))
))
))
))
))
))
))
)

and RELmaxsize (ARG0 : Lty, ARG1 : Lnum)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lty (fn T1 : Lty =>
(SMLOG.EXISTS Lty (fn T2 : Lty =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG0 (LTyFun (TUPLECON2 (T1, T2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 D ARGtc)
)))
))
))
))
))
, (SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG0 (LTyUnit)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 (LZero)
 ARGtc)
)))
))
, (SMLOG.EXISTS Lty (fn T1 : Lty =>
(SMLOG.EXISTS Lty (fn T2 : Lty =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG0 (LTyTup (TUPLECON2 (T1, T2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 (LZero)
 ARGtc)
)))
))
))
))
, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lnum (fn D1 : Lnum =>
(SMLOG.EXISTS Lnum (fn D2 : Lnum =>
(SMLOG.EXISTS Lty (fn T1 : Lty =>
(SMLOG.EXISTS Lty (fn T2 : Lty =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG0 (LTyTup (TUPLECON2 (T1, T2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 (LSucc D)
 ARGtc)
)))
, (RELmax (D1, D2, D))
))
, (RELmaxsize (T1, D1))
))
, (RELmaxsize (T2, D2))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG0 (LTyList T)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 D ARGtc)
)))
))
))
))
, (SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLty (ARG0, ARG1) ARGtc)
)) ARG0 (LTyExn)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 (LZero)
 ARGtc)
)))
))
)


fun genEnv (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLnum ARG0)
, ((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLvalue ARGt)
)))
 ARGt)
))
 ARG1)
, ((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
))
 ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELgenEnv (ARG0, ARG1, ARG2) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLnum ARG0)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
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
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
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
 ARG2)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
))

and enumTypVal (ARG0, ARG1, ARG2, ARG3)  = ((fn (ARG0, ARG1, ARG2, ARG3) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLnum ARG0)
, (PRINTLnum ARG1)
, (PRINTLvalue ARG2)
, (PRINTLty ARG3)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELenumTypVal (ARG0, ARG1, ARG2, ARG3) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLnum ARG0)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLnum ARG1)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
(EMBEDLvalue ARG2)
 | Option.NONE =>
(Lvalue ((SMLOG.freshString ())
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

and maxsize (ARG0, ARG1)  = ((fn (ARG0, ARG1) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLty ARG0)
, (PRINTLnum ARG1)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELmaxsize (ARG0, ARG1) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLty ARG0)
 | Option.NONE =>
(Lty ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLnum ARG1)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
))


fun RELeval (ARG0 : Lnum, ARG1 : (Lstring , Lvalue) TUPLETYPE2 Llist, ARG2 : Lexp, ARG3 : Lvalue)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lnum (fn H : Lnum =>
(SMLOG.EXISTS Lvalue (fn V : Lvalue =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc H)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 ENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LVar X)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG3 V ARGtc)
)))
, (RELlookupEnv (X, ENV, V))
))
))
))
))
))
))
, (SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lstring (fn F : Lstring =>
(SMLOG.EXISTS Lnum (fn H : Lnum =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc H)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 ENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LFix (TUPLECON4 (F, T, X, E))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG3 (LValClo (TUPLECON3 (ENV, TENV, (TUPLECON4 (F, T, X, E))
))
)
 ARGtc)
)))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Lexp (fn E1 : Lexp =>
(SMLOG.EXISTS Lexp (fn E2 : Lexp =>
(SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Llist (fn ENV0 : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lstring (fn F : Lstring =>
(SMLOG.EXISTS Lnum (fn H : Lnum =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Llist (fn TENV0 : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lvalue (fn V : Lvalue =>
(SMLOG.EXISTS Lvalue (fn V2 : Lvalue =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc H)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 ENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LApp (TUPLECON2 (E1, E2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG3 V ARGtc)
)))
, (RELeval (H, ENV, E1, (LValClo (TUPLECON3 (ENV0, TENV0, (TUPLECON4 (F, T, X, E))
))
)
))
))
, (RELeval (H, ENV, E2, V2))
))
, (RELeval (H, (Lopcoco (TUPLECON2 ((TUPLECON2 (X, V2))
, (Lopcoco (TUPLECON2 ((TUPLECON2 (F, (LValClo (TUPLECON3 (ENV0, TENV0, (TUPLECON4 (F, T, X, E))
))
)
))
, ENV0))
)
))
)
, E, V))
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
, (SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lnum (fn H : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc H)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 ENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LUnit)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG3 (LValUnit)
 ARGtc)
)))
))
))
))
, (SMLOG.EXISTS Lexp (fn E1 : Lexp =>
(SMLOG.EXISTS Lexp (fn E2 : Lexp =>
(SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lnum (fn H : Lnum =>
(SMLOG.EXISTS Lvalue (fn V1 : Lvalue =>
(SMLOG.EXISTS Lvalue (fn V2 : Lvalue =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc H)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 ENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LTup (TUPLECON2 (E1, E2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG3 (LValTup (TUPLECON2 (V1, V2))
)
 ARGtc)
)))
, (RELeval (H, ENV, E1, V1))
))
, (RELeval (H, ENV, E2, V2))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lnum (fn H : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc H)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 ENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LNil)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG3 (LValNil)
 ARGtc)
)))
))
))
))
, (SMLOG.EXISTS Lexp (fn E1 : Lexp =>
(SMLOG.EXISTS Lexp (fn E2 : Lexp =>
(SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lnum (fn H : Lnum =>
(SMLOG.EXISTS Lvalue (fn V1 : Lvalue =>
(SMLOG.EXISTS Lvalue (fn V2 : Lvalue =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc H)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 ENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LCons (TUPLECON2 (E1, E2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG3 (LValCons (TUPLECON2 (V1, V2))
)
 ARGtc)
)))
, (RELeval (H, ENV, E1, V1))
))
, (RELeval (H, ENV, E2, V2))
))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Lexp (fn E1 : Lexp =>
(SMLOG.EXISTS Lexp (fn E2 : Lexp =>
(SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lnum (fn H : Lnum =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Lvalue (fn V : Lvalue =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.EXISTS Lstring (fn Y : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc H)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 ENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LCaseList (TUPLECON3 (E, E1, (TUPLECON4 (X, Y, T, E2))
))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG3 V ARGtc)
)))
, (RELeval (H, ENV, E, (LValNil)
))
))
, (RELeval (H, ENV, E1, V))
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
, (SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Lexp (fn E1 : Lexp =>
(SMLOG.EXISTS Lexp (fn E2 : Lexp =>
(SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lnum (fn H : Lnum =>
(SMLOG.EXISTS Lty (fn T : Lty =>
(SMLOG.EXISTS Lvalue (fn V : Lvalue =>
(SMLOG.EXISTS Lvalue (fn V1 : Lvalue =>
(SMLOG.EXISTS Lvalue (fn V2 : Lvalue =>
(SMLOG.EXISTS Lstring (fn X : Lstring =>
(SMLOG.EXISTS Lstring (fn Y : Lstring =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc H)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 ENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LCaseList (TUPLECON3 (E, E1, (TUPLECON4 (X, Y, T, E2))
))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG3 V ARGtc)
)))
, (RELeval (H, ENV, E, (LValCons (TUPLECON2 (V1, V2))
)
))
))
, (RELeval (H, (Lopcoco (TUPLECON2 ((TUPLECON2 (Y, V2))
, (Lopcoco (TUPLECON2 ((TUPLECON2 (X, V1))
, ENV))
)
))
)
, E2, V))
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
, (SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lnum (fn H : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc H)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 ENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LExn)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG3 (LValExn)
 ARGtc)
)))
))
))
))
, (SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lnum (fn H : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc H)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 ENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LRaise E)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG3 (LValExn)
 ARGtc)
)))
, (RELeval (H, ENV, E, (LValExn)
))
))
))
))
))
))
, (SMLOG.EXISTS Lexp (fn E1 : Lexp =>
(SMLOG.EXISTS Lexp (fn E2 : Lexp =>
(SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lnum (fn H : Lnum =>
(SMLOG.EXISTS Lvalue (fn V : Lvalue =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc H)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 ENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LHandle (TUPLECON2 (E1, E2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG3 V ARGtc)
)))
, (RELeval (H, ENV, E1, (LValExn)
))
))
, (RELeval (H, ENV, E2, V))
))
))
))
))
))
))
))
, (SMLOG.EXISTS Lexp (fn E1 : Lexp =>
(SMLOG.EXISTS Lexp (fn E2 : Lexp =>
(SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lnum (fn H : Lnum =>
(SMLOG.EXISTS Lvalue (fn ORPAT_9 : Lvalue =>
(SMLOG.EXISTS Lvalue (fn V : Lvalue =>
(SMLOG.EXISTS TUPLETYPE2 (fn WILD_7 : (Lvalue , Lvalue) TUPLETYPE2 =>
(SMLOG.EXISTS TUPLETYPE3 (fn WILD_8 : ((Lstring , Lvalue) TUPLETYPE2 Llist , (Lstring , Lty) TUPLETYPE2 Llist , (Lstring , Lty , Lstring , Lexp) TUPLETYPE4) TUPLETYPE3 =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 (LSucc H)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG1 ENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG2 (LHandle (TUPLECON2 (E1, E2))
)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG3 V ARGtc)
)))
, (SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ORPAT_9 (LValClo WILD_8)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ORPAT_9 (LValUnit)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ORPAT_9 (LValNil)
 ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ORPAT_9 (LValCons WILD_7)
 ARGtc)
)))
))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) V ORPAT_9 ARGtc)
)))
, (RELeval (H, ENV, E1, V))
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


fun eval (ARG0, ARG1, ARG2, ARG3)  = ((fn (ARG0, ARG1, ARG2, ARG3) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLnum ARG0)
, ((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLvalue ARGt)
)))
 ARGt)
))
 ARG1)
, (PRINTLexp ARG2)
, (PRINTLvalue ARG3)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELeval (ARG0, ARG1, ARG2, ARG3) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLnum ARG0)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
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
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
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
(EMBEDLexp ARG2)
 | Option.NONE =>
(Lexp ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
, ((fn (Option.SOME ARG3) =>
(EMBEDLvalue ARG3)
 | Option.NONE =>
(Lvalue ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG3)
))


fun RELtestdata (ARG0 : Lnum, ARG1 : Lnum, ARG2 : (Lstring , Lty) TUPLETYPE2 Llist, ARG3 : (Lstring , Lvalue) TUPLETYPE2 Llist, ARG4 : Lexp, ARG5 : Lvalue)  = (SMLOG.STEP (SMLOG.EXISTS Lnum (fn D : Lnum =>
(SMLOG.EXISTS Lexp (fn E : Lexp =>
(SMLOG.EXISTS Llist (fn ENV : (Lstring , Lvalue) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lnum (fn H : Lnum =>
(SMLOG.EXISTS Llist (fn TENV : (Lstring , Lty) TUPLETYPE2 Llist =>
(SMLOG.EXISTS Lvalue (fn V : Lvalue =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 D ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 H ARGtc)
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
)) ARG2 TENV ARGtc)
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
(UNIFYLvalue (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
)))))
 (ARGt1, ARGt2) ARGtc)
)), (fn ARGv =>
(fn ARGt =>
((OCCURTUPLETYPE2 ((fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLvalue ARGv ARGt)
))))
 ARGv ARGt)
))))
 (ARG0, ARG1) ARGtc)
)) ARG3 ENV ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLexp (ARG0, ARG1) ARGtc)
)) ARG4 E ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG5 V ARGtc)
)))
, (RELgenEnv (D, ENV, TENV))
))
, (RELeval (H, ENV, E, V))
))
))
))
))
))
))
))
)


fun testdata (ARG0, ARG1, ARG2, ARG3, ARG4, ARG5)  = ((fn (ARG0, ARG1, ARG2, ARG3, ARG4, ARG5) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLnum ARG0)
, (PRINTLnum ARG1)
, ((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLty ARGt)
)))
 ARGt)
))
 ARG2)
, ((PRINTLlist (fn ARGt =>
((PRINTTUPLETYPE2 ((fn ARGt =>
(PRINTLstring ARGt)
), (fn ARGt =>
(PRINTLvalue ARGt)
)))
 ARGt)
))
 ARG3)
, (PRINTLexp ARG4)
, (PRINTLvalue ARG5)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELtestdata (ARG0, ARG1, ARG2, ARG3, ARG4, ARG5) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLnum ARG0)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLnum ARG1)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
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
 ARG2)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
, ((fn (Option.SOME ARG3) =>
((EMBEDLlist ((fn ARGt =>
((EMBEDTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
), (fn ARGt =>
((PROJTUPLETYPE2 (((fn ARGt =>
(EMBEDLstring ARGt)
), (fn ARGt =>
(PROJLstring ARGt)
)), ((fn ARGt =>
(EMBEDLvalue ARGt)
), (fn ARGt =>
(PROJLvalue ARGt)
))))
 ARGt)
)))
 ARG3)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG3)
, ((fn (Option.SOME ARG4) =>
(EMBEDLexp ARG4)
 | Option.NONE =>
(Lexp ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG4)
, ((fn (Option.SOME ARG5) =>
(EMBEDLvalue ARG5)
 | Option.NONE =>
(Lvalue ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG5)
))


fun RELquery1 (ARG0 : Lnum, ARG1 : Lnum, ARG2 : Lvalue)  = (SMLOG.STEP (SMLOG.EXISTS Lnum (fn B : Lnum =>
(SMLOG.EXISTS Lvalue (fn G : Lvalue =>
(SMLOG.EXISTS Lnum (fn N : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 B ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 N ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG2 G ARGtc)
)))
, (RELtestdata (B, N, (Lopcoco (TUPLECON2 ((TUPLECON2 ((THEstring "x")
, (LTyList (LTyUnit)
)
))
, (Lnil)
))
)
, (Lopcoco (TUPLECON2 ((TUPLECON2 ((THEstring "x")
, G))
, (Lnil)
))
)
, (LCaseList (TUPLECON3 ((LVar (THEstring "x")
)
, (LNil)
, (TUPLECON4 ((THEstring "h")
, (THEstring "t")
, (LTyList (LTyUnit)
)
, (LVar (THEstring "x")
)
))
))
)
, (LValNil)
))
))
))
))
))
)


fun query1 (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLnum ARG0)
, (PRINTLnum ARG1)
, (PRINTLvalue ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELquery1 (ARG0, ARG1, ARG2) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLnum ARG0)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLnum ARG1)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
(EMBEDLvalue ARG2)
 | Option.NONE =>
(Lvalue ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
))


fun RELquery2 (ARG0 : Lnum, ARG1 : Lnum, ARG2 : Lvalue)  = (SMLOG.STEP (SMLOG.EXISTS Lnum (fn B : Lnum =>
(SMLOG.EXISTS Lvalue (fn G : Lvalue =>
(SMLOG.EXISTS Lnum (fn N : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 B ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 N ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG2 G ARGtc)
)))
, (RELtestdata (B, N, (Lopcoco (TUPLECON2 ((TUPLECON2 ((THEstring "x")
, (LTyList (LTyUnit)
)
))
, (Lnil)
))
)
, (Lopcoco (TUPLECON2 ((TUPLECON2 ((THEstring "x")
, G))
, (Lnil)
))
)
, (LApp (TUPLECON2 ((LFix (TUPLECON4 ((THEstring "f")
, (LTyFun (TUPLECON2 ((LTyList (LTyUnit)
)
, (LTyList (LTyUnit)
)
))
)
, (THEstring "y")
, (LCaseList (TUPLECON3 ((LVar (THEstring "y")
)
, (LNil)
, (TUPLECON4 ((THEstring "h")
, (THEstring "t")
, (LTyList (LTyUnit)
)
, (LApp (TUPLECON2 ((LVar (THEstring "f")
)
, (LVar (THEstring "t")
)
))
)
))
))
)
))
)
, (LVar (THEstring "x")
)
))
)
, (LValNil)
))
))
))
))
))
)


fun query2 (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLnum ARG0)
, (PRINTLnum ARG1)
, (PRINTLvalue ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELquery2 (ARG0, ARG1, ARG2) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLnum ARG0)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLnum ARG1)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
(EMBEDLvalue ARG2)
 | Option.NONE =>
(Lvalue ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
))


fun RELquery3 (ARG0 : Lnum, ARG1 : Lnum, ARG2 : Lvalue)  = (SMLOG.STEP (SMLOG.EXISTS Lnum (fn B : Lnum =>
(SMLOG.EXISTS Lvalue (fn G : Lvalue =>
(SMLOG.EXISTS Lnum (fn N : Lnum =>
(SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG0 B ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLnum (ARG0, ARG1) ARGtc)
)) ARG1 N ARGtc)
)))
, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLvalue (ARG0, ARG1) ARGtc)
)) ARG2 G ARGtc)
)))
, (RELtestdata (B, N, (Lopcoco (TUPLECON2 ((TUPLECON2 ((THEstring "x")
, (LTyFun (TUPLECON2 ((LTyList (LTyList (LTyUnit)
)
)
, (LTyList (LTyList (LTyUnit)
)
)
))
)
))
, (Lnil)
))
)
, (Lopcoco (TUPLECON2 ((TUPLECON2 ((THEstring "x")
, G))
, (Lnil)
))
)
, (LApp (TUPLECON2 ((LFix (TUPLECON4 ((THEstring "f")
, (LTyFun (TUPLECON2 ((LTyList (LTyList (LTyUnit)
)
)
, (LTyList (LTyList (LTyUnit)
)
)
))
)
, (THEstring "y")
, (LApp (TUPLECON2 ((LVar (THEstring "x")
)
, (LVar (THEstring "y")
)
))
)
))
)
, (LCons (TUPLECON2 ((LNil)
, (LCons (TUPLECON2 ((LCons (TUPLECON2 ((LUnit)
, (LNil)
))
)
, (LNil)
))
)
))
)
))
)
, (LValCons (TUPLECON2 ((LValCons (TUPLECON2 ((LValUnit)
, (LValNil)
))
)
, (LValCons (TUPLECON2 ((LValNil)
, (LValNil)
))
)
))
)
))
))
))
))
))
)


fun query3 (ARG0, ARG1, ARG2)  = ((fn (ARG0, ARG1, ARG2) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((PRINTLnum ARG0)
, (PRINTLnum ARG1)
, (PRINTLvalue ARG2)
))
) (ARGtc SMLOG.Redo)
)
))
 (RELquery3 (ARG0, ARG1, ARG2) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
(EMBEDLnum ARG0)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
, ((fn (Option.SOME ARG1) =>
(EMBEDLnum ARG1)
 | Option.NONE =>
(Lnum ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
, ((fn (Option.SOME ARG2) =>
(EMBEDLvalue ARG2)
 | Option.NONE =>
(Lvalue ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG2)
))

