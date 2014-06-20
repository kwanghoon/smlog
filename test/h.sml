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


datatype ('a, 'b)  Lmigt = Lmigt of (Ostring * ('a , 'b) Lmigt Option.option ref) | THEmigt of ('a -> 'b)

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


datatype ('a0, 'a1)  TUPLETYPE2 = TUPLETYPE2 of (Ostring * ('a0 , 'a1) TUPLETYPE2 Option.option ref) | TUPLECON2 of ('a0 * 'a1)

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

datatype 'a  Llist = Llist of (Ostring * 'a Llist Option.option ref) | Lnil | Lopcoco of ('a , 'a Llist) TUPLETYPE2

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


datatype ('a0, 'a1)  RECORDTYPEagename = RECORDTYPEagename of (Ostring * ('a0 , 'a1) RECORDTYPEagename Option.option ref) | RECORDCONagename of {age:'a0, name:'a1}

fun EMBEDRECORDTYPEagename (qua0, qua1) ARGt0  = ((fn {age = age, name = name} =>
(RECORDCONagename {age = ((fn (ARG1, ARG2) =>
ARG1) qua0 age)
, name = ((fn (ARG1, ARG2) =>
ARG1) qua1 name)
})
) ARGt0)

and PROJRECORDTYPEagename (qua0, qua1) (RECORDTYPEagename (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJRECORDTYPEagename (qua0, qua1) (RECORDTYPEagename (_, (ref (Option.SOME ARGt))))  = ((PROJRECORDTYPEagename (qua0, qua1))
 ARGt)

| PROJRECORDTYPEagename (qua0, qua1) (RECORDCONagename ARGt0)  = ((fn {age = age, name = name} =>
{age = ((fn (ARG1, ARG2) =>
ARG2) qua0 age)
, name = ((fn (ARG1, ARG2) =>
ARG2) qua1 name)
}) ARGt0)


fun OCCURRECORDTYPEagename (qua0, qua1) ARG0 (RECORDTYPEagename (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURRECORDTYPEagename (qua0, qua1) ARG0 (RECORDTYPEagename (_, (ref (Option.SOME ARG2))))  = ((OCCURRECORDTYPEagename (qua0, qua1))
 ARG0 ARG2)

| OCCURRECORDTYPEagename (qua0, qua1) ARG0 (RECORDCONagename ARG1)  = ((fn ARGv =>
(fn {age = age, name = name} =>
((fn true =>
true | false =>
(qua1 ARGv name)
) ((fn true =>
true | false =>
(qua0 ARGv age)
) false)
)
)) ARG0 ARG1)


fun UNIFYRECORDTYPEagename (qua0, qua1) (ARGt1, (RECORDTYPEagename (_, (ref (Option.SOME ARGt2))))) ARGtc  = ((UNIFYRECORDTYPEagename (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEagename (qua0, qua1) (ARGt1, (RECORDTYPEagename (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) ((OCCURRECORDTYPEagename (((fn (ARG1, ARG2) =>
ARG2) qua0)
, ((fn (ARG1, ARG2) =>
ARG2) qua1)
))
 ARG0 ARGt1)
)

| UNIFYRECORDTYPEagename (qua0, qua1) ((RECORDTYPEagename (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEagename (qua0, qua1))
 (ARGt1, ARGt2) ARGtc)

| UNIFYRECORDTYPEagename (qua0, qua1) ((ARGt1 as (RECORDTYPEagename (_, (ref Option.NONE)))), ARGt2) ARGtc  = ((UNIFYRECORDTYPEagename (qua0, qua1))
 (ARGt2, ARGt1) ARGtc)

| UNIFYRECORDTYPEagename (qua0, qua1) ((RECORDCONagename ARGt1), (RECORDCONagename ARGt2)) ARGtc  = ((fn ({age = age1, name = name1}, {age = age2, name = name2}) =>
((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua1)
 (name1, name2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) ((fn (true, ARGtc) =>
(((fn (ARG1, ARG2) =>
ARG1) qua0)
 (age1, age2) ARGtc)
 | (false, ARGtc) =>
(false, ARGtc)) (true, ARGtc))
)
) (ARGt1, ARGt2))


fun PRINTRECORDTYPEagename (qua0, qua1) (RECORDTYPEagename (ARGs, (ref Option.NONE)))  = ARGs
| PRINTRECORDTYPEagename (qua0, qua1) (RECORDTYPEagename (_, (ref (Option.SOME ARGt))))  = ((PRINTRECORDTYPEagename (qua0, qua1))
 ARGt)

| PRINTRECORDTYPEagename (qua0, qua1) (RECORDCONagename ARGt0)  = ((fn {age = age, name = name} =>
(String.concat ["{", (SMLOG.intersperse "," [(String.concat ["age", "=", (qua0 age)
])
, (String.concat ["name", "=", (qua1 name)
])
])
, "}"])
) ARGt0)


datatype D = C of {age:int, name:string}

datatype OD = datatype D

datatype LD = LD of (Ostring * LD Option.option ref) | LC of (Lint , Lstring) RECORDTYPEagename

fun EMBEDLD (C ARGt)  = (LC ((EMBEDRECORDTYPEagename (((fn ARGt =>
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

and PROJLD (LD (_, (ref Option.NONE)))  = raise SMLOG.ProjectionError
| PROJLD (LD (_, (ref (Option.SOME ARGt))))  = (PROJLD ARGt)

| PROJLD (LC ARGt)  = (C ((PROJRECORDTYPEagename (((fn ARGt =>
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


fun OCCURLD ARG0 (LD (ARG1, (ref Option.NONE)))  = (op= (ARG0, ARG1))

| OCCURLD ARG0 (LD (_, (ref (Option.SOME ARG2))))  = (OCCURLD ARG0 ARG2)

| OCCURLD ARG0 (LC ARG1)  = ((OCCURRECORDTYPEagename ((fn ARGv =>
(fn ARGt =>
(OCCURLint ARGv ARGt)
)), (fn ARGv =>
(fn ARGt =>
(OCCURLstring ARGv ARGt)
))))
 ARG0 ARG1)


fun UNIFYLD (ARGt1, (LD (_, (ref (Option.SOME ARGt2))))) ARGtc  = (UNIFYLD (ARGt1, ARGt2) ARGtc)

| UNIFYLD (ARGt1, (LD (ARG0, (ARGr as (ref Option.NONE))))) ARGtc  = ((fn true =>
(false, ARGtc) | false =>
(true, (SMLOG.extendTc ARGtc ARGr ARGt1)
)) (OCCURLD ARG0 ARGt1)
)

| UNIFYLD ((LD (_, (ref (Option.SOME ARGt1)))), ARGt2) ARGtc  = (UNIFYLD (ARGt1, ARGt2) ARGtc)

| UNIFYLD ((ARGt1 as (LD (_, (ref Option.NONE)))), ARGt2) ARGtc  = (UNIFYLD (ARGt2, ARGt1) ARGtc)

| UNIFYLD ((LC ARGt1), (LC ARGt2)) ARGtc  = ((UNIFYRECORDTYPEagename (((fn (ARGt1, ARGt2) =>
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


fun PRINTLD (LD (ARGs, (ref Option.NONE)))  = ARGs
| PRINTLD (LD (_, (ref (Option.SOME ARGt))))  = (PRINTLD ARGt)

| PRINTLD (LC ARGt)  = (String.concat ["C", " ", ((PRINTRECORDTYPEagename ((fn ARGt =>
(PRINTLint ARGt)
), (fn ARGt =>
(PRINTLstring ARGt)
)))
 ARGt)
])

