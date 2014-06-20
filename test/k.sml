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
) ((((Option.SOME (PROJLint ARG0)
)
)
 handle _ =>
Option.NONE), (((Option.SOME ((PROJLlist (fn ARGt =>
(PROJLint ARGt)
))
 ARG1)
)
)
 handle _ =>
Option.NONE)))
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
((EMBEDLlist (fn ARGt =>
(EMBEDLint ARGt)
))
 ARG1)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG1)
))


fun RELrandomInt (ARG0 : Lint)  = (SMLOG.STEP (SMLOG.EXISTS Lint (fn X : Lint =>
(SMLOG.CONJ ((SMLOG.CONJ (SMLOG.TRUE, (fn ARGtc =>
(SMLOG.doUnify (fn (ARG0, ARG1) =>
(fn ARGtc =>
(UNIFYLint (ARG0, ARG1) ARGtc)
)) ARG0 X ARGtc)
)))
, (RELmember (X, (Lopcoco (TUPLECON2 ((THEint 1)
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
))
))
)


fun randomInt (ARG0)  = ((fn (ARG0) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((((Option.SOME (PROJLint ARG0)
)
)
 handle _ =>
Option.NONE)))
) (ARGtc SMLOG.Redo)
)
))
 (RELrandomInt (ARG0) (fn _ =>
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


fun RELrandomList (ARG0 : Lint Llist)  = (SMLOG.STEP (SMLOG.DISJ ((SMLOG.DISJ (SMLOG.FALSE, (fn ARGtc =>
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
, (SMLOG.EXISTS Lint (fn X : Lint =>
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
, (RELrandomInt (X))
))
, (RELrandomList (XS))
))
))
))
))
)


fun randomList (ARG0)  = ((fn (ARG0) =>
(SMLOG.mapS (List.map (fn ARGtc =>
((fn _ =>
((fn ARGres =>
((fn _ =>
ARGres) (ARGtc SMLOG.Undo)
)
) ((((Option.SOME ((PROJLlist (fn ARGt =>
(PROJLint ARGt)
))
 ARG0)
)
)
 handle _ =>
Option.NONE)))
) (ARGtc SMLOG.Redo)
)
))
 (RELrandomList (ARG0) (fn _ =>
()))
)
) (((fn (Option.SOME ARG0) =>
((EMBEDLlist (fn ARGt =>
(EMBEDLint ARGt)
))
 ARG0)
 | Option.NONE =>
(Llist ((SMLOG.freshString ())
, (ref Option.NONE)
))
) ARG0)
))

