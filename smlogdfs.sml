
(* Depth-first search *)

structure SMLOG =

struct

(* stream *)

datatype 'a stream = Nil | Cons of 'a * (unit -> 'a stream)

fun mapS f Nil = Nil
  | mapS f (Cons (x,d)) = Cons (f x, fn () => mapS f (d ()))

fun concatS Nil = Nil
  | concatS (Cons ([],d)) = concatS (d ())
  | concatS (Cons (Nil::xs,d)) = concatS (Cons (xs,d))
  | concatS (Cons (Cons(ys,d')::xs,d)) = Cons (ys, fn () => concatS (Cons (d' () :: xs, d)) )

fun step Nil          = Nil
  | step (Cons (_,d)) = d ()

fun next Nil = Nil
  | next (Cons (_,d)) = 
      (case d () of
         Nil         => Nil
       | Cons ([],d) => next (Cons ([],d))
       | Cons (x,d)  => Cons (x,d)    )

fun all n Nil          = []
  | all 0 (Cons (v,d)) = []
  | all n (Cons (v,d)) = v :: all (n-1) (d ())

(* fresh string *)

val (freshString, reset) =
    let val r = ref 0
    in  
        (fn () =>
           let val n = !r
               val _ = r := !r + 1
           in
               "V" ^ Int.toString n
           end,
         fn () => ( r := 0) 
        )
    end


(* substitution *)

datatype Action = Undo | Redo

type TrailCont = Action -> unit  

fun extendTc tc r t = 
    let val _ = r := SOME t

        fun tc' Undo = (tc Undo; r:=NONE  )
          | tc' Redo = (tc Redo; r:=SOME t)

    in  tc'
    end

fun doUnify (unify : 'a  * 'a -> TrailCont -> (bool * TrailCont))
            (t1 : 'a) 
            (t2 : 'a)
            (tc : TrailCont) =
    let 
        val _ = tc Redo
        val (bool,tc) = unify (t1,t2) tc
        val _ = tc Undo
    in
        if bool then Cons ([tc], fn () => Nil) else Nil
    end

fun doAppUnderSubst (f : unit -> 'a ) (tc : TrailCont) : 'a =
    let
        val _  = tc Redo
        val v  = f ()
        val _  = tc Undo
    in
        v
    end

(* predicates *)

type Predicate = TrailCont -> (TrailCont list) stream

local

fun plusplus  Nil q a = q a
  | plusplus (Cons (xs,d)) q a = Cons (xs, fn () => plusplus (d ()) q a)

in

fun DISJ (p : Predicate, q : Predicate) : Predicate = fn tc => plusplus (p tc) q tc

end

fun CONJ (p : Predicate, q : Predicate) : Predicate = concatS o mapS (map q) o p

fun NOT (p : Predicate) : Predicate = 
      (fn tc => case p tc of 
                  Nil    => Cons ([tc], fn () => Nil)
                | Cons _ => Nil)

fun STEP (p : Predicate) : Predicate = p

fun EXISTS c p = (fn tc => p (c (freshString (), ref NONE)) tc) : Predicate

fun TRUE  tc = Cons ([tc], fn () => Nil)

fun FALSE tc = Nil

(* projection error *)

exception ProjectionError

(* printing lifted values *)

fun intersperse sep [] = ""
  | intersperse sep [x] = x
  | intersperse sep (x::xs) = x ^ sep ^ intersperse sep xs

end

