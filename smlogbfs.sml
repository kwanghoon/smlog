
(* Breadth-first search *)

structure SMLOG =

struct

(* stream *)

datatype 'a stream = Nil | Cons of 'a * (unit -> 'a stream)

fun mapS f Nil = Nil
  | mapS f (Cons (x,d)) = Cons (f x, fn () => mapS f (d ()))

fun concatS (Nil : 'a stream stream) = Nil
  | concatS (Cons (Nil,d)) = concatS (d ())
  | concatS (Cons (Cons (x,d1),d2)) = Cons (x, fn () => concatS (Cons (d1 (),d2)))

fun step Nil          = Nil
  | step (Cons (_,d)) = d ()

fun next Nil = Nil
  | next (Cons (_,d)) = 
      (case d () of
         Nil         => Nil
       | Cons ([],d) => next (Cons ([],d))
       | Cons (x,d)  => Cons (x,d)    )


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

fun doAppUnderSubst (f : 'a -> 'b) (t : 'a) (tc : TrailCont) : 'b =
    let
        val _ = tc Redo
        val t'  = f t
        val _ = tc Undo
    in
        t'
    end

(* predicates *)

type Predicate = TrailCont -> (TrailCont list) stream

local
        fun zipWith f (Cons (e1,fs1)) (Cons (e2,fs2)) = 
                Cons (f (e1,e2), fn () => zipWith f (fs1 ()) (fs2 ()))
          | zipWith f (Cons (e1,fs1)) Nil = Cons (e1,fs1)
          | zipWith f Nil (Cons (e2,fs2)) = Cons (e2,fs2)
          | zipWith f Nil Nil = Nil
in

fun DISJ (p : Predicate, q : Predicate) : Predicate = 
    fn tc => zipWith op@ (p tc) (q tc)
end

local 

        fun transpose (sl : 'a stream list) : 'a list stream = 
            let 
                fun transpose [] ls fsl = 
                       Cons (ls, fn () => transpose (map (fn fs => fs ()) fsl) [] [])

                  | transpose (s::ss) ls fsl = 
                      (case s of
                         Cons (e,fs') => transpose ss (ls @ [e]) (fsl @ [fs']) 
                       | Nil => transpose ss ls fsl)

            in  
                transpose sl [] []
            end

        fun diag (ss : 'a stream stream) : 'a list stream =
            let
                fun diag x y l passed (Cons (Cons (e, fs), fss)) =
                      if x >= y
                      then 
                           diag x (y+1) 
                               (e::l) 
                               (fn ss => passed (Cons (fs (), fn ()=>ss)))
                               (fss ())
                      else 
                           Cons (l,fn () => diag (x+1) 0 
                                                 [] 
                                                 (fn ss=>ss)
                                                 (passed (Cons (Cons (e, fs), fss))))

                  | diag x y l passed (Cons (Nil, fss)) =
                      if x >= y
                      then 
                           diag x (y+1) 
                                l 
                                passed 
                                (fss ())
                      else 
                           Cons (l, fn ()=> diag (x+1) 0 
                                                 [] 
                                                 (fn ss=>ss)
                                                 (passed (fss ())))

                  | diag x y l passed Nil =
                      (case passed Nil of
                         Nil => (case l of
                                  [] => Nil
                                | _ => Cons (l, fn ()=>Nil))
                       | _ => Cons (l, fn ()=> diag (x+1) 0 
                                                    [] 
                                                    (fn ss=>ss)
                                                    (passed Nil)))
            in  
                diag 0 0 [] (fn ss=>ss) ss
            end

in

fun CONJ (p : Predicate, q : Predicate) : Predicate = 
     mapS (List.concat o List.concat) o diag o mapS transpose o mapS (map q) o p 

(*
    let
        val sl   = p tc
        val slsl = mapS (map q) sl

        val ssll = mapS transpose slsl
        val slll = diag ssll
        val sl   = mapS (List.concat o List.concat) slll
    in
        sl 
    end
*)

end

fun STEP (p : Predicate) : Predicate = fn tc => Cons ([], fn () => p tc)

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

