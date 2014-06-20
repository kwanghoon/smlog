
structure Main =
struct

(* val _ = Compiler.Control.Print.printDepth  := 10000000 *)
(* val _ = Compiler.Control.Print.printLength := 10000000 *)
(* val _ = Compiler.Control.Print.stringDepth := 10000000 *)

(* val _ = SMLofNJ.Internals.GC.messages false *)

exception ParseErr

fun compile filename = 
    let
        (* parsing *)

        val declList = 
            case Parser.parse filename of 
              (declList :: _) => declList
            | _ => raise ParseErr

        (* elaboration *)

        val elaboratedDeclList = Elaborate.elaboration declList

        (* completion *)

        val completedDeclList = Completion.completion elaboratedDeclList

        (* optimization: rearrangement of predicates *)

        val optDeclList = Optimization.optimization completedDeclList

        (* lifting *)

        val liftedDeclList = Lifting.lifting optDeclList

        (* code generation *)

        val cgDeclList = CodeGeneration.codegeneration liftedDeclList

        (* printing *)
 
        val _ = PrettyPrint.prettyprint filename cgDeclList

    in
        print "OK.\n"
    end

    handle (exn as Elaborate.ErrElaboration s) => 
              (print s; print "\n"; raise exn)

         | (exn as Completion.ErrCompletion s) =>
              (print s; print "\n"; raise exn)

         | (exn as Lifting.ErrLifting s) =>
              (print s; print "\n"; raise exn)

         | (exn as CodeGeneration.ErrCodeGeneration s) =>
              (print s; print "\n"; raise exn)

fun test () =
    let val y = [ (* "test/a.smlog", *)
                  "test/b.smlog",
(*                   "test/c.smlog", *)
                  "test/d.smlog",
                  "test/e.smlog",
                  "test/f.smlog",
                  "test/g.smlog",
                  "test/h.smlog",
                  "test/i.smlog"  ]
        val _ = map (fn x => (print ("\n" ^ x ^ ":\n"); compile x)) y
    in
        ()
    end

val _ = test()

end