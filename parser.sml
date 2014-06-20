
structure Parser =
struct

(****************************************************************)

structure SteadyCheckLrVals =
   SteadyCheckLrValsFun(structure Token = LrParser.Token)

structure SteadyCheckLex =
   SteadyCheckLexFun(structure Tokens = SteadyCheckLrVals.Tokens)

structure SteadyCheckParser =
   JoinWithArg(structure ParserData = SteadyCheckLrVals.ParserData
               structure Lex = SteadyCheckLex
               structure LrParser = LrParser)

(****************************************************************)

fun parse fileName =
    let 
        exception EndOfParse

        type pos = int

        type lexarg =
             {
               comLevel : int ref,
               commonOperations : ParserUtil.PositionMap.operations,
               error : (string * int * int) -> unit,
               stringBuf : string list ref,
               stringStart : pos ref,
               stringType : bool ref
             }

        val sourceStream = TextIO.openIn fileName
        val parserOperations = ParserUtil.PositionMap.create fileName

        fun onParseError arg = print (#makeMessage parserOperations arg)

        val initialArg =
            {
              comLevel = ref 0,
              commonOperations = parserOperations,
              error = onParseError,
              stringBuf = ref nil : string list ref,
              stringStart = ref 0,
              stringType = ref true
            } : lexarg

        local
          val dummyEOF  = SteadyCheckLrVals.Tokens.EOF (0,0)
        in
        fun oneParse lexer = 
            let
              val (nextToken,lexer') = SteadyCheckParser.Stream.get lexer
            in
              if SteadyCheckParser.sameToken(nextToken, dummyEOF)
              then raise EndOfParse
              else SteadyCheckParser.parse (0, lexer, onParseError, ())
            end
        end

        fun untilEOF lexer results =
            let val (ast,lexer') = oneParse lexer
            in  untilEOF lexer' (ast :: results)
            end
                handle EndOfParse => List.rev results
                     | SteadyCheckParser.ParseError => List.rev results

        fun getLine length = case TextIO.inputLine sourceStream of
	    	    	       SOME s => s
			     | NONE => ""

        val asts =
            untilEOF (SteadyCheckParser.makeLexer getLine initialArg) []
            handle e => (TextIO.closeIn sourceStream; raise e)

        val _ = TextIO.closeIn sourceStream

    in  
        asts
    end

end

