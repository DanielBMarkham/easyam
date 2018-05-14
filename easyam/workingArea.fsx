#if INTERACTIVE
let msg = "Interactive"
#I @"C:\Users\Daniel_Main\Dropbox\source\git-repos\easyam\easyam\packages\FParsec.1.0.4-RC3\lib\net40-client\"
#r @"FParsecCS.dll"
#r @"FParsec.dll"
#I @"C:\Users\Daniel_Main\Dropbox\source\git-repos\easyam\easyam\"
#load @"EasyAMParser.fs"
#else
let msg = "Not Interactive"
#endif
open EasyAMParser
open FParsec
open FParsecCS
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Internal
open FParsec.Internals

let p =FParsec.CharStream.EndOfStreamChar
let s= FParsec.CharStream.ParseString

let str s = FParsec.CharParsers.pstring s
let test p s =
    match run p s with
    | Success(result,_,_)  -> printfn "%A" result
    | Failure(message,_,_) -> eprintfn "%A" message    

type Parser<'t> = FParsec.Primitives.Parser<'t, UserState>
    

    
//let pBasicText = manyChars anyChar
//let pComment = pstring "//" >>. pBasicText
//test pComment "// This is a comment"

type EasyAMVal =
    | Comment of CommentDetail
    | ModelItem of ModelItemDetail
    | Attribute of AttributeDetail
    | Annotation of AnnotationDetail
    | AttributeAnnotation of AttributeAnnotationDetail


//type EasyAMParser = FParsec.Primitives.Parser.Parser<EasyAMVal, unit>

