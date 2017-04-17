module FreeFormTextTests
    open NUnit.Framework
    open FsUnit
    open EasyamParsingEngine
    open Types
    open SAModel



    [<Test>]
    let ``Empty line has no warnings or errors test``() =
        let testText = [|""|]
        let compilerMessages,modelItems = parseLines testText
        compilerMessages |> should haveLength 0
    [<Test>]
    let ``Empty line has only one modelItem test``() =
        let testText = [|""|]
        let compilerMessages,modelItems = parseLines testText
        modelItems |> should haveLength 0
    [<Test>]
    let ``Empty line has proper result test``() =
        let testText = [|""|]
        let compilerMessages,modelItems = parseLines testText
        modelItems |> should haveLength 0
    [<Test>]
    let ``Single line freeform text has no warnings or errors test``() =
        let testText = [|"This is some freeform text"|]
        let compilerMessages,modelItems = parseLines testText
        compilerMessages |> should haveLength 0
//    [<Test>]
//    let ``Single line freeform text has only one modelItem test``() =
//        let testText = [|"This is some freeform text"|]
//        let compilerMessages,modelItems = parseLines testText
//        modelItems |> should haveLength 1
//    [<Test>]
//    let ``Single line freeform text has proper result test``() =
//        let testText = [|"This is some freeform text"|]
//        let compilerMessages,modelItems = parseLines testText
//        modelItems.Item(0).ItemType= SAModel.Note("", defaultContextInfo) |> should haveLength 1
//
