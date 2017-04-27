module FreeFormTextTests
    open NUnit.Framework
    open FsUnit
    open EasyamParsingEngine
    open Types
    open SAModel



    [<Test>]
    let ``FREEFORM TEXT: Empty line has no warnings or errors test``() =
        let testText = [|""|]
        let compilerMessages,modelItems = parseLines testText
        compilerMessages |> should haveLength 0
    [<Test>]
    let ``FREEFORM TEXT: Empty line has only one modelItem test``() =
        let testText = [|""|]
        let compilerMessages,modelItems = parseLines testText
        modelItems |> should haveLength 0
    [<Test>]
    let ``FREEFORM TEXT: Empty line has proper result test``() =
        let testText = [|""|]
        let compilerMessages,modelItems = parseLines testText
        modelItems |> should haveLength 0
    [<Test>]
    let ``FREEFORM TEXT: Single line freeform text has no warnings or errors test``() =
        let testText = [|"This is some freeform text"|]
        let compilerMessages,modelItems = parseLines testText
        compilerMessages |> should haveLength 0
    [<Test>]
    let ``FREEFORM TEXT: Single line freeform text has only one modelItem test``() =
        let testText = [|"This is some freeform text"|]
        let compilerMessages,modelItems = parseLines testText
        modelItems |> should haveLength 1
    [<Test>]
    let ``FREEFORM TEXT: Two lines freeform text has only two modelItems test``() =
        let testText = [|"This is some freeform text";"This is some more"|]
        let compilerMessages,modelItems = parseLines testText
        modelItems |> should haveLength 2
    [<Test>]
    let ``FREEFORM TEXT: Single line freeform text has proper result NodeType test``() =
        let testText = [|"This is some freeform text"|]
        let compilerMessages,modelItems = parseLines testText
        let objType=modelItems.Item(0).ItemType.ToString()
        objType.GetLeft 5 |> should equal "NOTE:"
    [<Test>]
    let ``FREEFORM TEXT: Single line freeform text has proper result Bucket tag test``() =
        let testText = [|"This is some freeform text"|]
        let compilerMessages,modelItems = parseLines testText
        modelItems.Item(0).Bucket |> should equal Buckets.None
    [<Test>]
    let ``FREEFORM TEXT: Single line freeform text has proper result Abstraction Level tag test``() =
        let testText = [|"This is some freeform text"|]
        let compilerMessages,modelItems = parseLines testText
        modelItems.Item(0).AbstractionLevel |> should equal AbstractionLevels.None
    [<Test>]
    let ``FREEFORM TEXT: Single line freeform text has proper result temporal tag test``() =
        let testText = [|"This is some freeform text"|]
        let compilerMessages,modelItems = parseLines testText
        modelItems.Item(0).TemporalIndicator |> should equal TemporalIndicators.None
        
        
