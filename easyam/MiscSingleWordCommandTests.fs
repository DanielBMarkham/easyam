module MiscSingleWordCommandTests
    open NUnit.Framework
    open FsUnit
    open EasyamParsingEngine
    open Types
    open SAModel

    [<Test>]
    let ``MISC SINGLE WORD: Null test``() =
        let testText = [||]
        let compilerMessages,modelItems = parseLines testText
        compilerMessages |> should be Empty
        modelItems |> should be Empty

    [<Test>]
    let ``MISC SINGLE WORD: One line freeform text is not a question``() =
        let testText = [|"Dogs live here"|]
        let compilerMessages,modelItems = parseLines testText
        modelItems |> should haveLength 1
        let objType=modelItems.Item(0).ItemType.ToString()
        objType.GetLeft 5 |> should equal "NOTE:"

    [<Test>]
    let ``MISC SINGLE WORD: Lines prefixed with a Q: are tagged as questions``() =
        let testText = [|"Q: What is your name?"|]
        let compilerMessages,modelItems = parseLines testText
        modelItems |> should haveLength 1
        let objType=modelItems.Item(0).ItemType.ToString()
        objType.GetLeft 9 |> should equal "QUESTION:"

    [<Test>]
    let ``MISC SINGLE WORD: The command word is stripped out of the line``() =
        let testText = [|"Q: What is your name?"|]
        let compilerMessages,modelItems = parseLines testText
        modelItems |> should haveLength 1
        let modelItemQuestionText = match modelItems.[0].ItemType with 
            |Question(x)->x.Text 
            |_->""
        (modelItemQuestionText.GetLeft 2 = "Q:") |> should equal false

    [<Test>]
    let ``MISC SINGLE WORD: Freetext line followed by question line registers correcly``() =
        let testText = [|"This is my freetext";"Q: What is your name?"|]
        let compilerMessages,modelItems = parseLines testText
        modelItems |> should haveLength 2
        let objType=modelItems.Item(0).ItemType.ToString()
        objType.GetLeft 5 |> should equal "NOTE:"
        let objType=modelItems.Item(1).ItemType.ToString()
        objType.GetLeft 9 |> should equal "QUESTION:"

    [<Test>]
    let ``MISC SINGLE WORD: Question line followed by freetext line should register correctly``() =
        let testText = [|"Q: What is your name?"; "This is my freetext"|]
        let compilerMessages,modelItems = parseLines testText
        modelItems |> should haveLength 2
        let objType=modelItems.Item(0).ItemType.ToString()
        objType.GetLeft 9 |> should equal "QUESTION:"
        let objType=modelItems.Item(1).ItemType.ToString()
        objType.GetLeft 5 |> should equal "NOTE:"

    [<Test>]
    let ``MISC SINGLE WORD: Question line nestled between two freetext lines works``() =
        let testText = [|"The moon rises at midnight"; "Q: What is your name?"; "This is my freetext"|]
        let compilerMessages,modelItems = parseLines testText
        modelItems |> should haveLength 3
        let objType=modelItems.Item(0).ItemType.ToString()
        objType.GetLeft 5 |> should equal "NOTE:"
        let objType=modelItems.Item(1).ItemType.ToString()
        objType.GetLeft 9 |> should equal "QUESTION:"
        let objType=modelItems.Item(2).ItemType.ToString()
        objType.GetLeft 5 |> should equal "NOTE:"

//    [<Test>]
//    let ``MISC SINGLE WORD: Single line of freetext with question in it works correctly``() =
//        let testText = [|"The meeting is going well Q: Who is the guy in the yellow hat?"|]
//        let compilerMessages,modelItems = parseLines testText
//        modelItems |> should haveLength 2
//        let objType=modelItems.Item(0).ItemType.ToString()
//        objType.GetLeft 5 |> should equal "NOTE:"
//        let objType=modelItems.Item(1).ItemType.ToString()
//        objType.GetLeft 9 |> should equal "QUESTION:"
