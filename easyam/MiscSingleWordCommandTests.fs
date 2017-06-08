module MiscSingleWordCommandTests
    open NUnit.Framework
    open FsUnit
    open EasyamParsingEngine
    open Types
    open SAModel

    // Funky code. I need some way to pretend I have a real OS incoming file I'm processing
    let getFakeFileInfo() = 
        let tempColl = (new System.CodeDom.Compiler.TempFileCollection(System.AppDomain.CurrentDomain.BaseDirectory, false))
        tempColl.AddExtension("bsx") |> ignore
        let tempFileName = System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, "tester.bsx")
        tempColl.AddFile(tempFileName,false)
        let fs1=System.IO.File.OpenWrite(tempFileName)
        let sw1=new System.IO.StreamWriter(fs1)
        sw1.WriteLine("test")
        sw1.Close()
        fs1.Close()
        let ret=new System.IO.FileInfo(tempFileName)
        tempColl.Delete()
        ret
    // memoize one to reuse
    let dummyFileInfo = getFakeFileInfo()

    let setupCompilationScenario fileNumber incomingRawLineCount incomingLineCountWithEmptyLinesDeletedCount rawLineArray =
        initialProcessingOfIncomingFileLines fileNumber dummyFileInfo incomingRawLineCount incomingLineCountWithEmptyLinesDeletedCount rawLineArray

    [<Test>]
    let ``MISC SINGLE WORD: Null test``() =
        let testText = [||]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 0
    [<Test>]
    let ``MISC SINGLE WORD: Empty Line Test``() =
        let testText = [|""|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 0
    [<Test>]
    let ``MISC SINGLE WORD: Single Command Has Correct CommandLevel``() =
        let testText = [|"Q:"|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 1
        ret.[0].Commands.Length |> should equal 1
        ret.[0].Commands.[0].CommandIndentLevel |> should equal 0
    [<Test>]
    let ``MISC SINGLE WORD: Single Command In Middle Of Comment Has Correct Command Level``() =
        let testText = [|"This is a fun day to go to the beach Q: What's a beach?"|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 1
        ret.[0].Commands.Length |> should equal 2
        ret.[0].Commands.[0].CommandIndentLevel |> should equal 0
        ret.[0].Commands.[1].CommandIndentLevel |> should equal 1
    [<Test>]
    let ``MISC SINGLE WORD: Complex line Has Correct Command Levels``() =
        let testText = [|"This is a fun day to go to the beach //May need to buy an umbrella Q: What's a beach?"|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 1
        ret.[0].Commands.Length |> should equal 3
        ret.[0].Commands.[0].CommandIndentLevel |> should equal 0
        ret.[0].Commands.[1].CommandIndentLevel |> should equal 1
        ret.[0].Commands.[2].CommandIndentLevel |> should equal 2

    // Begin testing the creation of the raw model item list
    [<Test>]
    let ``MISC SINGLE WORD: A simple command returns one Model Item``() =
        let testText = [|"Q: Is this the real life? Is this just fantasy?"|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 1
        let beginningCompilerStatus = {CompilerMessages=Array.empty; ModelItems=Array.empty}
        let rawModelItems=makeRawModel ret beginningCompilerStatus
        rawModelItems |> should haveLength 1
    [<Test>]
    let ``MISC SINGLE WORD: Two lines don't have the same ID``() =
        let testText = [|"Q: Is this the real life? Is this just fantasy?";"Caught in a landslide, no escape from reality"|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 2
        let beginningCompilerStatus = {CompilerMessages=Array.empty; ModelItems=Array.empty}
        let rawModelItems=makeRawModel ret beginningCompilerStatus
        rawModelItems |> should haveLength 2
        rawModelItems.[0].Id=rawModelItems.[1].Id |> should equal false
    [<Test>]
    let ``MISC SINGLE WORD: Multiple lines have the correct SourceReferences``() =
        let testText = [|"Hi there"; "I'm a complext line. I have many uses NOTE:Like I believe that! Q:Who is talking?"; "I am not a complex line."|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 3
        let beginningCompilerStatus = {CompilerMessages=Array.empty; ModelItems=Array.empty}
        let rawModelItems=makeRawModel ret beginningCompilerStatus
        rawModelItems |> should haveLength 5


    //[<Test>]
    //let ``MISC SINGLE WORD: Lines prefixed with a Q: are tagged as questions``() =
    //    let testText = [|"Q: What is your name?"|]
    //    let compilerMessages,modelItems = parseLines testText
    //    modelItems |> should haveLength 1
    //    let objType=modelItems.Item(0).ItemType.ToString()
    //    objType.GetLeft 9 |> should equal "QUESTION:"

    //[<Test>]
    //let ``MISC SINGLE WORD: The command word is stripped out of the line``() =
    //    let testText = [|"Q: What is your name?"|]
    //    let compilerMessages,modelItems = parseLines testText
    //    modelItems |> should haveLength 1
    //    let modelItemQuestionText = match modelItems.[0].ItemType with 
    //                                    |Question(x)->x.Text 
    //                                    |_->""
    //    (modelItemQuestionText.GetLeft 2 = "Q:") |> should equal false

    //[<Test>]
    //let ``MISC SINGLE WORD: Freetext line followed by question line registers correcly``() =
    //    let testText = [|"This is my freetext";"Q: What is your name?"|]
    //    let compilerMessages,modelItems = parseLines testText
    //    modelItems |> should haveLength 2
    //    let objType=modelItems.Item(0).ItemType.ToString()
    //    objType.GetLeft 5 |> should equal "NOTE:"
    //    let objType=modelItems.Item(1).ItemType.ToString()
    //    objType.GetLeft 9 |> should equal "QUESTION:"

    //[<Test>]
    //let ``MISC SINGLE WORD: Question line followed by freetext line should register correctly``() =
    //    let testText = [|"Q: What is your name?"; "This is my freetext"|]
    //    let compilerMessages,modelItems = parseLines testText
    //    modelItems |> should haveLength 2
    //    let objType=modelItems.Item(0).ItemType.ToString()
    //    objType.GetLeft 9 |> should equal "QUESTION:"
    //    let objType=modelItems.Item(1).ItemType.ToString()
    //    objType.GetLeft 5 |> should equal "NOTE:"

    //[<Test>]
    //let ``MISC SINGLE WORD: Question line nestled between two freetext lines works``() =
    //    let testText = [|"The moon rises at midnight"; "Q: What is your name?"; "This is my freetext"|]
    //    let compilerMessages,modelItems = parseLines testText
    //    modelItems |> should haveLength 3
    //    let objType=modelItems.Item(0).ItemType.ToString()
    //    objType.GetLeft 5 |> should equal "NOTE:"
    //    let objType=modelItems.Item(1).ItemType.ToString()
    //    objType.GetLeft 9 |> should equal "QUESTION:"
    //    let objType=modelItems.Item(2).ItemType.ToString()
    //    objType.GetLeft 5 |> should equal "NOTE:"

    //[<Test>]
    //let ``MISC SINGLE WORD: Single line of freetext with question in it works correctly``() =
    //    let testText = [|"The meeting is going well Q: Who is the guy in the yellow hat?"|]
    //    let compilerMessages,modelItems = parseLines testText
    //    modelItems |> should haveLength 2
    //    let objType=modelItems.Item(0).ItemType.ToString()
    //    objType.GetLeft 5 |> should equal "NOTE:"
    //    let objType=modelItems.Item(1).ItemType.ToString()
    //    objType.GetLeft 9 |> should equal "QUESTION:"
