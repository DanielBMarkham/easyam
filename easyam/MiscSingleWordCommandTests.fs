module MiscSingleWordCommandTests
    open NUnit.Framework
    open FsUnit
    open EasyamParsingEngine
    open Types
    open SAModel
    open Lenses
    open Utils

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
    let ``MISC SINGLE WORD: Question with indent is only one command``() =
        let testText = [|"    Q: Which account are we talking about?"|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 1
        ret.[0].Commands.Length |> should equal 1


    [<Test>]
    let ``MISC SINGLE WORD: Behavior without a target works``() =
        let testText = [|"BEHAVIOR"|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 1
        ret.[0].Commands.Length |> should equal 1
        ret.[0].Commands.[0].CommandIndentLevel |> should equal 0
        ret.[0].Commands.[0].Token |> should equal "BEHAVIOR"
        ret.[0].Commands.[0].Value |> should equal ""
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
        ret.[0].Commands.Length |> should equal 2
        ret.[0].Commands.[0].CommandIndentLevel |> should equal 0
        ret.[0].Commands.[1].CommandIndentLevel |> should equal 1

    // Begin testing the creation of the model
    [<Test>]
    let ``MISC SINGLE WORD: Free text sent by itself adds a note to the default item``() =
        let testText = [|"Dogs are not considered wolves"|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 1
        let newCompilerStatus=makeRawModel ret beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations.[0].AnnotationType |> should equal AnnotationTokenType.Note
    [<Test>]
    let ``MISC SINGLE WORD: A simple annotation Q: command adds an annotation to the default item``() =
        let testText = [|"Q: Is this the real life? Is this just fantasy?"|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 1
        let newCompilerStatus=makeRawModel ret beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations.[0].AnnotationType |> should equal AnnotationTokenType.Question
    [<Test>]
    let ``MISC SINGLE WORD: A simple annotation TODO: command adds an annotation to the default item``() =
        let testText = [|"TODO: Check on Pluto"|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 1
        let newCompilerStatus=makeRawModel ret beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations.[0].AnnotationType |> should equal AnnotationTokenType.ToDo
