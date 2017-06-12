module BasicModelCreationTests
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
    let ``BASIC MODEL CREATION: A simple annotation TODO: command adds an annotation to the default item``() =
        let testText = [|"TODO: Check on Pluto"|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 1
        let newCompilerStatus=makeRawModel ret beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations |> should haveLength 1
        fst newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal ANNOTATION_TOKEN_TYPE.ToDo
    [<Test>]
    let ``BASIC MODEL CREATION: Two annotations go in the right buckets``() =
        let testText = [|"TODO: Check on Pluto"; "It should be a planet by now"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations |> should haveLength 2
        fst newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal ANNOTATION_TOKEN_TYPE.ToDo
        fst newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal ANNOTATION_TOKEN_TYPE.Note
    [<Test>]
    let ``BASIC MODEL CREATION: Freetext followed by a question registers correctly``() =
        let testText = [|"This is the same things they said last time. Q: Wonder why they repeat themselves?"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations |> should haveLength 2
        fst newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal ANNOTATION_TOKEN_TYPE.Note
        snd newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal "This is the same things they said last time."
        fst newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal ANNOTATION_TOKEN_TYPE.Question
        snd newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal "Wonder why they repeat themselves?"
    [<Test>]
    let ``BASIC MODEL CREATION: Question line followed by freetext registers correctly``() =
        let testText = [|"Q: Why all the salsa?";"Salsa might be the key to a long life."|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations |> should haveLength 2
        fst newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal ANNOTATION_TOKEN_TYPE.Question
        snd newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal "Why all the salsa?"
        fst newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal ANNOTATION_TOKEN_TYPE.Note
        snd newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal "Salsa might be the key to a long life."
    [<Test>]
    let ``BASIC MODEL CREATION: TODO put between two freetext lines works correctly``() =
        let testText = [|"Many a time";"TODO: Find out how many";"Sheep jump over the moon"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations |> should haveLength 3
        fst newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal ANNOTATION_TOKEN_TYPE.Note
        snd newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal "Many a time"
        fst newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal ANNOTATION_TOKEN_TYPE.ToDo
        snd newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal "Find out how many"
        fst newCompilerStatus.ModelItems.[0].Annotations.[2] |> should equal ANNOTATION_TOKEN_TYPE.Note
        snd newCompilerStatus.ModelItems.[0].Annotations.[2] |> should equal "Sheep jump over the moon"
    [<Test>]
    let ``BASIC MODEL CREATION: Multiple notes on the same line works``() =
        let testText = [|"NOTES: The fever is hot, the beer is cold, the chickens are squawking in the barnyard"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations |> should haveLength 3
        fst newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal ANNOTATION_TOKEN_TYPE.Note
        snd newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal "The fever is hot"
        fst newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal ANNOTATION_TOKEN_TYPE.Note
        snd newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal "the beer is cold"
        fst newCompilerStatus.ModelItems.[0].Annotations.[2] |> should equal ANNOTATION_TOKEN_TYPE.Note
        snd newCompilerStatus.ModelItems.[0].Annotations.[2] |> should equal "the chickens are squawking in the barnyard"
    [<Test>]
    let ``BASIC MODEL CREATION: TODOS on multiple lines work``() =
        let testText = [|"TODOS: Mix pumpkin juice"; "Attack lunar base"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations |> should haveLength 2
        fst newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal ANNOTATION_TOKEN_TYPE.ToDo
        snd newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal "Mix pumpkin juice"
        fst newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal ANNOTATION_TOKEN_TYPE.ToDo
        snd newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal "Attack lunar base"
    [<Test>]
    let ``BASIC MODEL CREATION: TODO by itself followed by list works``() =
        let testText = [|"TODOS: "; "    Attack lunar base"; "    Gain superpowers"; "    Discover the atom"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations |> should haveLength 3
        fst newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal ANNOTATION_TOKEN_TYPE.ToDo
        snd newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal "Attack lunar base"
        fst newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal ANNOTATION_TOKEN_TYPE.ToDo
        snd newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal "Gain superpowers"
        fst newCompilerStatus.ModelItems.[0].Annotations.[2] |> should equal ANNOTATION_TOKEN_TYPE.ToDo
        snd newCompilerStatus.ModelItems.[0].Annotations.[2] |> should equal "Discover the atom"
    [<Test>]
    let ``BASIC MODEL CREATION: Two multiples in a row works``() =
        let testText = [|"WORKS: "; "    Paved driveway"; "    Fixed roof"; "QUESTIONS:"; "    Why zero?";"    Eggs?"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations |> should haveLength 4
        fst newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal ANNOTATION_TOKEN_TYPE.Work
        snd newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal "Paved driveway"
        fst newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal ANNOTATION_TOKEN_TYPE.Work
        snd newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal "Fixed roof"
        fst newCompilerStatus.ModelItems.[0].Annotations.[2] |> should equal ANNOTATION_TOKEN_TYPE.Question
        snd newCompilerStatus.ModelItems.[0].Annotations.[2] |> should equal "Why zero?"
        fst newCompilerStatus.ModelItems.[0].Annotations.[3] |> should equal ANNOTATION_TOKEN_TYPE.Question
        snd newCompilerStatus.ModelItems.[0].Annotations.[3] |> should equal "Eggs?"
    [<Test>]
    let ``BASIC MODEL CREATION: Simple annotation followed by complex annotation followed by multiple works``() =
        let testText = [|"Here's some freeform text"; "TODOS: do stuff, do more stuff, do nothing"; "QUESTIONS: "; "What is this?"; "    Why are we here?"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.ModelItems.[0].Annotations |> should haveLength 6
        fst newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal ANNOTATION_TOKEN_TYPE.Note
        snd newCompilerStatus.ModelItems.[0].Annotations.[0] |> should equal "Here's some freeform text"
        fst newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal ANNOTATION_TOKEN_TYPE.ToDo
        snd newCompilerStatus.ModelItems.[0].Annotations.[1] |> should equal "do stuff"
        fst newCompilerStatus.ModelItems.[0].Annotations.[2] |> should equal ANNOTATION_TOKEN_TYPE.ToDo
        snd newCompilerStatus.ModelItems.[0].Annotations.[2] |> should equal "do more stuff"
        fst newCompilerStatus.ModelItems.[0].Annotations.[3] |> should equal ANNOTATION_TOKEN_TYPE.ToDo
        snd newCompilerStatus.ModelItems.[0].Annotations.[3] |> should equal "do nothing"
        fst newCompilerStatus.ModelItems.[0].Annotations.[4] |> should equal ANNOTATION_TOKEN_TYPE.Question
        snd newCompilerStatus.ModelItems.[0].Annotations.[4] |> should equal "What is this?"
        fst newCompilerStatus.ModelItems.[0].Annotations.[5] |> should equal ANNOTATION_TOKEN_TYPE.Question
        snd newCompilerStatus.ModelItems.[0].Annotations.[5] |> should equal "Why are we here?"


    [<Test>]
    let ``BASIC MODEL CREATION: Behavior tag changes the Bucket pointer``() =
        let testText = [|"BEHAVIOR"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.CurrentLocation.Bucket |> should equal Buckets.Behavior
    [<Test>]
    let ``BASIC MODEL CREATION: Two different Temporal tags results to the last one``() =
        let testText = [|"TO-BE AS-IS"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.CurrentLocation.TemporalIndicator |> should equal TemporalIndicators.AsIs
    [<Test>]
    let ``BASIC MODEL CREATION: Two different genre tags on different lines results to last one``() =
        let testText = [|"SYSTEM"; "META"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.CurrentLocation.Genre |> should equal Genres.Meta
    [<Test>]
    let ``BASIC MODEL CREATION: Two different Abstraction levels separated by a note works``() =
        let testText = [|"ABSTRACT"; "// What is this supposed to do anyway"; "REALIZED"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.CurrentLocation.AbstractionLevel |> should equal AbstractionLevels.Realized
        newCompilerStatus.ModelItems.[0].Annotations.Length |> should equal 1
    [<Test>]
    let ``BASIC MODEL CREATION: Multi-part pointer movement on one line works``() =
        let testText = [|"SYSTEM BEHAVIOR ABSTRACT TO-BE"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.CurrentLocation.AbstractionLevel |> should equal AbstractionLevels.Abstract
        newCompilerStatus.CurrentLocation.Bucket |> should equal Buckets.Behavior
        newCompilerStatus.CurrentLocation.Genre |> should equal Genres.System
        newCompilerStatus.CurrentLocation.TemporalIndicator |> should equal TemporalIndicators.ToBe
    [<Test>]
    let ``BASIC MODEL CREATION: Multi-part pointer movement split across two lines works``() =
        let testText = [|"REALIZED AS-IS"; "STRUCTURE META"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.CurrentLocation.AbstractionLevel |> should equal AbstractionLevels.Realized
        newCompilerStatus.CurrentLocation.Bucket |> should equal Buckets.Structure
        newCompilerStatus.CurrentLocation.Genre |> should equal Genres.Meta
        newCompilerStatus.CurrentLocation.TemporalIndicator |> should equal TemporalIndicators.AsIs

        



    //[<Test>]
    //let ``MISC SINGLE WORD: Two lines don't have the same ID``() =
    //    let testText = [|"Q: Is this the real life? Is this just fantasy?";"Caught in a landslide, no escape from reality"|]
    //    let ret = setupCompilationScenario 0 0 0 testText
    //    ret |> should haveLength 2
    //    let rawModelItems=makeRawModel ret beginningCompilerStatus
    //    rawModelItems.ModelItems |> should haveLength 2
    //    rawModelItems.ModelItems.[0].Id=rawModelItems.ModelItems.[1].Id |> should equal false
    //[<Test>]
    //let ``MISC SINGLE WORD: Multiple lines have the correct SourceReferences``() =
    //    let testText = [|"Hi there"; "I'm a complext line. I have many uses NOTE:Like I believe that! Q:Who is talking?"; "I am not a complex line."|]
    //    let ret = setupCompilationScenario 0 0 0 testText
    //    ret |> should haveLength 3
    //    let rawModelItems=makeRawModel ret beginningCompilerStatus
    //    rawModelItems |> should haveLength 5



    //[<Test>]
    //let ``MISC SINGLE WORD: Single line of freetext with question in it works correctly``() =
    //    let testText = [|"The meeting is going well Q: Who is the guy in the yellow hat?"|]
    //    let compilerMessages,modelItems = parseLines testText
    //    modelItems |> should haveLength 2
    //    let objType=modelItems.Item(0).ItemType.ToString()
    //    objType.GetLeft 5 |> should equal "NOTE:"
    //    let objType=modelItems.Item(1).ItemType.ToString()
    //    objType.GetLeft 9 |> should equal "QUESTION:"
