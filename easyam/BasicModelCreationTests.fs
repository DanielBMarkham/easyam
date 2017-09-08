module BasicModelCreationTests
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
        let testText = [|"TODOS"; "    Attack lunar base"; "    Gain superpowers"; "    Discover the atom"|]
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
        newCompilerStatus.CurrentLocation.InHDDMode |> should equal false
    [<Test>]
    let ``BASIC MODEL CREATION: Setting up the pointer and then adding a HDD resets it``() =
        let testText = [|"REALIZED AS-IS"; "STRUCTURE META"; "HDD"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 1
        newCompilerStatus.CurrentLocation.AbstractionLevel |> should equal AbstractionLevels.None
        newCompilerStatus.CurrentLocation.Bucket |> should equal Buckets.None
        newCompilerStatus.CurrentLocation.Genre |> should equal Genres.None
        newCompilerStatus.CurrentLocation.TemporalIndicator |> should equal TemporalIndicators.None
        newCompilerStatus.CurrentLocation.InHDDMode |> should equal true


    [<Test>]
    let ``BASIC MODEL CREATION: Behavior plust text on next line creates a new default item``() =
        let testText = [|"BEHAVIOR"; "Do some things"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 2
        newCompilerStatus.CurrentLocation.Bucket |> should equal Buckets.Behavior
        // These should be the defaults when we have a behavior alone. It allows direct entry of MUS
        newCompilerStatus.CurrentLocation.AbstractionLevel |> should equal AbstractionLevels.Abstract
        newCompilerStatus.CurrentLocation.Genre |> should equal Genres.Business
        newCompilerStatus.CurrentLocation.TemporalIndicator |> should equal TemporalIndicators.ToBe
    [<Test>]
    let ``BASIC MODEL CREATION: Behavior plus a couple freetext lines means new MUS``() =
        let testText = [|"BEHAVIOR"; "    Do stuff"; "    Do stuff more"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 3
        newCompilerStatus.CurrentLocation.Bucket |> should equal Buckets.Behavior
        newCompilerStatus.CurrentLocation.AbstractionLevel |> should equal AbstractionLevels.Abstract
        newCompilerStatus.CurrentLocation.Genre |> should equal Genres.Business
        newCompilerStatus.CurrentLocation.TemporalIndicator |> should equal TemporalIndicators.ToBe
        newCompilerStatus.ModelItems.[1].Description |> should equal "Do stuff"
        newCompilerStatus.ModelItems.[2].Description |> should equal "Do stuff more"
    [<Test>]
    let ``BASIC MODEL CREATION: A couple of structure items have right defaults and Ids don't equal``() =
        let testText = [|"STRUCTURE"; "    Customer"; "    Staff"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 3
        newCompilerStatus.CurrentLocation.Bucket |> should equal Buckets.Structure
        newCompilerStatus.CurrentLocation.AbstractionLevel |> should equal AbstractionLevels.Abstract
        newCompilerStatus.CurrentLocation.Genre |> should equal Genres.Business
        newCompilerStatus.CurrentLocation.TemporalIndicator |> should equal TemporalIndicators.ToBe
        newCompilerStatus.ModelItems.[1].Description |> should equal "Customer"
        newCompilerStatus.ModelItems.[2].Description |> should equal "Staff"
        newCompilerStatus.ModelItems.[1].Id=newCompilerStatus.ModelItems.[2].Id |> should equal false
    [<Test>]
    let ``BASIC MODEL CREATION: Multiple Supplementals on a line work``() =
        let testText = [|"SUPPLEMENTALS: be young, be strong, be foolish"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 4
        newCompilerStatus.CurrentLocation.Bucket |> should equal Buckets.Supplemental
        newCompilerStatus.CurrentLocation.AbstractionLevel |> should equal AbstractionLevels.Abstract
        newCompilerStatus.CurrentLocation.Genre |> should equal Genres.Business
        newCompilerStatus.CurrentLocation.TemporalIndicator |> should equal TemporalIndicators.ToBe
        newCompilerStatus.ModelItems.[1].Description |> should equal "be young"
        newCompilerStatus.ModelItems.[2].Description |> should equal "be strong"
        newCompilerStatus.ModelItems.[3].Description |> should equal "be foolish"
        newCompilerStatus.ModelItems.[2].Id=newCompilerStatus.ModelItems.[3].Id |> should equal false

    [<Test>]
    let ``BASIC MODEL CREATION: A new MUS can have annotations``() =
        let testText = [|"BUSINESS BEHAVIOR ABSTRACT TO-BE"; "  Balance Account"; "    Q: Which account are we talking about?"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 2
        newCompilerStatus.ModelItems.[1].Description |> should equal "Balance Account"
        newCompilerStatus.ModelItems.[1].Annotations.Length |> should equal 1
        let annotationTokenType, tokenValue=newCompilerStatus.ModelItems.[1].Annotations.[0]
        annotationTokenType |> should equal ANNOTATION_TOKEN_TYPE.Question
    [<Test>]
    let ``BASIC MODEL CREATION: Multiple annotations for an item in a list works``() =
        let testText = [|"SYSTEM STRUCTURE REALIZED AS-IS"; "  Customer"; "    Q: What's a customer?"; "    // Not sure of the weasel"; "    TO-DO: Look up 'customer' in dictionary"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 2
        newCompilerStatus.ModelItems.[1].Description |> should equal "Customer"
        newCompilerStatus.ModelItems.[1].Annotations.Length |> should equal 3
        let annotationTokenType, tokenValue=newCompilerStatus.ModelItems.[1].Annotations.[0]
        annotationTokenType |> should equal ANNOTATION_TOKEN_TYPE.Question
        let annotationTokenType, tokenValue=newCompilerStatus.ModelItems.[1].Annotations.[1]
        annotationTokenType |> should equal ANNOTATION_TOKEN_TYPE.Note
        let annotationTokenType, tokenValue=newCompilerStatus.ModelItems.[1].Annotations.[2]
        annotationTokenType |> should equal ANNOTATION_TOKEN_TYPE.ToDo
    [<Test>]
    let ``BASIC MODEL CREATION: Multiple master items each with annotations``() =
        let testText = [|
                              "BUSINESS SUPPLEMENTAL ABSTRACT TO-BE"
                            ; "  Be nice to customers"
                            ; "      Q: What's a customer?"
                            ; "  Don't fear the reaper"
                            ; "  WORK: Ate a reaper last night"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 3
        newCompilerStatus.ModelItems.[1].Description |> should equal "Be nice to customers"
        newCompilerStatus.ModelItems.[1].Annotations.Length |> should equal 1
        fst newCompilerStatus.ModelItems.[1].Annotations.[0] |> should equal ANNOTATION_TOKEN_TYPE.Question
        newCompilerStatus.ModelItems.[2].Description |> should equal "Don't fear the reaper"
        newCompilerStatus.ModelItems.[2].Annotations.Length |> should equal 1
        fst newCompilerStatus.ModelItems.[2].Annotations.[0] |> should equal ANNOTATION_TOKEN_TYPE.Work
    [<Test>]
    let ``BASIC MODEL CREATION: Changing item types``() =
        let testText = [|"BUSINESS BEHAVIOR ABSTRACT TO-BE"; "  Reconcile account"; "      //Not the joo-joo beans"; "SYSTEM STRUCTURE REALIZED AS-IS"; "    Customer transport page"; "        TO-DO: Buy a nice truck"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 3
        newCompilerStatus.ModelItems.[1].Description |> should equal "Reconcile account"
        newCompilerStatus.ModelItems.[1].Annotations.Length |> should equal 1
        fst newCompilerStatus.ModelItems.[1].Annotations.[0] |> should equal ANNOTATION_TOKEN_TYPE.Note
        newCompilerStatus.ModelItems.[2].Description |> should equal "Customer transport page"
        newCompilerStatus.ModelItems.[2].Annotations.Length |> should equal 1
        fst newCompilerStatus.ModelItems.[2].Annotations.[0] |> should equal ANNOTATION_TOKEN_TYPE.ToDo

    [<Test>]
    let ``BASIC MODEL CREATION: Context resets over file change``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|"Here's some text"; "BUSINESS BEHAVIOR ABSTRACT TO-BE"; "  Reconcile account"; "      Pull the plug"|]
        let testText2 = [|"A new beginning"; "SYSTEM SUPPLEMENTAL REALIZED WAS"; "  Everybody gets a sticker"; "      The stickers blow up"|]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 5


    