module BasicModelOutputTests
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
    let ``BASIC MODEL OUTPUT: name-value tag works on simple line``() =
        let testText = [|
                          "BUSINESS STRUCTURE ABSTRACT TO-BE"
                        ; "@dogs=7"
                        ; "  Customer"
                        ; "      //People who give us money"
                        |]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 2
        newCompilerStatus.ModelItems.[1].Tags |> should haveLength 1
        newCompilerStatus.ModelItems.[1].Tags.[0].Key |> should equal "dogs"
        newCompilerStatus.ModelItems.[1].Tags.[0].Value |> should equal "7"

    [<Test>]
    let ``BASIC MODEL OUTPUT: tags using commas1``() =
        let testText = [|
                          "BUSINESS STRUCTURE ABSTRACT TO-BE"
                        ; "@dogs=7,9"
                        ; "  Customer"
                        |]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 2
        newCompilerStatus.ModelItems.[1].Tags |> should haveLength 2
        newCompilerStatus.ModelItems.[1].Tags.[0].Key |> should equal "dogs"
        newCompilerStatus.ModelItems.[1].Tags.[0].Value |> should equal "7"
        newCompilerStatus.ModelItems.[1].Tags.[1].Key |> should equal "dogs"
        newCompilerStatus.ModelItems.[1].Tags.[1].Value |> should equal "9"
    [<Test>]
    let ``BASIC MODEL OUTPUT: tags using commas2``() =
        let testText = [|
                          "BUSINESS STRUCTURE ABSTRACT TO-BE"
                        ; "@dogs,cats=9"
                        ; "  Customer"
                        |]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 2
        newCompilerStatus.ModelItems.[1].Tags |> should haveLength 2
        newCompilerStatus.ModelItems.[1].Tags.[0].Key |> should equal "dogs"
        newCompilerStatus.ModelItems.[1].Tags.[0].Value |> should equal "9"
        newCompilerStatus.ModelItems.[1].Tags.[1].Key |> should equal "cats"
        newCompilerStatus.ModelItems.[1].Tags.[1].Value |> should equal "9"
    [<Test>]
    let ``BASIC MODEL OUTPUT: tags using commas3``() =
        let testText = [|
                          "BUSINESS STRUCTURE ABSTRACT TO-BE"
                        ; "@mouse,skunk=11,15"
                        ; "  Customer"
                        |]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 2
        newCompilerStatus.ModelItems.[1].Tags |> should haveLength 4
        newCompilerStatus.ModelItems.[1].Tags.[0].Key |> should equal "mouse"
        newCompilerStatus.ModelItems.[1].Tags.[0].Value |> should equal "11"
        newCompilerStatus.ModelItems.[1].Tags.[1].Key |> should equal "mouse"
        newCompilerStatus.ModelItems.[1].Tags.[1].Value |> should equal "15"
        newCompilerStatus.ModelItems.[1].Tags.[2].Key |> should equal "skunk"
        newCompilerStatus.ModelItems.[1].Tags.[2].Value |> should equal "11"
        newCompilerStatus.ModelItems.[1].Tags.[3].Key |> should equal "skunk"
        newCompilerStatus.ModelItems.[1].Tags.[3].Value |> should equal "15"
    [<Test>]
    let ``BASIC MODEL OUTPUT: tags work on pre-existing items``() =
        let testText = [|
                          "BUSINESS STRUCTURE ABSTRACT TO-BE"
                        ; "  Customer"
                        ; "  App"
                        ; "  Dog house"
                        ;  "MASTER BACKLOG"
                        ;  "Do some things"
                        ;  "Do some other things"
                        ; "@mysteries of life=I have found you"
                        ; "MASTER DOMAIN MODEL"
                        ; "  Customer"
                        |]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 6
        newCompilerStatus.ModelItems.[1].Tags |> should haveLength 1
        newCompilerStatus.ModelItems.[1].Tags.[0].Key |> should equal "mysteries of life"
        newCompilerStatus.ModelItems.[1].Tags.[0].Value |> should equal "I have found you"
    [<Test>]
    let ``BASIC MODEL OUTPUT: tags accumulate and carry across different kinds of items``() =
        let testText = [|
                          "BUSINESS STRUCTURE ABSTRACT TO-BE"
                        ; "  @dogs=4"
                        ; "  Customer"
                        ; "  Vendor"
                        ; "  @cats=7"
                        ; "  App"
                        ; "  Dog house"
                        ;  "MASTER BACKLOG"
                        ;  "  Do some things"
                        ; "  @monkeylike=banana"
                        ;  "  Do some other things"
                        ;  "  Do some other things"
                        ; "MASTER SUPPLEMENTALS"
                        ; "@working=no"
                        ; "  Be good to yourself"
                        ; "  Never leave a lonely heart"
                        |]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 10
        newCompilerStatus.ModelItems.[1].Tags |>should haveLength 1
        newCompilerStatus.ModelItems.[3].Tags |>should haveLength 2
        newCompilerStatus.ModelItems.[6].Tags |>should haveLength 3
        newCompilerStatus.ModelItems.[8].Tags |>should haveLength 4
        newCompilerStatus.ModelItems.[9].Tags |>should haveLength 4
    [<Test>]
    let ``BASIC MODEL OUTPUT: tags don't carry over file breaks``() =
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          "BUSINESS STRUCTURE ABSTRACT TO-BE"
                        ; "  @dogs=4"
                        ; "  Customer"
                        ; "  Vendor"
                        ; "  @cats=7"
                        ; "  App"
                        ; "  Dog house"
                        |]
        let fileInfo2 = getFakeFileInfo()
        let testText2 = [|
                          "MASTER BACKLOG"
                        ;  "  Do some things"
                        ; "  @monkeylike=banana"
                        ;  "  Do some other things"
                        ;  "  Do some other things"
                        ; "MASTER SUPPLEMENTALS"
                        ; "@working=no"
                        ; "  Be good to yourself"
                        ; "  Never leave a lonely heart"
                        |]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 10
        newCompilerStatus.ModelItems.[1].Tags |>should haveLength 1
        newCompilerStatus.ModelItems.[3].Tags |>should haveLength 2
        newCompilerStatus.ModelItems.[5].Tags |>should haveLength 0
        newCompilerStatus.ModelItems.[6].Tags |>should haveLength 1
        newCompilerStatus.ModelItems.[9].Tags |>should haveLength 2
    // the @ tag just throws it all in there. The & tag overwrites keys if they already exist
    [<Test>]
    let ``BASIC MODEL OUTPUT: tag overwrite works``() =
        let testText = [|
                          "BUSINESS STRUCTURE ABSTRACT TO-BE"
                        ; "  &dogs=4"
                        ; "  Customer"
                        ; "  Vendor"
                        ; "  &cats=7"
                        ; "  App"
                        ; "  Dog house"
                        ;  "MASTER BACKLOG"
                        ;  "  Do some things"
                        ; "  &monkeylike=banana"
                        ;  "  Do some other things"
                        ;  "  Do some other things"
                        ; "MASTER DOMAIN MODEL"
                        ; "&dogs=barking"
                        ; "  Vendor"
                        ; "&cats=meow"
                        ; "  Customer"
                        ; "  &monkeylike=acorns"
                        |]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 7
        newCompilerStatus.ModelItems.[1].Tags |>should haveLength 3
        newCompilerStatus.ModelItems.[1].Tags.[0].Key |> should equal "dogs"
        newCompilerStatus.ModelItems.[1].Tags.[0].Value |> should equal "barking"
        newCompilerStatus.ModelItems.[1].Tags.[1].Key |> should equal "cats"
        newCompilerStatus.ModelItems.[1].Tags.[1].Value |> should equal "meow"
        newCompilerStatus.ModelItems.[1].Tags.[2].Key |> should equal "monkeylike"
        newCompilerStatus.ModelItems.[1].Tags.[2].Value |> should equal "banana"

        newCompilerStatus.ModelItems.[3].Tags |>should haveLength 2
