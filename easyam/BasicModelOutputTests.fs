﻿module BasicModelOutputTests
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
                        ; "@'mysteries of life'='I have found you'"
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

    // Combining the two tags resets everything, ie @& or &@
    [<Test>]
    let ``BASIC MODEL OUTPUT: tag reset``() =
        let testText = [|
                          "BUSINESS STRUCTURE ABSTRACT TO-BE"
                        ; "  &dogs=4"
                        ; "  Customer"
                        ; "  Vendor"
                        ; "  &cats=7"
                        ; "  App"
                        ; "  @&"
                        ; "  Dog house"
                        |]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 5
        newCompilerStatus.ModelItems.[1].Tags |>should haveLength 1
        newCompilerStatus.ModelItems.[3].Tags |> should haveLength 2
        newCompilerStatus.ModelItems.[4].Tags |> should haveLength 0
    [<Test>]
    let ``BASIC MODEL OUTPUT: multiple tags per line works``() =
        let testText = [|
                          "BUSINESS STRUCTURE ABSTRACT TO-BE"
                        ; "  &dogs=4"
                        ; "  Customer"
                        ; "  Vendor"
                        ; "  &cats=7 &dogs=5 @weasels=9 @microphone"
                        ; "  App"
                        ; "  @mousefart @cats=13 &pointyhead &dogs=5 &weasels=1 @microphone"
                        ; "  Site"
                        |]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 5
        newCompilerStatus.ModelItems.[1].Tags |>should haveLength 1
        newCompilerStatus.ModelItems.[3].Tags |> should haveLength 4
        newCompilerStatus.ModelItems.[4].Tags |> should haveLength 8

    // Sorting and filtering
    let sortFilterFileInfo = getFakeFileInfo()
    let sortFilterModel= [|
            ""
        ; "MASTER BACKLOG"
        ; "  @Sprint=1 &Points=7 &ApprovalStatus=Tentative &Rank=3 &'Review Date'='03/01/2020'"
        ; "  Make the world a better place"
        ; "  @Sprint=2 &Points=5  &'Review Date'='01/01/2020' &ApprovalStatus=Confirmed &Rank=1"
        ; "  Raise money for orphans"
        ; "   &'Review Date'='04/01/2020' @Sprint=2 &Points=12 &ApprovalStatus=Tentative &Rank=2"
        ; "  Be the change you want to see"
        ; "  "
        ; "  Be good to yourself"
        ; "MASTER DOMAIN MODEL"
        ; "  &ApprovalStatus=Tentative  &'Review Date'='03/11/2020' @ConflictingInformation"
        ; "  Orphans"
        ; "  @&"
        ; "  &ApprovalStatus=Confirmed  &'Review Date'='02/10/2020'"
        ; "  Children"
        ; "  &ApprovalStatus=Confirmed &'Review Date'='03/01/2019'"
        ; "  Countries"
        ; "  "
        ; "  Donations"
        ; "MASTER SUPPLEMENTAL MODEL"
        ; "   &'Review Date'='01/01/2020' &ApprovalStatus=Confirmed &Rank=3"
        ; "  Be faster than an attack of bees"
        ; "  &ApprovalStatus=Tentative &Rank=2  &'Review Date'='03/02/2020' @ConflictingInformation"
        ; "  Never stay too long"
        ; "  @&"
        ; "  &ApprovalStatus=Confirmed  &'Review Date'='9/19/2019' &Rank=1"
        ; "  Always be nice"
        ; "  "
        ; "  Be as honest as Abe Lincoln"
        |]
    let sortFilterListToProcess = [|(sortFilterFileInfo,sortFilterModel)|]
    let sortFilterProcessedIncomingLines, sortFilterCompilerReturn = bulkFileLineProcessing sortFilterListToProcess
    let sortFilterCompilerStatus=makeRawModel sortFilterProcessedIncomingLines sortFilterCompilerReturn

    [<Test>]
    let ``BASIC MODEL OUTPUT: sort by tag integer value``() =
        let sortParameter =
            {
                TagOrAttName=Tag
                ThingToInspect="Rank"
                ThingToTryToConvertItTo=Int
                SortOrder=Descending
            }
        let sortedModel = sortModelByOneParameter sortFilterCompilerStatus.ModelItems sortParameter
        sortedModel.Length |> should equal 13
        sortedModel.[1].Description |> should equal "Make the world a better place"
        sortedModel.[12].Description |> should equal ""
    [<Test>]
    let ``BASIC MODEL OUTPUT: sort by tag date value``() =
        let sortParameter =
            {
                TagOrAttName=Tag
                ThingToInspect="Review Date"
                ThingToTryToConvertItTo=DateTime
                SortOrder=Ascending
            }
        let sortedModel = sortModelByOneParameter sortFilterCompilerStatus.ModelItems sortParameter
        sortedModel.Length |> should equal 13
        sortedModel.[1].Description |> should equal "Countries"
        sortedModel.[12].Description |> should equal "Be good to yourself"
    [<Test>]
    let ``BASIC MODEL OUTPUT: sort by basic attribute``() =
        let sortParameter =
            {
                TagOrAttName=AttName
                ThingToInspect="Description"
                ThingToTryToConvertItTo=None
                SortOrder=Ascending
            }
        let sortedModel = sortModelByOneParameter sortFilterCompilerStatus.ModelItems sortParameter
        sortedModel.Length |> should equal 13
        sortedModel.[1].Description |> should equal "Always be nice"
        sortedModel.[12].Description |> should equal "Raise money for orphans"

    [<Test>]
    let ``BASIC MODEL OUTPUT: filter by Genre positive``() =
        let checkParameter =
            {
                TagOrAttName=Tag
                ThingToInspect=""
                ThingToTryToConvertItTo=None
                SortOrder=Ascending
            }
        let filterParameter =
            {
                Genre=Genres.Business
                Bucket=Buckets.Unknown
                AbstractionLevel=AbstractionLevels.Unknown
                TemporalIndicator=TemporalIndicators.Unknown
                CheckValue=checkParameter
                FromVal=""
                ToVal=""
            }
        let filteredModel = filterModelByOneParameter sortFilterCompilerStatus.ModelItems filterParameter
        filteredModel.Length |> should equal 12
    [<Test>]
    let ``BASIC MODEL OUTPUT: filter by Genre negative``() =
        let checkParameter =
            {
                TagOrAttName=Tag
                ThingToInspect=""
                ThingToTryToConvertItTo=None
                SortOrder=Ascending
            }
        let filterParameter =
            {
                Genre=Genres.System
                Bucket=Buckets.Unknown
                AbstractionLevel=AbstractionLevels.Unknown
                TemporalIndicator=TemporalIndicators.Unknown
                CheckValue=checkParameter
                FromVal=""
                ToVal=""
            }
        let filteredModel = filterModelByOneParameter sortFilterCompilerStatus.ModelItems filterParameter
        filteredModel.Length |> should equal 0
    [<Test>]
    let ``BASIC MODEL OUTPUT: filter by Bucket``() =
        let checkParameter =
            {
                TagOrAttName=Tag
                ThingToInspect=""
                ThingToTryToConvertItTo=None
                SortOrder=Ascending
            }
        let filterParameter =
            {
                Genre=Genres.Unknown
                Bucket=Buckets.Behavior
                AbstractionLevel=AbstractionLevels.Unknown
                TemporalIndicator=TemporalIndicators.Unknown
                CheckValue=checkParameter
                FromVal=""
                ToVal=""
            }
        let filteredModel = filterModelByOneParameter sortFilterCompilerStatus.ModelItems filterParameter
        filteredModel.Length |> should equal 4
    [<Test>]
    let ``BASIC MODEL OUTPUT: filter by AbstractionLevel positive``() =
        let checkParameter =
            {
                TagOrAttName=Tag
                ThingToInspect=""
                ThingToTryToConvertItTo=None
                SortOrder=Ascending
            }
        let filterParameter =
            {
                Genre=Genres.Business
                Bucket=Buckets.Unknown
                AbstractionLevel=AbstractionLevels.Abstract
                TemporalIndicator=TemporalIndicators.Unknown
                CheckValue=checkParameter
                FromVal=""
                ToVal=""
            }
        let filteredModel = filterModelByOneParameter sortFilterCompilerStatus.ModelItems filterParameter
        filteredModel.Length |> should equal 12
    [<Test>]
    let ``BASIC MODEL OUTPUT: filter by AbstractionLevel negative``() =
        let checkParameter =
            {
                TagOrAttName=Tag
                ThingToInspect=""
                ThingToTryToConvertItTo=None
                SortOrder=Ascending
            }
        let filterParameter =
            {
                Genre=Genres.Unknown
                Bucket=Buckets.Unknown
                AbstractionLevel=AbstractionLevels.Realized
                TemporalIndicator=TemporalIndicators.Unknown
                CheckValue=checkParameter
                FromVal=""
                ToVal=""
            }
        let filteredModel = filterModelByOneParameter sortFilterCompilerStatus.ModelItems filterParameter
        filteredModel.Length |> should equal 0
    [<Test>]
    let ``BASIC MODEL OUTPUT: filter by Temporal positive``() =
        let checkParameter =
            {
                TagOrAttName=Tag
                ThingToInspect=""
                ThingToTryToConvertItTo=None
                SortOrder=Ascending
            }
        let filterParameter =
            {
                Genre=Genres.Business
                Bucket=Buckets.Unknown
                AbstractionLevel=AbstractionLevels.Unknown
                TemporalIndicator=TemporalIndicators.ToBe
                CheckValue=checkParameter
                FromVal=""
                ToVal=""
            }
        let filteredModel = filterModelByOneParameter sortFilterCompilerStatus.ModelItems filterParameter
        filteredModel.Length |> should equal 12
    [<Test>]
    let ``BASIC MODEL OUTPUT: filter by Temporal negative``() =
        let checkParameter =
            {
                TagOrAttName=Tag
                ThingToInspect=""
                ThingToTryToConvertItTo=None
                SortOrder=Ascending
            }
        let filterParameter =
            {
                Genre=Genres.Unknown
                Bucket=Buckets.Unknown
                AbstractionLevel=AbstractionLevels.Unknown
                TemporalIndicator=TemporalIndicators.Was
                CheckValue=checkParameter
                FromVal=""
                ToVal=""
            }
        let filteredModel = filterModelByOneParameter sortFilterCompilerStatus.ModelItems filterParameter
        filteredModel.Length |> should equal 0
    [<Test>]
    let ``BASIC MODEL OUTPUT: filter by date range``() =
        let checkParameter =
            {
                TagOrAttName=Tag
                ThingToInspect="Review Date"
                ThingToTryToConvertItTo=DateTime
                SortOrder=Ascending
            }
        let filterParameter =
            {
                Genre=Genres.Unknown
                Bucket=Buckets.Unknown
                AbstractionLevel=AbstractionLevels.Unknown
                TemporalIndicator=TemporalIndicators.Unknown
                CheckValue=checkParameter
                FromVal="1/1/2019"
                ToVal="12/31/2019"
            }
        let filteredModel = filterModelByOneParameter sortFilterCompilerStatus.ModelItems filterParameter
        filteredModel.Length |> should equal 4
    [<Test>]
    let ``BASIC MODEL OUTPUT: filter by string range``() =
        let checkParameter =
            {
                TagOrAttName=AttName
                ThingToInspect="Description"
                ThingToTryToConvertItTo=None
                SortOrder=Ascending
            }
        let filterParameter =
            {
                Genre=Genres.Unknown
                Bucket=Buckets.Unknown
                AbstractionLevel=AbstractionLevels.Unknown
                TemporalIndicator=TemporalIndicators.Unknown
                CheckValue=checkParameter
                FromVal="Apple"
                ToVal="Cat"
            }
        let filteredModel = filterModelByOneParameter sortFilterCompilerStatus.ModelItems filterParameter
        filteredModel.Length |> should equal 4
    [<Test>]
    let ``BASIC MODEL OUTPUT: filter by integer range``() =
        let checkParameter =
            {
                TagOrAttName=Tag
                ThingToInspect="Points"
                ThingToTryToConvertItTo=Int
                SortOrder=Ascending
            }
        let filterParameter =
            {
                Genre=Genres.Unknown
                Bucket=Buckets.Unknown
                AbstractionLevel=AbstractionLevels.Unknown
                TemporalIndicator=TemporalIndicators.Unknown
                CheckValue=checkParameter
                FromVal="0"
                ToVal="6"
            }
        let filteredModel = filterModelByOneParameter sortFilterCompilerStatus.ModelItems filterParameter
        filteredModel.Length |> should equal 1
        filteredModel.[0].Description |> should equal "Raise money for orphans"
    [<Test>]
    let ``BASIC MODEL OUTPUT: filter by presence of a tag``() =
        let checkParameter =
            {
                TagOrAttName=Tag
                ThingToInspect="ConflictingInformation"
                ThingToTryToConvertItTo=None
                SortOrder=Ascending
            }
        let filterParameter =
            {
                Genre=Genres.Unknown
                Bucket=Buckets.Unknown
                AbstractionLevel=AbstractionLevels.Unknown
                TemporalIndicator=TemporalIndicators.Unknown
                CheckValue=checkParameter
                FromVal=""
                ToVal=""
            }
        let filteredModel = filterModelByOneParameter sortFilterCompilerStatus.ModelItems filterParameter
        filteredModel.Length |> should equal 2
    [<Test>]
    let ``BASIC MODEL OUTPUT: combine filter and sort``() =
        let checkParameter =
            {
                TagOrAttName=Tag
                ThingToInspect=""
                ThingToTryToConvertItTo=None
                SortOrder=Ascending
            }
        let filterParameter =
            {
                Genre=Genres.Business
                Bucket=Buckets.Behavior
                AbstractionLevel=AbstractionLevels.Unknown
                TemporalIndicator=TemporalIndicators.Unknown
                CheckValue=checkParameter
                FromVal=""
                ToVal=""
            }
        let filteredModel = filterModelByOneParameter sortFilterCompilerStatus.ModelItems filterParameter
        let sortParameter =
            {
                TagOrAttName=Tag
                ThingToInspect="Rank"
                ThingToTryToConvertItTo=Int
                SortOrder=Ascending
            }
        let sortedModel = sortModelByOneParameter filteredModel sortParameter
        sortedModel.Length |> should equal 4
        sortedModel.[0].Description |> should equal "Raise money for orphans"

