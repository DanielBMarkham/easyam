module initialProcessingOfIncomingFileLinesTest
    open NUnit.Framework
    open FsUnit
    open EasyamParsingEngine


    [<Test>]
    let ``INITIAL PROCESSING: Empty Array compiles to nothing``() =
        let testText = [||]
        let fileLoadingStatus=dummyFileLoadingStatus
        let ret = initialProcessingOfIncomingFileLines fileLoadingStatus testText
        ret |> should haveLength 0

    [<Test>]
    let ``INITIAL PROCESSING: One empty string compiles to nothing``() =
        let testText = [|""|]
        let fileLoadingStatus=dummyFileLoadingStatus
        let ret = initialProcessingOfIncomingFileLines fileLoadingStatus testText
        ret |> should haveLength 0

    [<Test>]
    let ``INITIAL PROCESSING: Mix of empty strings and text strips out blank lines``() =
        let testText = [|"";"test1";"";"test2";"";"";"";"test3";|]
        let fileLoadingStatus=dummyFileLoadingStatus
        let ret = initialProcessingOfIncomingFileLines fileLoadingStatus testText
        ret |> should haveLength 3
        ret.[2].LineText |> should equal "test3"

    [<Test>]
    let ``INITIAL PROCESSING: Mix of empty strings and text strips out blank lines counters update``() =
        let testText = [|"";"test1";"";"test2";"";"";"";"test3";|]
        let fileLoadingStatus=dummyFileLoadingStatus
        let ret = initialProcessingOfIncomingFileLines fileLoadingStatus testText
        ret.[2].FileRawLineNumber |> should equal 8
        ret.[2].SourceRawLineNumber |> should equal 8
        ret.[2].FileEmptyLinesStrippedLineNumber |> should equal 3
        ret.[2].SourceEmptyLinesStrippedLineNumber |> should equal 3


    [<Test>]
    let ``INITIAL PROCESSING: Using a Second File Mix of empty strings and text strips out blank lines counters update``() =
        let testText = [|"";"test1";"";"test2";"";"";"";"test3";|]
        let fileLoadingStatus={dummyFileLoadingStatus with FileNumber=1;IncomingRawLineCount=7;IncomingLineCountWithEmptyLinesDeletedCount=5}
        let ret = initialProcessingOfIncomingFileLines fileLoadingStatus testText
        ret.[2].FileRawLineNumber |> should equal 8
        ret.[2].SourceRawLineNumber |> should equal 15
        ret.[2].FileEmptyLinesStrippedLineNumber |> should equal 3
        ret.[2].SourceEmptyLinesStrippedLineNumber |> should equal 8

    [<Test>]
    let ``INITIAL PROCESSING: No tabs or spaces means no indents``() =
        let testText = [|"";"test1";"";"test2";"";"";"";"test3";|]
        let fileLoadingStatus=dummyFileLoadingStatus
        let ret = initialProcessingOfIncomingFileLines fileLoadingStatus testText
        ret.[2].IndentLevel |> should equal 0
    [<Test>]
    let ``INITIAL PROCESSING: Using tabs as an indent works``() =
        let testText = [|"";"test1";"";"\ttest2";"";"";"";"    test3";|]
        let fileLoadingStatus=dummyFileLoadingStatus
        let ret = initialProcessingOfIncomingFileLines fileLoadingStatus testText
        ret.[1].IndentLevel |> should equal 1
    [<Test>]
    let ``INITIAL PROCESSING: Using spaces as an indent works``() =
        let testText = [|"";"test1";"";"\ttest2";"";"";"";"    test3";|]
        let fileLoadingStatus=dummyFileLoadingStatus
        let ret = initialProcessingOfIncomingFileLines fileLoadingStatus testText
        ret.[2].IndentLevel |> should equal 1
    [<Test>]
    let ``INITIAL PROCESSING: Mixed spaces and tabs work together``() =
        let testText = [|"";"test1";"";"\t    test2";"";"";"";"    \t\ttest3";|]
        let fileLoadingStatus=dummyFileLoadingStatus
        let ret = initialProcessingOfIncomingFileLines fileLoadingStatus testText
        ret.[1].IndentLevel |> should equal 2
    [<Test>]
    let ``INITIAL PROCESSING: Mixed spaces and tabs work together adv``() =
        let testText = [|"";"test1";"";"\t    test2";"";"";"";"    \t\t    \ttest3";|]
        let fileLoadingStatus=dummyFileLoadingStatus
        let ret = initialProcessingOfIncomingFileLines fileLoadingStatus testText
        ret.[2].IndentLevel |> should equal 5
    [<Test>]
    let ``INITIAL PROCESSING: Spaces in the middle of the line do not cause indents``() =
        let testText = [|"";"test1    dogs!";"";"\t    test2";"";"";"";"    \t\t    \ttest3";|]
        let fileLoadingStatus=dummyFileLoadingStatus
        let ret = initialProcessingOfIncomingFileLines fileLoadingStatus testText
        ret.[0].IndentLevel |> should equal 0
    [<Test>]
    let ``INITIAL PROCESSING: Tabs in the middle of the line do not cause indents``() =
        let testText = [|"";"test1\tdogs!";"";"\t    test2";"";"";"";"    \t\t    \ttest3";|]
        let fileLoadingStatus=dummyFileLoadingStatus
        let ret = initialProcessingOfIncomingFileLines fileLoadingStatus testText
        ret.[0].IndentLevel |> should equal 0
    [<Test>]
    let ``INITIAL PROCESSING: Both Spaces and Tabs together in the middle of the line do not cause indents``() =
        let testText = [|"";"test1\t    \t\t    dogs  I like cats    really!\t\ttrust me!";"";"\t    test2";"";"";"";"    \t\t    \ttest3";|]
        let fileLoadingStatus=dummyFileLoadingStatus
        let ret = initialProcessingOfIncomingFileLines fileLoadingStatus testText
        ret.[0].IndentLevel |> should equal 0



