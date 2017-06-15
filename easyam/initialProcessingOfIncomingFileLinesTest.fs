module initialProcessingOfIncomingFileLinesTest
    open NUnit.Framework
    open FsUnit
    open EasyamParsingEngine

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
    let ``INITIAL PROCESSING: Empty Array compiles to nothing``() =
        let testText = [||]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 0

    [<Test>]
    let ``INITIAL PROCESSING: One empty string compiles to nothing``() =
        let testText = [|""|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 0

    [<Test>]
    let ``INITIAL PROCESSING: Mix of empty strings and text strips out blank lines``() =
        let testText = [|"";"test1";"";"test2";"";"";"";"test3";|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret |> should haveLength 3
        ret.[2].LineText |> should equal "test3"

    [<Test>]
    let ``INITIAL PROCESSING: Mix of empty strings and text strips out blank lines counters update``() =
        let testText = [|"";"test1";"";"test2";"";"";"";"test3";|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret.[2].FileRawLineNumber |> should equal 8
        ret.[2].SourceRawLineNumber |> should equal 8
        ret.[2].FileEmptyLinesStrippedLineNumber |> should equal 3
        ret.[2].SourceEmptyLinesStrippedLineNumber |> should equal 3


    [<Test>]
    let ``INITIAL PROCESSING: Using a Second File Mix of empty strings and text strips out blank lines counters update``() =
        let testText = [|"";"test1";"";"test2";"";"";"";"test3";|]
        //let fileLoadingStatus={dummyFileLoadingStatus with FileNumber=1;IncomingRawLineCount=7;IncomingLineCountWithEmptyLinesDeletedCount=5}
        let ret = setupCompilationScenario 1 7 5 testText
        ret.[2].FileRawLineNumber |> should equal 8
        ret.[2].SourceRawLineNumber |> should equal 15
        ret.[2].FileEmptyLinesStrippedLineNumber |> should equal 3
        ret.[2].SourceEmptyLinesStrippedLineNumber |> should equal 8

    [<Test>]
    let ``INITIAL PROCESSING: No tabs or spaces means no indents``() =
        let testText = [|"";"test1";"";"test2";"";"";"";"test3";|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret.[2].IndentLevel |> should equal 0
    [<Test>]
    let ``INITIAL PROCESSING: Using tabs as an indent works``() =
        let testText = [|"";"test1";"";"\ttest2";"";"";"";"    test3";|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret.[1].IndentLevel |> should equal 1
    [<Test>]
    let ``INITIAL PROCESSING: Using spaces as an indent works``() =
        let testText = [|"";"test1";"";"\ttest2";"";"";"";"    test3";|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret.[2].IndentLevel |> should equal 1
    [<Test>]
    let ``INITIAL PROCESSING: Mixed spaces and tabs work together``() =
        let testText = [|"";"test1";"";"\t    test2";"";"";"";"    \t\ttest3";|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret.[1].IndentLevel |> should equal 2
    [<Test>]
    let ``INITIAL PROCESSING: Mixed spaces and tabs work together adv``() =
        let testText = [|"";"test1";"";"\t    test2";"";"";"";"    \t\t    \ttest3";|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret.[2].IndentLevel |> should equal 5
    [<Test>]
    let ``INITIAL PROCESSING: Spaces in the middle of the line do not cause indents``() =
        let testText = [|"";"test1    dogs!";"";"\t    test2";"";"";"";"    \t\t    \ttest3";|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret.[0].IndentLevel |> should equal 0
    [<Test>]
    let ``INITIAL PROCESSING: Tabs in the middle of the line do not cause indents``() =
        let testText = [|"";"test1\tdogs!";"";"\t    test2";"";"";"";"    \t\t    \ttest3";|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret.[0].IndentLevel |> should equal 0
    [<Test>]
    let ``INITIAL PROCESSING: Both Spaces and Tabs together in the middle of the line do not cause indents``() =
        let testText = [|"";"test1\t    \t\t    dogs  I like cats    really!\t\ttrust me!";"";"\t    test2";"";"";"";"    \t\t    \ttest3";|]
        let ret = setupCompilationScenario 0 0 0 testText
        ret.[0].IndentLevel |> should equal 0


    // more complex tests involving multiple files
    [<Test>]
    let ``INITIAL PROCESSING 2: Two empty files resolves to nothing``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testTextArray1:string [] = [||]
        let testTextArray2:string [] = [||]
        let listToProcess = [|(fileInfo1,testTextArray1);(fileInfo2,testTextArray2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        processedIncomingLines.Length |> should equal 0
        compilerReturn.CompilerMessages.Length |> should equal 1
        compilerReturn.ModelItems.Length |> should equal 1
    [<Test>]
    let ``INITIAL PROCESSING 2: Two one-line files resolve correctly``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testTextArray1:string [] = [|"Line 1 in file 1"|]
        let testTextArray2:string [] = [|"Line 1 in file 2"|]
        let listToProcess = [|(fileInfo1,testTextArray1);(fileInfo2,testTextArray2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        processedIncomingLines.Length |> should equal 2
        compilerReturn.CompilerMessages.Length |> should equal 1
        compilerReturn.ModelItems.Length |> should equal 1
        processedIncomingLines.[0].LineText |> should equal "Line 1 in file 1"
        processedIncomingLines.[1].LineText |> should equal "Line 1 in file 2"
        processedIncomingLines.[0].FileCompilationNumber |> should equal 0
        processedIncomingLines.[1].FileCompilationNumber |> should equal 1
        processedIncomingLines.[0].File |> should equal fileInfo1
        processedIncomingLines.[1].File |> should equal fileInfo2
    [<Test>]
    let ``INITIAL PROCESSING 2: Two more complex files resolve correctly``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testTextArray1:string [] = [|"";"test1\t    \t\t    dogs  I like cats    really!\t\ttrust me!";"";"\t    test2";"";"";"";"    \t\t    \ttest3";|]
        let testTextArray2:string [] = [|"";"test4\t    \t\t    cats  I like dogs    really!\t\ttrust me!";"";"\t    test5";"";"";"";"    \t\t    \ttest6";|]
        let listToProcess = [|(fileInfo1,testTextArray1);(fileInfo2,testTextArray2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        processedIncomingLines.Length |> should equal 6
        compilerReturn.CompilerMessages.Length |> should equal 1
        compilerReturn.ModelItems.Length |> should equal 1
        processedIncomingLines.[5].IndentLevel |> should equal 5
    [<Test>]
    let ``INITIAL PROCESSING 2: Empty file in middle of list doesn't crash it``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let fileInfo3 = getFakeFileInfo()
        let testTextArray1:string [] = [|"";"test1\t    \t\t    dogs  I like cats    really!\t\ttrust me!";"";"\t    test2";"";"";"";"    \t\t    \ttest3";|]
        let testTextArray2:string [] = [||]
        let testTextArray3:string [] = [|"";"test4\t    \t\t    cats  I like dogs    really!\t\ttrust me!";"";"\t    test5";"";"";"";"    \t\t    \ttest6";|]
        let listToProcess = [|(fileInfo1,testTextArray1);(fileInfo2,testTextArray2);(fileInfo3,testTextArray3)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        processedIncomingLines.Length |> should equal 6
        compilerReturn.CompilerMessages.Length |> should equal 1
        compilerReturn.ModelItems.Length |> should equal 1
        processedIncomingLines.[5].IndentLevel |> should equal 5
        processedIncomingLines.[4].FileCompilationNumber |> should equal 2







