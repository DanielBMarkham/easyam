module BasicModelCheckTests
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
    let ``BASIC MODEL CHECK: No files creates one message``() =
        let listToProcess = [||]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.CompilerMessages.Length |> should equal 1
    [<Test>]
    let ``BASIC MODEL CHECK: Couple of files with no errors creates the right message``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|"Here's some text"; "BUSINESS BEHAVIOR ABSTRACT TO-BE"; ""; "  Reconcile account"; "      Pull the plug"; ""; ""|]
        let testText2 = [|"A new beginning"; "SYSTEM SUPPLEMENTAL REALIZED WAS"; "  Everybody gets a sticker"; "      The stickers blow up"; ""; ""; "BUSINESS BEHAVIOR ABSTRACT TO-BE"; "    Reconcile account"; "        Q:What's a sprog?"|]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.CompilerMessages.Length |> should equal 1
        newCompilerStatus.CompilerMessages.[0].Message |> should equal "file loading completed with 2 files processed along with 11 lines of code reviewed"
    [<Test>]
    let ``BASIC MODEL CHECK: 30 items of the same type produce a warning``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let fileInfo3 = getFakeFileInfo()
        let fileInfo4 = getFakeFileInfo()
        let fileInfo5 = getFakeFileInfo()
        let testText1 = [|"BUSINESS BEHAVIOR ABSTRACT TO-BE"; ""; "  MUS 1"; "      MUS 2"; "      MUS 3"; "      MUS 4"; "      MUS 5"; "      MUS 6"|]
        let testText2 = [|"BUSINESS BEHAVIOR ABSTRACT TO-BE"; ""; "  MUS 7"; "      MUS 8"; "      MUS 9"; "      MUS 10"; "      MUS 11"; "      MUS 12"|]
        let testText3 = [|"BUSINESS BEHAVIOR ABSTRACT TO-BE"; ""; "  MUS 13"; "      MUS 14"; "      MUS 15"; "      MUS 16"; "      MUS 17"; "      MUS 18"|]
        let testText4 = [|"BUSINESS BEHAVIOR ABSTRACT TO-BE"; ""; "  MUS 19"; "      MUS 20"; "      MUS 21"; "      MUS 22"; "      MUS 23"; "      MUS 24"|]
        let testText5 = [|"BUSINESS BEHAVIOR ABSTRACT TO-BE"; ""; "  MUS 25"; "      MUS 26"; "      MUS 27"; "      MUS 28"; "      MUS 29"; "      MUS 30"|]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2);(fileInfo3,testText3);(fileInfo4,testText4);(fileInfo5,testText5)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.CompilerMessages.Length |> should equal 1
        newCompilerStatus.CompilerMessages.[0].Message |> should equal "file loading completed with 5 files processed along with 35 lines of code reviewed"


