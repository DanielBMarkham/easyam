module EasyamParsingEngine
    open Types
    open SAModel
    open Utils
    open Persist

    type FileLoadingState = 
        {
            File:System.IO.FileInfo
            FileNumber:int
            IncomingRawLineCount:int
            IncomingLineCountWithEmptyLinesDeletedCount:int
        }
    let dummyFileLoadingStatus =
        {
            File=new System.IO.FileInfo("test")
            //File=new System.IO.FileInfo("")
            FileNumber=0
            IncomingRawLineCount=0
            IncomingLineCountWithEmptyLinesDeletedCount=0
        }
    type IncomingLine =
        {
            FileCompilationNumber:int
            File:System.IO.FileInfo
            FileRawLineNumber:int
            FileEmptyLinesStrippedLineNumber:int
            SourceRawLineNumber:int
            SourceEmptyLinesStrippedLineNumber:int
            LineText:string
            LineWithoutLeadingSpaces:string
            IndentLevel:int
        }
    type CompilerMessageType =
        | Info
        | Warning
        | Error
    type CompilerMessage =
        {
            MessageType:CompilerMessageType
            Message:string
            SourceLine:IncomingLine
        }

    let initialProcessingOfIncomingFileLines (fileLoadingState:FileLoadingState) (rawLines:string []) =
        let whiteSpaceRegex=new System.Text.RegularExpressions.Regex("^\s+")
        let initialMapBeforeProcessing = rawLines |> Array.mapi(fun i x->
                {
                    FileCompilationNumber=fileLoadingState.FileNumber
                    File=fileLoadingState.File
                    FileRawLineNumber= i + 1
                    FileEmptyLinesStrippedLineNumber=0 // add this later
                    SourceRawLineNumber= fileLoadingState.IncomingRawLineCount + i + 1
                    SourceEmptyLinesStrippedLineNumber=0 // add this later
                    LineText=x
                    LineWithoutLeadingSpaces="" // fix this in next step
                    IndentLevel=0 // fix this in next step
                }            
            )
        let linesWithWhiteSpacesProcessed:IncomingLine [] =initialMapBeforeProcessing |> Array.mapi(fun j incomingLineBeingProcessed->
                let indentLevel, lineWithoutLeadingSpaces = 
                    let whiteSpaceMatches = whiteSpaceRegex.Matches(incomingLineBeingProcessed.LineText).toArray
                    if whiteSpaceMatches.Length>0
                        then
                            let leadingWhiteSpace = whiteSpaceMatches.[0].Value
                            let tabCount = leadingWhiteSpace.CountOccurences("\t")-1
                            let spaceCount = leadingWhiteSpace.CountOccurences(" ")-1
                            ((tabCount + (spaceCount/4)),incomingLineBeingProcessed.LineText.Substring(leadingWhiteSpace.Length))
                        else (0,incomingLineBeingProcessed.LineText)
                {
                    incomingLineBeingProcessed with
                        LineWithoutLeadingSpaces=lineWithoutLeadingSpaces
                        IndentLevel=indentLevel
                }
            )
        let linesStrippedForEmptyLines = linesWithWhiteSpacesProcessed |> Array.filter(fun x->
            x.LineText.Trim().Length >0
            )
        linesStrippedForEmptyLines |> Array.mapi(fun i x->
                {
                    x with
                        FileEmptyLinesStrippedLineNumber=i+1
                        SourceEmptyLinesStrippedLineNumber= fileLoadingState.IncomingLineCountWithEmptyLinesDeletedCount+i+1
                }
            )


    let parseLines (rawLines:string[]) : CompilerMessage list*ModelItemType list = 
        let fileLoadingStatus=dummyFileLoadingStatus
        let ret = initialProcessingOfIncomingFileLines fileLoadingStatus rawLines
        (List.empty<CompilerMessage>, List.empty<ModelItemType>)

