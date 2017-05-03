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
    type CompilerReturn = CompilerMessage list*ModelItem list

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
    let stuffModel (incomingLines:IncomingLine []):CompilerReturn =
        let compilerMessageList = List.empty<CompilerMessage>
        let modelItemList = List.empty<ModelItem>
        // process entire line
        let compiledContext = 
            incomingLines |> Array.fold(fun (lineProcessorAccumulator:CompilerReturn) incomingLineBeingProcessed->
                //let matchUntilNextToken="(?:(?!//|&|:|PARENT|HASA|AFFECTS|CONTAINS|WORK:|Q:|TODO:|NOTE:|MASTER DOMAIN MODEL|MASTER BACKLOG|MASTER SUPPLEMENTAL MODEL|PRODUCT BACKLOG|BUSINESS BEHAVIOR ABSTRACT|WHEN|ASA|INEEDTO|SOTHAT).)*"
                let matchUntilNextToken= ".*(?:(?!Q:).)*"
                let matchCommandItselfRegex= new System.Text.RegularExpressions.Regex("(Q:)")
                let matchATokenRegex= new System.Text.RegularExpressions.Regex(".*(?!Q:)")
                let freeTextRegex = new System.Text.RegularExpressions.Regex("^" + matchUntilNextToken)
                let regexMatches = freeTextRegex.Matches(incomingLineBeingProcessed.LineWithoutLeadingSpaces)
                // process multiple tokens inside a line 
                let newCompilerReturn =
                    if regexMatches.Count>0 
                        then
                            let newModelItem =
                                match matchCommandItselfRegex.IsMatch(regexMatches.[0].Value) with
                                    |true-> // there's a command here
                                        let commandFound = matchCommandItselfRegex.Matches(regexMatches.[0].Value).[0].Value
                                        // Just question for now
                                        let theQuestionCommand = matchATokenRegex.Matches(commandFound)
                                        let textWithoutCommand = if theQuestionCommand.Count>0 then commandFound.Substring (theQuestionCommand.[0].Value.Length) else commandFound
                                        let newModelItemType = Question({Text=textWithoutCommand; SourceReference={File=incomingLineBeingProcessed.File; LineNumber=incomingLineBeingProcessed.SourceRawLineNumber; LineLevelIndent=incomingLineBeingProcessed.IndentLevel}})
                                        {
                                            defaultModelItem with
                                                Id=getNextItemNumber()
                                                SourceCodeParent=0
                                                ModelParent=0
                                                ItemType= newModelItemType
                                                SourceReferences=[{File=incomingLineBeingProcessed.File; LineNumber=incomingLineBeingProcessed.SourceRawLineNumber; LineLevelIndent=incomingLineBeingProcessed.IndentLevel}]
                                        }                                

                                    |false->  // it's just text until the end of the line
                                        let newModelItemType = Note({Text=regexMatches.[0].Value; SourceReference={File=incomingLineBeingProcessed.File; LineNumber=incomingLineBeingProcessed.SourceRawLineNumber; LineLevelIndent=incomingLineBeingProcessed.IndentLevel}})
                                        {
                                            defaultModelItem with
                                                Id=getNextItemNumber()
                                                SourceCodeParent=0
                                                ModelParent=0
                                                ItemType= newModelItemType
                                                SourceReferences=[{File=incomingLineBeingProcessed.File; LineNumber=incomingLineBeingProcessed.SourceRawLineNumber; LineLevelIndent=incomingLineBeingProcessed.IndentLevel}]
                                        }                                
                            let oldModelItemList = (snd lineProcessorAccumulator)
                            let newModelItems = [newModelItem] |> List.append  oldModelItemList
                            ((fst lineProcessorAccumulator), newModelItems)
                        else 
                            lineProcessorAccumulator
                newCompilerReturn
                ) (compilerMessageList,modelItemList)

        compiledContext

    let parseLines (rawLines:string[]) : CompilerReturn = 
        let fileLoadingStatus=dummyFileLoadingStatus
        let incomingLines = initialProcessingOfIncomingFileLines fileLoadingStatus rawLines
        let stufferCompilerMessages, stuffedNotValidatedModel  =  stuffModel incomingLines
        (stufferCompilerMessages, stuffedNotValidatedModel)
        

