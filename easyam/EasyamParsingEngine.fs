module EasyamParsingEngine
    open Types
    open SAModel
    open Utils
    open Persist

    let CommandTokens = [
        "NOTE:";
        "NOTE ";
        "NOTES ";
        "//";
        "Q:";
        "QUESTION:";
        "QUESTION ";
        "QUESTIONS ";
        "TODO:";
        "TODO ";
        "TODOS ";
        "WORK:";
        "WORK ";
        "WORKS "
        ]

    ///
    /// TOKEN PROCESSING. TAKES A LINE AND MAKES A LIST OF COMMANDS AND VALUES
    ///
    let findInitialTextKeywordAndRemainingTextOnALine (tokenList:string list) (incomingLine:string):(string*string*string) option =
        let tokensInARegexORStatement = tokenList |> String.concat "|"
        let regexJustFindToken = new System.Text.RegularExpressions.Regex(tokensInARegexORStatement)
        let regexParseTokenOutOfString = "(?:(?!" + tokensInARegexORStatement + ").)*"
        let tokenRegex = new System.Text.RegularExpressions.Regex(regexParseTokenOutOfString)
        let stringMatches = tokenRegex.Matches(incomingLine).toSeq |> Seq.toList
        let stringMatchesFiltered = stringMatches|> List.filter(fun x->x.Value.Length>0)
        match stringMatchesFiltered.Length with
            |0-> Some("","","")
            |_-> 
                let matchFound = stringMatchesFiltered.Item(0)
                let tokenAndDetail = 
                    match matchFound.Index with
                        |0->matchFound.Value
                        |1->incomingLine.Substring(0, matchFound.Value.Length+1)
                        |_->raise(new System.Exception("Regex Matching is whack"))
                let tokenInsideTokenAndValueMatchString = regexJustFindToken.Matches(tokenAndDetail)
                match tokenInsideTokenAndValueMatchString.Count, stringMatchesFiltered.Length with                     
                    |0,0->Some("", "", stringMatchesFiltered.[0].Value)
                    |0,_->
                        let retTextAfterToken=tokenAndDetail.Trim()
                        let retTextFollowingCurrentTokenAndValue=incomingLine.Substring(tokenAndDetail.Length)
                        Some(retTextFollowingCurrentTokenAndValue, "", retTextAfterToken)
                    |1,_->
                        let retTextAfterToken=tokenAndDetail.Substring(tokenInsideTokenAndValueMatchString.[0].Length).Trim()
                        let retToken=tokenInsideTokenAndValueMatchString.[0].Value
                        let retTextFollowingCurrentTokenAndValue=incomingLine.Substring(tokenAndDetail.Length)
                        Some(retTextFollowingCurrentTokenAndValue, retToken, retTextAfterToken)
                    |_,_->Option<string*string*string>.None
    type Command =
        {
            CommandIndentLevel:int
            Token:string
            Value:string
        }
    exception CommandParsingException of Command list
    let splitOutIncomingLineIntoCommandList (tokenList:string list) (incomingLine:string):Command list =
        if incomingLine="" 
            then List<Command>.Empty 
            else
                let rec takeOutNextCommand (acc:Command list) (remainingLine:string) =
                    let parserReturn = findInitialTextKeywordAndRemainingTextOnALine tokenList remainingLine                
                    if parserReturn.IsSome 
                        then 
                            let textFollowingCurrentTokenAndValue, token, textAfterToken=parserReturn.Value
                            let newCommand = {Token=token;Value=textAfterToken;CommandIndentLevel=0}
                            let newAcc = [newCommand] |> List.append acc
                            if textFollowingCurrentTokenAndValue.Length=0 then newAcc else takeOutNextCommand newAcc textFollowingCurrentTokenAndValue
                        else 
                            let newCommand = {Token="";Value=remainingLine;CommandIndentLevel=0}
                            let newAcc = [newCommand] |> List.append acc
                            raise (CommandParsingException(newAcc))
                takeOutNextCommand List<Command>.Empty incomingLine

    ///
    /// INITIAL INCOMING FILE PROCESSING. TAKES A LIST OF STRINGS AND MAKES A LIST OF COMMANDS
    ///
    [<NoComparison>]
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
            Commands:Command []
        }
    let initialProcessingOfIncomingFileLines fileNumber fileInfo incomingRawLineCount incomingLineCountWithEmptyLinesDeletedCount (rawLines:string []) =
        let whiteSpaceRegex=new System.Text.RegularExpressions.Regex("^\s+")
        let initialMapBeforeProcessing = rawLines |> Array.mapi(fun i x->
                {
                    FileCompilationNumber=fileNumber
                    File=fileInfo
                    FileRawLineNumber= i + 1
                    FileEmptyLinesStrippedLineNumber=0 // add this later
                    SourceRawLineNumber= incomingRawLineCount + i + 1
                    SourceEmptyLinesStrippedLineNumber=0 // add this later
                    LineText=x
                    LineWithoutLeadingSpaces="" // fix this in next step
                    IndentLevel=0 // fix this in next step
                    Commands=Array.empty // fix later
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
        let emptyLinesStripped = linesStrippedForEmptyLines |> Array.mapi(fun i x->
                {
                    x with
                        FileEmptyLinesStrippedLineNumber=i+1
                        SourceEmptyLinesStrippedLineNumber= incomingLineCountWithEmptyLinesDeletedCount+i+1
                }
            )
        let finishedProduct = emptyLinesStripped |> Array.map(fun x->
            let commandsFoundInLine=splitOutIncomingLineIntoCommandList CommandTokens x.LineText
            let commandsFoundInLineWithCommandLevels = commandsFoundInLine |> List.mapi(fun j y->
                {y with CommandIndentLevel=x.IndentLevel+j}
                )
            {x with Commands=commandsFoundInLineWithCommandLevels |> List.toArray}
            )
        finishedProduct
    type CompilerMessageType =
        | Info
        | Warning
        | Error
    [<NoComparison>]
    type CompilerMessage =
        {
            MessageType:CompilerMessageType
            Message:string
            SourceLine:IncomingLine
        }
    [<NoComparison>]
    type ModelItem2 =
        {
            Id:int
            SourceReferences:IncomingLine []
            //ParentId:int
            //ItemType:ModelItemType
            //Bucket:Buckets
            //Genre:Genres
            //AbstractionLevel:AbstractionLevels
            //TemporalIndicator:TemporalIndicators
            //ItemAnnotation:ItemAnnotation
            //ModelItemName:string
        }

    [<NoComparison>]
    type CompilerReturn = 
        {
            CompilerMessages:CompilerMessage list
            ModelItems:ModelItem2 list
        }
    [<NoComparison>]
    type RunningStatus =
        {
            FileNumber:int
            IncomingRawLineCount:int
            IncomingLineCountWithEmptyLinesStripped:int
            IncomingLinesConcatenated:IncomingLine []
            CompilerReturn:CompilerReturn
        }
    ///
    /// Takes a list of files, cleans and concatenates the contents of each one
    ///
    let bulkFileLineProcessing (filesToProcess:(System.IO.FileInfo*string []) list) =
        let newCompilerReturn = {CompilerMessages=List<CompilerMessage>.Empty; ModelItems=List<ModelItem2>.Empty}
        let completedRunningStatus =
            filesToProcess |> List.fold(fun (acc:RunningStatus) x->
                let allFileText=snd x
                let fileBeingProcessedInfo=fst x
                let ret = initialProcessingOfIncomingFileLines acc.FileNumber fileBeingProcessedInfo acc.IncomingRawLineCount acc.IncomingLineCountWithEmptyLinesStripped allFileText
                let newacc = {acc with 
                                FileNumber=acc.FileNumber+1
                                IncomingRawLineCount=acc.IncomingRawLineCount+allFileText.Length
                                IncomingLineCountWithEmptyLinesStripped=acc.IncomingLineCountWithEmptyLinesStripped+ret.Length
                                IncomingLinesConcatenated=ret |> Array.append acc.IncomingLinesConcatenated
                              }
                newacc
                ) {FileNumber=0; IncomingRawLineCount=0; IncomingLineCountWithEmptyLinesStripped=0; IncomingLinesConcatenated=[||]; CompilerReturn=newCompilerReturn}
        completedRunningStatus.IncomingLinesConcatenated, completedRunningStatus.CompilerReturn

    
    let makeRawModel (incomingLines:IncomingLine []) (currentCompilerStatus:CompilerReturn) =
        let initialModelLines = incomingLines |> Array.fold(fun acc x->
                                let modelItemsOnThisLine = x.Commands |> Array.map(fun y->
                                        {
                                            Id=getNextItemNumber()
                                            SourceReferences=[|x|]
                                        }
                                    )
                                let newacc = modelItemsOnThisLine |> Array.append acc
                                newacc
                                ) Array.empty
        initialModelLines

    //let stuffModel (incomingLines:IncomingLine []):CompilerReturn =
    //    let compilerMessageList = List.empty<CompilerMessage>
    //    let modelItemList = List.empty<ModelItem>
    //    // process entire line
    //    let compiledContext = 
    //        incomingLines |> Array.fold(fun (lineProcessorAccumulator:CompilerReturn) incomingLineBeingProcessed->
    //            //let matchUntilNextToken="(?:(?!//|&|:|PARENT|HASA|AFFECTS|CONTAINS|WORK:|Q:|TODO:|NOTE:|MASTER DOMAIN MODEL|MASTER BACKLOG|MASTER SUPPLEMENTAL MODEL|PRODUCT BACKLOG|BUSINESS BEHAVIOR ABSTRACT|WHEN|ASA|INEEDTO|SOTHAT).)*"
    //            //let matchUntilNextToken= ".*(?:(?!Q:).)*"

    //            let matchUntilNextToken= "(?:(?!Q:).)*"
    //            let matchCommandItselfRegex= new System.Text.RegularExpressions.Regex("(Q:)")
    //            let matchATokenRegex= new System.Text.RegularExpressions.Regex(".*(?!Q:)")
    //            let freeTextRegex = new System.Text.RegularExpressions.Regex("" + matchUntilNextToken)
    //            let getTokenMatchAndRemainingString (incomingLine:string) = 
    //                let regexMatches = freeTextRegex.Matches(incomingLineBeingProcessed.LineWithoutLeadingSpaces)
    //                let lengthOfFirstMatch=if regexMatches.Count>0 then regexMatches.[0].Value.Length else 0
    //                match regexMatches.Count with
    //                    | 0-> "",""
    //                    | 1-> regexMatches.[0].Value, ""
    //                    | _->
    //                            let firstMatch = if regexMatches.[0].Value="" then regexMatches.[1].Value else regexMatches.[0].Value
    //                            let remainingString = if lengthOfFirstMatch<=incomingLine.Length then incomingLine.Substring(lengthOfFirstMatch) else ""
    //                            firstMatch, remainingString

    //            // process multiple tokens inside a line 
    //            let rec processKeyWords (currentMatch:string) (remainingLine:string) (currentCompilerState:CompilerReturn):CompilerReturn =
    //                    if currentMatch.Length>0
    //                        then
    //                            let newModelItem =
    //                                match matchCommandItselfRegex.IsMatch(currentMatch) with
    //                                    |true-> // there's a command here
    //                                        let commandFound = matchCommandItselfRegex.Matches(currentMatch).[0].Value
    //                                        // Just question for now
    //                                        let theQuestionCommand = matchATokenRegex.Matches(commandFound)
    //                                        let textWithoutCommand = if theQuestionCommand.Count>0 then commandFound.Substring (theQuestionCommand.[0].Value.Length) else commandFound
    //                                        let newModelItemType = Question({Text=textWithoutCommand; SourceReference={File=incomingLineBeingProcessed.File; LineNumber=incomingLineBeingProcessed.SourceRawLineNumber; LineLevelIndent=incomingLineBeingProcessed.IndentLevel}})
    //                                        {
    //                                            defaultModelItem with
    //                                                Id=getNextItemNumber()
    //                                                SourceCodeParent=0
    //                                                ModelParent=0
    //                                                ItemType= newModelItemType
    //                                                SourceReferences=[{File=incomingLineBeingProcessed.File; LineNumber=incomingLineBeingProcessed.SourceRawLineNumber; LineLevelIndent=incomingLineBeingProcessed.IndentLevel}]
    //                                        }                                

    //                                    |false->  // it's just text until the end of the line
    //                                        let newModelItemType = Note({Text=currentMatch; SourceReference={File=incomingLineBeingProcessed.File; LineNumber=incomingLineBeingProcessed.SourceRawLineNumber; LineLevelIndent=incomingLineBeingProcessed.IndentLevel}})
    //                                        {
    //                                            defaultModelItem with
    //                                                Id=getNextItemNumber()
    //                                                SourceCodeParent=0
    //                                                ModelParent=0
    //                                                ItemType= newModelItemType
    //                                                SourceReferences=[{File=incomingLineBeingProcessed.File; LineNumber=incomingLineBeingProcessed.SourceRawLineNumber; LineLevelIndent=incomingLineBeingProcessed.IndentLevel}]
    //                                        }                                
    //                            let oldModelItemList = lineProcessorAccumulator.ModelItems
    //                            let newModelItems = [newModelItem] |> List.append  oldModelItemList
    //                            // do we have another match?
    //                            let newFirstMatch,newRemainingLine = getTokenMatchAndRemainingString remainingLine
    //                            if newFirstMatch.Length>0
    //                                then
    //                                    processKeyWords newFirstMatch newRemainingLine {CompilerMessages=lineProcessorAccumulator.CompilerMessages; ModelItems=newModelItems}
    //                                else {CompilerMessages=lineProcessorAccumulator.CompilerMessages; ModelItems=newModelItems}
    //                        else currentCompilerState

    //            let firstMatch,remainingString = getTokenMatchAndRemainingString incomingLineBeingProcessed.LineText
    //            processKeyWords firstMatch remainingString lineProcessorAccumulator
    //            ) {CompilerMessages=compilerMessageList; ModelItems=modelItemList}

    //    compiledContext
