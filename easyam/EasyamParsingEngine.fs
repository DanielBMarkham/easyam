module EasyamParsingEngine
    open Types
    open SAModel
    open Utils
    open Persist

    let ModelItemIntegerFactory = 
        let counter = ref 0
        fun () -> 
            counter.Value <- !counter + 1
            !counter
    let getNextModelItemNumber()=ModelItemIntegerFactory()

    [<NoComparison>]
    type TOKEN_TYPE =
        |RELATIVE_LOCATOR
        |ABSOLUTE_LOCATOR
        |JOINER
    [<NoComparison>]
    type TOKEN_TARGET_TYPE =
        |SINGLE_TARGET
        |MULTIPLE_TARGETS
    [<NoComparison>]
    type TOKEN_CATEGORY = 
        |BUCKETS
        |GENRE
        |TEMPORAL
        |ABSTRACTION_LEVEL
        |TASKS
        |MISC
        |HDD
        |SCOPING
        |SHORTCUT
        |CONNECTIVE
    [<NoComparison>]
    type EASYAM_TOKEN =
        {
            Type:TOKEN_TYPE
            TargetType:TOKEN_TARGET_TYPE
            Category:TOKEN_CATEGORY
            Token:string
        }
    type ANNOTATION_TOKEN_TYPE =
        | None
        | Note
        | Question
        | ToDo
        | Work
    let EasyAMTokens = 
        [
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="NOTES:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="NOTES"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="NOTE:"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="//"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="Q:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="QUESTIONS:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="QUESTIONS"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="QUESTION:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="TODOS:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="TODOS"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="TODO:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="TO-DOS:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="TO-DOS"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="TO-DO:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="WORKS:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="WORKS"}
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="WORK:"};

            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=BUCKETS;              Token="BEHAVIOR"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=BUCKETS;              Token="STRUCTURE"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=BUCKETS;              Token="SUPPLEMENTALS:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=BUCKETS;              Token="SUPPLEMENTALS"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=BUCKETS;              Token="SUPPLEMENTAL:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=BUCKETS;              Token="SUPPLEMENTAL"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=GENRE;                Token="BUSINESS"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=GENRE;                Token="SYSTEM"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=GENRE;                Token="META"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=TEMPORAL;             Token="WAS"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=TEMPORAL;             Token="AS-IS"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=TEMPORAL;             Token="TO-BE"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ABSTRACTION_LEVEL;    Token="ABSTRACT"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ABSTRACTION_LEVEL;    Token="REALIZED"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=HDD;                  Token="HDD"};

            {Type=JOINER;               TargetType=SINGLE_TARGET;        Category=CONNECTIVE;           Token="PARENT"};
            {Type=JOINER;               TargetType=SINGLE_TARGET;        Category=CONNECTIVE;           Token="CHILD"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="CHILDREN"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="AFFECTS"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="AFFECTEDBY"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="USES"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="USEDBY"};

        ]
    let CommandTokens = EasyAMTokens |> List.map(fun x->x.Token)

    ///
    /// TOKEN PROCESSING. TAKES A LINE AND MAKES A LIST OF COMMANDS AND VALUES
    ///
    let findInitialTextKeywordAndRemainingTextOnALine (tokenList:string list) (incomingLine:string):(string*string*string) option =
        let tokensInARegexORStatement = tokenList |> String.concat "|"
        let regexJustFindToken = new System.Text.RegularExpressions.Regex(tokensInARegexORStatement)
        let regexParseTokenOutOfString = "(?:(?!" + tokensInARegexORStatement + "\b).)*"
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
                (takeOutNextCommand List<Command>.Empty incomingLine) |> List.filter(fun x->(x.Value=""&&x.Token="")=false)

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
            let commandsFoundInLine=(splitOutIncomingLineIntoCommandList CommandTokens x.LineText) 
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
                SourceFile:string
                SourceLineBegin:int
                SourceLineEnd:int option
                SourceLineColumnBegin:int option
                SourceLineColumnEnd:int option
            }
    let printCompilerMessages (compilerMessages:CompilerMessage []) =
        compilerMessages |> Array.iteri(fun i x->
            let messagePrefix=match x.MessageType with
                                    |CompilerMessageType.Info->"INFO: "
                                    |CompilerMessageType.Warning->"WARN: "
                                    |CompilerMessageType.Error->"ERROR: "
            let formattedMessage=match x.SourceLineEnd,x.SourceLineColumnBegin,x.SourceLineColumnEnd with 
                                    |option.None, option.None, option.None->
                                        x.SourceFile + ":" + string x.SourceLineBegin + ": " + messagePrefix + x.Message
                                    |Some endingLine, option.None, option.None->
                                        x.SourceFile + ":" + string x.SourceLineBegin + "-" + string endingLine + ": " + messagePrefix + x.Message
                                    |_,_,_->
                                        x.SourceFile + ": " + messagePrefix + x.Message
            System.Console.WriteLine(formattedMessage)
            )
    [<NoComparison>]
        type LastCompilerOperations =
            | PointerReset
            | NewModelItem
            | LocationChange
            | NewJoin
    [<NoComparison>]
    type ModelLocationPointer =
        {
            Namespace:string
            ParentId:int
            InHDDMode:bool
            Bucket:Buckets
            Genre:Genres
            AbstractionLevel:AbstractionLevels
            TemporalIndicator:TemporalIndicators
            AnnotationIndicator:ANNOTATION_TOKEN_TYPE
        }
    let defaultModelLocationPointer =
        {
            Namespace = ""
            ParentId = -1
            InHDDMode=false
            Bucket=Buckets.None
            Genre=Genres.None
            AbstractionLevel=AbstractionLevels.None
            TemporalIndicator=TemporalIndicators.None
            AnnotationIndicator=ANNOTATION_TOKEN_TYPE.None
        }
    [<NoComparison>]
    type ModelJoin =
        |Parent
        |Child
        |Affects
        |AffectedBy
        |Uses
        |UsedBy
    let getReverseJoin (sourceJoin:ModelJoin) =
        match sourceJoin with 
            | Parent->Child
            | Child->Parent
            | Affects->AffectedBy
            | AffectedBy->Affects
            | Uses->UsedBy
            | UsedBy->Uses
    [<NoComparison>]
    type ModelRelation =
        {
            id:int
            ModelJoinType:ModelJoin
            TargetId:int
            SourceReference:IncomingLine
        }
    [<NoComparison>]
    type ModelItem2 =
        {
            Id:int
            Location:ModelLocationPointer
            Description:string
            Annotations:(ANNOTATION_TOKEN_TYPE*string) []
            SourceReferences:IncomingLine []
            Relations:ModelRelation []
        }
    let defaultModelItem2:ModelItem2 =
        {
            Id=(-1)
            Location=defaultModelLocationPointer
            Description=""
            Annotations=[||]
            SourceReferences=[||]
            Relations=[||]
        }
    [<NoComparison>]
    type CompilerWaitingFor =
        |Nothing
        |SingleTarget
        |MultipleTargets
    [<NoComparison>]
    type IndentLevelComparisons =
        | IdentIsSameAsPreviousIndent
        | IdentIsLessThanPreviousIndent
        | IdentIsMoreThanPreviousIndent
    [<NoComparison>]
    type CompilerState =
        {
            WaitingFor:CompilerWaitingFor
            LastFileNameProcessed:string
            TagValueList:(string*string) list
            LastCompilerOperation:LastCompilerOperations
            LastJoinType:ModelJoin option
            CurrentIndentLevel:int
            IndentLevelChange:IndentLevelComparisons
        }
    let defaultCompilerState=
        {
            WaitingFor=CompilerWaitingFor.Nothing
            LastFileNameProcessed=""
            TagValueList=[]
            LastCompilerOperation=LastCompilerOperations.PointerReset
            LastJoinType=option.None
            CurrentIndentLevel=0
            IndentLevelChange=IndentLevelComparisons.IdentIsSameAsPreviousIndent
        }
    [<NoComparison>]
    type CompilerReturn = 
        {
            CompilerState:CompilerState
            CurrentLocation:ModelLocationPointer
            //CompilerWaitingForState:CompilerWaitingFor
            CompilerMessages:CompilerMessage []
            ModelItems:ModelItem2 []
        }
    let beginningCompilerStatus =
        {
            CompilerState=defaultCompilerState
            CurrentLocation=defaultModelLocationPointer
            //CompilerWaitingForState=CompilerWaitingFor.Nothing
            CompilerMessages=[||]
            ModelItems= [|defaultModelItem2|]
        }
    [<NoComparison>]
    type IncomingFileProcessingStatus =
        {
            FileNumber:int
            IncomingRawLineCount:int
            IncomingLineCountWithEmptyLinesStripped:int
            IncomingLinesConcatenated:IncomingLine []
            CompilerReturn:CompilerReturn
        }

    let logCompilerMessage (compilerStatus:CompilerReturn) (newMessage:CompilerMessage) =
        let newMessages = [|newMessage|] |> Array.append compilerStatus.CompilerMessages
        {compilerStatus with CompilerMessages=newMessages}
    let logCompilerMessageForASingleLine (compilerStatus:CompilerReturn) (messageType:CompilerMessageType) (messageDesc:string) (sourceLine:IncomingLine)  =
        let newCompilerMessage=
            {
                MessageType=messageType
                Message=messageDesc
                SourceFile=sourceLine.File.FullName
                SourceLineBegin=sourceLine.FileRawLineNumber
                SourceLineEnd=option.None
                SourceLineColumnBegin=option.None
                SourceLineColumnEnd=option.None
            }
        logCompilerMessage compilerStatus newCompilerMessage
    ///
    /// Takes a list of files, cleans and concatenates the contents of each one
    ///
    let bulkFileLineProcessing (filesToProcess:(System.IO.FileInfo*string []) []) =
        let newCompilerReturn = beginningCompilerStatus
        let completedRunningStatus =
            filesToProcess |> Array.fold(fun (acc:IncomingFileProcessingStatus) x->
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
        let newMessage = "file loading completed with " + string filesToProcess.Length + " files processed along with " + string completedRunningStatus.IncomingLinesConcatenated.Length + " lines of code reviewed"
        let firstSourceFileName=if filesToProcess.Length>0 then (fst filesToProcess.[0]).Name else ""
        let newCompilerMessage=
            {
                MessageType=CompilerMessageType.Info
                Message=newMessage
                SourceFile=firstSourceFileName
                SourceLineBegin=1
                SourceLineEnd=option.None
                SourceLineColumnBegin=option.None
                SourceLineColumnEnd=option.None
            }
        let completedAndUpdatedCompilerReturn=logCompilerMessage completedRunningStatus.CompilerReturn newCompilerMessage        
        completedRunningStatus.IncomingLinesConcatenated, completedAndUpdatedCompilerReturn

    let addModelItem (compilerStatus:CompilerReturn) (location:ModelLocationPointer) (incomingLine:IncomingLine) (desription:string) =
        if desription.Trim().Length>0
            then
                // some kind of tags are set. Use defaults if missing
                let newBucket=if location.Bucket=Buckets.None then Buckets.Behavior else location.Bucket
                let newGenre= if location.Genre=Genres.None then Genres.Business else location.Genre
                let newAbstractionLevel=if location.AbstractionLevel=AbstractionLevels.None then AbstractionLevels.Abstract else location.AbstractionLevel
                let newTemporalIndicator=if location.TemporalIndicator=TemporalIndicators.None then TemporalIndicators.ToBe else location.TemporalIndicator
                let newLocationPointer = {compilerStatus.CurrentLocation with Bucket=newBucket; Genre=newGenre; AbstractionLevel=newAbstractionLevel; TemporalIndicator=newTemporalIndicator}
                let newModelItem =
                    {
                        Id=getNextModelItemNumber()
                        Location=newLocationPointer
                        Description=desription.Trim()
                        Annotations= [||]
                        SourceReferences=[|incomingLine|]
                        Relations=[||]
                    }
                let newLoc = {newLocationPointer with ParentId=newModelItem.Id}
                let newModelItems = [|newModelItem|] |> Array.append compilerStatus.ModelItems
                {compilerStatus with ModelItems=newModelItems; CurrentLocation=newLoc;CompilerState={compilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.NewModelItem}}
            else compilerStatus

    let updateModelItem (compilerStatus:CompilerReturn) (updatedModelItem:ModelItem2) = 
        let splitItemsListFirstPart =fst (compilerStatus.ModelItems |> Array.partition(fun z->z.Id<updatedModelItem.Id))
        let previousVersionOfItem = compilerStatus.ModelItems |> Array.find(fun x->x.Id=updatedModelItem.Id)
        let splitItemsListSecondPart =fst (compilerStatus.ModelItems |> Array.partition(fun z->z.Id>updatedModelItem.Id))
        let newModelItems =  splitItemsListSecondPart |> Array.append [|updatedModelItem|] |> Array.append splitItemsListFirstPart
        {compilerStatus with ModelItems=newModelItems}

    let joinModelItems (compilerStatus:CompilerReturn) (location:ModelLocationPointer) (incomingLine:IncomingLine) (joinType:ModelJoin) (description:string) =
        // find the model item that is the target of this join and update both the current and target item
        // if the target does not exist, register a compiler error (but keep processing)
        let possibleSource=compilerStatus.ModelItems |> Array.tryFind(fun x->x.Id=compilerStatus.CurrentLocation.ParentId && x.Id<>(-1))
        let possibleTarget=compilerStatus.ModelItems |> Array.tryFind(fun x->x.Description=description && x.Id<>(-1))
        let reverseJoin = getReverseJoin joinType
        match possibleSource, possibleTarget with
            |Some sourceItem, Some targetItem->
                // add a join to both model items. Be sure to include the source reference
                let newSourceItemJoinRelation={id=getNextModelItemNumber(); ModelJoinType=joinType; TargetId=targetItem.Id; SourceReference=incomingLine}
                let newTargetItemJoinRelation={id=getNextModelItemNumber(); ModelJoinType=reverseJoin; TargetId=sourceItem.Id; SourceReference=incomingLine}
                let newSourceItemJoinRelations=[|newSourceItemJoinRelation|] |> Array.append sourceItem.Relations
                let newTargetItemJoinRelations=[|newTargetItemJoinRelation|] |> Array.append targetItem.Relations
                let newSourceItem={sourceItem with Relations=newSourceItemJoinRelations}
                let newTargetItem={targetItem with Relations=newTargetItemJoinRelations}
                let compilerStatusWithNewSourcce = updateModelItem compilerStatus newSourceItem
                let compilerStatusWithNewSourceAndTarget = updateModelItem compilerStatusWithNewSourcce newTargetItem
                compilerStatusWithNewSourceAndTarget
            |Some sourceItem, option.None->
                logCompilerMessageForASingleLine compilerStatus CompilerMessageType.Error ("target item " + description + " does not currently exist in model when trying to make join") incomingLine
            |option.None, Some targetItem->
                logCompilerMessageForASingleLine compilerStatus CompilerMessageType.Error ("source item id:" + string compilerStatus.CurrentLocation.ParentId + " does not currently exist in model when trying to make join") incomingLine
            |option.None, option.None->
                logCompilerMessageForASingleLine compilerStatus CompilerMessageType.Info "programming error. Trying to create a join where neither item exists in the model" incomingLine
            

    let addAnnotation (parentId:int) (annotationType:ANNOTATION_TOKEN_TYPE) (annotationValue:string) (sourceLine:IncomingLine) (currentCompilerStatus:CompilerReturn) =
        if annotationValue="" then currentCompilerStatus
            else
                let modelItemToChange = currentCompilerStatus.ModelItems |> Array.tryFind(fun x->x.Id=parentId)
                if modelItemToChange.IsNone
                    then currentCompilerStatus
                    else
                        let newAnnotation = (annotationType,annotationValue)
                        let newAnnotations = [|newAnnotation|] |> Array.append modelItemToChange.Value.Annotations
                        let newSourceReferences = [|sourceLine|] |> Array.append modelItemToChange.Value.SourceReferences
                        let newModelItem = {modelItemToChange.Value with Annotations=newAnnotations; SourceReferences=newSourceReferences}
                        let newCompilerStatus=updateModelItem currentCompilerStatus newModelItem
                        newCompilerStatus
    let updateModelLocationPointer (originalCompilerStatus:CompilerReturn) (incomingLine:IncomingLine) (incomingCommand:Command):CompilerReturn =
        // context resets when the file changes

        let fileCheckedCompilerStatus=if incomingLine.File.FullName=originalCompilerStatus.CompilerState.LastFileNameProcessed
                                        then originalCompilerStatus
                                        //else {originalCompilerStatus with CompilerWaitingForState=Nothing; CompilerState={defaultCompilerState with LastFileNameProcessed=incomingLine.File.FullName}}
                                        else {originalCompilerStatus with CompilerState={defaultCompilerState with LastFileNameProcessed=incomingLine.File.FullName; WaitingFor=CompilerWaitingFor.Nothing; CurrentIndentLevel=0}}
        let newIndentLevel=if incomingCommand.CommandIndentLevel<fileCheckedCompilerStatus.CompilerState.CurrentIndentLevel 
                                then IndentLevelComparisons.IdentIsLessThanPreviousIndent
                            elif incomingCommand.CommandIndentLevel=fileCheckedCompilerStatus.CompilerState.CurrentIndentLevel
                                then IndentLevelComparisons.IdentIsSameAsPreviousIndent
                            else IndentLevelComparisons.IdentIsMoreThanPreviousIndent
        let newCompilerState={fileCheckedCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; IndentLevelChange=newIndentLevel}
        let incomingCompilerStatus = {fileCheckedCompilerStatus with CompilerState=newCompilerState}

        let tokenForCommand = EasyAMTokens |> List.tryFind(fun z->z.Token.Trim()=incomingCommand.Token.Trim())

        match tokenForCommand with 
            |option.None->
                //match incomingCompilerStatus.CompilerWaitingForState with 
                match incomingCompilerStatus.CompilerState.WaitingFor with 
                    |CompilerWaitingFor.Nothing->
                        let newCompilerStatus = addAnnotation incomingCompilerStatus.CurrentLocation.ParentId ANNOTATION_TOKEN_TYPE.Note incomingCommand.Value incomingLine incomingCompilerStatus 
                        newCompilerStatus
                    |CompilerWaitingFor.MultipleTargets->
                        let itemAlreadyExists=incomingCompilerStatus.ModelItems |> Array.tryFind(fun z->z.Description=incomingCommand.Value.Trim())
                        let lastModelParent = incomingCompilerStatus.ModelItems |> Array.find(fun x->x.Id=incomingCompilerStatus.CurrentLocation.ParentId)
                        let targetForPossibleComments = if lastModelParent.Relations.Length>0
                                                        then
                                                            let lastJoinedTarget=lastModelParent.Relations.[lastModelParent.Relations.Length-1].TargetId
                                                            incomingCompilerStatus.ModelItems |> Array.find(fun x->x.Id=lastJoinedTarget)
                                                        else
                                                            incomingCompilerStatus.ModelItems |> Array.find(fun x->x.Id=incomingCompilerStatus.CurrentLocation.ParentId)
                        if incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewJoin
                            then
                                // waiting on a join
                                match incomingCompilerStatus.CompilerState.IndentLevelChange, itemAlreadyExists.IsSome with 
                                    | IndentLevelComparisons.IdentIsLessThanPreviousIndent, false->
                                        // if the indent is less than before, and there's no match, we have a new item
                                        addModelItem incomingCompilerStatus incomingCompilerStatus.CurrentLocation incomingLine (incomingCommand.Value.Trim())
                                    | IndentLevelComparisons.IdentIsLessThanPreviousIndent, true->
                                        // if the indent is less than before, and there IS A match, we have a new badly-formatted join
                                        let lastJoinType = if incomingCompilerStatus.CompilerState.LastJoinType.IsSome then incomingCompilerStatus.CompilerState.LastJoinType.Value else raise(new System.Exception("We have a join but no join type to use"))
                                        joinModelItems incomingCompilerStatus incomingCompilerStatus.CurrentLocation incomingLine lastJoinType  (incomingCommand.Value)
                                    | IndentLevelComparisons.IdentIsMoreThanPreviousIndent, false->
                                        // if the indent is more, and we can't find this text anywnhere in the model, it's a note
                                        addAnnotation targetForPossibleComments.Id ANNOTATION_TOKEN_TYPE.Note (incomingCommand.Value.Trim()) incomingLine incomingCompilerStatus
                                    | IndentLevelComparisons.IdentIsMoreThanPreviousIndent, true->
                                        // if the indent's more and we can find this text in the model, it's a badly formatted join
                                        let lastJoinType = if incomingCompilerStatus.CompilerState.LastJoinType.IsSome then incomingCompilerStatus.CompilerState.LastJoinType.Value else raise(new System.Exception("We have a join but no join type to use"))
                                        joinModelItems incomingCompilerStatus incomingCompilerStatus.CurrentLocation incomingLine lastJoinType  (incomingCommand.Value)
                                    | IndentLevelComparisons.IdentIsSameAsPreviousIndent, true->
                                        // if the indent is the same and we can find this text in the model, it's just another join
                                        let lastJoinType = if incomingCompilerStatus.CompilerState.LastJoinType.IsSome then incomingCompilerStatus.CompilerState.LastJoinType.Value else raise(new System.Exception("We have a join but no join type to use"))
                                        joinModelItems incomingCompilerStatus incomingCompilerStatus.CurrentLocation incomingLine lastJoinType  (incomingCommand.Value)
                                    | IndentLevelComparisons.IdentIsSameAsPreviousIndent, false->
                                        // if the indent is the same and we can't find this text in the model, it's a badly-formatted note
                                        addAnnotation targetForPossibleComments.Id ANNOTATION_TOKEN_TYPE.Note (incomingCommand.Value.Trim()) incomingLine incomingCompilerStatus
                            else
                                // not waiting on a join, but waiting for multiple targets
                                if itemAlreadyExists.IsNone
                                    then
                                        // If there are any tags at all set, you put stuff in under the defaults. Otherwise it's a Note
                                        let newCompilerStatus = 
                                            let currentLocation = incomingCompilerStatus.CurrentLocation
                                            if currentLocation.InHDDMode=false && currentLocation.AbstractionLevel=AbstractionLevels.None && currentLocation.Bucket=Buckets.None && currentLocation.Genre=Genres.None && currentLocation.TemporalIndicator=TemporalIndicators.None && currentLocation.AnnotationIndicator<>ANNOTATION_TOKEN_TYPE.None
                                                then
                                                    addAnnotation incomingCompilerStatus.CurrentLocation.ParentId incomingCompilerStatus.CurrentLocation.AnnotationIndicator incomingCommand.Value incomingLine incomingCompilerStatus 
                                                else
                                                    addModelItem incomingCompilerStatus incomingCompilerStatus.CurrentLocation incomingLine incomingCommand.Value
                                        newCompilerStatus
                                    else
                                        let newLocation = {incomingCompilerStatus.CurrentLocation with ParentId=itemAlreadyExists.Value.Id}
                                        {incomingCompilerStatus with CurrentLocation=newLocation}
                    |_->
                        let newModelItem =
                            {
                                Id=getNextModelItemNumber()
                                Location=incomingCompilerStatus.CurrentLocation
                                Description=incomingCommand.Value
                                Annotations=[||]
                                SourceReferences= [||]
                                Relations=[||]
                            }
                        let newModelItemList = [|newModelItem|] |> Array.append incomingCompilerStatus.ModelItems
                        {incomingCompilerStatus with ModelItems=newModelItemList}
            |Some token->
                match token.TargetType,token.Type,token.Category with 
                    |TOKEN_TARGET_TYPE.SINGLE_TARGET,TOKEN_TYPE.RELATIVE_LOCATOR,TOKEN_CATEGORY.MISC->
                        let newTempAnnotationIndicator=
                            match token.Token with 
                                | "Q " | "Q:" | "QUESTION " | "QUESTION"|"QUESTION:"->ANNOTATION_TOKEN_TYPE.Question
                                | "//" | "NOTE " | "NOTE: "|"NOTE:"->ANNOTATION_TOKEN_TYPE.Note
                                | "TODO " | "TODO: "|"TODO:"|"TO-DO"|"TO-DO: "|"TO-DO:"->ANNOTATION_TOKEN_TYPE.ToDo
                                | "WORK: " | "WORK "|"WORK:"->ANNOTATION_TOKEN_TYPE.Work
                                |_->ANNOTATION_TOKEN_TYPE.Note // ERROR ERROR
                        let newCompilerStatus = addAnnotation incomingCompilerStatus.CurrentLocation.ParentId newTempAnnotationIndicator incomingCommand.Value incomingLine incomingCompilerStatus 
                        //{newCompilerStatus with CompilerWaitingForState=CompilerWaitingFor.Nothing}
                        newCompilerStatus
                    |TOKEN_TARGET_TYPE.MULTIPLE_TARGETS,TOKEN_TYPE.RELATIVE_LOCATOR,TOKEN_CATEGORY.MISC->
                        let newTempAnnotationIndicator=
                            match token.Token with 
                                | "QUESTIONS"|"QUESTIONS " | "QUESTIONS: " | "QUESTIONS:"->ANNOTATION_TOKEN_TYPE.Question
                                | "NOTES"|"NOTES " | "NOTES: "|"NOTES:"->ANNOTATION_TOKEN_TYPE.Note
                                | "TODOS"|"TODOS " | "TODOS: "|"TODOS:"|"TO-DOS"|"TO-DOS "|"TO-DOS: "|"TO-DOS:"->ANNOTATION_TOKEN_TYPE.ToDo
                                | "WORKS"|"WORKS: " | "WORKS "|"WORKS:"->ANNOTATION_TOKEN_TYPE.Work
                                |_->ANNOTATION_TOKEN_TYPE.Note // ERROR ERROR
                        // run through everything split on this line by a comma and add
                        let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                        let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                let newAccumulatorCompilerStatus=addAnnotation accumulatorCompilerStatus.CurrentLocation.ParentId newTempAnnotationIndicator (x.Trim()) incomingLine accumulatorCompilerStatus 
                                                newAccumulatorCompilerStatus
                                                ) incomingCompilerStatus
                        let newLocation = {newCompilerStatus.CurrentLocation with AnnotationIndicator=newTempAnnotationIndicator}
                        //{newCompilerStatus with CompilerWaitingForState=CompilerWaitingFor.MultipleTargets; CurrentLocation=newLocation}
                        {newCompilerStatus with CurrentLocation=newLocation; CompilerState={newCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleTargets}}
                    |TOKEN_TARGET_TYPE.MULTIPLE_TARGETS,TOKEN_TYPE.ABSOLUTE_LOCATOR,_->
                        match token.Category with 
                            |TOKEN_CATEGORY.BUCKETS->
                                let newBucket = match token.Token with 
                                                | "BEHAVIOR" | "BEHAVIOR " | "BEHAVIORS" |"BEHAVIORS "->Buckets.Behavior
                                                | "STRUCTURE" | "STRUCTURE " | "STRUCTURES" | "STRUCTURES " ->Buckets.Structure
                                                | "SUPPLEMENTAL" | "SUPPLEMENTAL "|"SUPPLEMENTALS" |"SUPPLEMENTALS " |_->Buckets.Supplemental
                                let newParentId=if incomingCompilerStatus.CurrentLocation.Bucket<>Buckets.None then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                let currentLocation = incomingCompilerStatus.CurrentLocation
                                let newGenre= if currentLocation.Genre=Genres.None then Genres.Business else currentLocation.Genre
                                let newAbstractionLevel=if currentLocation.AbstractionLevel=AbstractionLevels.None then AbstractionLevels.Abstract else currentLocation.AbstractionLevel
                                let newTemporalIndicator=if currentLocation.TemporalIndicator=TemporalIndicators.None then TemporalIndicators.ToBe else currentLocation.TemporalIndicator
                                let newLocationPointer = {incomingCompilerStatus.CurrentLocation with Bucket=newBucket; Genre=newGenre; AbstractionLevel=newAbstractionLevel; TemporalIndicator=newTemporalIndicator; ParentId=newParentId}
                                let updatedCompilerStatus = {incomingCompilerStatus with CurrentLocation=newLocationPointer; CompilerState={incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleTargets}}
                                let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                            addModelItem accumulatorCompilerStatus newLocationPointer incomingLine (x.Trim())
                                                        ) updatedCompilerStatus
                                newCompilerStatus
                            |TOKEN_CATEGORY.ABSTRACTION_LEVEL->
                                let newAbstractionLevel = match token.Token with 
                                                            | "ABSTRACT" | "ABSTRACT "->AbstractionLevels.Abstract
                                                            | "REALIZED" | "REALIZED "|_->AbstractionLevels.Realized
                                let newParentId=if incomingCompilerStatus.CurrentLocation.AbstractionLevel<>AbstractionLevels.None then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                let newLocation = {incomingCompilerStatus.CurrentLocation with AbstractionLevel=newAbstractionLevel; ParentId=newParentId}
                                //{incomingCompilerStatus with CompilerWaitingForState=CompilerWaitingFor.MultipleTargets; CurrentLocation=newLocation}
                                {incomingCompilerStatus with CurrentLocation=newLocation; CompilerState={incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleTargets}}
                            |TOKEN_CATEGORY.GENRE->
                                let newGenre = match token.Token with 
                                                | "SYSTEM" | "SYSTEM "->Genres.System
                                                | "META" | "META "->Genres.Meta
                                                | "BUSINESS" | "BUSINESS "|_->Genres.Business
                                let newParentId=if incomingCompilerStatus.CurrentLocation.Genre<>Genres.None then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                let newLocation = {incomingCompilerStatus.CurrentLocation with Genre=newGenre; ParentId=newParentId}
                                //{incomingCompilerStatus with CompilerWaitingForState=CompilerWaitingFor.MultipleTargets; CurrentLocation=newLocation}
                                {incomingCompilerStatus with CurrentLocation=newLocation; CompilerState={incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleTargets}}
                            |TOKEN_CATEGORY.TEMPORAL->
                                let newTemporalIndicator = match token.Token with 
                                                            | "WAS" | "WAS "->TemporalIndicators.Was
                                                            | "TO-BE" | "TO-BE "->TemporalIndicators.ToBe
                                                            | "AS-IS" | "AS-IS "|_->TemporalIndicators.AsIs
                                let newParentId=if incomingCompilerStatus.CurrentLocation.TemporalIndicator<>TemporalIndicators.None then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                let newLocation = {incomingCompilerStatus.CurrentLocation with TemporalIndicator=newTemporalIndicator; ParentId=newParentId}
                                //{incomingCompilerStatus with CompilerWaitingForState=CompilerWaitingFor.MultipleTargets; CurrentLocation=newLocation}
                                {incomingCompilerStatus with CurrentLocation=newLocation; CompilerState={incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleTargets}}
                            |TOKEN_CATEGORY.HDD->
                                let newBucket=Buckets.None
                                let newAbstractionLevel=AbstractionLevels.None
                                let newGenre=Genres.None
                                let newTemporalIndicator=TemporalIndicators.None
                                let newInHDDMode=true
                                let newParentId=if incomingCompilerStatus.CurrentLocation.InHDDMode=false then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                let newLocation = {incomingCompilerStatus.CurrentLocation with InHDDMode=newInHDDMode; Bucket=newBucket; AbstractionLevel=newAbstractionLevel; Genre=newGenre; TemporalIndicator=newTemporalIndicator; ParentId=newParentId}
                                //{incomingCompilerStatus with CompilerWaitingForState=CompilerWaitingFor.MultipleTargets; CurrentLocation=newLocation}
                                {incomingCompilerStatus with CurrentLocation=newLocation; CompilerState={incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleTargets}}
                            |_->raise(new System.Exception("messed up"))
                    |TOKEN_TARGET_TYPE.SINGLE_TARGET,TOKEN_TYPE.JOINER,_->
                        incomingCompilerStatus
                    |TOKEN_TARGET_TYPE.MULTIPLE_TARGETS,TOKEN_TYPE.JOINER,_->
                        let joinType=match token.Token with 
                            | "PARENT"->ModelJoin.Parent
                            | "CHILD"->ModelJoin.Child
                            | "CHILDREN"->ModelJoin.Child
                            | "USES"->ModelJoin.Uses
                            | "USEDBY"->ModelJoin.UsedBy
                            | "AFFECTS"->ModelJoin.Affects
                            | "AFFECTEDBY"->ModelJoin.AffectedBy
                            |_->ModelJoin.Child
                        let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                        let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                    joinModelItems accumulatorCompilerStatus incomingCompilerStatus.CurrentLocation incomingLine joinType  (x.Trim())
                                                ) incomingCompilerStatus
                        {newCompilerStatus with CompilerState={newCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.NewJoin; LastJoinType=Some joinType}}
                    |_,_,_->incomingCompilerStatus
                

    let makeRawModel (incomingLines:IncomingLine []) (incomingCompilerStatus:CompilerReturn) =
        let initialModelLines = incomingLines |> Array.fold(fun (currentCompilerStatus:CompilerReturn) x->
                                        let modelItemsOnThisLine = x.Commands |> Array.fold(fun (currentLineCompilerStatus:CompilerReturn) y->
                                                                    let newCompilerStatus=updateModelLocationPointer currentLineCompilerStatus x y
                                                                    newCompilerStatus
                                                                    ) currentCompilerStatus
                                        let newacc = modelItemsOnThisLine
                                        newacc
                                ) incomingCompilerStatus
        initialModelLines

