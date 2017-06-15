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
    let getNextModelItemNumber()=IntegerFactory()

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
    type ModelLocationPointer =
        {
            LastFileNameProcessed:string
            Namespace:string
            ParentId:int
            TagValueList:(string*string) list
            InHDDMode:bool
            Bucket:Buckets
            Genre:Genres
            AbstractionLevel:AbstractionLevels
            TemporalIndicator:TemporalIndicators
            AnnotationIndicator:ANNOTATION_TOKEN_TYPE
        }
    let defaultModelLocationPointer =
        {
            LastFileNameProcessed=""
            Namespace = ""
            ParentId = -1
            TagValueList = []
            InHDDMode=false
            Bucket=Buckets.None
            Genre=Genres.None
            AbstractionLevel=AbstractionLevels.None
            TemporalIndicator=TemporalIndicators.None
            AnnotationIndicator=ANNOTATION_TOKEN_TYPE.None
        }
    [<NoComparison>]
    type ModelItem2 =
        {
            Id:int
            Location:ModelLocationPointer
            Description:string
            Annotations:(ANNOTATION_TOKEN_TYPE*string) []
            SourceReferences:IncomingLine []
        }
    let defaultModelItem2:ModelItem2 =
        {
            Id=(-1)
            Location=defaultModelLocationPointer
            Description=""
            Annotations=[||]
            SourceReferences=[||]
        }
    [<NoComparison>]
    type CompilerWaitingFor =
        |Nothing
        |SingleTarget
        |MultipleTargets
    [<NoComparison>]
    type CompilerReturn = 
        {
            CurrentLocation:ModelLocationPointer
            CompilerWaitingForState:CompilerWaitingFor
            CompilerMessages:CompilerMessage []
            ModelItems:ModelItem2 []
        }
    let beginningCompilerStatus =
        {
            CurrentLocation=defaultModelLocationPointer
            CompilerWaitingForState=CompilerWaitingFor.Nothing
            CompilerMessages=[||]
            ModelItems= [|defaultModelItem2|]
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
    let logCompilerMessage (compilerStatus:CompilerReturn) (newMessage:CompilerMessage) =
        let newMessages = [|newMessage|] |> Array.append compilerStatus.CompilerMessages
        {compilerStatus with CompilerMessages=newMessages}
    ///
    /// Takes a list of files, cleans and concatenates the contents of each one
    ///
    let bulkFileLineProcessing (filesToProcess:(System.IO.FileInfo*string []) []) =
        let newCompilerReturn = beginningCompilerStatus
        let completedRunningStatus =
            filesToProcess |> Array.fold(fun (acc:RunningStatus) x->
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
                    }
                let newLoc = {newLocationPointer with ParentId=newModelItem.Id}
                let newModelItems = [|newModelItem|] |> Array.append compilerStatus.ModelItems
                {compilerStatus with ModelItems=newModelItems; CurrentLocation=newLoc}
            else compilerStatus

    let updateModelItem (compilerStatus:CompilerReturn) (updatedModelItem:ModelItem2) = 
        let splitItemsListFirstPart =fst (compilerStatus.ModelItems |> Array.partition(fun z->z.Id<updatedModelItem.Id))
        let previousVersionOfItem = compilerStatus.ModelItems |> Array.find(fun x->x.Id=updatedModelItem.Id)
        let splitItemsListSecondPart =fst (compilerStatus.ModelItems |> Array.partition(fun z->z.Id>updatedModelItem.Id))
        let newModelItems =  splitItemsListSecondPart |> Array.append [|updatedModelItem|] |> Array.append splitItemsListFirstPart
        {compilerStatus with ModelItems=newModelItems}

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
        let incomingCompilerStatus=if incomingLine.File.FullName=originalCompilerStatus.CurrentLocation.LastFileNameProcessed
                                    then originalCompilerStatus
                                    else {originalCompilerStatus with CompilerWaitingForState=Nothing; CurrentLocation={defaultModelLocationPointer with LastFileNameProcessed=incomingLine.File.FullName}}
        let tokenForCommand = EasyAMTokens |> List.tryFind(fun z->z.Token.Trim()=incomingCommand.Token.Trim())
        match tokenForCommand with 
            |option.None->
                match incomingCompilerStatus.CompilerWaitingForState with 
                    |CompilerWaitingFor.Nothing->
                        let newCompilerStatus = addAnnotation incomingCompilerStatus.CurrentLocation.ParentId ANNOTATION_TOKEN_TYPE.Note incomingCommand.Value incomingLine incomingCompilerStatus 
                        newCompilerStatus
                    |CompilerWaitingFor.MultipleTargets->
                        // Does this item already exist? If so, don't make a new one. Use the old one as a new parent for whatever follows
                        let itemAlreadyExists=incomingCompilerStatus.ModelItems |> Array.tryFind(fun z->z.Description=incomingCommand.Value.Trim())
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
                        {newCompilerStatus with CompilerWaitingForState=CompilerWaitingFor.MultipleTargets; CurrentLocation=newLocation}
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
                                let updatedCompilerStatus = {incomingCompilerStatus with CompilerWaitingForState=CompilerWaitingFor.MultipleTargets; CurrentLocation=newLocationPointer}
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
                                {incomingCompilerStatus with CompilerWaitingForState=CompilerWaitingFor.MultipleTargets; CurrentLocation=newLocation}
                            |TOKEN_CATEGORY.GENRE->
                                let newGenre = match token.Token with 
                                                | "SYSTEM" | "SYSTEM "->Genres.System
                                                | "META" | "META "->Genres.Meta
                                                | "BUSINESS" | "BUSINESS "|_->Genres.Business
                                let newParentId=if incomingCompilerStatus.CurrentLocation.Genre<>Genres.None then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                let newLocation = {incomingCompilerStatus.CurrentLocation with Genre=newGenre; ParentId=newParentId}
                                {incomingCompilerStatus with CompilerWaitingForState=CompilerWaitingFor.MultipleTargets; CurrentLocation=newLocation}
                            |TOKEN_CATEGORY.TEMPORAL->
                                let newTemporalIndicator = match token.Token with 
                                                            | "WAS" | "WAS "->TemporalIndicators.Was
                                                            | "TO-BE" | "TO-BE "->TemporalIndicators.ToBe
                                                            | "AS-IS" | "AS-IS "|_->TemporalIndicators.AsIs
                                let newParentId=if incomingCompilerStatus.CurrentLocation.TemporalIndicator<>TemporalIndicators.None then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                let newLocation = {incomingCompilerStatus.CurrentLocation with TemporalIndicator=newTemporalIndicator; ParentId=newParentId}
                                {incomingCompilerStatus with CompilerWaitingForState=CompilerWaitingFor.MultipleTargets; CurrentLocation=newLocation}
                            |TOKEN_CATEGORY.HDD->
                                let newBucket=Buckets.None
                                let newAbstractionLevel=AbstractionLevels.None
                                let newGenre=Genres.None
                                let newTemporalIndicator=TemporalIndicators.None
                                let newInHDDMode=true
                                let newParentId=if incomingCompilerStatus.CurrentLocation.InHDDMode=false then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                let newLocation = {incomingCompilerStatus.CurrentLocation with InHDDMode=newInHDDMode; Bucket=newBucket; AbstractionLevel=newAbstractionLevel; Genre=newGenre; TemporalIndicator=newTemporalIndicator; ParentId=newParentId}
                                {incomingCompilerStatus with CompilerWaitingForState=CompilerWaitingFor.MultipleTargets; CurrentLocation=newLocation}
                            |_->raise(new System.Exception("messed up"))
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

