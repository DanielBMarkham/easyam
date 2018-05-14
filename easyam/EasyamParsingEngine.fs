module EasyamParsingEngine
    open Types
    open SAModel
    open Lenses
    open Utils
    open Persist

    let ModelItemIntegerFactory = 
        let counter = ref 0
        fun () -> 
            counter.Value <- !counter + 1
            !counter
    let getNextModelItemNumber()=ModelItemIntegerFactory()

    /// Helper function for token processing first step. Takes a list of tokens and a string and
    /// splits it into 3 parts: text before first-token-found, token text, and text after token
    let rec findInitialTextKeywordAndRemainingTextOnALine (tokenList:string list) (incomingLine:string):(string*string*string) option =
        // There's one oddball exception: if you have // at the beginning, the rest is a comment and there's nothing else to do
        if incomingLine.Length>1 && (incomingLine.GetLeft 2)="//"
            then Some("", "//", incomingLine.Substring(2))
            else
                let sourceStringFindAToken = tokenList |> String.concat "|" 
                let sourceStringFindTextUpToNextToken = tokenList |> List.map(fun x->"\b " + x + "")|> String.concat "|" 
                let regexFindAToken = new System.Text.RegularExpressions.Regex(sourceStringFindAToken)
                let regexFindTextUpToNextToken = new System.Text.RegularExpressions.Regex(sourceStringFindTextUpToNextToken)
        
                let tokensFound = regexFindAToken.Matches(incomingLine).toArray |> Array.filter(fun x->x.Length>0)
                let textUpUntilNextToken = regexFindTextUpToNextToken.Matches(incomingLine).toArray |> Array.filter(fun x->x.Length>0)
                let tokensWereFound = tokensFound.Length>0
                let textFoundUpUntilNextToken = textUpUntilNextToken.Length>0

                if tokensWereFound=false then Some("","", incomingLine.Trim())
                else
                    // we have tokens
                    if tokensFound.[0].Index>0
                        then // there's text before the token. Any text before a token starts is just text
                            let textBeforeToken = incomingLine.Substring(0, tokensFound.[0].Index-1)
                            let tokenAndStuffFollowing= incomingLine.Substring(tokensFound.[0].Index)
                            if textBeforeToken.Trim().Length=0
                                then
                                    findInitialTextKeywordAndRemainingTextOnALine tokenList tokenAndStuffFollowing
                                else
                                    Some(tokenAndStuffFollowing.Trim(), "", textBeforeToken.Trim())
                        else // it's just the token at the start and some stuff after that
                            let tokenText=tokensFound.[0]
                            let remainingLine=if tokenText.Length<incomingLine.Length then incomingLine.Substring(tokenText.Length) else ""
                            let tokensRemaining=regexFindAToken.Matches(remainingLine).toArray
                            let theresMoreTokensOnThisLine = tokensRemaining.Length>0

                            if theresMoreTokensOnThisLine
                                then // it's the token and everything up until the next token
                                    let nextTokenStartsAtIndex = tokensRemaining.[0].Index
                                    let textUpUntilNextToken:string = remainingLine.Substring(0, nextTokenStartsAtIndex)
                                    let nextTokenAndTextAfterIt:string = remainingLine.Substring(nextTokenStartsAtIndex)
                                    Some(nextTokenAndTextAfterIt.Trim(), tokenText.Value.Trim(), textUpUntilNextToken.Trim())
                                else // it's just the token and anything after it
                                    Some("", tokenText.Value.Trim(), remainingLine.Trim())
    exception CommandParsingException of Command list
    /// TOKEN PROCESSING FIRST STEP. TAKES A LINE AND MAKES A LIST OF COMMANDS AND VALUES
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
                            ((tabCount + (spaceCount/2)),incomingLineBeingProcessed.LineText.Substring(leadingWhiteSpace.Length))
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
    let logCompilerMessage (compilerStatus:CompilerReturn) (newMessage:CompilerMessage) =
        let newMessages = [|newMessage|] |> Array.append compilerStatus.CompilerMessages
        {compilerStatus with CompilerMessages=newMessages}
    let logCompilerMessageForASingleLine (compilerStatus:CompilerReturn) (messageType:CompilerMessageType) (messageDesc:string) (sourceLine:IncomingLine)  =
        let newCompilerMessage=
            {
                MessageType=messageType
                Message=messageDesc
                SourceFileShort=sourceLine.File.Name
                SourceFileLong=sourceLine.File.FullName
                SourceLineBegin=Some sourceLine.FileRawLineNumber
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
                SourceFileShort=firstSourceFileName
                SourceFileLong=firstSourceFileName
                SourceLineBegin=option.None
                SourceLineEnd=option.None
                SourceLineColumnBegin=option.None
                SourceLineColumnEnd=option.None
            }
        let completedAndUpdatedCompilerReturn=logCompilerMessage completedRunningStatus.CompilerReturn newCompilerMessage        
        completedRunningStatus.IncomingLinesConcatenated, completedAndUpdatedCompilerReturn

    /// adds/finds an item, updates the model, then returns the new model and the found/added item
    let addFindModelItem (compilerStatus:CompilerReturn) (incomingLine:IncomingLine) (forJoinType:ModelJoin option) (description:string) =
        if description.Trim().Length>0
            then
                let currentLocation=compilerStatus.CurrentLocation
                let targetLocation=
                    let defaultBucket=if currentLocation.Bucket=Buckets.None then Buckets.Behavior else currentLocation.Bucket
                    let newBucket=
                        if forJoinType.IsNone then
                            defaultBucket
                        else
                            match forJoinType.Value with 
                                |ModelJoin.AffectedBy->if defaultBucket=Behavior then Supplemental else defaultBucket
                                |ModelJoin.Affects->if defaultBucket=Supplemental then Behavior else defaultBucket
                                |ModelJoin.Child->defaultBucket
                                |ModelJoin.HasA->if defaultBucket=Structure then Structure else defaultBucket
                                |ModelJoin.IsOwnedByA->if defaultBucket=Structure then Structure else defaultBucket
                                |ModelJoin.Parent->defaultBucket
                                |ModelJoin.UsedBy->if defaultBucket=Structure then Behavior else defaultBucket
                                |ModelJoin.Uses->if defaultBucket=Behavior then Structure else defaultBucket
                    let newGenre= if currentLocation.Genre=Genres.None then Genres.Business else currentLocation.Genre
                    let newAbstractionLevel=if currentLocation.AbstractionLevel=AbstractionLevels.None then AbstractionLevels.Abstract else currentLocation.AbstractionLevel
                    let newTemporalIndicator=if currentLocation.TemporalIndicator=TemporalIndicators.None then TemporalIndicators.ToBe else currentLocation.TemporalIndicator
                    let newLocationPointer = {currentLocation with Bucket=newBucket; Genre=newGenre; AbstractionLevel=newAbstractionLevel; TemporalIndicator=newTemporalIndicator}
                    newLocationPointer
                let itemWithExactMatchExistsAtThisLocation =itemWithThisNameAlreadyExistsAtThisLocation compilerStatus targetLocation description
                let itemWithMatchExistsAtThisOrAHigherLevel=
                    // certain kinds of joins cannot point to higher-level items. It wouldn't make sense
                    if forJoinType.Value=ModelJoin.Affects || forJoinType.Value=ModelJoin.Child || forJoinType.Value=ModelJoin.HasA
                        then false
                        else itemWithThisNameAlreadyExistsEitherAtThisLocationOrHigher compilerStatus targetLocation description
                let itemWithoutRegardToNameSpacesExistsAtThisOrAHigherLevel=
                    // certain kinds of joins cannot point to higher-level items. It wouldn't make sense
                    if forJoinType.Value=ModelJoin.Affects || forJoinType.Value=ModelJoin.Child || forJoinType.Value=ModelJoin.HasA
                        then false                
                        else itemWithThisNameAlreadyExistsEitherAtThisLocationOrHigher compilerStatus {targetLocation with Namespace=""} description
                //let itemWithNoNameSpaceAndAbstratExistsAtTheTop= itemWithThisNameAlreadyExistsAtThisLocation compilerStatus {targetLocation with AbstractionLevel=Abstract; Namespace=""} description

                if itemWithExactMatchExistsAtThisLocation || itemWithMatchExistsAtThisOrAHigherLevel || itemWithoutRegardToNameSpacesExistsAtThisOrAHigherLevel
                    then
                        let alreadyExistingItem=
                            if itemWithExactMatchExistsAtThisLocation
                                then getItemWithThisNameAtThisLocation compilerStatus targetLocation description
                            elif itemWithMatchExistsAtThisOrAHigherLevel
                                then getItemWithThisNameEitherAtThisLocationOrHigher compilerStatus targetLocation description
                            else getItemWithThisNameEitherAtThisLocationOrHigher compilerStatus {targetLocation with Namespace=""} description
                        (compilerStatus,alreadyExistingItem)
                    else
                        let newModelItem =
                            {
                                Id=getNextModelItemNumber()
                                Location=targetLocation
                                Description=description.Trim()
                                Attributes=[||]
                                Annotations= [||]
                                SourceReferences=[|incomingLine|]
                                Relations=[||]
                                Tags=currentLocation.Tags
                            }
                        let newLoc = {targetLocation with ParentId=newModelItem.Id}
                        let newModelItems = [|newModelItem|] |> Array.append compilerStatus.ModelItems
                        //let newCompilerStatus={compilerStatus with ModelItems=newModelItems; CurrentLocation=newLoc;CompilerState={compilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.NewModelItem}}
                        let newCompilerStatus={compilerStatus with ModelItems=newModelItems;CompilerState={compilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.NewModelItem}}
                        (newCompilerStatus, newModelItem)
            else
                let rootItem=getModelItemById compilerStatus.ModelItems -1
                (compilerStatus,rootItem)

    let addModelItem (compilerStatus:CompilerReturn) (location:ModelLocationPointer) (incomingLine:IncomingLine) (desription:string) =
        if desription.Trim().Length>0
            then
                if itemWithThisNameAlreadyExistsAtThisLocation compilerStatus location desription
                    then
                        compilerStatus
                    else
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
                                Attributes=[||]
                                Annotations= [||]
                                SourceReferences=[|incomingLine|]
                                Relations=[||]
                                Tags=location.Tags
                            }
                        let newLoc = {newLocationPointer with ParentId=newModelItem.Id}
                        let newModelItems = [|newModelItem|] |> Array.append compilerStatus.ModelItems
                        {compilerStatus with ModelItems=newModelItems; CurrentLocation=newLoc;CompilerState={compilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.NewModelItem}}
            else compilerStatus
    /// Updates a ModelItem in place. Change your item, then pass it here for it to be updated in the array    
    let updateModelItem (compilerStatus:CompilerReturn) (updatedModelItem:ModelItem) = 
        let splitItemsListFirstPart =fst (compilerStatus.ModelItems |> Array.partition(fun z->z.Id<updatedModelItem.Id))
        let previousVersionOfItem = compilerStatus.ModelItems |> Array.find(fun x->x.Id=updatedModelItem.Id)
        let splitItemsListSecondPart =fst (compilerStatus.ModelItems |> Array.partition(fun z->z.Id>updatedModelItem.Id))
        let newModelItems =  splitItemsListSecondPart |> Array.append [|updatedModelItem|] |> Array.append splitItemsListFirstPart
        {compilerStatus with ModelItems=newModelItems}

    let makeATargetOfAJoinIfYouCan (compilerStatus:CompilerReturn) (incomingLine:IncomingLine) (joinType:ModelJoin) (description:string) =
        let itemAlreadyExists=compilerStatus.ModelItems|>Array.exists(fun x->x.Description=description)
        if itemAlreadyExists then compilerStatus else
        let newBucket = match joinType with
                        |ModelJoin.AffectedBy->Buckets.Supplemental
                        |ModelJoin.Affects->Buckets.Behavior
                        |ModelJoin.Child->compilerStatus.CurrentLocation.Bucket
                        |ModelJoin.Parent->compilerStatus.CurrentLocation.Bucket
                        |ModelJoin.HasA->Buckets.Structure
                        |ModelJoin.IsOwnedByA->Buckets.Structure
                        |ModelJoin.Uses->Buckets.Behavior
                        |ModelJoin.UsedBy->Buckets.Structure
        let newLocationForTarget={compilerStatus.CurrentLocation with Bucket=newBucket}
        addModelItem compilerStatus newLocationForTarget incomingLine description
    let rec joinModelItems2 (compilerStatus:CompilerReturn) (incomingLine:IncomingLine) (sourceItem:ModelItem) (targetItem:ModelItem) (joinType:ModelJoin) =
        // if the target is already in the relations list, log and bail out
        let reverseJoin = getReverseJoin joinType
        let targetAlreadyExistsInSourcesRelationsArray = sourceItem.Relations|>Array.exists(fun z->z.TargetId=targetItem.Id)
        if targetAlreadyExistsInSourcesRelationsArray
            then
                let alreadyExistEntry=sourceItem.Relations|>Array.find(fun z->z.TargetId=targetItem.Id)
                let reference="Previous join exists on " + (alreadyExistEntry.SourceReference.File.Name + "(" + string alreadyExistEntry.SourceReference.FileCompilationNumber + ") line " + string alreadyExistEntry.SourceReference.FileRawLineNumber + ".")
                let ret=logCompilerMessageForASingleLine compilerStatus CompilerMessageType.Warning ("target item " + targetItem.Description+ " already exists once as a join. There is a duplicate entry. " + reference) incomingLine
                let newLocation = {compilerStatus.CurrentLocation with RelationTargetId=Some targetItem.Id; RelationSourceId=Some sourceItem.Id}
                {ret with CurrentLocation=newLocation}
            else
                // add a join to both model items. Be sure to include the source reference
                let newSourceItemJoinRelation={id=getNextModelItemNumber(); ModelJoinType=joinType; TargetId=targetItem.Id; SourceReference=incomingLine}
                let newTargetItemJoinRelation={id=getNextModelItemNumber(); ModelJoinType=reverseJoin; TargetId=sourceItem.Id; SourceReference=incomingLine}
                let newSourceItemJoinRelations=[|newSourceItemJoinRelation|] |> Array.append sourceItem.Relations
                let newTargetItemJoinRelations=[|newTargetItemJoinRelation|] |> Array.append targetItem.Relations
                let newSourceItem={sourceItem with Relations=newSourceItemJoinRelations}
                let newTargetItem={targetItem with Relations=newTargetItemJoinRelations}
                let compilerStatusWithNewSourcce = updateModelItem compilerStatus newSourceItem
                let compilerStatusWithNewSourceAndTarget = updateModelItem compilerStatusWithNewSourcce newTargetItem                        
                let newLocation = {compilerStatus.CurrentLocation with RelationTargetId=Some targetItem.Id; RelationSourceId=Some newSourceItem.Id; RelationType=Some joinType}
                let newState = {compilerStatusWithNewSourceAndTarget.CompilerState with LastCompilerOperation=LastCompilerOperations.NewJoin}
                {compilerStatusWithNewSourceAndTarget with CurrentLocation=newLocation; CompilerState=newState}

    let rec joinModelItems (compilerStatus:CompilerReturn) (location:ModelLocationPointer) (incomingLine:IncomingLine) (joinType:ModelJoin) (sourceDescription:string) (targetDescription:string) =
        // find the model item that is the target of this join and update both the current and target item
        // if the target does not exist, register a compiler error (but keep processing)
        if sourceDescription=""||targetDescription="" then raise (new System.Exception("asd"))
        let possibleSource=compilerStatus.ModelItems |> Array.tryFind(fun x->x.Description=sourceDescription && x.Id<>(-1))
        let possibleTarget=compilerStatus.ModelItems |> Array.tryFind(fun x->x.Description.Trim()=targetDescription.Trim() && x.Id<>(-1))
        let reverseJoin = getReverseJoin joinType
        match possibleSource, possibleTarget with
            |Some sourceItem, Some targetItem->
                // if the target is already in the relations list, log and bail out
                let targetAlreadyExists = sourceItem.Relations|>Array.exists(fun z->z.TargetId=targetItem.Id)
                if targetAlreadyExists
                    then
                        let alreadyExistEntry=sourceItem.Relations|>Array.find(fun z->z.TargetId=targetItem.Id)
                        let reference="Previous join exists on " + (alreadyExistEntry.SourceReference.File.Name + "(" + string alreadyExistEntry.SourceReference.FileCompilationNumber + ") line " + string alreadyExistEntry.SourceReference.FileRawLineNumber + ".")
                        let ret=logCompilerMessageForASingleLine compilerStatus CompilerMessageType.Warning ("target item " + targetDescription + " already exists once as a join. There is a duplicate entry. " + reference) incomingLine
                        let newLocation = {compilerStatus.CurrentLocation with RelationTargetId=Some targetItem.Id; RelationSourceId=Some possibleSource.Value.Id}
                        {ret with CurrentLocation=newLocation}
                    else
                        // add a join to both model items. Be sure to include the source reference
                        let newSourceItemJoinRelation={id=getNextModelItemNumber(); ModelJoinType=joinType; TargetId=targetItem.Id; SourceReference=incomingLine}
                        let newTargetItemJoinRelation={id=getNextModelItemNumber(); ModelJoinType=reverseJoin; TargetId=sourceItem.Id; SourceReference=incomingLine}
                        let newSourceItemJoinRelations=[|newSourceItemJoinRelation|] |> Array.append sourceItem.Relations
                        let newTargetItemJoinRelations=[|newTargetItemJoinRelation|] |> Array.append targetItem.Relations
                        let newSourceItem={sourceItem with Relations=newSourceItemJoinRelations}
                        let newTargetItem={targetItem with Relations=newTargetItemJoinRelations}
                        let compilerStatusWithNewSourcce = updateModelItem compilerStatus newSourceItem
                        let compilerStatusWithNewSourceAndTarget = updateModelItem compilerStatusWithNewSourcce newTargetItem                        
                        let newLocation = {compilerStatus.CurrentLocation with RelationTargetId=Some targetItem.Id; RelationSourceId=Some newSourceItem.Id; RelationType=Some joinType}
                        let newState = {compilerStatusWithNewSourceAndTarget.CompilerState with LastCompilerOperation=LastCompilerOperations.NewJoin}
                        {compilerStatusWithNewSourceAndTarget with CurrentLocation=newLocation; CompilerState=newState}
            |Some sourceItem, option.None->
                if joinType=ModelJoin.HasA
                    then
                        let updatedCompilerStatus=addModelItem compilerStatus location incomingLine targetDescription
                        joinModelItems updatedCompilerStatus location incomingLine joinType sourceDescription targetDescription
                    elif joinType=ModelJoin.AffectedBy then
                        let newLoc = {location with Bucket=Buckets.Supplemental}
                        let updatedCompilerStatus=addModelItem compilerStatus newLoc incomingLine targetDescription
                        joinModelItems updatedCompilerStatus location incomingLine joinType sourceDescription targetDescription
                    else
                        logCompilerMessageForASingleLine compilerStatus CompilerMessageType.Warning ("target item " + targetDescription + " does not currently exist in model when trying to make join. Skipping.") incomingLine
            |option.None, Some targetItem->
                if joinType=ModelJoin.HasA
                    then
                        let updatedCompilerStatus=addModelItem compilerStatus location incomingLine targetDescription
                        joinModelItems updatedCompilerStatus location incomingLine joinType sourceDescription targetDescription
                    elif joinType=ModelJoin.AffectedBy then
                        let newLoc = {location with Bucket=Buckets.Supplemental}
                        let updatedCompilerStatus=addModelItem compilerStatus newLoc incomingLine targetDescription
                        joinModelItems updatedCompilerStatus location incomingLine joinType sourceDescription targetDescription
                    else
                        logCompilerMessageForASingleLine compilerStatus CompilerMessageType.Error ("source item " + targetDescription+ " does not currently exist in model when trying to make join") incomingLine
            |option.None, option.None->
                logCompilerMessageForASingleLine compilerStatus CompilerMessageType.Info "programming error. Trying to create a join where neither item exists in the model" incomingLine
     
    let oneItemCanConnectLikeThisToAnother (item1:ModelItem) (item2:ModelItem) (joinType:ModelJoin) =
        // parent-child relationships work on everything as long as they're the same bucket
        if (joinType=ModelJoin.Parent || joinType=ModelJoin.Child) && (item1.Location.Bucket=item2.Location.Bucket)
            then true
            else
                match joinType, item1.Location.Bucket, item2.Location.Bucket with 
                        |ModelJoin.Affects, Buckets.Supplemental, Buckets.Behavior->true
                        |ModelJoin.AffectedBy, Buckets.Behavior, Buckets.Supplemental->true
                        |ModelJoin.Affects,_,_ | ModelJoin.AffectedBy,_,_->false
                        |ModelJoin.Uses, Buckets.Behavior, Buckets.Structure->true
                        |ModelJoin.UsedBy, Buckets.Structure, Buckets.Behavior->true
                        |ModelJoin.Uses,_,_ | ModelJoin.UsedBy,_,_->false
                        |ModelJoin.HasA, Buckets.Structure, Buckets.Structure->true
                        |ModelJoin.IsOwnedByA,Buckets.Structure, Buckets.Structure->true
                        |ModelJoin.HasA,_,_ | ModelJoin.IsOwnedByA,_,_->false
                        //TODO add in joins for HDD and Tasks
                        |_,_,_->false
    let itemsThatCanJoinToMe (compilerStatus:CompilerReturn) (item:ModelItem) (joinType:ModelJoin) =
        let itemArray=compilerStatus.ModelItems
        itemArray |> Array.filter(fun x->
            (oneItemCanConnectLikeThisToAnother item x joinType)
            )

    let addAnnotation (parentId:int) (annotationType:AnnotationTokenType) (annotationValue:string) (sourceLine:IncomingLine) (currentCompilerStatus:CompilerReturn) =
        if annotationValue="" then currentCompilerStatus
            else
                let modelItemToChange = currentCompilerStatus.ModelItems |> Array.tryFind(fun x->x.Id=parentId)
                if modelItemToChange.IsNone
                    then currentCompilerStatus
                    else
                        let annotationAlreadyExists=modelItemToChange.Value.Annotations |> Array.exists(fun z->(z.AnnotationType=annotationType) && ( z.AnnotationText=annotationValue.Trim()))
                        if annotationAlreadyExists 
                            then currentCompilerStatus 
                            else
                                let newAnnotation = {AnnotationType=annotationType;AnnotationText=annotationValue.Trim()}
                                let newAnnotations = [|newAnnotation|] |> Array.append modelItemToChange.Value.Annotations
                                let newSourceReferences = [|sourceLine|] |> Array.append modelItemToChange.Value.SourceReferences
                                let newModelItem = {modelItemToChange.Value with Annotations=newAnnotations; SourceReferences=newSourceReferences}
                                // the indent does not go up for notes ON THE SAME LINE AS COMMANDS
                                let newIndent = 
                                    if annotationType=AnnotationTokenType.Note && sourceLine.Commands.Length>1 then
                                        if currentCompilerStatus.CompilerState.CurrentIndentLevel>0 then currentCompilerStatus.CompilerState.CurrentIndentLevel-1 else 0
                                    else currentCompilerStatus.CompilerState.CurrentIndentLevel
                                let newCompilerState = {currentCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.NewAnnotation; CurrentIndentLevel=newIndent}
                                let newLoc = {currentCompilerStatus.CurrentLocation with AttributeId=option.None; AttributeType=option.None; CurrentId=newModelItem.Id}
                                let newCompilerStatus=updateModelItem {currentCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newLoc} newModelItem
                                newCompilerStatus
    let addAttributeAnnotation (parentId:int) (attributeTargetId:int) (annotationType:AnnotationTokenType) (annotationValue:string) (sourceLine:IncomingLine) (currentCompilerStatus:CompilerReturn) =
        let modelItemToChange = currentCompilerStatus.ModelItems |> Array.tryFind(fun x->x.Id=parentId)
        let attributeToChange = modelItemToChange.Value.Attributes|> Array.tryFind(fun x->x.id=attributeTargetId)
        if annotationValue="" 
            then 
                let newCompilerState = {currentCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotations}
                let newCompilerLocation = {currentCompilerStatus.CurrentLocation with AttributeId = Some attributeTargetId; AttributeType = Some attributeToChange.Value.AttributeType}
                {currentCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newCompilerLocation}                        
                //currentCompilerStatus
            else
                //let modelItemToChange = currentCompilerStatus.ModelItems |> Array.tryFind(fun x->x.Id=parentId)
                if modelItemToChange.IsNone
                    then currentCompilerStatus
                    else                
                        //let attributeToChange = modelItemToChange.Value.Attributes|> Array.tryFind(fun x->x.id=attributeTargetId)
                        if attributeToChange.IsNone then currentCompilerStatus
                            else
                                let newAnnotation = {AnnotationType=annotationType;AnnotationText=annotationValue.Trim()}
                                let newAnnotations = [|newAnnotation|] |> Array.append attributeToChange.Value.Annotations
                                let newAttribute = {attributeToChange.Value with Annotations=newAnnotations}
                                let newAttributes = replaceArrayItemInPlace modelItemToChange.Value.Attributes newAttribute (fun x y->x.id=y.id)
                                let isLineAlreadyREferencedInModelItem = modelItemToChange.Value.SourceReferences|>Array.exists(fun x->x.SourceRawLineNumber=sourceLine.SourceRawLineNumber)
                                let newModelItemSourceReferences=if isLineAlreadyREferencedInModelItem then modelItemToChange.Value.SourceReferences else  ([|sourceLine|] |> Array.append modelItemToChange.Value.SourceReferences)
                                let newModelItem = {modelItemToChange.Value with SourceReferences=newModelItemSourceReferences; Attributes=newAttributes}
                                let newCompilerState = {currentCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.NewAttributeAnnotation; WaitingFor=MultipleAttributeAnnotations}
                                let newCompilerLocation = {currentCompilerStatus.CurrentLocation with AttributeId = Some newAttribute.id; AttributeType = Some newAttribute.AttributeType; CurrentId=newModelItem.Id;}
                                updateModelItem {currentCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newCompilerLocation} newModelItem
    let addModelAttribute(attributeType:ModelAttributeTypes) (attributeDescription:string) (sourceLine:IncomingLine) (currentCompilerStatus:CompilerReturn) =
        if attributeDescription="" then currentCompilerStatus
            else
                let newModelAttribute=
                    {
                        id=getNextModelItemNumber()
                        ModelItemParentId=currentCompilerStatus.CurrentLocation.ParentId
                        AttributeType=attributeType
                        Description=attributeDescription
                        Annotations= [||]
                        SourceReferences=[|sourceLine|]
                    }
                let targetModelItem=currentCompilerStatus.ModelItems|>Array.find(fun x->x.Id=currentCompilerStatus.CurrentLocation.ParentId)
                // if the attribute already exists then bail, otherwise add
                if targetModelItem.Attributes|>Array.exists(fun x->x.Description=attributeDescription&&x.AttributeType=attributeType)
                    then currentCompilerStatus
                    else
                        let newTargetModelItemSourceReferences = [|sourceLine|] |> Array.append targetModelItem.SourceReferences
                        let newTargetModelItemAttributes = [|newModelAttribute|] |> Array.append targetModelItem.Attributes
                        let newCompilerState = {currentCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.NewAttribute}
                        let modifiedModelItem = {targetModelItem with SourceReferences=newTargetModelItemSourceReferences; Attributes=newTargetModelItemAttributes}
                        let newCompilerLocation ={currentCompilerStatus.CurrentLocation with AttributeId = Some newModelAttribute.id; AttributeType=Some newModelAttribute.AttributeType; CurrentId=modifiedModelItem.Id}
                        updateModelItem {currentCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newCompilerLocation} modifiedModelItem
    let areTheseTwoKeyValueStringArraysEqual (array1:System.Collections.Generic.KeyValuePair<string,string> []) (array2:System.Collections.Generic.KeyValuePair<string,string> []) =
        if array1.Length<>array2.Length then false else
        if array1.Length=0 then true else
        let itemsAreEqual (item1:System.Collections.Generic.KeyValuePair<string,string>) (item2:System.Collections.Generic.KeyValuePair<string,string>) = (item1.Key=item2.Key) && (item1.Value=item2.Value)
        let array1FoundIn2 = array1|>Array.fold(fun acc x->array2|>Array.exists(fun y->(itemsAreEqual y x)) && acc) true
        let array2FoundIn1 = array2|>Array.fold(fun acc x->array1|>Array.exists(fun y->(itemsAreEqual y x)) && acc) true
        (array1FoundIn2 && array2FoundIn1)
    /// This takes a ModelItem and the current compiler status and adds all the name/val tags for where we are and updates the item
    let updateExistingItemWithCurrentLocationExtras (compilerStatus:CompilerReturn) (id:int) =
        let doesItemExistInModel = doesModelItemExistInModelById compilerStatus.ModelItems id 
        if doesItemExistInModel=false then compilerStatus else
        let itemToChange = getModelItemById compilerStatus.ModelItems id
        let tagsAreTheSame = areTheseTwoKeyValueStringArraysEqual itemToChange.Tags compilerStatus.CurrentLocation.Tags
        if tagsAreTheSame=true then compilerStatus else
        // if they're not the same, there's no merge. Instead whatever's in location gets overwritten into the item
        let changedItem={itemToChange with Tags=compilerStatus.CurrentLocation.Tags}
        updateModelItem compilerStatus changedItem

    let updateTheModelThroughCommaSplittedItems (incomingCompilerStatus:CompilerReturn) (stringToSplit:string) (fnProcessItem:CompilerReturn->string->CompilerReturn) =
        let splitByComma = stringToSplit.Split([|","|], System.StringSplitOptions.None)
        splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
            fnProcessItem accumulatorCompilerStatus x 
        ) incomingCompilerStatus



    /// Main loop. Processes a command token, changes the model array, compiler state and location pointer
    let rec updateModelLocationPointer (originalCompilerStatus:CompilerReturn) (incomingLine:IncomingLine) (incomingCommand:Command):CompilerReturn =
        // context resets when the file changes

        let fileCheckedCompilerStatus=if incomingLine.File.FullName=originalCompilerStatus.CompilerState.LastFileNameProcessed
                                        then
                                            let locationPointerBeforeWeStart=originalCompilerStatus.CurrentLocation
                                            let newLocationStack=
                                                // if we're at a new location, push it on the stack. Otherwise, not.
                                                if originalCompilerStatus.CompilerState.LocationStack.length>0 && locationPointerBeforeWeStart<>(peek originalCompilerStatus.CompilerState.LocationStack) then pushStack (originalCompilerStatus.CurrentLocation) originalCompilerStatus.CompilerState.LocationStack else originalCompilerStatus.CompilerState.LocationStack
                                            let newCompilerState={originalCompilerStatus.CompilerState with LocationStack=newLocationStack}
                                            {originalCompilerStatus with CompilerState=newCompilerState}
                                        else 
                                            {originalCompilerStatus with CompilerState={defaultCompilerState with LastFileNameProcessed=incomingLine.File.FullName; LocationStack=Types.Stack<ModelLocationPointer>.empty}; CurrentLocation=defaultModelLocationPointer}
        let newIndentLevelChange=if incomingCommand.CommandIndentLevel<fileCheckedCompilerStatus.CompilerState.CurrentIndentLevel 
                                        then IndentLevelComparisons.IndentIsLessThanPreviousIndent
                                    elif incomingCommand.CommandIndentLevel=fileCheckedCompilerStatus.CompilerState.CurrentIndentLevel
                                        then IndentLevelComparisons.IndentIsSameAsPreviousIndent
                                    else IndentLevelComparisons.IndentIsMoreThanPreviousIndent
        let newCompilerState={fileCheckedCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; IndentLevelChange=newIndentLevelChange}
        let compilerStatusWithIndentsApplied = {fileCheckedCompilerStatus with CompilerState=newCompilerState}
        let levelsJumped = System.Math.Abs(originalCompilerStatus.CompilerState.CurrentIndentLevel-compilerStatusWithIndentsApplied.CompilerState.CurrentIndentLevel)
        let newLocationStack = match newIndentLevelChange with 
                                |IndentIsLessThanPreviousIndent->snd (compilerStatusWithIndentsApplied.CompilerState.LocationStack |> popMany levelsJumped)
                                |IndentIsSameAsPreviousIndent->compilerStatusWithIndentsApplied.CompilerState.LocationStack
                                |IndentIsMoreThanPreviousIndent->
                                    let topOfLocationStack=(peek compilerStatusWithIndentsApplied.CompilerState.LocationStack)
                                    pushStackNTimes compilerStatusWithIndentsApplied.CompilerState.LocationStack topOfLocationStack (levelsJumped-1)
        let incomingCompilerStatus={compilerStatusWithIndentsApplied with CompilerState={compilerStatusWithIndentsApplied.CompilerState with LocationStack=newLocationStack}}
        let numberOfCommaSeparatedCommandSegments=incomingCommand.Value.Split([|","|], System.StringSplitOptions.None).Length
        let tokenForCommand = 
            let tokenSimpleFind=EasyAMTokens |> List.tryFind(fun z->z.Token.Trim()=incomingCommand.Token.Trim())
            if tokenSimpleFind.IsSome then tokenSimpleFind
                elif ((incomingCommand.Token + incomingCommand.Value).GetLeft 1 = "@") || ((incomingCommand.Token + incomingCommand.Value).GetLeft 1 = "&") then EasyAMTokens |> List.tryFind(fun z->(z.Token.GetLeft 1 = "@") || (z.Token.GetLeft 1 = "&"))
                else Option.None
        // If there's no token, we look at the indent level and compare it to what we were expecting
        // Otherwise process the token
        match tokenForCommand with 
            |option.None->
                match incomingCompilerStatus.CompilerState.WaitingFor with 
                    |CompilerWaitingFor.Nothing->
                        let newCompilerStatus = addAnnotation incomingCompilerStatus.CurrentLocation.ParentId AnnotationTokenType.Note incomingCommand.Value incomingLine incomingCompilerStatus 
                        newCompilerStatus
                    |CompilerWaitingFor.MultipleModelItems->
                            let itemAlreadyExists = doesModelItemExistInModelByDescription incomingCompilerStatus.ModelItems incomingCommand.Value
                            if itemAlreadyExists=false
                                then
                                    // If there are any tags at all set, you put stuff in under the defaults. Otherwise it's a Note
                                    let newCompilerStatus = 
                                        let currentLocation = incomingCompilerStatus.CurrentLocation
                                        if currentLocation.InHDDMode=false && currentLocation.AbstractionLevel=AbstractionLevels.None && currentLocation.Bucket=Buckets.None && currentLocation.Genre=Genres.None && currentLocation.TemporalIndicator=TemporalIndicators.None && currentLocation.AnnotationIndicator<>AnnotationTokenType.None
                                            then
                                                addAnnotation incomingCompilerStatus.CurrentLocation.ParentId incomingCompilerStatus.CurrentLocation.AnnotationIndicator incomingCommand.Value incomingLine incomingCompilerStatus 
                                            else
                                                addModelItem incomingCompilerStatus incomingCompilerStatus.CurrentLocation incomingLine incomingCommand.Value
                                    newCompilerStatus
                                elif incomingCompilerStatus.CompilerState.LastCompilerOperation<>LastCompilerOperations.NewJoin && incomingCompilerStatus.CompilerState.WaitingFor<>CompilerWaitingFor.MultipleRelations
                                    then
                                        let alreadyExistingItem=getModelItemByDescription incomingCompilerStatus.ModelItems incomingCommand.Value
                                        let compilerStatusWithExtrasCheckedForExistingItem=updateExistingItemWithCurrentLocationExtras incomingCompilerStatus alreadyExistingItem.Id
                                        let newLocation = {compilerStatusWithExtrasCheckedForExistingItem.CurrentLocation with ParentId=alreadyExistingItem.Id; CurrentId=alreadyExistingItem.Id}
                                        let newState = {compilerStatusWithExtrasCheckedForExistingItem.CompilerState with WaitingFor=compilerStatusWithExtrasCheckedForExistingItem.CompilerState.WaitingFor; LastJoinType=option.None; LastCompilerOperation=LastCompilerOperations.ReferenceExistingItem}
                                        {compilerStatusWithExtrasCheckedForExistingItem with CurrentLocation=newLocation; CompilerState=newState}
                                else
                                match incomingCompilerStatus.CompilerState.IndentLevelChange with 
                                        |IndentIsSameAsPreviousIndent |IndentIsMoreThanPreviousIndent->
                                                updateTheModelThroughCommaSplittedItems incomingCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                                    let itemAlreadyExists = doesModelItemExistInModelByDescription incomingCompilerStatus.ModelItems incomingCommand.Value
                                                    let lastJoinType=incomingCompilerStatus.CurrentLocation.RelationType.Value
                                                    let commaSplitItemAlreadyExists=doesModelItemExistInModelByDescription accumulatorCompilerStatus.ModelItems x 
                                                    if commaSplitItemAlreadyExists
                                                        then
                                                            let currentTargetDescription=tryFindModelItemDescriptionForId accumulatorCompilerStatus.ModelItems accumulatorCompilerStatus.CurrentLocation.ParentId //accumulatorCompilerStatus.ModelItems|>Array.tryFind(fun x->x.Id=accumulatorCompilerStatus.CurrentLocation.ParentId)
                                                            let currentTargetDescription=if currentTargetDescription.IsSome then currentTargetDescription.Value.Description else ""
                                                            let joinedCompilerStatus=joinModelItems accumulatorCompilerStatus accumulatorCompilerStatus.CurrentLocation incomingLine lastJoinType currentTargetDescription  (x.Trim())
                                                            let compilerStatusWithExtrasCheckedForExistingItem=updateExistingItemWithCurrentLocationExtras joinedCompilerStatus joinedCompilerStatus.CurrentLocation.ParentId
                                                            compilerStatusWithExtrasCheckedForExistingItem
                                                        else
                                                        let lastModelParent = findModelItemForId incomingCompilerStatus.ModelItems incomingCompilerStatus.CurrentLocation.ParentId
                                                        let targetForPossibleComments = if lastModelParent.Relations.Length>0
                                                                                        then
                                                                                            let lastJoinedTarget=lastModelParent.Relations.[lastModelParent.Relations.Length-1].TargetId
                                                                                            incomingCompilerStatus.ModelItems |> Array.find(fun x->x.Id=lastJoinedTarget)
                                                                                        else
                                                                                            incomingCompilerStatus.ModelItems |> Array.find(fun x->x.Id=incomingCompilerStatus.CurrentLocation.ParentId)
                                                        let updatedCompilerStatus=addModelItem incomingCompilerStatus incomingCompilerStatus.CurrentLocation incomingLine incomingCommand.Value
                                                        let currentTargetDescription=updatedCompilerStatus.ModelItems|>Array.tryFind(fun x->x.Id=updatedCompilerStatus.CurrentLocation.ParentId)
                                                        let currentTargetDescription=if currentTargetDescription.IsSome then currentTargetDescription.Value.Description else ""
                                                        joinModelItems updatedCompilerStatus updatedCompilerStatus.CurrentLocation incomingLine lastJoinType currentTargetDescription (incomingCommand.Value)
                                                )
                                        |IndentIsLessThanPreviousIndent->
                                            if itemAlreadyExists
                                                then
                                                    let alreadyExistingItem=getModelItemByDescription incomingCompilerStatus.ModelItems incomingCommand.Value
                                                    // multiple targets, there's a join, the indent is less, and the item already exists
                                                    // We have a new parent
                                                    let newLocation = {incomingCompilerStatus.CurrentLocation with ParentId=alreadyExistingItem.Id}
                                                    let newState = {incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.Nothing; LastJoinType=option.None; LastCompilerOperation=LastCompilerOperations.ReferenceExistingItem}
                                                    let compilerStatusWithExtrasCheckedForExistingItem=updateExistingItemWithCurrentLocationExtras incomingCompilerStatus alreadyExistingItem.Id
                                                    {compilerStatusWithExtrasCheckedForExistingItem with CurrentLocation=newLocation; CompilerState=newState}                                                
                                                else
                                                    addModelItem incomingCompilerStatus incomingCompilerStatus.CurrentLocation incomingLine (incomingCommand.Value.Trim())
                    |CompilerWaitingFor.MultipleAttributes->
                        let newAttributeType = 
                            if incomingCompilerStatus.CurrentLocation.AttributeType.IsSome
                                then
                                    incomingCompilerStatus.CurrentLocation.AttributeType.Value
                                elif incomingCompilerStatus.CurrentLocation.AttributeId.IsSome 
                                    then 
                                        let oldAttId=incomingCompilerStatus.CurrentLocation.AttributeId.Value
                                        (getAttributeById incomingCompilerStatus.ModelItems oldAttId).AttributeType
                                else defaultModelItemAttribute.AttributeType
                        //match newIndentLevel with 
                        match incomingCompilerStatus.CompilerState.IndentLevelChange with
                            IndentIsSameAsPreviousIndent|IndentIsMoreThanPreviousIndent->
                                        // if we're further idented, we're continuing the previous att list
                                        updateTheModelThroughCommaSplittedItems incomingCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                            if x.Length=0 then accumulatorCompilerStatus else
                                            let modelWithAddedAttribute=addModelAttribute newAttributeType (x.Trim()) incomingLine accumulatorCompilerStatus
                                            let newCompilerState={modelWithAddedAttribute.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAttributes}
                                            {modelWithAddedAttribute with CompilerState=newCompilerState}
                                        )
                            |IndentIsLessThanPreviousIndent->
                                if levelsJumped=1
                                    then
                                        // if you jump one level, wait for whatever it was you were waiting for last
                                        // if you were adding attributes and pop back one, you have to be on the model item list
                                        let lastLoc=
                                            {incomingCompilerStatus.CurrentLocation with AttributeId=option.None; AttributeType=option.None}
                                            //if incomingCompilerStatus.CompilerState.LocationStack.length>0 
                                            //    then (snd (pop incomingCompilerStatus.CompilerState.LocationStack)).asList.Head // ugly early morn coding
                                            //    else incomingCompilerStatus.CurrentLocation
                                        let newCompilerState={incomingCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; LastCompilerOperation=LastCompilerOperations.PointerReset; WaitingFor=CompilerWaitingFor.MultipleModelItems}
                                        //let newCompilerLocation={incomingCompilerStatus.CurrentLocation with AttributeType=option.None; AttributeId=option.None; ParentId=(-1); CurrentId=(-1)}
                                        //let newCompilerStatus={incomingCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newCompilerLocation}
                                        let newCompilerStatus={incomingCompilerStatus with CompilerState=newCompilerState; CurrentLocation=lastLoc}
                                        updateModelLocationPointer newCompilerStatus incomingLine incomingCommand 
                                    else
                                        let newCompilerState={incomingCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; LastCompilerOperation=LastCompilerOperations.PointerReset; WaitingFor=CompilerWaitingFor.MultipleModelItems}
                                        let newCompilerLocation={incomingCompilerStatus.CurrentLocation with AttributeType=option.None; AttributeId=option.None; ParentId=(-1); CurrentId=(-1)}
                                        let newCompilerStatus={incomingCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newCompilerLocation}
                                        updateModelLocationPointer newCompilerStatus incomingLine incomingCommand 
                    |CompilerWaitingFor.MultipleAnnotations | CompilerWaitingFor.MultipleAttributeAnnotations->
                        match incomingCompilerStatus.CompilerState.IndentLevelChange with 
                            |IndentIsLessThanPreviousIndent->
                                // what's the next level up?
                                if incomingCompilerStatus.CurrentLocation.AttributeId.IsSome && incomingCompilerStatus.CurrentLocation.AttributeType.IsSome
                                    then
                                        // if the last annotation was on an attribute, then we pop back to the attribute, depending on levels jumped
                                        let welastAnnotatedAnAttribute = incomingCompilerStatus.CurrentLocation.AttributeId.IsSome
                                        let newCompilerLocation={incomingCompilerStatus.CurrentLocation with AnnotationIndicator=AnnotationTokenType.None}
                                        let newCompilerState=
                                            if levelsJumped = 1 && welastAnnotatedAnAttribute
                                                then
                                                    // it's popped one level back up to the attribute
                                                    {incomingCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; LastCompilerOperation=LastCompilerOperations.PointerReset; WaitingFor=CompilerWaitingFor.MultipleAttributes}
                                                elif welastAnnotatedAnAttribute&& levelsJumped=2 then
                                                    // it's popped two levels back up to the item
                                                    {incomingCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; LastCompilerOperation=LastCompilerOperations.PointerReset; WaitingFor=CompilerWaitingFor.MultipleAttributes}
                                                else
                                                    {incomingCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; LastCompilerOperation=LastCompilerOperations.PointerReset; WaitingFor=CompilerWaitingFor.MultipleModelItems}
                                        let newCompilerStatus={incomingCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newCompilerLocation}        
                                        updateModelLocationPointer newCompilerStatus incomingLine incomingCommand 
                                    else
                                        // we assume the next level up is the parent pop back up to the parent, not waiting on atts
                                        // reset the pointer to the location without annotations and wait on multiple stuff
                                        let newCompilerState={incomingCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; LastCompilerOperation=LastCompilerOperations.PointerReset; WaitingFor=CompilerWaitingFor.MultipleModelItems}
                                        let newCompilerLocation={incomingCompilerStatus.CurrentLocation with ParentId=(-1); AnnotationIndicator=AnnotationTokenType.None}
                                        let newCompilerStatus={incomingCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newCompilerLocation}
                                        updateModelLocationPointer newCompilerStatus incomingLine incomingCommand 
                            |IndentIsSameAsPreviousIndent->
                                        if incomingCompilerStatus.CompilerState.LastCompilerOperation<>LastCompilerOperations.NewAttribute && (incomingCompilerStatus.CompilerState.LastCompilerOperation<>LastCompilerOperations.LocationChange || incomingCompilerStatus.CurrentLocation.AttributeId.IsNone)
                                            then
                                                updateTheModelThroughCommaSplittedItems incomingCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                                        if x.Length=0 then accumulatorCompilerStatus else
                                                        let modelWithNewAnnotation=addAnnotation incomingCompilerStatus.CurrentLocation.ParentId incomingCompilerStatus.CurrentLocation.AnnotationIndicator (x.Trim()) incomingLine incomingCompilerStatus
                                                        let newCompilerState={modelWithNewAnnotation.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotations}
                                                        {modelWithNewAnnotation with CompilerState=newCompilerState}
                                                    )
                                            else
                                                let itemParentId=incomingCompilerStatus.CurrentLocation.ParentId
                                                let attributeTargetId=incomingCompilerStatus.CurrentLocation.AttributeId.Value
                                                let attributeType=incomingCompilerStatus.CurrentLocation.AttributeType.Value
                                                let annotationTokenType=incomingCompilerStatus.CurrentLocation.AnnotationIndicator
                                                updateTheModelThroughCommaSplittedItems incomingCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                                        if x.Length=0 then accumulatorCompilerStatus else
                                                        let modelWithNewAttributeAnnotaion=addAttributeAnnotation itemParentId attributeTargetId annotationTokenType x incomingLine  accumulatorCompilerStatus
                                                        let newCompilerState={modelWithNewAttributeAnnotaion.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotations}
                                                        {modelWithNewAttributeAnnotaion with CompilerState=newCompilerState}
                                                    )
                            |IndentIsMoreThanPreviousIndent->
                                // if we're further idented, we're continuing the previous annotation list
                                // if the last operation was a new attribute, then we're annotating on that attribute
                                match incomingCompilerStatus.CompilerState.LastCompilerOperation with 
                                    |LastCompilerOperations.LocationChange->
                                        if incomingCompilerStatus.CurrentLocation.AttributeId.IsSome
                                            then
                                                let itemParentId=incomingCompilerStatus.CurrentLocation.ParentId
                                                let attributeTargetId=incomingCompilerStatus.CurrentLocation.AttributeId.Value
                                                let attributeType=incomingCompilerStatus.CurrentLocation.AttributeType.Value
                                                let annotationTokenType=incomingCompilerStatus.CurrentLocation.AnnotationIndicator
                                                updateTheModelThroughCommaSplittedItems incomingCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                                    if x.Length=0 then accumulatorCompilerStatus else
                                                    let modelWithNewAttributeAnnotation=addAttributeAnnotation itemParentId attributeTargetId annotationTokenType x incomingLine  accumulatorCompilerStatus
                                                    let newCompilerState={modelWithNewAttributeAnnotation.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotations}
                                                    {modelWithNewAttributeAnnotation with CompilerState=newCompilerState}
                                                )
                                            else
                                                updateTheModelThroughCommaSplittedItems incomingCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                                    if x.Length=0 then accumulatorCompilerStatus else
                                                    let modelWithNewAnnotation=addAnnotation incomingCompilerStatus.CurrentLocation.ParentId incomingCompilerStatus.CurrentLocation.AnnotationIndicator (x.Trim()) incomingLine incomingCompilerStatus
                                                    let newCompilerState={modelWithNewAnnotation.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotations}
                                                    {modelWithNewAnnotation with CompilerState=newCompilerState}
                                                )
                                    |LastCompilerOperations.NewAttribute->
                                                updateTheModelThroughCommaSplittedItems incomingCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                                        if x.Length=0 then accumulatorCompilerStatus else
                                                        let modelWithNewAttributeAnnotation= addAttributeAnnotation incomingCompilerStatus.CurrentLocation.ParentId incomingCompilerStatus.CurrentLocation.AttributeId.Value incomingCompilerStatus.CurrentLocation.AnnotationIndicator (x.Trim()) incomingLine incomingCompilerStatus
                                                        let newCompilerState={modelWithNewAttributeAnnotation.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAttributeAnnotations}
                                                        {modelWithNewAttributeAnnotation with CompilerState=newCompilerState}
                                                    )
                                    |LastCompilerOperations.NewModelItem
                                    |LastCompilerOperations.NewAttributeAnnotation
                                    |LastCompilerOperations.NewAnnotation
                                    |LastCompilerOperations.NewJoin
                                    |LastCompilerOperations.PointerReset
                                    |LastCompilerOperations.NewAttribute
                                    |LastCompilerOperations.ReferenceExistingAttribute
                                    |LastCompilerOperations.ReferenceExistingItem->
                                                updateTheModelThroughCommaSplittedItems incomingCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                                        if x.Length=0 then accumulatorCompilerStatus else
                                                        let modelWithNewAnnotation=addAnnotation incomingCompilerStatus.CurrentLocation.ParentId incomingCompilerStatus.CurrentLocation.AnnotationIndicator (x.Trim()) incomingLine incomingCompilerStatus
                                                        let newCompilerState={modelWithNewAnnotation.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotations}
                                                        {modelWithNewAnnotation with CompilerState=newCompilerState}
                                                    )
                    |CompilerWaitingFor.MultipleRelations->
                        let joinType=incomingCompilerStatus.CompilerState.LastJoinType.Value
                        match incomingCompilerStatus.CompilerState.IndentLevelChange with 
                            |IndentIsLessThanPreviousIndent->
                                // first we're no longer waiting on a join
                                let newState={incomingCompilerStatus.CompilerState with LastJoinType=option.None; WaitingFor=CompilerWaitingFor.MultipleModelItems; LastCompilerOperation=LastCompilerOperations.LocationChange}
                                let newLoc={incomingCompilerStatus.CurrentLocation with RelationTargetId=option.None}
                                let newStat={incomingCompilerStatus with CurrentLocation=newLoc; CompilerState=newState}
                                updateModelLocationPointer newStat incomingLine incomingCommand
                            |IndentIsSameAsPreviousIndent->
                                // since we're at the same indent level and waiting for relations, we're continuing a join
                                let parentForAllOfTheseRelations=
                                    if incomingCompilerStatus.CurrentLocation.RelationSourceId.IsSome 
                                        then incomingCompilerStatus.CurrentLocation.RelationSourceId.Value
                                        else incomingCompilerStatus.CurrentLocation.ParentId
                                let sourceItemForAllOfTheseRelations=getModelItemById incomingCompilerStatus.ModelItems parentForAllOfTheseRelations
                                //let joinType=incomingCompilerStatus.CurrentLocation.RelationType.Value
                                updateTheModelThroughCommaSplittedItems incomingCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                    if x.Length=0 then accumulatorCompilerStatus else
                                    let newAcc,targetItem=addFindModelItem accumulatorCompilerStatus incomingLine (Some joinType) (x.Trim())
                                    let joinedAccumulator=joinModelItems2 newAcc incomingLine sourceItemForAllOfTheseRelations targetItem joinType
                                    joinedAccumulator
                                    )
                            |IndentIsMoreThanPreviousIndent->
                                updateTheModelThroughCommaSplittedItems incomingCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                        if x.Length=0 then accumulatorCompilerStatus else
                                        // in a multiple join with a new item added, should it be the new parent?
                                        let modCompilerStatus=makeATargetOfAJoinIfYouCan accumulatorCompilerStatus incomingLine joinType (x.Trim())
                                        let parentToFindId=
                                            if modCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewModelItem 
                                                then accumulatorCompilerStatus.CurrentLocation.ParentId 
                                                elif modCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewJoin
                                                    then
                                                        let targetId = if modCompilerStatus.CurrentLocation.RelationTargetId.IsNone then modCompilerStatus.CurrentLocation.ParentId else modCompilerStatus.CurrentLocation.RelationTargetId.Value
                                                        let joinedTarget= getModelItemById modCompilerStatus.ModelItems targetId
                                                        if joinedTarget.Relations.Length>0 then joinedTarget.Relations.[joinedTarget.Relations.Length-1].TargetId else joinedTarget.Id
                                                else modCompilerStatus.CurrentLocation.ParentId
                                        let currentTargetDescription=modCompilerStatus.ModelItems|>Array.tryFind(fun x->x.Id=parentToFindId)
                                        let modelWithPossibleNewJoinedItems=
                                            if currentTargetDescription.IsSome && currentTargetDescription.Value.Description<>""
                                                then joinModelItems modCompilerStatus modCompilerStatus.CurrentLocation incomingLine joinType currentTargetDescription.Value.Description  (x.Trim())                                                                 
                                                else modCompilerStatus
                                        {modelWithPossibleNewJoinedItems with CompilerState={modelWithPossibleNewJoinedItems.CompilerState with LastCompilerOperation=LastCompilerOperations.NewJoin; LastJoinType=Some joinType; WaitingFor=CompilerWaitingFor.MultipleRelations}}
                                    )
            |Some token->
                if token.Category=NAMESPACE
                    then
                        let newNamespace=incomingCommand.Value
                        let newLocationPointer = {incomingCompilerStatus.CurrentLocation with Namespace=newNamespace}
                        let updatedCompilerStatus = {incomingCompilerStatus with CurrentLocation=newLocationPointer; CompilerState={incomingCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.LocationChange}}
                        updatedCompilerStatus
                    else
                match token.Type with 
                    |RELATIVE_LOCATOR->
                        match token.TargetType with 
                            |SINGLE_TARGET->
                                match token.Category with 
                                    |SHORTCUT->
                                        // these are relative to wherever the compiler already is.
                                        // if it's nowhere, then provide defaults
                                        let oldBucket = if incomingCompilerStatus.CurrentLocation.Bucket=Buckets.None then Buckets.Behavior else incomingCompilerStatus.CurrentLocation.Bucket
                                        let oldGenre = if incomingCompilerStatus.CurrentLocation.Genre=Genres.None then Genres.Business else incomingCompilerStatus.CurrentLocation.Genre
                                        let oldTemporal=if incomingCompilerStatus.CurrentLocation.TemporalIndicator=TemporalIndicators.None then TemporalIndicators.ToBe else incomingCompilerStatus.CurrentLocation.TemporalIndicator
                                        let oldAbstraction=if incomingCompilerStatus.CurrentLocation.AbstractionLevel=AbstractionLevels.None then AbstractionLevels.Abstract else incomingCompilerStatus.CurrentLocation.AbstractionLevel
                                        let newBucket, newGenre, newTemporal, newAbstraction =
                                            match token.Token with 
                                                | "US:"|"USER STORY:"|"USER STORIES:"->Buckets.Behavior, oldGenre, oldTemporal,oldAbstraction
                                                | "ENT:" | "ENTITY:"|"ENTITIES:"->Buckets.Structure, oldGenre, oldTemporal,oldAbstraction
                                                | "SUPPL:"->Buckets.Supplemental, oldGenre, oldTemporal,oldAbstraction
                                                |_->oldBucket,oldGenre,oldTemporal,oldAbstraction // ERROR ERROR
                                        let newLocation = {incomingCompilerStatus.CurrentLocation with AbstractionLevel=newAbstraction; Bucket=newBucket; Genre=newGenre; TemporalIndicator=newTemporal}
                                        let newState = {incomingCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.LocationChange; WaitingFor=CompilerWaitingFor.MultipleModelItems}
                                        let updatedCompilerStatus={incomingCompilerStatus with CurrentLocation=newLocation; CompilerState=newState}
                                        let newCompilerStatus=updateTheModelThroughCommaSplittedItems updatedCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                            if x.Length=0 then accumulatorCompilerStatus else
                                            addModelItem accumulatorCompilerStatus newLocation incomingLine (x.Trim())
                                        )
                                        let newIndentLevelWithCommasConsidered=newCompilerStatus.CompilerState.CurrentIndentLevel + incomingCommand.Value.Split([|","|], System.StringSplitOptions.None).Length
                                        {newCompilerStatus with CompilerState={newCompilerStatus.CompilerState with CurrentIndentLevel=newIndentLevelWithCommasConsidered}}
                                    |TokenCategory.MISC |_->
                                        let newTempAnnotationIndicator=
                                            match token.Token with 
                                                | "Q " | "Q:" | "QUESTION " | "QUESTION"|"QUESTION:"->AnnotationTokenType.Question
                                                | "//" | "NOTE " | "NOTE: "|"NOTE:"->AnnotationTokenType.Note
                                                | "TODO " | "TODO: "|"TODO:"|"TO-DO"|"TO-DO: "|"TO-DO:"->AnnotationTokenType.ToDo
                                                | "WORK: " | "WORK "|"WORK:"->AnnotationTokenType.Work
                                                | "DIAGRAM:" | "DIAGRAMS"|"DIAGRAMS"->AnnotationTokenType.Diagram
                                                | "CODE:"->AnnotationTokenType.Code
                                                |_->AnnotationTokenType.Note // ERROR ERROR
                                        let lastTokenWasAnAttributeIdentifier=
                                            if incomingLine.Commands.Length<2 then false else
                                            let currentCommandIndex = incomingLine.Commands |> Array.findIndex(fun x->x.Token=incomingCommand.Token && x.Value=incomingCommand.Value)
                                            if currentCommandIndex <1 then false else
                                            let attributeTokens=EasyAMTokens|>List.filter(fun x->x.Category=ATTRIBUTE) |> List.toArray
                                            attributeTokens |> Array.exists(fun x->x.Token=incomingLine.Commands.[currentCommandIndex-1].Token)
                                        if (incomingCommand.CommandIndentLevel<originalCompilerStatus.CompilerState.CurrentIndentLevel)
                                            then
                                                // ident has popped up. Whatever we were referencing, we are now referencing the item
                                                let newParentId=
                                                    if incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewJoin then (incomingCompilerStatus.CurrentLocation.RelationSourceId |? (-1)) else incomingCompilerStatus.CurrentLocation.ParentId
                                                let newLocation={incomingCompilerStatus.CurrentLocation with AttributeId=option.None; AttributeType=option.None; ParentId=newParentId; AnnotationIndicator=newTempAnnotationIndicator}
                                                let newState={incomingCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; WaitingFor=CompilerWaitingFor.MultipleAnnotations; LastCompilerOperation=LastCompilerOperations.LocationChange}
                                                let adjustedCompilerStatus={incomingCompilerStatus with CurrentLocation=newLocation; CompilerState=newState}
                                                addAnnotation adjustedCompilerStatus.CurrentLocation.ParentId newTempAnnotationIndicator incomingCommand.Value incomingLine adjustedCompilerStatus
                                            else
                                                // the indent has popped down. We're referencing whatever item we just referred to
                //                                if incomingCompilerStatus.CompilerState.WaitingFor=CompilerWaitingFor.MultipleAttributeTargets && (incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewAttribute || incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.ReferenceExistingAttribute)
                                                if incomingCompilerStatus.CompilerState.WaitingFor=CompilerWaitingFor.MultipleAttributes && lastTokenWasAnAttributeIdentifier && incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.LocationChange
                                                then
                                                    let newState=if incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewModelItem then {incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotations} else incomingCompilerStatus.CompilerState
                                                    let newLocation=if incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewModelItem then {incomingCompilerStatus.CurrentLocation with AnnotationIndicator=newTempAnnotationIndicator} else incomingCompilerStatus.CurrentLocation
                                                    // We're doing a "reach-around" and annotating the root item, so we need to reset the location back to the attribute list
                                                    let tempCompStat=addAnnotation incomingCompilerStatus.CurrentLocation.ParentId newTempAnnotationIndicator incomingCommand.Value incomingLine {incomingCompilerStatus with CompilerState=newState; CurrentLocation=newLocation} 
                                                    {tempCompStat with CurrentLocation={tempCompStat.CurrentLocation with AttributeType=originalCompilerStatus.CurrentLocation.AttributeType }} 
                                                else
                                                if incomingCompilerStatus.CompilerState.WaitingFor=CompilerWaitingFor.MultipleAttributes && (incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewAttribute || incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.ReferenceExistingAttribute || incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.LocationChange)
                                                then
                                                        // it was an attribute
                                                        let newState =
                                                            if token.Token="//" then {incomingCompilerStatus.CompilerState with WaitingFor=originalCompilerStatus.CompilerState.WaitingFor; CurrentIndentLevel=incomingCompilerStatus.CompilerState.CurrentIndentLevel-1} else {incomingCompilerStatus.CompilerState with WaitingFor=MultipleAttributeAnnotations} //incomingCompilerStatus.{}
                                                        let ret = addAttributeAnnotation incomingCompilerStatus.CurrentLocation.ParentId incomingCompilerStatus.CurrentLocation.AttributeId.Value newTempAnnotationIndicator incomingCommand.Value incomingLine incomingCompilerStatus
                                                        {ret with CompilerState=newState}
                                                    else
                                                        // it wasn't an attribute. Was it a new model item?
                                                        match incomingCompilerStatus.CompilerState.LastCompilerOperation with 
                                                            | LastCompilerOperations.NewJoin->
                                                                let joinParentId=incomingCompilerStatus.CurrentLocation.RelationTargetId.Value
                                                                let ret=addAnnotation joinParentId newTempAnnotationIndicator incomingCommand.Value incomingLine incomingCompilerStatus  
                                                                // if this is a same-line annotation, the last action wasn't adding an annotation, it was whatever it was originally
                                                                if incomingCommand.Token="//"
                                                                    then
                                                                        {ret with CompilerState={ret.CompilerState with LastCompilerOperation=originalCompilerStatus.CompilerState.LastCompilerOperation}}
                                                                    else ret
                                                            | LastCompilerOperations.NewModelItem | LastCompilerOperations.ReferenceExistingItem->
                                                                if incomingCommand.Token="//"
                                                                    then
                                                                        let ret=addAnnotation incomingCompilerStatus.CurrentLocation.ParentId AnnotationTokenType.Note incomingCommand.Value incomingLine incomingCompilerStatus
                                                                        {ret with CompilerState={ret.CompilerState with LastCompilerOperation=originalCompilerStatus.CompilerState.LastCompilerOperation}}
                                                                    else
                                                                        let newState=if incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewModelItem then {incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotations} else incomingCompilerStatus.CompilerState
                                                                        let newLocation=if incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewModelItem then {incomingCompilerStatus.CurrentLocation with AnnotationIndicator=newTempAnnotationIndicator} else incomingCompilerStatus.CurrentLocation
                                                                        // We're doing a "reach-around" and annotating the root item, so we need to reset the location back to the attribute list
                                                                        let tempCompStat=addAnnotation incomingCompilerStatus.CurrentLocation.ParentId newTempAnnotationIndicator incomingCommand.Value incomingLine {incomingCompilerStatus with CompilerState=newState; CurrentLocation=newLocation} 
                                                                        {tempCompStat with CurrentLocation={tempCompStat.CurrentLocation with AttributeType=originalCompilerStatus.CurrentLocation.AttributeType }} 
                                                            |_->
                                                                let newState=if incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewModelItem then {incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotations} else incomingCompilerStatus.CompilerState
                                                                let newLocation=if incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewModelItem then {incomingCompilerStatus.CurrentLocation with AnnotationIndicator=newTempAnnotationIndicator} else incomingCompilerStatus.CurrentLocation
                                                                // We're doing a "reach-around" and annotating the root item, so we need to reset the location back to the attribute list
                                                                let tempCompStat=addAnnotation incomingCompilerStatus.CurrentLocation.ParentId newTempAnnotationIndicator incomingCommand.Value incomingLine {incomingCompilerStatus with CompilerState=newState; CurrentLocation=newLocation} 
                                                                {tempCompStat with CurrentLocation={tempCompStat.CurrentLocation with AttributeType=originalCompilerStatus.CurrentLocation.AttributeType }}
                            |MULTIPLE_TARGETS->
                                let newAttributeType,newAttributeId =
                                    if incomingCompilerStatus.CompilerState.IndentLevelChange=IndentLevelComparisons.IndentIsLessThanPreviousIndent
                                        then
                                            option.None,option.None
                                        else
                                            incomingCompilerStatus.CurrentLocation.AttributeType,incomingCompilerStatus.CurrentLocation.AttributeId
                                let newTempAnnotationIndicator=
                                    match token.Token with 
                                        | "QUESTIONS"|"QUESTIONS " | "QUESTIONS: " | "QUESTIONS:"->AnnotationTokenType.Question
                                        | "NOTES"|"NOTES " | "NOTES: "|"NOTES:"->AnnotationTokenType.Note
                                        | "TODOS"|"TODOS " | "TODOS: "|"TODOS:"|"TO-DOS"|"TO-DOS "|"TO-DOS: "|"TO-DOS:"->AnnotationTokenType.ToDo
                                        | "WORKS"|"WORKS: " | "WORKS "|"WORKS:"|"WORK:"->AnnotationTokenType.Work
                                        | "DEFECTS"|"DEFECTS: "|"DEFECT:"|"DEFECT" | "BUGS"|"BUGSS: "|"BUG:"|"BUG"->AnnotationTokenType.Work
                                        | "CODE"|"CODE:"->AnnotationTokenType.Code
                                        |_->AnnotationTokenType.Note // ERROR ERROR
                                // run through everything split on this line by a comma and add
                                let newLoc={incomingCompilerStatus.CurrentLocation with AttributeType=newAttributeType; AttributeId=newAttributeId}
                                let newCompilerStatus=updateTheModelThroughCommaSplittedItems {incomingCompilerStatus with CurrentLocation=newLoc} incomingCommand.Value (fun accumulatorCompilerStatus x->
                                    if x.Length=0 then accumulatorCompilerStatus else
                                    let newAccumulatorCompilerStatus=addAnnotation accumulatorCompilerStatus.CurrentLocation.ParentId newTempAnnotationIndicator (x.Trim()) incomingLine accumulatorCompilerStatus 
                                    newAccumulatorCompilerStatus
                                    )
                                let newLocation = {newCompilerStatus.CurrentLocation with AnnotationIndicator=newTempAnnotationIndicator}
                                let newIndentLevelWithCommasConsidered=newCompilerStatus.CompilerState.CurrentIndentLevel + numberOfCommaSeparatedCommandSegments
                                {newCompilerStatus with CurrentLocation=newLocation; CompilerState={newCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.LocationChange; WaitingFor=CompilerWaitingFor.MultipleAnnotations}}
                    |ABSOLUTE_LOCATOR->
                        match token.TargetType with 
                            |SINGLE_TARGET->
                                let oldBucket = incomingCompilerStatus.CurrentLocation.Bucket
                                let oldGenre =incomingCompilerStatus.CurrentLocation.Genre
                                let oldTemporal=incomingCompilerStatus.CurrentLocation.TemporalIndicator
                                let oldAbstraction=incomingCompilerStatus.CurrentLocation.AbstractionLevel
                                let newBucket, newGenre, newTemporal, newAbstraction =
                                    match token.Token with 
                                        | "PROGRAM BACKLOG"|"PROGRAM BACKLOG:"->Buckets.Behavior, Genres.Business, TemporalIndicators.ToBe, AbstractionLevels.Realized
                                        | "PRODUCT BACKLOG" | "PROJECT BACKLOG"|"PRODUCT BACKLOG:" | "PROJECT BACKLOG:"->Buckets.Behavior, Genres.Business, TemporalIndicators.ToBe, AbstractionLevels.Realized
                                        | "SPRINT BACKLOG"|"SPRINT BACKLOG:"->Buckets.Behavior, Genres.System, TemporalIndicators.ToBe, AbstractionLevels.Abstract
                                        | "MASTER BACKLOG"|"MASTER BACKLOG:"|"MASTER USER STORY"|"MASTER USER STORY:"|"MASTER USER STORIES"|"MASTER USER STORIES:"->Buckets.Behavior, Genres.Business, TemporalIndicators.ToBe, AbstractionLevels.Abstract
                                        | "MASTER DOMAIN MODEL"|"MASTER DOMAIN MODEL:"->Buckets.Structure, Genres.Business, TemporalIndicators.ToBe, AbstractionLevels.Abstract
                                        | "PROJECT DOMAIN MODEL" | "PRODUCT DOMAIN MODEL"|"PROJECT DOMAIN MODEL:" | "PRODUCT DOMAIN MODEL:"->Buckets.Structure, Genres.Business, TemporalIndicators.ToBe, AbstractionLevels.Realized
                                        | "MASTER SUPPLEMENTAL MODEL"|"MASTER SUPPLEMENTAL MODEL:"->Buckets.Supplemental, Genres.Business, TemporalIndicators.ToBe, AbstractionLevels.Abstract
                                        | "PRODUCT SUPPLEMENTAL MODEL" | "PROJECT SUPPLEMENTAL MODEL"|"PRODUCT SUPPLEMENTAL MODEL:" | "PROJECT SUPPLEMENTAL MODEL:"->Buckets.Supplemental, Genres.Business, TemporalIndicators.ToBe, AbstractionLevels.Realized
                                        |_->oldBucket,oldGenre,oldTemporal,oldAbstraction // ERROR ERROR
                                let newLocation = {incomingCompilerStatus.CurrentLocation with AbstractionLevel=newAbstraction; Bucket=newBucket; Genre=newGenre; TemporalIndicator=newTemporal}
                                let newState = {incomingCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.LocationChange; WaitingFor=CompilerWaitingFor.MultipleModelItems}
                                let updatedCompilerStatus={incomingCompilerStatus with CurrentLocation=newLocation; CompilerState=newState}
                                let newCompilerStatus=updateTheModelThroughCommaSplittedItems updatedCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                    if x.Length=0 then accumulatorCompilerStatus else
                                    addModelItem accumulatorCompilerStatus newLocation incomingLine (x.Trim())
                                )
                                let newIndentLevelWithCommasConsidered=newCompilerStatus.CompilerState.CurrentIndentLevel + incomingCommand.Value.Split([|","|], System.StringSplitOptions.None).Length
                                {newCompilerStatus with CompilerState={newCompilerStatus.CompilerState with CurrentIndentLevel=newIndentLevelWithCommasConsidered}}
                            |MULTIPLE_TARGETS->
                                match token.Category with 
                                    |TokenCategory.TAG->
                                        if (incomingCommand.Token + incomingCommand.Value).GetLeft 2="@&" || (incomingCommand.Token + incomingCommand.Value).GetLeft 2="&@"
                                        then
                                            let newLocation={incomingCompilerStatus.CurrentLocation with Tags=[||]}
                                            {incomingCompilerStatus with CurrentLocation=newLocation}                                    
                                        else
                                            let newToken=incomingCommand.Token.Substring(1)
                                            let isEqualsPresent=newToken.Contains("=")
                                            let isCommaPresent=newToken.Contains(",")
                                            let removeQuotes (str:string) =
                                                if ((str.GetLeft 1 = "\"") || (str.GetLeft 1 = "'")) && ((str.GetRight 1 = "\"") || (str.GetRight 1 = "'"))
                                                    then str.TrimBoth 1 1
                                                    else str
                                            let addDupeKeysOkay (newTags:System.Collections.Generic.KeyValuePair<string,string> []) (existingTags:System.Collections.Generic.KeyValuePair<string,string> []) = newTags |> Array.append existingTags
                                            let addOverwriteDupeKeys (newTags:System.Collections.Generic.KeyValuePair<string,string> []) (existingTags:System.Collections.Generic.KeyValuePair<string,string> []) =
                                                newTags |> Array.fold(fun acc x->
                                                    let alreadyExists = acc |> Array.exists(fun (y:System.Collections.Generic.KeyValuePair<string,string>)->x.Key=y.Key)
                                                    if alreadyExists=false then [|x|] |> Array.append acc else
                                                    acc|>Array.map(fun z->if z.Key=x.Key then x else z)
                                                    ) existingTags
                                            let addKeyValueTags (newTags:System.Collections.Generic.KeyValuePair<string,string> []) (existingTags:System.Collections.Generic.KeyValuePair<string,string> []) =
                                                if incomingCommand.Token.GetLeft 1 = "@" then addDupeKeysOkay newTags existingTags else addOverwriteDupeKeys newTags existingTags
                                            if isEqualsPresent=false
                                                then
                                                    let newTag=new System.Collections.Generic.KeyValuePair<string,string>(removeQuotes(newToken),"")
                                                    let newTags=addKeyValueTags [|newTag|] incomingCompilerStatus.CurrentLocation.Tags //[|newTag|] |> Array.append incomingCompilerStatus.CurrentLocation.Tags
                                                    let newLocation={incomingCompilerStatus.CurrentLocation with Tags=newTags}
                                                    {incomingCompilerStatus with CurrentLocation=newLocation}
                                                else
                                                    if isCommaPresent=false
                                                        then
                                                            let splitLocation=newToken.IndexOf("=")
                                                            let key=removeQuotes(newToken.GetLeft splitLocation)
                                                            let value=if splitLocation<newToken.Length-1 then removeQuotes(newToken.Substring(splitLocation+1)) else ""
                                                            let newTag=new System.Collections.Generic.KeyValuePair<string,string>(key,value)
                                                            let newTags=addKeyValueTags [|newTag|] incomingCompilerStatus.CurrentLocation.Tags //[|newTag|] |> Array.append incomingCompilerStatus.CurrentLocation.Tags
                                                            let newLocation={incomingCompilerStatus.CurrentLocation with Tags=newTags}
                                                            {incomingCompilerStatus with CurrentLocation=newLocation}
                                                        else
                                                            let splitLocation=newToken.IndexOf("=")
                                                            let keys=newToken.GetLeft splitLocation
                                                            let values=if splitLocation<newToken.Length-1 then newToken.Substring(splitLocation+1) else ""
                                                            let valArray=values.Split([|","|], System.StringSplitOptions.None)
                                                            let keyArray=keys.Split([|","|], System.StringSplitOptions.None)
                                                            let tagArray = keyArray|> Array.fold(fun acc2 y->
                                                                valArray |> Array.fold(fun acc x->
                                                                    let newItem=new System.Collections.Generic.KeyValuePair<string,string>(removeQuotes(y),removeQuotes(x))
                                                                    [|newItem|] |> Array.append acc) acc2) [||]
                                                            let newTags= addKeyValueTags tagArray incomingCompilerStatus.CurrentLocation.Tags //tagArray |> Array.append incomingCompilerStatus.CurrentLocation.Tags
                                                            let newLocation={incomingCompilerStatus.CurrentLocation with Tags=newTags}
                                                            {incomingCompilerStatus with CurrentLocation=newLocation}
                                    |TokenCategory.BUCKETS->
                                        let newBucket = match token.Token with 
                                                        | "BEHAVIOR" | "BEHAVIOR:" | "BEHAVIORS" |"BEHAVIORS:"->Buckets.Behavior
                                                        | "STRUCTURE" | "STRUCTURE:" | "STRUCTURES" | "STRUCTURES:" ->Buckets.Structure
                                                        | "SUPPLEMENTAL" | "SUPPLEMENTAL:"|"SUPPLEMENTALS" |"SUPPLEMENTALS:" |_->Buckets.Supplemental
                                        let newParentId=if incomingCompilerStatus.CurrentLocation.Bucket<>Buckets.None then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                        let currentLocation = incomingCompilerStatus.CurrentLocation
                                        let newGenre= if currentLocation.Genre=Genres.None then Genres.Business else currentLocation.Genre
                                        let newAbstractionLevel=if currentLocation.AbstractionLevel=AbstractionLevels.None then AbstractionLevels.Abstract else currentLocation.AbstractionLevel
                                        let newTemporalIndicator=if currentLocation.TemporalIndicator=TemporalIndicators.None then TemporalIndicators.ToBe else currentLocation.TemporalIndicator
                                        let newLocationPointer = 
                                            if incomingCompilerStatus.CurrentLocation.Bucket<>newBucket
                                                then
                                                    {incomingCompilerStatus.CurrentLocation with Bucket=newBucket; Genre=newGenre; AbstractionLevel=newAbstractionLevel; TemporalIndicator=newTemporalIndicator; ParentId=newParentId; RelationType=option.None; RelationSourceId=option.None; RelationTargetId=option.None}
                                                else
                                                    {incomingCompilerStatus.CurrentLocation with Bucket=newBucket; Genre=newGenre; AbstractionLevel=newAbstractionLevel; TemporalIndicator=newTemporalIndicator; ParentId=newParentId}
                                        let updatedCompilerStatus = {incomingCompilerStatus with CurrentLocation=newLocationPointer; CompilerState={incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleModelItems}}
                                        updateTheModelThroughCommaSplittedItems updatedCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                            if x.Length=0 then accumulatorCompilerStatus else
                                            let modelWithNewModelItem=addModelItem accumulatorCompilerStatus newLocationPointer incomingLine (x.Trim())
                                            let newIndentLevelWithCommasConsidered=modelWithNewModelItem.CompilerState.CurrentIndentLevel + numberOfCommaSeparatedCommandSegments
                                            {modelWithNewModelItem with CompilerState={modelWithNewModelItem.CompilerState with CurrentIndentLevel=newIndentLevelWithCommasConsidered}}
                                            )
                                    |TokenCategory.ABSTRACTION_LEVEL->
                                        let newAbstractionLevel = match token.Token with 
                                                                    | "ABSTRACT" | "ABSTRACT:"->AbstractionLevels.Abstract
                                                                    | "REALIZED" | "REALIZED:"|_->AbstractionLevels.Realized
                                        let newParentId=if incomingCompilerStatus.CurrentLocation.AbstractionLevel<>AbstractionLevels.None then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                        let newLocation = 
                                            if incomingCompilerStatus.CurrentLocation.AbstractionLevel<>newAbstractionLevel
                                                then {incomingCompilerStatus.CurrentLocation with AbstractionLevel=newAbstractionLevel; ParentId=newParentId; RelationType=option.None; RelationSourceId=option.None; RelationTargetId=option.None}
                                                else {incomingCompilerStatus.CurrentLocation with AbstractionLevel=newAbstractionLevel; ParentId=newParentId}
                                        let updatedCompilerStatus={incomingCompilerStatus with CurrentLocation=newLocation; CompilerState={incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleModelItems}}
                                        updateTheModelThroughCommaSplittedItems updatedCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                            if x.Length=0 then accumulatorCompilerStatus else
                                            let modelWithNewModelItem=addModelItem accumulatorCompilerStatus newLocation incomingLine (x.Trim())
                                            let newIndentLevelWithCommasConsidered=modelWithNewModelItem.CompilerState.CurrentIndentLevel + numberOfCommaSeparatedCommandSegments
                                            {modelWithNewModelItem with CompilerState={modelWithNewModelItem.CompilerState with CurrentIndentLevel=newIndentLevelWithCommasConsidered}}
                                            )
                                    |TokenCategory.GENRE->
                                        let newGenre = match token.Token with 
                                                        | "SYSTEM" | "SYSTEM:"->Genres.System
                                                        | "META" | "META:"->Genres.Meta
                                                        | "BUSINESS" | "BUSINESS:"|_->Genres.Business
                                        let newParentId=if incomingCompilerStatus.CurrentLocation.Genre<>Genres.None then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                        let newLocation = if incomingCompilerStatus.CurrentLocation.Genre<>newGenre
                                                            then
                                                                 {incomingCompilerStatus.CurrentLocation with Genre=newGenre; ParentId=newParentId; RelationType=option.None; RelationSourceId=option.None; RelationTargetId=option.None}
                                                            else {incomingCompilerStatus.CurrentLocation with Genre=newGenre; ParentId=newParentId}
                                        let updatedCompilerStatus = {incomingCompilerStatus with CurrentLocation=newLocation; CompilerState={incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleModelItems}}
                                        updateTheModelThroughCommaSplittedItems updatedCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                            if x.Length=0 then accumulatorCompilerStatus else
                                            let modelWithNewModelItem=addModelItem accumulatorCompilerStatus newLocation incomingLine (x.Trim())
                                            let newIndentLevelWithCommasConsidered=modelWithNewModelItem.CompilerState.CurrentIndentLevel + numberOfCommaSeparatedCommandSegments
                                            {modelWithNewModelItem with CompilerState={modelWithNewModelItem.CompilerState with CurrentIndentLevel=newIndentLevelWithCommasConsidered}}
                                            )
                                    |TokenCategory.TEMPORAL->
                                        let newTemporalIndicator = match token.Token with 
                                                                    | "WAS" | "WAS:"->TemporalIndicators.Was
                                                                    | "TO-BE" | "TO-BE:"->TemporalIndicators.ToBe
                                                                    | "AS-IS" | "AS-IS:"|_->TemporalIndicators.AsIs
                                        let newParentId=if incomingCompilerStatus.CurrentLocation.TemporalIndicator<>TemporalIndicators.None then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                        let newLocation = 
                                            if incomingCompilerStatus.CurrentLocation.TemporalIndicator<>newTemporalIndicator
                                                then {incomingCompilerStatus.CurrentLocation with TemporalIndicator=newTemporalIndicator; ParentId=newParentId; RelationType=option.None;RelationTargetId=option.None;RelationSourceId=option.None}
                                                else {incomingCompilerStatus.CurrentLocation with TemporalIndicator=newTemporalIndicator; ParentId=newParentId}
                                        let updatedCompilerStatus = {incomingCompilerStatus with CurrentLocation=newLocation; CompilerState={incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleModelItems}}
                                        updateTheModelThroughCommaSplittedItems updatedCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                            if x.Length=0 then accumulatorCompilerStatus else
                                            let modCompilerStatus=addModelItem accumulatorCompilerStatus newLocation incomingLine (x.Trim())
                                            let reallyNewParentId=modCompilerStatus.ModelItems|>Array.find(fun z->z.Description=(x.Trim()))
                                            let reallyNewLocation={modCompilerStatus.CurrentLocation with ParentId=reallyNewParentId.Id}
                                            let newCompilerStatus={modCompilerStatus with CurrentLocation=reallyNewLocation}
                                            let newIndentLevelWithCommasConsidered=newCompilerStatus.CompilerState.CurrentIndentLevel + numberOfCommaSeparatedCommandSegments
                                            {newCompilerStatus with CompilerState={newCompilerStatus.CompilerState with CurrentIndentLevel=newIndentLevelWithCommasConsidered}}
                                            )
                                    |TokenCategory.HDD->
                                        let newBucket=Buckets.None
                                        let newAbstractionLevel=AbstractionLevels.None
                                        let newGenre=Genres.None
                                        let newTemporalIndicator=TemporalIndicators.None
                                        let newInHDDMode=true
                                        let newParentId=if incomingCompilerStatus.CurrentLocation.InHDDMode=false then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                        let newLocation = {incomingCompilerStatus.CurrentLocation with InHDDMode=newInHDDMode; Bucket=newBucket; AbstractionLevel=newAbstractionLevel; Genre=newGenre; TemporalIndicator=newTemporalIndicator; ParentId=newParentId}
                                        {incomingCompilerStatus with CurrentLocation=newLocation; CompilerState={incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleModelItems}}
                                    |TokenCategory.ATTRIBUTE->
                                        let newAttributeType = match token.Token with
                                                                | "WHEN"|"WHEN:"->ModelAttributeTypes.Trigger
                                                                | "ASA"|"ASA:"->ModelAttributeTypes.Actor
                                                                | "INEEDTO"|"INEEDTO:"->ModelAttributeTypes.Goal
                                                                | "SOTHAT"|"SOTHAT:|OUTCOME|OUTCOME:|OUTCOMES|OUTCOMES:"->ModelAttributeTypes.BusinessContext
                                                                | "SCENARIO"|"SCENARIO:"->ModelAttributeTypes.Scenario
                                                                | "BECAUSE"|"BECAUSE:"->ModelAttributeTypes.Because
                                                                | "WHENEVER"|"WHENEVER:"->ModelAttributeTypes.Whenever
                                                                | "ITHASTOBETHAT"|"ITHASTOBETHAT:"->ModelAttributeTypes.ItHasToBeThat
                                                                | "CONTAINS"|"CONTAINS:"->ModelAttributeTypes.Contains
                                                                |_->raise(new System.Exception("We're supposed to have an attribute, but none are there"))
                                        let newCompilerStatus=updateTheModelThroughCommaSplittedItems incomingCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                                if x.Length=0 then accumulatorCompilerStatus else
                                                addModelAttribute newAttributeType (x.Trim()) incomingLine accumulatorCompilerStatus
                                            )
                                        let newCompilerLocation={newCompilerStatus.CurrentLocation with AttributeType = Some newAttributeType}
                                        let newIndentLevelWithCommasConsidered=newCompilerStatus.CompilerState.CurrentIndentLevel + numberOfCommaSeparatedCommandSegments
                                        let newCompilerState={newCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAttributes; LastCompilerOperation=LastCompilerOperations.LocationChange; CurrentIndentLevel=newIndentLevelWithCommasConsidered}
                                        {newCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newCompilerLocation}
                                    |_->raise(new System.Exception("messed up"))
                    |JOINER->
                        let joinType=match token.Token with 
                                        | "PARENT"|"PARENT:"->ModelJoin.Parent
                                        | "CHILD"|"CHILD:"->ModelJoin.Child
                                        | "CHILDREN"|"CHILDREN:"->ModelJoin.Child
                                        | "USES"|"USES:"->ModelJoin.Uses
                                        | "USEDBY"|"USEDBY:"->ModelJoin.UsedBy
                                        | "AFFECTS"|"AFFECTS:"->ModelJoin.Affects
                                        | "AFFECTEDBY"|"AFFECTEDBY:"->ModelJoin.AffectedBy
                                        | "HASA"|"HASA:"->ModelJoin.HasA
                                        | "ISOWNEDBYA"|"ISOWNEDBYA:"->ModelJoin.IsOwnedByA
                                        |_->ModelJoin.Child
                        let newCompilerStatus=updateTheModelThroughCommaSplittedItems incomingCompilerStatus incomingCommand.Value (fun accumulatorCompilerStatus x->
                                if x.Length=0 then accumulatorCompilerStatus else
                                let newAcc,targetItem=addFindModelItem accumulatorCompilerStatus incomingLine (Some joinType) (x.Trim())
                                let sourceItem=getModelItemById newAcc.ModelItems newAcc.CurrentLocation.ParentId
                                if targetItem.Id=(-1) || sourceItem.Id=(-1) then newAcc else 
                                joinModelItems2 newAcc incomingLine sourceItem targetItem joinType
                            )
                        // if the indent is less, pick up the source of a join if you were doing joins, also it's location, otherwise make it root (fail)
                        let newLoc =
                            match newCompilerStatus.CompilerState.IndentLevelChange with 
                                |IndentIsLessThanPreviousIndent | IndentIsSameAsPreviousIndent->
                                    let parentId =
                                            if newCompilerStatus.CurrentLocation.RelationSourceId.IsSome 
                                                then newCompilerStatus.CurrentLocation.RelationSourceId.Value 
                                                else newCompilerStatus.CurrentLocation.ParentId
                                    let retLoc= if (joinType<>HasA && joinType<>IsOwnedByA)// || joinType <> newCompilerStatus.CurrentLocation.RelationType.Value
                                                then {newCompilerStatus.CurrentLocation with AttributeId=option.None; AttributeType=option.None; RelationSourceId=option.None; RelationTargetId=option.None}
                                                else
                                                    let parentItem=getModelItemById newCompilerStatus.ModelItems parentId
                                                    {newCompilerStatus.CurrentLocation with AttributeId=option.None; AttributeType=option.None; RelationSourceId=option.None; RelationTargetId=option.None; ParentId=parentItem.Id; Bucket=parentItem.Location.Bucket; Genre=parentItem.Location.Genre; AbstractionLevel=parentItem.Location.AbstractionLevel; TemporalIndicator=parentItem.Location.TemporalIndicator}
                                    retLoc
                                |IndentIsMoreThanPreviousIndent->
                                    {newCompilerStatus.CurrentLocation with RelationSourceId=Some newCompilerStatus.CurrentLocation.ParentId; RelationTargetId=option.None}

                        let newIndentLevelWithCommasConsidered=newCompilerStatus.CompilerState.CurrentIndentLevel + numberOfCommaSeparatedCommandSegments
                        {newCompilerStatus with CompilerState={newCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.NewJoin; LastJoinType=Some joinType; WaitingFor=CompilerWaitingFor.MultipleRelations; CurrentIndentLevel=newIndentLevelWithCommasConsidered}; CurrentLocation=newLoc}

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

    let persistenceRoundTripCheck (currentCompilerState:CompilerReturn) = 
        let firstModelItems=currentCompilerState.ModelItems
        let firstModelItemsSorted=firstModelItems|>Array.sortBy(fun x->x.Description)

        let originalConsoleOut = System.Console.Out
        let writer=new System.IO.StringWriter()
        System.Console.SetOut(writer)
        let dummyOutputDirectory=new System.IO.DirectoryInfo(System.IO.Directory.GetCurrentDirectory())
        writeOutModel currentCompilerState.ModelItems currentCompilerState.ModelItems ModelOutputType.AMOUT dummyOutputDirectory  true ""
        let firstModelOutput = writer.GetStringBuilder().ToString()

        let tempFileName=System.IO.Path.GetRandomFileName()
        let currentDirectoryInfo=(new System.IO.DirectoryInfo(System.Environment.CurrentDirectory))
        Persist.writeOutModel firstModelItems firstModelItems ModelOutputType.AMOUT currentDirectoryInfo true tempFileName
        let tempFileInfo=new System.IO.FileInfo(tempFileName)
        // load back in. Is it the same?
        let listToProcess = loadInAllIncomingLines [|tempFileInfo|]
        let secondProcessedIncomingLines, secondCompilerReturn = bulkFileLineProcessing listToProcess
        
        let secondCompilerStatus = makeRawModel secondProcessedIncomingLines secondCompilerReturn
        let secondModelItemsSorted=secondCompilerStatus.ModelItems|>Array.sortBy(fun x->x.Description)
        let areSame=areModelsEqual firstModelItemsSorted secondModelItemsSorted
        // comment out next line to have program leave a file on disk with single file dump
        System.IO.File.Delete(tempFileName)
        if areSame
            then ()
            else raise (new System.Exception("ROUND TRIP IN persistenceRoundTripCheck failed"))

