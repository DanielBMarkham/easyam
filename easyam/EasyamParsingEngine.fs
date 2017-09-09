﻿module EasyamParsingEngine
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


    ///
    /// TOKEN PROCESSING. TAKES A LINE AND MAKES A LIST OF COMMANDS AND VALUES
    ///
    let rec findInitialTextKeywordAndRemainingTextOnALine (tokenList:string list) (incomingLine:string):(string*string*string) option =
        // There's one oddball exception: if you have // at the beginning, the rest is a comment and there's nothing left
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

    let itemWithThisNameAlreadyExistsAtThisLocation (compilerStatus:CompilerReturn) (location:ModelLocationPointer) (desription:string) =
        let itemsWithThisNameAlreadyInTheModel = compilerStatus.ModelItems|>Array.filter(fun x->x.Description.Trim()=desription.Trim())
        if itemsWithThisNameAlreadyInTheModel.Length=0 then false
        else
            let itemsFurtherMatchedUpByLocation = itemsWithThisNameAlreadyInTheModel |> Array.filter(fun x->
                let xLoc = x.Location
                (xLoc.AbstractionLevel=location.AbstractionLevel && xLoc.Bucket=location.Bucket && xLoc.Genre=location.Genre && xLoc.TemporalIndicator=location.TemporalIndicator && xLoc.Namespace=location.Namespace)
                )
            itemsFurtherMatchedUpByLocation.Length>0
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
    let replaceArrayItemInPlace (arr:'A []) (itemToReplace:'A) (funEquality:'A->'A->bool):'A [] =
        let itemIndex=
            let tempFind = arr |> Array.tryFindIndex(funEquality itemToReplace)
            if tempFind.IsSome then tempFind.Value else raise(new System.ArgumentOutOfRangeException())
        Array.set arr itemIndex itemToReplace
        arr
        
    let updateModelItem (compilerStatus:CompilerReturn) (updatedModelItem:ModelItem2) = 
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
                        let newLocation = {compilerStatus.CurrentLocation with LastJoinTargetId=Some targetItem.Id}
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
                        let newLocation = {compilerStatus.CurrentLocation with LastJoinTargetId=Some targetItem.Id}
                        {compilerStatusWithNewSourceAndTarget with CurrentLocation=newLocation}
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
     
    let oneItemCanConnectLikeThisToAnother (item1:ModelItem2) (item2:ModelItem2) (joinType:ModelJoin) =
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
    let itemsThatCanJoinToMe (compilerStatus:CompilerReturn) (item:ModelItem2) (joinType:ModelJoin) =
        let itemArray=compilerStatus.ModelItems
        itemArray |> Array.filter(fun x->
            (oneItemCanConnectLikeThisToAnother item x joinType)
            )

    let addAnnotation (parentId:int) (annotationType:ANNOTATION_TOKEN_TYPE) (annotationValue:string) (sourceLine:IncomingLine) (currentCompilerStatus:CompilerReturn) =
        if annotationValue="" then currentCompilerStatus
            else
                let modelItemToChange = currentCompilerStatus.ModelItems |> Array.tryFind(fun x->x.Id=parentId)
                if modelItemToChange.IsNone
                    then currentCompilerStatus
                    else
                        let annotationAlreadyExists=modelItemToChange.Value.Annotations |> Array.exists(fun z->((fst z)=annotationType) && ( (snd z)=annotationValue.Trim()))
                        if annotationAlreadyExists 
                            then currentCompilerStatus 
                            else
                                let newAnnotation = (annotationType,annotationValue.Trim())
                                let newAnnotations = [|newAnnotation|] |> Array.append modelItemToChange.Value.Annotations
                                let newSourceReferences = [|sourceLine|] |> Array.append modelItemToChange.Value.SourceReferences
                                let newModelItem = {modelItemToChange.Value with Annotations=newAnnotations; SourceReferences=newSourceReferences}
                                // the indent does not go up for notes ON THE SAME LINE AS COMMANDS
                                let newIndent = 
                                    if annotationType=ANNOTATION_TOKEN_TYPE.Note && sourceLine.Commands.Length>1 then
                                        if currentCompilerStatus.CompilerState.CurrentIndentLevel>0 then currentCompilerStatus.CompilerState.CurrentIndentLevel-1 else 0
                                    else currentCompilerStatus.CompilerState.CurrentIndentLevel
                                let newCompilerState = {currentCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.NewAnnotation; CurrentIndentLevel=newIndent}
                                let newLoc = {currentCompilerStatus.CurrentLocation with AttributeId=option.None; AttributeType=option.None}
                                let newCompilerStatus=updateModelItem {currentCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newLoc} newModelItem
                                newCompilerStatus
    let addAttributeAnnotation (parentId:int) (attributeTargetId:int) (annotationType:ANNOTATION_TOKEN_TYPE) (annotationValue:string) (sourceLine:IncomingLine) (currentCompilerStatus:CompilerReturn) =
        if annotationValue="" then currentCompilerStatus
            else
                let modelItemToChange = currentCompilerStatus.ModelItems |> Array.tryFind(fun x->x.Id=parentId)
                if modelItemToChange.IsNone
                    then currentCompilerStatus
                    else                
                        let attributeToChange = modelItemToChange.Value.Attributes|> Array.tryFind(fun x->x.id=attributeTargetId)
                        if attributeToChange.IsNone then currentCompilerStatus
                            else
                                let newAnnotation = (annotationType,annotationValue)
                                let newAnnotations = [|newAnnotation|] |> Array.append attributeToChange.Value.Annotations
                                let newAttribute = {attributeToChange.Value with Annotations=newAnnotations}
                                let newAttributes = replaceArrayItemInPlace modelItemToChange.Value.Attributes newAttribute (fun x y->x.id=y.id)
                                let isLineAlreadyREferencedInModelItem = modelItemToChange.Value.SourceReferences|>Array.exists(fun x->x.SourceRawLineNumber=sourceLine.SourceRawLineNumber)
                                let newModelItemSourceReferences=if isLineAlreadyREferencedInModelItem then modelItemToChange.Value.SourceReferences else  ([|sourceLine|] |> Array.append modelItemToChange.Value.SourceReferences)
                                let newModelItem = {modelItemToChange.Value with SourceReferences=newModelItemSourceReferences; Attributes=newAttributes}
                                let newCompilerState = {currentCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.NewAttributeAnnotation}
                                let newCompilerLocation = {currentCompilerStatus.CurrentLocation with AttributeId = Some newAttribute.id; AttributeType = Some newAttribute.AttributeType}
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
                        let newCompilerLocation ={currentCompilerStatus.CurrentLocation with AttributeId = Some newModelAttribute.id; AttributeType=Some newModelAttribute.AttributeType}
                        let modifiedModelItem = {targetModelItem with SourceReferences=newTargetModelItemSourceReferences; Attributes=newTargetModelItemAttributes}
                        updateModelItem {currentCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newCompilerLocation} modifiedModelItem
    let areTheseTwoKeyValueStringArraysEqual (array1:System.Collections.Generic.KeyValuePair<string,string> []) (array2:System.Collections.Generic.KeyValuePair<string,string> []) =
        if array1.Length<>array2.Length then false else
        if array1.Length=0 then true else
        let itemsAreEqual (item1:System.Collections.Generic.KeyValuePair<string,string>) (item2:System.Collections.Generic.KeyValuePair<string,string>) = (item1.Key=item2.Key) && (item1.Value=item2.Value)
        let array1FoundIn2 = array1|>Array.fold(fun acc x->array2|>Array.exists(fun y->(itemsAreEqual y x)) && acc) true
        let array2FoundIn1 = array2|>Array.fold(fun acc x->array1|>Array.exists(fun y->(itemsAreEqual y x)) && acc) true
        (array1FoundIn2 && array2FoundIn1)
    let updateExistingItemWithCurrentLocationExtras (compilerStatus:CompilerReturn) (id:int) =
        let doesItemExistInModel = compilerStatus.ModelItems |> Array.exists(fun x->x.Id=id)
        if doesItemExistInModel=false then compilerStatus else
        let itemToChange = compilerStatus.ModelItems |> Array.find(fun x->x.Id=id)
        let tagsAreTheSame = areTheseTwoKeyValueStringArraysEqual itemToChange.Tags compilerStatus.CurrentLocation.Tags
        if tagsAreTheSame=true then compilerStatus else
        // if they're not the same, there's no merge. Instead whatever's in location gets overwritten into the item
        let changedItem={itemToChange with Tags=compilerStatus.CurrentLocation.Tags}
        updateModelItem compilerStatus changedItem

    let rec updateModelLocationPointer (originalCompilerStatus:CompilerReturn) (incomingLine:IncomingLine) (incomingCommand:Command):CompilerReturn =
        // context resets when the file changes

        let fileCheckedCompilerStatus=if incomingLine.File.FullName=originalCompilerStatus.CompilerState.LastFileNameProcessed
                                        then originalCompilerStatus
                                        //else {originalCompilerStatus with CompilerWaitingForState=Nothing; CompilerState={defaultCompilerState with LastFileNameProcessed=incomingLine.File.FullName}}
                                        else 
                                            {originalCompilerStatus with CompilerState={defaultCompilerState with LastFileNameProcessed=incomingLine.File.FullName}; CurrentLocation=defaultModelLocationPointer}
                                            //{originalCompilerStatus with CompilerState={defaultCompilerState with LastFileNameProcessed=incomingLine.File.FullName; WaitingFor=CompilerWaitingFor.Nothing; CurrentIndentLevel=0}; CurrentLocation=defaultModelLocationPointer}
        let newIndentLevel=if incomingCommand.CommandIndentLevel<fileCheckedCompilerStatus.CompilerState.CurrentIndentLevel 
                                then IndentLevelComparisons.IndentIsLessThanPreviousIndent
                            elif incomingCommand.CommandIndentLevel=fileCheckedCompilerStatus.CompilerState.CurrentIndentLevel
                                then IndentLevelComparisons.IndentIsSameAsPreviousIndent
                            else IndentLevelComparisons.IndentIsMoreThanPreviousIndent
        let newCompilerState={fileCheckedCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; IndentLevelChange=newIndentLevel}
        let incomingCompilerStatus = {fileCheckedCompilerStatus with CompilerState=newCompilerState}

        let tokenForCommand = 
            let tokenSimpleFind=EasyAMTokens |> List.tryFind(fun z->z.Token.Trim()=incomingCommand.Token.Trim())
            if tokenSimpleFind.IsSome then tokenSimpleFind
                elif ((incomingCommand.Token + incomingCommand.Value).GetLeft 1 = "@") || ((incomingCommand.Token + incomingCommand.Value).GetLeft 1 = "&") then EasyAMTokens |> List.tryFind(fun z->(z.Token.GetLeft 1 = "@") || (z.Token.GetLeft 1 = "&"))
                else Option.None

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
                        if incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewJoin || incomingCompilerStatus.CompilerState.WaitingFor=CompilerWaitingFor.MultipleJoinTargets
                            then
                                // waiting on a join
                                match incomingCompilerStatus.CompilerState.IndentLevelChange, itemAlreadyExists.IsSome with 
                                    | IndentLevelComparisons.IndentIsLessThanPreviousIndent, false->
                                        // multiple targets, there's a join, the indent is less, and the item does not already exist
                                        // new item
                                        addModelItem incomingCompilerStatus incomingCompilerStatus.CurrentLocation incomingLine (incomingCommand.Value.Trim())
                                    | IndentLevelComparisons.IndentIsLessThanPreviousIndent, true->
                                        // multiple targets, there's a join, the indent is less, and the item already exists
                                        // We have a new parent
                                        let newLocation = {incomingCompilerStatus.CurrentLocation with ParentId=itemAlreadyExists.Value.Id}
                                        let newState = {incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.Nothing; LastJoinType=option.None; LastCompilerOperation=LastCompilerOperations.ReferenceExistingItem}
                                        let compilerStatusWithExtrasCheckedForExistingItem=updateExistingItemWithCurrentLocationExtras incomingCompilerStatus itemAlreadyExists.Value.Id
                                        {compilerStatusWithExtrasCheckedForExistingItem with CurrentLocation=newLocation; CompilerState=newState}                                                
                                    | IndentLevelComparisons.IndentIsMoreThanPreviousIndent, false->
                                        // multiple targets, there's a join, the indent is more, and the item does not already exist
                                        // if there's a comma, check each item separately
                                        // if it's not already in the model it's a note on the previous parent
                                        if incomingCommand.Value.Contains(",")
                                            then
                                                let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                                let lastJoinType = if incomingCompilerStatus.CompilerState.LastJoinType.IsSome then incomingCompilerStatus.CompilerState.LastJoinType.Value else raise(new System.Exception("We have a join but no join type to use"))
                                                let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                                            let commaSplitItemAlreadyExists=incomingCompilerStatus.ModelItems |> Array.tryFind(fun z->z.Description=x.Trim())
                                                                            if commaSplitItemAlreadyExists.IsSome
                                                                                then
                                                                                    let currentTargetDescription=accumulatorCompilerStatus.ModelItems|>Array.tryFind(fun x->x.Id=accumulatorCompilerStatus.CurrentLocation.ParentId)
                                                                                    let currentTargetDescription=if currentTargetDescription.IsSome then currentTargetDescription.Value.Description else ""
                                                                                    joinModelItems accumulatorCompilerStatus accumulatorCompilerStatus.CurrentLocation incomingLine lastJoinType currentTargetDescription  (x.Trim())
                                                                                else
                                                                                    addAnnotation targetForPossibleComments.Id ANNOTATION_TOKEN_TYPE.Note (incomingCommand.Value.Trim()) incomingLine incomingCompilerStatus
                                                                        ) incomingCompilerStatus
                                                newCompilerStatus
                                            else
                                                let lastJoinType = if incomingCompilerStatus.CompilerState.LastJoinType.IsSome then incomingCompilerStatus.CompilerState.LastJoinType.Value else raise(new System.Exception("We have a join but no join type to use"))
                                                let updatedCompilerStatus=addModelItem incomingCompilerStatus incomingCompilerStatus.CurrentLocation incomingLine incomingCommand.Value
                                                let currentTargetDescription=updatedCompilerStatus.ModelItems|>Array.tryFind(fun x->x.Id=updatedCompilerStatus.CurrentLocation.ParentId)
                                                let currentTargetDescription=if currentTargetDescription.IsSome then currentTargetDescription.Value.Description else ""
                                                joinModelItems updatedCompilerStatus updatedCompilerStatus.CurrentLocation incomingLine lastJoinType currentTargetDescription (incomingCommand.Value)
                                                //addAnnotation targetForPossibleComments.Id ANNOTATION_TOKEN_TYPE.Note (incomingCommand.Value.Trim()) incomingLine incomingCompilerStatus
                                    | IndentLevelComparisons.IndentIsMoreThanPreviousIndent, true->
                                        // multiple targets, there's a join, the indent is more, and the item already exists
                                        // it's a badly formatted join
                                        let lastJoinType = if incomingCompilerStatus.CompilerState.LastJoinType.IsSome then incomingCompilerStatus.CompilerState.LastJoinType.Value else raise(new System.Exception("We have a join but no join type to use"))
                                        let currentTargetDescription=incomingCompilerStatus.ModelItems|>Array.tryFind(fun x->x.Id=incomingCompilerStatus.CurrentLocation.ParentId)
                                        let currentTargetDescription=if currentTargetDescription.IsSome then currentTargetDescription.Value.Description else ""
                                        let joinedCompilerStatus=joinModelItems incomingCompilerStatus incomingCompilerStatus.CurrentLocation incomingLine lastJoinType currentTargetDescription  (incomingCommand.Value)
                                        let compilerStatusWithExtrasCheckedForExistingItem=updateExistingItemWithCurrentLocationExtras joinedCompilerStatus joinedCompilerStatus.CurrentLocation.ParentId
                                        compilerStatusWithExtrasCheckedForExistingItem
                                    | IndentLevelComparisons.IndentIsSameAsPreviousIndent, true->
                                        // multiple targets, there's a join, the indent is the same, and the item does exist
                                        // it's just another join
                                        let lastJoinType = if incomingCompilerStatus.CompilerState.LastJoinType.IsSome then incomingCompilerStatus.CompilerState.LastJoinType.Value else raise(new System.Exception("We have a join but no join type to use"))
                                        let currentTargetDescription=incomingCompilerStatus.ModelItems|>Array.tryFind(fun x->x.Id=incomingCompilerStatus.CurrentLocation.ParentId)
                                        let currentTargetDescription=if currentTargetDescription.IsSome then currentTargetDescription.Value.Description else ""
                                        let compilerStatusWithExtrasCheckedForExistingItem=updateExistingItemWithCurrentLocationExtras incomingCompilerStatus itemAlreadyExists.Value.Id
                                        joinModelItems incomingCompilerStatus incomingCompilerStatus.CurrentLocation incomingLine lastJoinType currentTargetDescription  (incomingCommand.Value)
                                    | IndentLevelComparisons.IndentIsSameAsPreviousIndent, false->
                                        // multiple targets, there's a join, the indent is the same, and the item does not exist

                                        // NO NO NO. We can't do that. Instead we need to add items whenever we find them
                                        // it's a badly-formatted note
                                        //addAnnotation targetForPossibleComments.Id ANNOTATION_TOKEN_TYPE.Note (incomingCommand.Value.Trim()) incomingLine incomingCompilerStatus

                                        let lastJoinType = if incomingCompilerStatus.CompilerState.LastJoinType.IsSome then incomingCompilerStatus.CompilerState.LastJoinType.Value else raise(new System.Exception("We have a join but no join type to use"))
                                        let updatedCompilerStatus=addModelItem incomingCompilerStatus incomingCompilerStatus.CurrentLocation incomingLine incomingCommand.Value
                                        let currentTargetDescription=updatedCompilerStatus.ModelItems|>Array.tryFind(fun x->x.Id=updatedCompilerStatus.CurrentLocation.ParentId)
                                        let currentTargetDescription=if currentTargetDescription.IsSome then currentTargetDescription.Value.Description else ""
                                        let ret=joinModelItems updatedCompilerStatus updatedCompilerStatus.CurrentLocation incomingLine lastJoinType currentTargetDescription   (incomingCommand.Value)
                                        // since we're adding as we go, our mode is now waiting on new join items
                                        let newState={ret.CompilerState with WaitingFor=CompilerWaitingFor.MultipleJoinTargets}
                                        {ret with CompilerState=newState}
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
                                        let compilerStatusWithExtrasCheckedForExistingItem=updateExistingItemWithCurrentLocationExtras incomingCompilerStatus itemAlreadyExists.Value.Id
                                        let newLocation = {compilerStatusWithExtrasCheckedForExistingItem.CurrentLocation with ParentId=itemAlreadyExists.Value.Id}
                                        //let newState = {incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.Nothing; LastJoinType=option.None; LastCompilerOperation=LastCompilerOperations.ReferenceExistingItem}
                                        let newState = {compilerStatusWithExtrasCheckedForExistingItem.CompilerState with WaitingFor=compilerStatusWithExtrasCheckedForExistingItem.CompilerState.WaitingFor; LastJoinType=option.None; LastCompilerOperation=LastCompilerOperations.ReferenceExistingItem}
                                        {compilerStatusWithExtrasCheckedForExistingItem with CurrentLocation=newLocation; CompilerState=newState}
                    |CompilerWaitingFor.MultipleAttributeTargets->
                        match newIndentLevel with 
                            |IndentIsMoreThanPreviousIndent->
                                        // if we're further idented, we're continuing the previous att list
                                        let newAttributeType = incomingCompilerStatus.CurrentLocation.AttributeType.Value
                                        let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                        let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                                    if x.Length=0 then accumulatorCompilerStatus else
                                                                    addModelAttribute newAttributeType (x.Trim()) incomingLine accumulatorCompilerStatus
                                                                ) incomingCompilerStatus
                                        let newCompilerState={newCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAttributeTargets}
                                        {newCompilerStatus with CompilerState=newCompilerState}
                            |IndentIsSameAsPreviousIndent->
                                        // if we're further idented, we're continuing the previous att list
                                        let newAttributeType = incomingCompilerStatus.CurrentLocation.AttributeType.Value
                                        let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                        let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                                    if x.Length=0 then accumulatorCompilerStatus else
                                                                    addModelAttribute newAttributeType (x.Trim()) incomingLine accumulatorCompilerStatus
                                                                ) incomingCompilerStatus
                                        let newCompilerState={newCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAttributeTargets}
                                        {newCompilerStatus with CompilerState=newCompilerState}
                            |IndentIsLessThanPreviousIndent->
                                if incomingCompilerStatus.CompilerState.LastCompilerOperation<>LastCompilerOperations.NewAnnotation
                                    then
                                        // if we're not further idented, we pop back up to the parent, not waiting on atts
                                        // reset the pointer to the location without attributes and wait on multiple stuff
                                        let newCompilerState={incomingCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; LastCompilerOperation=LastCompilerOperations.PointerReset; WaitingFor=CompilerWaitingFor.MultipleTargets}
                                        let newCompilerLocation={incomingCompilerStatus.CurrentLocation with AttributeType=option.None; AttributeId=option.None; ParentId=(-1)}
                                        let newCompilerStatus={incomingCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newCompilerLocation}
                                        updateModelLocationPointer newCompilerStatus incomingLine incomingCommand 
                                    else
                                        // if we're further idented, we're continuing the previous att list
                                        let newAttributeType = incomingCompilerStatus.CurrentLocation.AttributeType.Value
                                        let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                        let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                                    if x.Length=0 then accumulatorCompilerStatus else
                                                                    addModelAttribute newAttributeType (x.Trim()) incomingLine accumulatorCompilerStatus
                                                                ) incomingCompilerStatus
                                        let newCompilerState={newCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAttributeTargets}
                                        {newCompilerStatus with CompilerState=newCompilerState}
                    |CompilerWaitingFor.MultipleAnnotationTargets->
                        match newIndentLevel with 
                            |IndentIsLessThanPreviousIndent->
                                // what's the next level up?
                                if incomingCompilerStatus.CurrentLocation.AttributeId.IsSome && incomingCompilerStatus.CurrentLocation.AttributeType.IsSome
                                    then
                                        // if the last annotation was on an attribute, then we pop back to the attribute, depending on levels jumped
                                        let levelsJumped = originalCompilerStatus.CompilerState.CurrentIndentLevel-incomingCompilerStatus.CompilerState.CurrentIndentLevel
                                        let welastAnnotatedAnAttribute = incomingCompilerStatus.CurrentLocation.AttributeId.IsSome
                                        if levelsJumped = 1 && welastAnnotatedAnAttribute
                                            then
                                                // it's popped one level back up to the attribute
                                                let newCompilerState={incomingCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; LastCompilerOperation=LastCompilerOperations.PointerReset; WaitingFor=CompilerWaitingFor.MultipleAttributeTargets}
                                                let newCompilerLocation={incomingCompilerStatus.CurrentLocation with AnnotationIndicator=ANNOTATION_TOKEN_TYPE.None}
                                                let newCompilerStatus={incomingCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newCompilerLocation}
                                                updateModelLocationPointer newCompilerStatus incomingLine incomingCommand 
                                            elif welastAnnotatedAnAttribute&& levelsJumped=2 then
                                                // it's popped two levels back up to the item
                                                //let newCompilerState={incomingCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; LastCompilerOperation=LastCompilerOperations.PointerReset; WaitingFor=CompilerWaitingFor.MultipleTargets}
                                                //let newCompilerLocation={incomingCompilerStatus.CurrentLocation with ParentId=(-1); AnnotationIndicator=ANNOTATION_TOKEN_TYPE.None}
                                                let newCompilerState={incomingCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; LastCompilerOperation=LastCompilerOperations.PointerReset; WaitingFor=CompilerWaitingFor.MultipleAttributeTargets}
                                                let newCompilerLocation={incomingCompilerStatus.CurrentLocation with AnnotationIndicator=ANNOTATION_TOKEN_TYPE.None}
                                                let newCompilerStatus={incomingCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newCompilerLocation}
                                                updateModelLocationPointer newCompilerStatus incomingLine incomingCommand 
                                            else
                                                let newCompilerState={incomingCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; LastCompilerOperation=LastCompilerOperations.PointerReset; WaitingFor=CompilerWaitingFor.MultipleTargets}
                                                let newCompilerLocation={incomingCompilerStatus.CurrentLocation with ParentId=(-1); AnnotationIndicator=ANNOTATION_TOKEN_TYPE.None}
                                                //let newCompilerState={incomingCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; LastCompilerOperation=LastCompilerOperations.PointerReset; WaitingFor=CompilerWaitingFor.MultipleAttributeTargets}
                                                //let newCompilerLocation={incomingCompilerStatus.CurrentLocation with AnnotationIndicator=ANNOTATION_TOKEN_TYPE.None}
                                                let newCompilerStatus={incomingCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newCompilerLocation}
                                                updateModelLocationPointer newCompilerStatus incomingLine incomingCommand 

                                    else
                                        // we assume the next level up is the parent pop back up to the parent, not waiting on atts
                                        // reset the pointer to the location without annotations and wait on multiple stuff
                                        let newCompilerState={incomingCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; LastCompilerOperation=LastCompilerOperations.PointerReset; WaitingFor=CompilerWaitingFor.MultipleTargets}
                                        let newCompilerLocation={incomingCompilerStatus.CurrentLocation with ParentId=(-1); AnnotationIndicator=ANNOTATION_TOKEN_TYPE.None}
                                        let newCompilerStatus={incomingCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newCompilerLocation}
                                        updateModelLocationPointer newCompilerStatus incomingLine incomingCommand 
                            |IndentIsSameAsPreviousIndent->
                                match incomingCompilerStatus.CompilerState.LastCompilerOperation with 
                                    | LastCompilerOperations.NewAttribute->
                                        if incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewAttribute || (incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.LocationChange && incomingCompilerStatus.CurrentLocation.AttributeId.IsSome)
                                            then
                                                let itemParentId=incomingCompilerStatus.CurrentLocation.ParentId
                                                let attributeTargetId=incomingCompilerStatus.CurrentLocation.AttributeId.Value
                                                let attributeType=incomingCompilerStatus.CurrentLocation.AttributeType.Value
                                                let annotationTokenType=incomingCompilerStatus.CurrentLocation.AnnotationIndicator
                                                let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                                let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                                            if x.Length=0 then accumulatorCompilerStatus else
                                                                            addAttributeAnnotation itemParentId attributeTargetId annotationTokenType x incomingLine  accumulatorCompilerStatus
                                                                        ) incomingCompilerStatus
                                                let newCompilerState={newCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotationTargets}
                                                {newCompilerStatus with CompilerState=newCompilerState}
                                            else
                                                let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                                let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                                            if x.Length=0 then accumulatorCompilerStatus else
                                                                            addAnnotation incomingCompilerStatus.CurrentLocation.ParentId incomingCompilerStatus.CurrentLocation.AnnotationIndicator (x.Trim()) incomingLine incomingCompilerStatus
                                                                        ) incomingCompilerStatus
                                                let newCompilerState={newCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotationTargets}
                                                {newCompilerStatus with CompilerState=newCompilerState}
                                    | LastCompilerOperations.NewModelItem->
                                                let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                                let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                                            if x.Length=0 then accumulatorCompilerStatus else
                                                                            addAnnotation incomingCompilerStatus.CurrentLocation.ParentId incomingCompilerStatus.CurrentLocation.AnnotationIndicator (x.Trim()) incomingLine incomingCompilerStatus
                                                                        ) incomingCompilerStatus
                                                let newCompilerState={newCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotationTargets}
                                                {newCompilerStatus with CompilerState=newCompilerState}
                                    |_->
                                                let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                                let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                                            if x.Length=0 then accumulatorCompilerStatus else
                                                                            addAnnotation incomingCompilerStatus.CurrentLocation.ParentId incomingCompilerStatus.CurrentLocation.AnnotationIndicator (x.Trim()) incomingLine incomingCompilerStatus
                                                                        ) incomingCompilerStatus
                                                let newCompilerState={newCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotationTargets}
                                                {newCompilerStatus with CompilerState=newCompilerState}
                            |IndentIsMoreThanPreviousIndent->
                                // if we're further idented, we're continuing the previous annotation list
                                // if the last operation was a new attribute, then we're annotating on that attribute
                                if incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewAttribute || (incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.LocationChange && incomingCompilerStatus.CurrentLocation.AttributeId.IsSome)
                                    then
                                        let itemParentId=incomingCompilerStatus.CurrentLocation.ParentId
                                        let attributeTargetId=incomingCompilerStatus.CurrentLocation.AttributeId.Value
                                        let attributeType=incomingCompilerStatus.CurrentLocation.AttributeType.Value
                                        let annotationTokenType=incomingCompilerStatus.CurrentLocation.AnnotationIndicator
                                        let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                        let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                                    if x.Length=0 then accumulatorCompilerStatus else
                                                                    addAttributeAnnotation itemParentId attributeTargetId annotationTokenType x incomingLine  accumulatorCompilerStatus
                                                                ) incomingCompilerStatus
                                        let newCompilerState={newCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotationTargets}
                                        {newCompilerStatus with CompilerState=newCompilerState}
                                    else
                                        let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                        let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                                    if x.Length=0 then accumulatorCompilerStatus else
                                                                    addAnnotation incomingCompilerStatus.CurrentLocation.ParentId incomingCompilerStatus.CurrentLocation.AnnotationIndicator (x.Trim()) incomingLine incomingCompilerStatus
                                                                ) incomingCompilerStatus
                                        let newCompilerState={newCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotationTargets}
                                        {newCompilerStatus with CompilerState=newCompilerState}
                    |CompilerWaitingFor.MultipleJoinTargets->
                        if incomingCompilerStatus.CompilerState.IndentLevelChange=IndentIsLessThanPreviousIndent
                            then
                                // first we're no longer waiting on a join
                                let newState={incomingCompilerStatus.CompilerState with LastJoinType=option.None; WaitingFor=CompilerWaitingFor.MultipleTargets; LastCompilerOperation=LastCompilerOperations.LocationChange}
                                let newLoc={incomingCompilerStatus.CurrentLocation with LastJoinTargetId=option.None}
                                let newStat={incomingCompilerStatus with CurrentLocation=newLoc; CompilerState=newState}
                                updateModelLocationPointer newStat incomingLine incomingCommand
                            else
                                let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                let joinType=incomingCompilerStatus.CompilerState.LastJoinType.Value
                                let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                            if x.Length=0 then accumulatorCompilerStatus else
                                                            // in a multiple join with a new item added, should it be the new parent?
                                                            let modCompilerStatus=makeATargetOfAJoinIfYouCan accumulatorCompilerStatus incomingLine joinType (x.Trim())
                                                            let parentToFindId=
                                                                if modCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewModelItem 
                                                                    then accumulatorCompilerStatus.CurrentLocation.ParentId 
                                                                    elif modCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewJoin
                                                                        then
                                                                            let joinedTarget=modCompilerStatus.ModelItems|>Array.find(fun z->z.Id=modCompilerStatus.CurrentLocation.LastJoinTargetId.Value)
                                                                            if joinedTarget.Relations.Length>0 then joinedTarget.Relations.[joinedTarget.Relations.Length-1].TargetId else joinedTarget.Id
                                                                    else modCompilerStatus.CurrentLocation.ParentId
                                                            let currentTargetDescription=modCompilerStatus.ModelItems|>Array.tryFind(fun x->x.Id=parentToFindId)
                                                            if currentTargetDescription.IsSome && currentTargetDescription.Value.Description<>""
                                                                then joinModelItems modCompilerStatus modCompilerStatus.CurrentLocation incomingLine joinType currentTargetDescription.Value.Description  (x.Trim())                                                                 
                                                                else modCompilerStatus
                                                            
                                                        ) incomingCompilerStatus
                                //let newLoc={newCompilerStatus.CurrentLocation witLastJoinTargetIdId=Some newCompilerStatus.CurrentLocation.ParentId}
                                {newCompilerStatus with CompilerState={newCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.NewJoin; LastJoinType=Some joinType; WaitingFor=CompilerWaitingFor.MultipleJoinTargets}}

                    |_->
                        let newModelItem =
                            {
                                Id=getNextModelItemNumber()
                                Location=incomingCompilerStatus.CurrentLocation
                                Description=incomingCommand.Value
                                Attributes=[||]
                                Annotations=[||]
                                SourceReferences= [||]
                                Relations=[||]
                                Tags=[||]
                            }
                        let newModelItemList = [|newModelItem|] |> Array.append incomingCompilerStatus.ModelItems
                        {incomingCompilerStatus with ModelItems=newModelItemList}
            |Some token->
                match token.TargetType,token.Type,token.Category with 
                    |TOKEN_TARGET_TYPE.SINGLE_TARGET,TOKEN_TYPE.ABSOLUTE_LOCATOR,TOKEN_CATEGORY.SHORTCUT->
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
                        let newState = {incomingCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.LocationChange; WaitingFor=CompilerWaitingFor.MultipleTargets}
                        let updatedCompilerStatus={incomingCompilerStatus with CurrentLocation=newLocation; CompilerState=newState}
                        let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                        let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                    if x.Length=0 then accumulatorCompilerStatus else
                                                    addModelItem accumulatorCompilerStatus newLocation incomingLine (x.Trim())
                                                ) updatedCompilerStatus
                        let newIndentLevelWithCommasConsidered=newCompilerStatus.CompilerState.CurrentIndentLevel + splitByComma.Length
                        {newCompilerStatus with CompilerState={newCompilerStatus.CompilerState with CurrentIndentLevel=newIndentLevelWithCommasConsidered}}
                    |TOKEN_TARGET_TYPE.SINGLE_TARGET,TOKEN_TYPE.RELATIVE_LOCATOR,TOKEN_CATEGORY.SHORTCUT->
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
                        let newState = {incomingCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.LocationChange; WaitingFor=CompilerWaitingFor.MultipleTargets}
                        let updatedCompilerStatus={incomingCompilerStatus with CurrentLocation=newLocation; CompilerState=newState}
                        let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                        let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                    if x.Length=0 then accumulatorCompilerStatus else
                                                    addModelItem accumulatorCompilerStatus newLocation incomingLine (x.Trim())
                                                ) updatedCompilerStatus
                        let newIndentLevelWithCommasConsidered=newCompilerStatus.CompilerState.CurrentIndentLevel + splitByComma.Length
                        {newCompilerStatus with CompilerState={newCompilerStatus.CompilerState with CurrentIndentLevel=newIndentLevelWithCommasConsidered}}
                    |TOKEN_TARGET_TYPE.SINGLE_TARGET,TOKEN_TYPE.RELATIVE_LOCATOR,TOKEN_CATEGORY.MISC->
                        let newTempAnnotationIndicator=
                            match token.Token with 
                                | "Q " | "Q:" | "QUESTION " | "QUESTION"|"QUESTION:"->ANNOTATION_TOKEN_TYPE.Question
                                | "//" | "NOTE " | "NOTE: "|"NOTE:"->ANNOTATION_TOKEN_TYPE.Note
                                | "TODO " | "TODO: "|"TODO:"|"TO-DO"|"TO-DO: "|"TO-DO:"->ANNOTATION_TOKEN_TYPE.ToDo
                                | "WORK: " | "WORK "|"WORK:"->ANNOTATION_TOKEN_TYPE.Work
                                | "DIAGRAM:" | "DIAGRAMS"|"DIAGRAMS"->ANNOTATION_TOKEN_TYPE.Diagram
                                |_->ANNOTATION_TOKEN_TYPE.Note // ERROR ERROR
                        if (incomingCommand.CommandIndentLevel<originalCompilerStatus.CompilerState.CurrentIndentLevel)
                            then
                                // ident has popped up. Whatever we were referencing, we are now referencing the item
                                let newLocation={incomingCompilerStatus.CurrentLocation with AttributeId=option.None; AttributeType=option.None;}
                                let newState={incomingCompilerStatus.CompilerState with CurrentIndentLevel=incomingCommand.CommandIndentLevel; WaitingFor=CompilerWaitingFor.MultipleAnnotationTargets; LastCompilerOperation=LastCompilerOperations.LocationChange}
                                let adjustedCompilerStatus={incomingCompilerStatus with CurrentLocation=newLocation; CompilerState=newState}
                                addAnnotation adjustedCompilerStatus.CurrentLocation.ParentId newTempAnnotationIndicator incomingCommand.Value incomingLine adjustedCompilerStatus
                            else
                                // the indent has popped down. We're referencing whatever item we just referred to
                                if incomingCompilerStatus.CompilerState.WaitingFor=CompilerWaitingFor.MultipleAttributeTargets && (incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewAttribute || incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.ReferenceExistingAttribute)
                                    then
                                        // it was an attribute
                                        let ret = addAttributeAnnotation incomingCompilerStatus.CurrentLocation.ParentId incomingCompilerStatus.CurrentLocation.AttributeId.Value newTempAnnotationIndicator incomingCommand.Value incomingLine incomingCompilerStatus
                                        {ret with CompilerState={ret.CompilerState with CurrentIndentLevel=ret.CompilerState.CurrentIndentLevel-1}}
                                    else
                                        // it wasn't an attribute. Was it a new model item?
                                        match incomingCompilerStatus.CompilerState.LastCompilerOperation with 
                                            | LastCompilerOperations.NewJoin->
                                                let joinParentId=incomingCompilerStatus.CurrentLocation.LastJoinTargetId.Value
                                                let ret=addAnnotation joinParentId newTempAnnotationIndicator incomingCommand.Value incomingLine incomingCompilerStatus  
                                                // if this is a same-line annotation, the last action wasn't adding an annotation, it was whatever it was originally
                                                if incomingCommand.Token="//"
                                                    then
                                                        {ret with CompilerState={ret.CompilerState with LastCompilerOperation=originalCompilerStatus.CompilerState.LastCompilerOperation}}
                                                    else ret
                                            | LastCompilerOperations.NewModelItem->
                                                if incomingCommand.Token="//"
                                                    then
                                                        let ret=addAnnotation incomingCompilerStatus.CurrentLocation.ParentId ANNOTATION_TOKEN_TYPE.Note incomingCommand.Value incomingLine incomingCompilerStatus
                                                        {ret with CompilerState={ret.CompilerState with LastCompilerOperation=originalCompilerStatus.CompilerState.LastCompilerOperation}}
                                                    else
                                                        let newState=if incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewModelItem then {incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotationTargets} else incomingCompilerStatus.CompilerState
                                                        let newLocation=if incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewModelItem then {incomingCompilerStatus.CurrentLocation with AnnotationIndicator=newTempAnnotationIndicator} else incomingCompilerStatus.CurrentLocation
                                                        // We're doing a "reach-around" and annotating the root item, so we need to reset the location back to the attribute list
                                                        let tempCompStat=addAnnotation incomingCompilerStatus.CurrentLocation.ParentId newTempAnnotationIndicator incomingCommand.Value incomingLine {incomingCompilerStatus with CompilerState=newState; CurrentLocation=newLocation} 
                                                        {tempCompStat with CurrentLocation={tempCompStat.CurrentLocation with AttributeType=originalCompilerStatus.CurrentLocation.AttributeType }}
                                            |_->
                                                let newState=if incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewModelItem then {incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAnnotationTargets} else incomingCompilerStatus.CompilerState
                                                let newLocation=if incomingCompilerStatus.CompilerState.LastCompilerOperation=LastCompilerOperations.NewModelItem then {incomingCompilerStatus.CurrentLocation with AnnotationIndicator=newTempAnnotationIndicator} else incomingCompilerStatus.CurrentLocation
                                                // We're doing a "reach-around" and annotating the root item, so we need to reset the location back to the attribute list
                                                let tempCompStat=addAnnotation incomingCompilerStatus.CurrentLocation.ParentId newTempAnnotationIndicator incomingCommand.Value incomingLine {incomingCompilerStatus with CompilerState=newState; CurrentLocation=newLocation} 
                                                {tempCompStat with CurrentLocation={tempCompStat.CurrentLocation with AttributeType=originalCompilerStatus.CurrentLocation.AttributeType }}
                    |TOKEN_TARGET_TYPE.MULTIPLE_TARGETS,TOKEN_TYPE.RELATIVE_LOCATOR,TOKEN_CATEGORY.MISC->
                        let newTempAnnotationIndicator=
                            match token.Token with 
                                | "QUESTIONS"|"QUESTIONS " | "QUESTIONS: " | "QUESTIONS:"->ANNOTATION_TOKEN_TYPE.Question
                                | "NOTES"|"NOTES " | "NOTES: "|"NOTES:"->ANNOTATION_TOKEN_TYPE.Note
                                | "TODOS"|"TODOS " | "TODOS: "|"TODOS:"|"TO-DOS"|"TO-DOS "|"TO-DOS: "|"TO-DOS:"->ANNOTATION_TOKEN_TYPE.ToDo
                                | "WORKS"|"WORKS: " | "WORKS "|"WORKS:"|"WORK:"->ANNOTATION_TOKEN_TYPE.Work
                                |_->ANNOTATION_TOKEN_TYPE.Note // ERROR ERROR
                        // run through everything split on this line by a comma and add
                        let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None) |> Array.filter(fun x->x.Length<>0)
                        let newAttributeType,newAttributeId =
                            if incomingCompilerStatus.CompilerState.IndentLevelChange=IndentLevelComparisons.IndentIsLessThanPreviousIndent
                                then
                                    option.None,option.None
                                else
                                    incomingCompilerStatus.CurrentLocation.AttributeType,incomingCompilerStatus.CurrentLocation.AttributeId
                        let newLoc={incomingCompilerStatus.CurrentLocation with AttributeType=newAttributeType; AttributeId=newAttributeId}
                        let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                if x.Length=0 then accumulatorCompilerStatus else
                                                let newAccumulatorCompilerStatus=addAnnotation accumulatorCompilerStatus.CurrentLocation.ParentId newTempAnnotationIndicator (x.Trim()) incomingLine accumulatorCompilerStatus 
                                                newAccumulatorCompilerStatus
                                                ) {incomingCompilerStatus with CurrentLocation=newLoc}
                        let newLocation = {newCompilerStatus.CurrentLocation with AnnotationIndicator=newTempAnnotationIndicator}
                        let newIndentLevelWithCommasConsidered=newCompilerStatus.CompilerState.CurrentIndentLevel + splitByComma.Length
                        {newCompilerStatus with CurrentLocation=newLocation; CompilerState={newCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.LocationChange; WaitingFor=CompilerWaitingFor.MultipleAnnotationTargets}}
                    |_,_,NAMESPACE->
                        let newNamespace=incomingCommand.Value
                        let newLocationPointer = {incomingCompilerStatus.CurrentLocation with Namespace=newNamespace}
                        let updatedCompilerStatus = {incomingCompilerStatus with CurrentLocation=newLocationPointer; CompilerState={incomingCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.LocationChange}}
                        updatedCompilerStatus
                    |TOKEN_TARGET_TYPE.MULTIPLE_TARGETS,TOKEN_TYPE.ABSOLUTE_LOCATOR,_->
                        match token.Category with 
                            |TOKEN_CATEGORY.TAG->
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
                                                    //let tagArray = valArray |> Array.fold(fun acc x->
                                                    //    let newItem=new System.Collections.Generic.KeyValuePair<string,string>(key,x)
                                                    //    [|newItem|] |> Array.append acc) [||]
                                                    let tagArray = keyArray|> Array.fold(fun acc2 y->
                                                        valArray |> Array.fold(fun acc x->
                                                            let newItem=new System.Collections.Generic.KeyValuePair<string,string>(removeQuotes(y),removeQuotes(x))
                                                            [|newItem|] |> Array.append acc) acc2) [||]
                                                    let newTags= addKeyValueTags tagArray incomingCompilerStatus.CurrentLocation.Tags //tagArray |> Array.append incomingCompilerStatus.CurrentLocation.Tags
                                                    let newLocation={incomingCompilerStatus.CurrentLocation with Tags=newTags}
                                                    {incomingCompilerStatus with CurrentLocation=newLocation}
                            |TOKEN_CATEGORY.BUCKETS->
                                let newBucket = match token.Token with 
                                                | "BEHAVIOR" | "BEHAVIOR:" | "BEHAVIORS" |"BEHAVIORS:"->Buckets.Behavior
                                                | "STRUCTURE" | "STRUCTURE:" | "STRUCTURES" | "STRUCTURES:" ->Buckets.Structure
                                                | "SUPPLEMENTAL" | "SUPPLEMENTAL:"|"SUPPLEMENTALS" |"SUPPLEMENTALS:" |_->Buckets.Supplemental
                                let newParentId=if incomingCompilerStatus.CurrentLocation.Bucket<>Buckets.None then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                let currentLocation = incomingCompilerStatus.CurrentLocation
                                let newGenre= if currentLocation.Genre=Genres.None then Genres.Business else currentLocation.Genre
                                let newAbstractionLevel=if currentLocation.AbstractionLevel=AbstractionLevels.None then AbstractionLevels.Abstract else currentLocation.AbstractionLevel
                                let newTemporalIndicator=if currentLocation.TemporalIndicator=TemporalIndicators.None then TemporalIndicators.ToBe else currentLocation.TemporalIndicator
                                let newLocationPointer = {incomingCompilerStatus.CurrentLocation with Bucket=newBucket; Genre=newGenre; AbstractionLevel=newAbstractionLevel; TemporalIndicator=newTemporalIndicator; ParentId=newParentId}
                                let updatedCompilerStatus = {incomingCompilerStatus with CurrentLocation=newLocationPointer; CompilerState={incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleTargets}}
                                let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                            if x.Length=0 then accumulatorCompilerStatus else
                                                            addModelItem accumulatorCompilerStatus newLocationPointer incomingLine (x.Trim())
                                                        ) updatedCompilerStatus
                                let newIndentLevelWithCommasConsidered=newCompilerStatus.CompilerState.CurrentIndentLevel + splitByComma.Length
                                {newCompilerStatus with CompilerState={newCompilerStatus.CompilerState with CurrentIndentLevel=newIndentLevelWithCommasConsidered}}
                            |TOKEN_CATEGORY.ABSTRACTION_LEVEL->
                                let newAbstractionLevel = match token.Token with 
                                                            | "ABSTRACT" | "ABSTRACT:"->AbstractionLevels.Abstract
                                                            | "REALIZED" | "REALIZED:"|_->AbstractionLevels.Realized
                                let newParentId=if incomingCompilerStatus.CurrentLocation.AbstractionLevel<>AbstractionLevels.None then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                let newLocation = {incomingCompilerStatus.CurrentLocation with AbstractionLevel=newAbstractionLevel; ParentId=newParentId}
                                let updatedCompilerStatus={incomingCompilerStatus with CurrentLocation=newLocation; CompilerState={incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleTargets}}
                                let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                            if x.Length=0 then accumulatorCompilerStatus else
                                                            addModelItem accumulatorCompilerStatus newLocation incomingLine (x.Trim())
                                                        ) updatedCompilerStatus
                                let newIndentLevelWithCommasConsidered=newCompilerStatus.CompilerState.CurrentIndentLevel + splitByComma.Length
                                {newCompilerStatus with CompilerState={newCompilerStatus.CompilerState with CurrentIndentLevel=newIndentLevelWithCommasConsidered}}
                            |TOKEN_CATEGORY.GENRE->
                                let newGenre = match token.Token with 
                                                | "SYSTEM" | "SYSTEM:"->Genres.System
                                                | "META" | "META:"->Genres.Meta
                                                | "BUSINESS" | "BUSINESS:"|_->Genres.Business
                                let newParentId=if incomingCompilerStatus.CurrentLocation.Genre<>Genres.None then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                let newLocation = {incomingCompilerStatus.CurrentLocation with Genre=newGenre; ParentId=newParentId}
                                let updatedCompilerStatus = {incomingCompilerStatus with CurrentLocation=newLocation; CompilerState={incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleTargets}}
                                let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                            if x.Length=0 then accumulatorCompilerStatus else
                                                            addModelItem accumulatorCompilerStatus newLocation incomingLine (x.Trim())
                                                        ) updatedCompilerStatus
                                let newIndentLevelWithCommasConsidered=newCompilerStatus.CompilerState.CurrentIndentLevel + splitByComma.Length
                                {newCompilerStatus with CompilerState={newCompilerStatus.CompilerState with CurrentIndentLevel=newIndentLevelWithCommasConsidered}}
                            |TOKEN_CATEGORY.TEMPORAL->
                                let newTemporalIndicator = match token.Token with 
                                                            | "WAS" | "WAS:"->TemporalIndicators.Was
                                                            | "TO-BE" | "TO-BE:"->TemporalIndicators.ToBe
                                                            | "AS-IS" | "AS-IS:"|_->TemporalIndicators.AsIs
                                let newParentId=if incomingCompilerStatus.CurrentLocation.TemporalIndicator<>TemporalIndicators.None then (-1) else incomingCompilerStatus.CurrentLocation.ParentId
                                let newLocation = {incomingCompilerStatus.CurrentLocation with TemporalIndicator=newTemporalIndicator; ParentId=newParentId}
                                let updatedCompilerStatus = {incomingCompilerStatus with CurrentLocation=newLocation; CompilerState={incomingCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleTargets}}
                                let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                            if x.Length=0 then accumulatorCompilerStatus else
                                                            let modCompilerStatus=addModelItem accumulatorCompilerStatus newLocation incomingLine (x.Trim())
                                                            let reallyNewParentId=modCompilerStatus.ModelItems|>Array.find(fun z->z.Description=(x.Trim()))
                                                            let reallyNewLocation={modCompilerStatus.CurrentLocation with ParentId=reallyNewParentId.Id}
                                                            {modCompilerStatus with CurrentLocation=reallyNewLocation}
                                                        ) updatedCompilerStatus
                                let newIndentLevelWithCommasConsidered=newCompilerStatus.CompilerState.CurrentIndentLevel + splitByComma.Length
                                {newCompilerStatus with CompilerState={newCompilerStatus.CompilerState with CurrentIndentLevel=newIndentLevelWithCommasConsidered}}
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
                            |TOKEN_CATEGORY.ATTRIBUTE->
                                let newAttributeType = match token.Token with
                                                        | "WHEN"|"WHEN:"->ModelAttributeTypes.Trigger
                                                        | "ASA"|"ASA:"->ModelAttributeTypes.Actor
                                                        | "INEEDTO"|"INEEDTO:"->ModelAttributeTypes.Goal
                                                        | "SOTHAT"|"SOTHAT:"->ModelAttributeTypes.BusinessContext
                                                        | "SCENARIO"|"SCENARIO:"->ModelAttributeTypes.Scenario
                                                        | "BECAUSE"|"BECAUSE:"->ModelAttributeTypes.Because
                                                        | "WHENEVER"|"WHENEVER:"->ModelAttributeTypes.Whenever
                                                        | "ITHASTOBETHAT"|"ITHASTOBETHAT:"->ModelAttributeTypes.ItHasToBeThat
                                                        | "CONTAINS"|"CONTAINS:"->ModelAttributeTypes.Contains
                                                        |_->raise(new System.Exception("We're supposed to have an attribute, but none are there"))
                                let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                                let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                            if x.Length=0 then accumulatorCompilerStatus else
                                                            addModelAttribute newAttributeType (x.Trim()) incomingLine accumulatorCompilerStatus
                                                        ) incomingCompilerStatus
                                let newCompilerLocation={newCompilerStatus.CurrentLocation with AttributeType = Some newAttributeType}
                                let newIndentLevelWithCommasConsidered=newCompilerStatus.CompilerState.CurrentIndentLevel + splitByComma.Length
                                let newCompilerState={newCompilerStatus.CompilerState with WaitingFor=CompilerWaitingFor.MultipleAttributeTargets; LastCompilerOperation=LastCompilerOperations.LocationChange; CurrentIndentLevel=newIndentLevelWithCommasConsidered}
                                {newCompilerStatus with CompilerState=newCompilerState; CurrentLocation=newCompilerLocation}
                            |_->raise(new System.Exception("messed up"))
                    |_,TOKEN_TYPE.JOINER,_->
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
                        let splitByComma = incomingCommand.Value.Split([|","|], System.StringSplitOptions.None)
                        let newCompilerStatus = splitByComma |> Array.fold(fun (accumulatorCompilerStatus:CompilerReturn) x->
                                                    if x.Length=0 then accumulatorCompilerStatus else
                                                    let currentTargetDescriptionOpt=accumulatorCompilerStatus.ModelItems|>Array.tryFind(fun x->x.Id=accumulatorCompilerStatus.CurrentLocation.ParentId)
                                                    let currentTargetDescription=if currentTargetDescriptionOpt.IsSome then currentTargetDescriptionOpt.Value.Description else ""
                                                    if currentTargetDescription=""||x.Trim()=""
                                                        then accumulatorCompilerStatus
                                                        else
                                                            joinModelItems accumulatorCompilerStatus incomingCompilerStatus.CurrentLocation incomingLine joinType currentTargetDescription (x.Trim())
                                                ) incomingCompilerStatus
                        let newLoc={newCompilerStatus.CurrentLocation with LastJoinTargetId=Some newCompilerStatus.CurrentLocation.ParentId; AttributeId=option.None; AttributeType=option.None}
                        let newIndentLevelWithCommasConsidered=newCompilerStatus.CompilerState.CurrentIndentLevel + splitByComma.Length
                        {newCompilerStatus with CompilerState={newCompilerStatus.CompilerState with LastCompilerOperation=LastCompilerOperations.NewJoin; LastJoinType=Some joinType; WaitingFor=CompilerWaitingFor.MultipleJoinTargets; CurrentIndentLevel=newIndentLevelWithCommasConsidered}; CurrentLocation=newLoc}
                    |_,_,_->incomingCompilerStatus
                
    let verifyOrAddAMUSItem compilerStatus description = 
        let allMUS=getMasterUserStories compilerStatus.ModelItems
        let alreadyExists=allMUS|>Array.exists(fun x->x.Description=description)
        if alreadyExists then compilerStatus
        else
            let newLocation = {defaultModelLocationPointer with Genre=Genres.Business; Bucket=Buckets.Behavior; AbstractionLevel=AbstractionLevels.Abstract; TemporalIndicator=TemporalIndicators.ToBe}
            let newIncomingLine:IncomingLine = defaultIncomingLine
            let newCompilerReturn = addModelItem compilerStatus newLocation newIncomingLine description
            newCompilerReturn
    //let verifyOrAddMISCandALL compilerStatus =
    //    let statWithMISCAdded = verifyOrAddAMUSItem compilerStatus "MISC"
    //    let statWithMISCandALL = verifyOrAddAMUSItem statWithMISCAdded "ALL"
    //    statWithMISCandALL

    let makeRawModel (incomingLines:IncomingLine []) (incomingCompilerStatus:CompilerReturn) =
        let initialModelLines = incomingLines |> Array.fold(fun (currentCompilerStatus:CompilerReturn) x->
                                        let modelItemsOnThisLine = x.Commands |> Array.fold(fun (currentLineCompilerStatus:CompilerReturn) y->
                                                                    let newCompilerStatus=updateModelLocationPointer currentLineCompilerStatus x y
                                                                    newCompilerStatus
                                                                    ) currentCompilerStatus
                                        let newacc = modelItemsOnThisLine
                                        newacc
                                ) incomingCompilerStatus
        //verifyOrAddMISCandALL initialModelLines
        initialModelLines

