module SAModel
    open Types

// REFACTOR
// Everything entered in has three things: an origin, a tagged context, and free text (markup)
// There are four types of models: Structure, Behavior, Supplemental, and Meta
// Each model is structured so: 
//      Model Business Abstract
//          Item(s) [Diagrams: Item/Relationship list]
//              Model Business Realized
//                  Item(s) [Diagrams: Item/Relationship list]
//                      Model System Abstract
//                          Items(s) [Diagrams: Item/Relationship list]
//                              Model System Realized
//                                  Item(s) [Diagrams: Item/Relationship list]
// HYPOTHESIS->Item (title format) -> Desired change list of behavior, structure, and suppl (at any level)
//
// Any one of these can have notes (freetext tagged to something), questions, TODOs, and name-value tags
// Items can be unique to that layer -- or simply refer to an ancestor item up the tree
// Items have a short name, a long name, and a detailed name (super long)? Ids are INTERNAL-ONLY,
// we force people organize their shared mental models around the English Language, not around codes and numbers
// Nesting is implied by whitespace or colons, eg BUSINESS BEHAVIOR: Do Dishes
//
// compilation rules:
//      no more than 40 items per layer (error)
//      no behavior nouns not mentioned in structure (and vice versa) (warning)
//      

    let IntegerFactory = 
        let counter = ref 0
        fun () -> 
            counter.Value <- !counter + 1
            !counter
    let getNextItemNumber()=IntegerFactory()
    // Structured Analysis Model Super Types
    type Buckets =
        | Unknown
        | None
        | Behavior
        | Structure
        | Supplemental
         static member ToList() =
            [Unknown;None;Behavior;Structure;Supplemental]
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | None->"None"
            | Behavior->"Behavior"
            | Structure->"Structure"
            | Supplemental->"Supplemental"
    type Genres =
        | Unknown
        | None
        | Business
        | System
        | Meta
         static member ToList() =
            [Unknown;None;Business; System; Meta]
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | None->"None"
            | Business->"Business"
            | System->"System"
            | Meta->"Meta"

    type AbstractionLevels = 
        | Unknown
        | None
        | Abstract
        | Realized
         static member ToList() =
            [Unknown;None;Abstract;Realized]
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | None->"None"
            | Abstract->"Abstract"
            | Realized->"Realized"
    type TemporalIndicators =
        | Unknown
        | None
        | Was
        | AsIs
        | ToBe
         static member ToList() =
            [Unknown;None;Was;AsIs;ToBe]
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | None->"None"
            | Was->"Was"
            | AsIs->"As-Is"
            | ToBe->"To-Be"

    // General Helper Types
    type NounClause =
        {
            text:string
        }
    type VerbClause =
        {
            text:string
        }
    type VerbNounClause = VerbClause*NounClause
    type SourceReference =
        {
            File:System.IO.FileInfo
            LineNumber:int
            LineLevelIndent:int
        }

    type NameValueTag =
        {
            Name:string
            Value:string
            SourceReference:SourceReference
        }
    type Note=
        {
            Text:string
            SourceReference:SourceReference
        }
    type Question=
        {
            Text:string
            SourceReference:SourceReference
        }
    type ToDo=
        {
            Text:string
            SourceReference:SourceReference
        }
    type Work=
        {
            Text:string
            SourceReference:SourceReference
        }
    type ContextShift=
        {
            Text:string
            SourceReference:SourceReference
        }
    type ItemAnnotation =
        {
            Notes:Note list
            Questions:Question list
            ToDos:ToDo list
            WorkHistory:Work list
        }
    type ItemConnectorType =
        | ParentChild
        | HasA
        | Affects
        | Contains
         static member ToList() =
            [ParentChild;Affects;HasA;Contains]
         override self.ToString() =
          match self with
            | ParentChild->"ParentChild"
            | HasA->"HasA"
            | Affects->"Affects"
            | Contains->"Contains"
    type ItemConnection = 
        {
            ConnectionType:ItemConnectorType
            LhsId:int
            RhsId:int
            SourceReference:SourceReference
        }
    type incomingLine = {FileNumber:int;FileInfo:System.IO.FileInfo; LineNumber:int; LineText:string; IndentLevel:int; LineWithoutLeadingSpaces:string}
    type ModelItemType =
        | None
        | Diagram
        | Label                         // arbitrary labels for items
        | Connection of ItemConnection  // joins up Items
        | ModelItem of ModelItem        // items
        | NameValueTag of NameValueTag
        | Question of Question
        | Note of Note
        | ToDo of ToDo
        | Work of Work
        | ContextShift of ContextShift
         override self.ToString() =
          match self with
            | None->"None"
            | Diagram->"Diagram"
            | Label->"Label"
            | Connection({
                            ConnectionType=a;LhsId=b;RhsId=c}) ->
                                match a with
                                    |ParentChild->"Parent-Child Connection: " + b.ToString() + " is parent of " + c.ToString()
                                    |HasA->"HasA Conntection: " + b.ToString() + " has " + c.ToString()
                                    |Affects->"Affects Conntection: " + b.ToString() + " affects " + c.ToString()
                                    |Contains->"Contains Connection: " + b.ToString() + " contains " + c.ToString()
            | ModelItem({
                            Id=a
                            SourceCodeParent=b
                            ItemType=c
                            Bucket=d
                            Genre=e
                            AbstractionLevel=f
                            TemporalIndicator=g
                            ItemAnnotation=h
                            SourceReferences=i
                            ModelItemName=j
                        })->"MODELITEM: " + j
            | NameValueTag({Name=x;Value=y;SourceReference=z})->"NAME-VALUE TAG: " + x + "=" + y
            | Question({Text=x; SourceReference=y})->"QUESTION: " + x
            | Note({Text=x; SourceReference=y})->"NOTE: " + x
            | ToDo({Text=x; SourceReference=y})->"TODO: " + x
            | Work({Text=x; SourceReference=y})->"WORK: " + x
            | ContextShift({Text=x; SourceReference=y})->"CONTEXT SHIFT: " + x
    and ModelItem =
        {
            Id:int
            SourceCodeParent:int
            ModelParent:int
            ItemType:ModelItemType
            Bucket:Buckets
            Genre:Genres
            AbstractionLevel:AbstractionLevels
            TemporalIndicator:TemporalIndicators
            ItemAnnotation:ItemAnnotation
            SourceReferences:SourceReference list
            ModelItemName:string
        }
        with
            member self.HasSomeContextToIt = (
                    ((self.Bucket<>Buckets.None) &&  (self.Bucket<>Buckets.Unknown))
                    ||  ((self.Genre<>Genres.None) &&  (self.Genre<>Genres.Unknown))
                    ||  ((self.AbstractionLevel<>AbstractionLevels.None) &&  (self.AbstractionLevel<>AbstractionLevels.Unknown))
                    ||  ((self.TemporalIndicator<>TemporalIndicators.None) &&  (self.TemporalIndicator<>TemporalIndicators.Unknown)) )
            member self.ToModelHeading =
                self.Genre.ToString().ToUpper() + " " + self.Bucket.ToString().ToUpper() + " " + self.AbstractionLevel.ToString().ToUpper() + " " + self.TemporalIndicator.ToString().ToUpper()
            member self.ToAbbreviatedModelHeading =
                (self.Genre.ToString().GetLeft 1).ToLower() + (self.Bucket.ToString().GetLeft 1).ToLower() + (self.AbstractionLevel.ToString().GetLeft 1).ToLower() + (self.TemporalIndicator.ToString().GetLeft 1).ToLower()
            member self.ToFileName =
                let tempName=(self.ToAbbreviatedModelHeading + "-" + self.ModelItemName.CovertIntoOSSafeFileName)
                let ret=tempName.ReplaceAny [|'/';'\\'|] '-'
                ret
            member self.HasAModelParent = 
                self.ModelParent<>0
    and ProcessContext = {ContextStack:System.Collections.Generic.Stack<ModelItem>; Lines:ModelItem list; LastAddedModelItem:ModelItem}
    let defaultModelItem = 
        {
            Id=0
            SourceCodeParent=0
            ModelParent=0
            ItemType=ModelItemType.None
            Bucket=Buckets.None
            Genre=Genres.None
            AbstractionLevel=AbstractionLevels.None
            TemporalIndicator=TemporalIndicators.None
            ItemAnnotation={Notes=[];Questions=[];ToDos=[];WorkHistory=[]}
            SourceReferences=[]
            ModelItemName=""
        }
    let defaultProcessContext = 
        let newContextStack = new System.Collections.Generic.Stack<ModelItem>()
        newContextStack.Push(defaultModelItem)
        {ContextStack=newContextStack; Lines=[]; LastAddedModelItem=defaultModelItem}
    let smashTwoModelItems (existingModelItem:ModelItem) (incomingModelItem:ModelItem) =
            let newId=incomingModelItem.Id
            let newSourceCodeParent=if incomingModelItem.SourceCodeParent<>0 then incomingModelItem.SourceCodeParent else existingModelItem.SourceCodeParent
            let newModelParent=if incomingModelItem.ModelParent<>0 then incomingModelItem.ModelParent else existingModelItem.ModelParent
            let newItemType=if ((incomingModelItem.ItemType<>ModelItemType.None)) then incomingModelItem.ItemType else existingModelItem.ItemType
            let newBucket=if ((incomingModelItem.Bucket<>Buckets.Unknown) && (incomingModelItem.Bucket<>Buckets.None)) then incomingModelItem.Bucket else existingModelItem.Bucket
            let newGenre=if ((incomingModelItem.Genre<>Genres.Unknown) && (incomingModelItem.Genre<>Genres.None)) then incomingModelItem.Genre else existingModelItem.Genre
            let newAbstractionLevel=if ((incomingModelItem.AbstractionLevel<>AbstractionLevels.Unknown) && (incomingModelItem.AbstractionLevel<>AbstractionLevels.None)) then incomingModelItem.AbstractionLevel else existingModelItem.AbstractionLevel
            let newTemporalIndicator=if ((incomingModelItem.TemporalIndicator<>TemporalIndicators.Unknown) && (incomingModelItem.TemporalIndicator<>TemporalIndicators.None)) then incomingModelItem.TemporalIndicator else existingModelItem.TemporalIndicator
            let newItemAnnotation=incomingModelItem.ItemAnnotation
            let newSourceReferences=incomingModelItem.SourceReferences
            let newModelItemName=incomingModelItem.ModelItemName
            {
                Id=newId
                SourceCodeParent=newSourceCodeParent
                ModelParent=newModelParent
                ItemType=newItemType
                Bucket=newBucket
                Genre=newGenre
                AbstractionLevel=newAbstractionLevel
                TemporalIndicator=newTemporalIndicator
                ItemAnnotation=newItemAnnotation
                SourceReferences=newSourceReferences
                ModelItemName=newModelItemName
            }
    let TokenSeparatorList =[" "; "&"; ":"]
//    .*[ |&|:]
    let TokenSeparatorRegex=".*[" + ( TokenSeparatorList |> String.concat "|" ) + "]"
    type SearchDirection =
        | FindFirstMatch
        | FindLastMatch
        | FindFirstMatchExactTokenUsage
        | FindLastMatchExactTokenUsage
    // 'A is the language item state type
    // 'B is the context type
    type Token<'A,'B> =
        {
            SearchDirection:SearchDirection
            RegexMatch:string
            MakeNewModelItemAndUpdateStack:(string*string*'B*incomingLine->'B)
        } with
            member self.IsMatch (sIncomingString) = System.Text.RegularExpressions.Regex.IsMatch(sIncomingString,self.RegexMatch)
            member self.AllMatches (sIncomingString) = System.Text.RegularExpressions.Regex.Matches(sIncomingString,self.RegexMatch)
            member self.FirstMatch (sIncomingString) = 
                let matches = System.Text.RegularExpressions.Regex.Matches(sIncomingString,self.RegexMatch)
                matches.[0]
            member self.LastMatch (sIncomingString) = 
                let matches = System.Text.RegularExpressions.Regex.Matches(sIncomingString,self.RegexMatch)
                matches.[matches.Count-1]
            member self.ConsumeToken(sIncomingString) =
                match self.SearchDirection with
                    | FindFirstMatch->
                        let matchIWant=self.FirstMatch(sIncomingString)
                        let matchBeginLocation = matchIWant.Index 
                        let matchEndLocation = matchBeginLocation + matchIWant.Value.Length 
                        let sMatchPart, sRemainingPart = // chop off separator token if not EOL
                            if matchEndLocation-matchBeginLocation = sIncomingString.Length
                                then matchIWant.Value, ""
                                else 
                                    matchIWant.Value, sIncomingString.Substring(matchEndLocation)
                        (sMatchPart.Trim(), sRemainingPart)
                    | FindFirstMatchExactTokenUsage->
                        let matchIWant=self.FirstMatch(sIncomingString)
                        let matchBeginLocation = matchIWant.Index 
                        let matchEndLocation = matchBeginLocation + matchIWant.Value.Length
                        let sMatchPart, sRemainingPart = // chop off separator token if not EOL
                            if matchEndLocation-matchBeginLocation = sIncomingString.Length
                                then matchIWant.Value, ""
                                else 
                                    matchIWant.Value, sIncomingString.Substring(matchBeginLocation,matchEndLocation-matchBeginLocation-1)
                        (sMatchPart.Trim(), sRemainingPart)

                    | FindLastMatch->
                        let matchIWant=self.LastMatch(sIncomingString)
                        let matchBeginLocation = matchIWant.Index 
                        let matchEndLocation = matchBeginLocation + matchIWant.Value.Length
                        let sMatchPart, sRemainingPart = // chop off separator token if not EOL
                            if matchEndLocation-matchBeginLocation = sIncomingString.Length-1
                                then matchIWant.Value, ""
                                else 
                                    matchIWant.Value, sIncomingString.Substring(matchEndLocation)
                        (sMatchPart.Trim(), sRemainingPart)
                    | FindLastMatchExactTokenUsage->
                        let matchIWant=self.LastMatch(sIncomingString)
                        let matchBeginLocation = matchIWant.Index 
                        let matchEndLocation = matchBeginLocation + matchIWant.Value.Length
                        let sMatchPart, sRemainingPart = // chop off separator token if not EOL
                            if matchEndLocation-matchBeginLocation = sIncomingString.Length
                                then matchIWant.Value, ""
                                else 
                                    matchIWant.Value, sIncomingString.Substring(matchBeginLocation,matchEndLocation-matchBeginLocation-1)
                        (sMatchPart.Trim(), sRemainingPart)

    // Gets the SourceCodeParent and the Model Parent, if it can find them
    let getParent currentContext lineBeingProcessed:(ModelItem*ModelItem)  = 
        match currentContext.Lines.Length with
            | 0->
                defaultModelItem,defaultModelItem
            | _->
                // Parent cannot be a NV tag or a Connection. Partent also can't be a comment
                let contextLinesWithoutNVTags = currentContext.Lines |> List.filter(fun x->
                    match x.ItemType with
                        | ModelItemType.NameValueTag(_)->false
                        | ModelItemType.Note(_)->false
                        | ModelItemType.Connection(_)->false
                        |_->true                
                )
                match contextLinesWithoutNVTags.Length with
                    | 0 ->defaultModelItem,defaultModelItem
                    |_->
                        let contextLinesNoTagsReferringToThisFile = contextLinesWithoutNVTags |> List.filter(fun x->x.SourceReferences.[x.SourceReferences.Length-1].File.FullName=lineBeingProcessed.FileInfo.FullName)
                        let sortedByLineNumber = contextLinesNoTagsReferringToThisFile |> List.sortBy(fun x->x.SourceReferences.[x.SourceReferences.Length-1].LineNumber)
                        let highestLNFirst = sortedByLineNumber |> List.rev
                        
                        let mostRecentItemProcessed =  if highestLNFirst.Length>0 then highestLNFirst.[0] else defaultModelItem // this may include earlier lines pointing to later lines
                        // mostRecentLineProcessed shouldn't include connection items, which can't be parents
                        let mostRecentLineProcessed = 
                            let currentContextLinesWithoutConnections = currentContext.Lines |> List.filter(fun x->match x.ItemType with |Connection(_)->false |_->true)
                            if currentContextLinesWithoutConnections.Length>0 then currentContextLinesWithoutConnections.[currentContextLinesWithoutConnections.Length-1] else defaultModelItem// this is just the physically last line in the queue
                        let mostRecentContextPushed = currentContext.ContextStack.Peek()
                        let mostRecentLineWithLessOfAnIndent = 
                            let ret = (highestLNFirst |> List.filter(fun x->x.SourceReferences.[x.SourceReferences.Length-1].LineLevelIndent<lineBeingProcessed.IndentLevel))
                            let retOnlyThingsThatCanBeParents = ret  |> List.filter(fun x->
                                match x.ItemType with 
                                    | ModelItem(_)->true 
                                    | Connection(_)->true
                                    | ContextShift(_)->true
                                    |_->false)
                            if ret.Length>0 then ret else [defaultModelItem]

                        // if the line number of the last contextlines of either recent itmes = incomingline last context line number, use that one
                        // otherwise use context
                        let lastOneInTheQueueIsOnOurLine = lineBeingProcessed.LineNumber = mostRecentLineProcessed.SourceReferences.[mostRecentLineProcessed.SourceReferences.Length-1].LineNumber
                        
                        let theresAMasterItemSomewherePointingToOurLine = 
                            if mostRecentItemProcessed.SourceReferences.Length>0
                                then
                                    lineBeingProcessed.LineNumber = mostRecentItemProcessed.SourceReferences.[mostRecentItemProcessed.SourceReferences.Length-1].LineNumber
                                else false

                        let sourceCodeParent, modelParent = 
                            match lastOneInTheQueueIsOnOurLine, theresAMasterItemSomewherePointingToOurLine with
                                | true,true->([mostRecentItemProcessed; mostRecentLineProcessed] |> List.maxBy(fun x->x.Id)), ([mostRecentItemProcessed; mostRecentLineProcessed] |> List.maxBy(fun x->x.Id))
                                | true, false->mostRecentItemProcessed,mostRecentItemProcessed
                                | false,true->mostRecentLineProcessed,mostRecentLineProcessed
                                | false,false->if mostRecentLineWithLessOfAnIndent.Length>0
                                                    then //mostRecentLineWithLessOfAnIndent.[0],mostRecentLineWithLessOfAnIndent.[0]
                                                        mostRecentContextPushed,mostRecentLineWithLessOfAnIndent.[0]
                                                    else mostRecentContextPushed,mostRecentContextPushed
                        sourceCodeParent,modelParent

    type TokenHandlingContext =
        {
            mostRecentStructuredAnalysisImportantToken:ModelItem
            closestModelParentOfAnyType:ModelItem
            closestSourceCodeParentOfAnyType:ModelItem
            rightHandValue:string
            tokenValue:string
            newlyCreatedSourceReference:SourceReference
        }
    let establishTokenHandlingContext (lineWithTokenConsumed:string) (tokenMatchText:string) (currentContext:ProcessContext) (lineBeingProcessed:incomingLine) (tokenLength:int):TokenHandlingContext=
        let mostRecentModelItem=currentContext.ContextStack.Peek()
        let possibleSourceCodeParent, possibleModelParent=getParent currentContext lineBeingProcessed
        let rValue=tokenMatchText.Substring(tokenLength).Trim()
        let newTokenValue=tokenMatchText.GetLeft tokenLength
        let newSourceReference={File=lineBeingProcessed.FileInfo;LineNumber=lineBeingProcessed.LineNumber; LineLevelIndent=lineBeingProcessed.IndentLevel}
        {
            mostRecentStructuredAnalysisImportantToken=mostRecentModelItem
            closestModelParentOfAnyType=possibleModelParent
            closestSourceCodeParentOfAnyType=possibleSourceCodeParent
            rightHandValue=rValue
            tokenValue=newTokenValue
            newlyCreatedSourceReference=newSourceReference
        }
    let makeNewModelItemInContext (ctx:TokenHandlingContext) itemType =
        let newlyCreatedModelItem = {
            defaultModelItem with
                Id=getNextItemNumber()
                SourceCodeParent=ctx.closestSourceCodeParentOfAnyType.Id
                ModelParent=ctx.closestModelParentOfAnyType.Id
                ItemType= itemType
                SourceReferences=[ctx.newlyCreatedSourceReference]
            }
        smashTwoModelItems ctx.closestModelParentOfAnyType newlyCreatedModelItem
    let makeNewBusinessBehaviorAbstractToBe (ctx:TokenHandlingContext) itemType =
        {
        defaultModelItem with
            Id=getNextItemNumber()
            ItemType=itemType
            Genre=Genres.Business
            Bucket=Buckets.Behavior
            AbstractionLevel=AbstractionLevels.Abstract
            TemporalIndicator=TemporalIndicators.ToBe
            SourceReferences=[ctx.newlyCreatedSourceReference]
        }
    let makeNewBusinessStructureAbstractToBe (ctx:TokenHandlingContext) itemType =
        {
        defaultModelItem with
            Id=getNextItemNumber()
            ItemType=itemType
            Genre=Genres.Business
            Bucket=Buckets.Structure
            AbstractionLevel=AbstractionLevels.Abstract
            TemporalIndicator=TemporalIndicators.ToBe
            SourceReferences=[ctx.newlyCreatedSourceReference]
        }
    let makeNewBusinessSupplementalAbstractToBe (ctx:TokenHandlingContext) itemType =
        {
        defaultModelItem with
            Id=getNextItemNumber()
            ItemType=itemType
            Genre=Genres.Business
            Bucket=Buckets.Supplemental
            AbstractionLevel=AbstractionLevels.Abstract
            TemporalIndicator=TemporalIndicators.ToBe
            SourceReferences=[ctx.newlyCreatedSourceReference]
        }
    let makeNewBusinessBehaviorRealizedToBe (ctx:TokenHandlingContext) itemType =
        {
        defaultModelItem with
            Id=getNextItemNumber()
            ItemType=itemType
            Genre=Genres.Business
            Bucket=Buckets.Behavior
            AbstractionLevel=AbstractionLevels.Realized
            TemporalIndicator=TemporalIndicators.ToBe
            SourceReferences=[ctx.newlyCreatedSourceReference]
        }
    let makeNewMetaBehaviorAbstractToBe (ctx:TokenHandlingContext) itemType =
        {
        defaultModelItem with
            Id=getNextItemNumber()
            ItemType=itemType
            Genre=Genres.Meta
            Bucket=Buckets.Behavior
            AbstractionLevel=AbstractionLevels.Abstract
            TemporalIndicator=TemporalIndicators.ToBe
            SourceReferences=[ctx.newlyCreatedSourceReference]
        }
    let makeNewSystemSupplementalAbstractToBe (ctx:TokenHandlingContext) itemType =
        {
        defaultModelItem with
            Id=getNextItemNumber()
            ItemType=itemType
            Genre=Genres.System
            Bucket=Buckets.Supplemental
            AbstractionLevel=AbstractionLevels.Abstract
            TemporalIndicator=TemporalIndicators.ToBe
            SourceReferences=[ctx.newlyCreatedSourceReference]
        }
    let makeNewMetaStructureAbstractToBe (ctx:TokenHandlingContext) itemType =
        {
        defaultModelItem with
            Id=getNextItemNumber()
            ItemType=itemType
            Genre=Genres.Meta
            Bucket=Buckets.Structure
            AbstractionLevel=AbstractionLevels.Abstract
            TemporalIndicator=TemporalIndicators.ToBe
            SourceReferences=[ctx.newlyCreatedSourceReference]
        }

    let currentContextWithNewLineInIt (newlyCreatedModelItem:ModelItem) (currentContext:ProcessContext)=
        let newLines= [newlyCreatedModelItem] |> List.append currentContext.Lines
        {currentContext with Lines=newLines}
    let isThereAModelItemAlreadyWithThisNameInThisContext (myModelItemToTest:ModelItem) (currentContext:ProcessContext) =
        currentContext.Lines |> List.tryFind(fun x->
            let genresEqual = x.Genre=myModelItemToTest.Genre
            let abstractionsEqual=x.AbstractionLevel=myModelItemToTest.AbstractionLevel
            let bucketsEqual=x.Bucket=myModelItemToTest.Bucket
            let temporalIndicatorsEqual=x.TemporalIndicator=myModelItemToTest.TemporalIndicator
            let modelItemNamesEqual=x.ModelItemName=myModelItemToTest.ModelItemName
            let r=9
            genresEqual && abstractionsEqual && bucketsEqual && temporalIndicatorsEqual && modelItemNamesEqual)
//            ((x.Genre=myModelItemToTest.Genre))
//            && ((x.AbstractionLevel=myModelItemToTest.AbstractionLevel))
//            && ((x.Bucket=myModelItemToTest.Bucket))
//            && ((x.TemporalIndicator=myModelItemToTest.TemporalIndicator))
//            && ((x.ModelItemName=myModelItemToTest.ModelItemName)))
    let isThereAnAbstractModelItemAlreadyWithThisNameInThisContext (myModelItemToTest:ModelItem) (currentContext:ProcessContext) =
        currentContext.Lines |> List.tryFind(fun x->
            let genresEqual = x.Genre=myModelItemToTest.Genre
            let abstractionsEqual=x.AbstractionLevel=AbstractionLevels.Abstract
            let bucketsEqual=x.Bucket=myModelItemToTest.Bucket
            let temporalIndicatorsEqual=x.TemporalIndicator=myModelItemToTest.TemporalIndicator
            let modelItemNamesEqual=x.ModelItemName=myModelItemToTest.ModelItemName
            let r=9
            genresEqual && abstractionsEqual && bucketsEqual && temporalIndicatorsEqual && modelItemNamesEqual)
//            ((x.Genre=myModelItemToTest.Genre))
//            && ((x.AbstractionLevel=AbstractionLevels.Abstract))
//            && ((x.Bucket=myModelItemToTest.Bucket))
//            && ((x.TemporalIndicator=myModelItemToTest.TemporalIndicator))
//            && ((x.ModelItemName=myModelItemToTest.ModelItemName)))
    let replaceAModelItemInAModelItemListById (modelLines:ModelItem list) (newModelItem:ModelItem)  = 
        // split the list taking out the dupe. Then replace
        let splitLineListFirstPart =fst (modelLines |> List.partition(fun z->z.Id<newModelItem.Id))
        let previousVersionOfItem = modelLines |> List.find(fun x->x.Id=newModelItem.Id)
        let splitLineListSecondPart =fst (modelLines |> List.partition(fun z->z.Id>newModelItem.Id))
        let newLines =  splitLineListSecondPart |> List.append [newModelItem] |> List.append splitLineListFirstPart
        newLines

    type EasyAMToken = Token<ModelItem, ProcessContext>
    let easyAMTokens:EasyAMToken[] = 
        [|
            {
                SearchDirection=FindFirstMatch
                RegexMatch="NOTE:[^&|^:]*" //+ TokenSeparatorRegex
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 5
                                        let newNoteModelItemType = Note({Text=ctx.rightHandValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewModelItemInContext ctx newNoteModelItemType 
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="TODO:[^&|^:]*" //+ TokenSeparatorRegex
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 5
                                        let newToDoModelItemType = ToDo({Text=ctx.rightHandValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewModelItemInContext ctx newToDoModelItemType 
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="WORK:[^&|^:]*" //+ TokenSeparatorRegex
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 5
                                        let newWorkModelItemType = Work({Text=ctx.rightHandValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewModelItemInContext ctx newWorkModelItemType 
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="MASTER DOMAIN MODEL"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 19
                                        let newContexShiftModelItemType = ContextShift({Text=ctx.tokenValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewBusinessStructureAbstractToBe ctx newContexShiftModelItemType
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="SYSTEM SUPPLEMENTAL ABSTRACT"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 28
                                        let newContexShiftModelItemType = ContextShift({Text=ctx.tokenValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewSystemSupplementalAbstractToBe ctx newContexShiftModelItemType
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="MASTER BACKLOG"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 14
                                        let newContexShiftModelItemType = ContextShift({Text=ctx.tokenValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewBusinessBehaviorAbstractToBe ctx newContexShiftModelItemType
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="BUSINESS BEHAVIOR ABSTRACT"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 26
                                        let newContexShiftModelItemType = ContextShift({Text=ctx.tokenValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewBusinessBehaviorAbstractToBe ctx newContexShiftModelItemType
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="BUSINESS BEHAVIOR REALIZED"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 26
                                        let newContexShiftModelItemType = ContextShift({Text=ctx.tokenValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewBusinessBehaviorRealizedToBe ctx newContexShiftModelItemType
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="BUSINESS STRUCTURE ABSTRACT"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 27
                                        let newContexShiftModelItemType = ContextShift({Text=ctx.tokenValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewBusinessStructureAbstractToBe ctx newContexShiftModelItemType
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="MASTER SUPPLEMENTAL MODEL"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 25
                                        let newContexShiftModelItemType = ContextShift({Text=ctx.tokenValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewBusinessSupplementalAbstractToBe ctx newContexShiftModelItemType
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="BUSINESS SUPPLEMENTAL ABSTRACT"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 30
                                        let newContexShiftModelItemType = ContextShift({Text=ctx.tokenValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewBusinessSupplementalAbstractToBe ctx newContexShiftModelItemType
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="PRODUCT BACKLOG"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 15
                                        let newContexShiftModelItemType = ContextShift({Text=ctx.tokenValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewBusinessBehaviorRealizedToBe ctx newContexShiftModelItemType
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="SPRINT BACKLOG"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 14
                                        let newContexShiftModelItemType = ContextShift({Text=ctx.tokenValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewBusinessBehaviorRealizedToBe ctx newContexShiftModelItemType
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="META STRUCTURE"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 14
                                        let newContexShiftModelItemType = ContextShift({Text=ctx.tokenValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewMetaStructureAbstractToBe ctx newContexShiftModelItemType
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="META BEHAVIOR"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 13
                                        let newContexShiftModelItemType = ContextShift({Text=ctx.tokenValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewMetaBehaviorAbstractToBe ctx newContexShiftModelItemType
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="TASKS"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 5
                                        let newContexShiftModelItemType = ContextShift({Text=ctx.tokenValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewMetaBehaviorAbstractToBe ctx newContexShiftModelItemType
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="NOTES"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 5
                                        let newContexShiftModelItemType = ContextShift({Text=ctx.tokenValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem =
                                            {defaultModelItem with
                                                Id=getNextItemNumber()
                                                ItemType=newContexShiftModelItemType
                                                SourceReferences=[ctx.newlyCreatedSourceReference]
                                            }
                                        // clear the context and start over
                                        currentContext.ContextStack.Clear()
                                        currentContext.ContextStack.Push(defaultModelItem)
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="Q:.*"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 2
                                        let newQuestionModelItemType = Question({Text=ctx.rightHandValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewModelItemInContext ctx newQuestionModelItemType 
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="(?:(?!//|&|:|PARENT|HASA|AFFECTS).)*"  //This is the catch-all  OLD RegexMatch="^.*(//|$)"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 0
                                        let sourceCodeParent, modelParent=getParent currentContext lineBeingProcessed
                                        //let incomingModelItem=currentContext.ContextStack.Peek()
                                        let matchText=tokenMatchText.Trim()

                                        let isParentTheDefaultModelItem = modelParent.Id=defaultModelItem.Id

                                        if isParentTheDefaultModelItem
                                            then // It's a comment
                                                let newNoteModelItemType = Note({Text=ctx.rightHandValue; SourceReference=ctx.newlyCreatedSourceReference})
                                                let newlyCreatedModelItem = makeNewModelItemInContext ctx newNoteModelItemType 
                                                currentContextWithNewLineInIt newlyCreatedModelItem currentContext
                                            else // It's a ModelItem
                                                let possibleNewItem = 
                                                    {
                                                        Id=getNextItemNumber()
                                                        SourceCodeParent=modelParent.Id
                                                        ModelParent=0
                                                        ItemType=ModelItemType.ModelItem(
                                                                                        {
                                                                                            Id=0
                                                                                            SourceCodeParent=modelParent.Id
                                                                                            ModelParent=0
                                                                                            ItemType=ModelItemType.None
                                                                                            Bucket=Buckets.None
                                                                                            Genre=Genres.None
                                                                                            AbstractionLevel=AbstractionLevels.None
                                                                                            TemporalIndicator=TemporalIndicators.None
                                                                                            ItemAnnotation={Notes=[];Questions=[];ToDos=[];WorkHistory=[]}
                                                                                            ModelItemName=matchText
                                                                                            SourceReferences=[ctx.newlyCreatedSourceReference]
                                                                                        }
                                                                                    )
                                                        Bucket=modelParent.Bucket
                                                        Genre=modelParent.Genre
                                                        AbstractionLevel=modelParent.AbstractionLevel
                                                        TemporalIndicator=modelParent.TemporalIndicator
                                                        ItemAnnotation={Notes=[];Questions=[];ToDos=[];WorkHistory=[]}
                                                        SourceReferences=[ctx.newlyCreatedSourceReference]
                                                        ModelItemName=matchText
                                                    }
                                                let possibleDupe = isThereAModelItemAlreadyWithThisNameInThisContext  possibleNewItem currentContext
                                                match possibleDupe with
                                                    | Some dupe->
                                                        let newLines= [possibleNewItem] |> List.append currentContext.Lines
                                                        // split the list taking out the dupe. Then replace
                                                        let splitLineListFirstPart =fst (currentContext.Lines |> List.partition(fun z->z.Id<dupe.Id))
                                                        let splitLineListSecondPart =fst (currentContext.Lines |> List.partition(fun z->z.Id>dupe.Id))
                                                        let newDupeSourceReferences = [ctx.newlyCreatedSourceReference] |> List.append dupe.SourceReferences
                                                        let newDupe={dupe with SourceReferences=newDupeSourceReferences}
                                                        currentContext.ContextStack.Push(newDupe)
                                                        let newContextLines =  splitLineListSecondPart |> List.append [newDupe] |> List.append splitLineListFirstPart
                                                        {currentContext with Lines=newContextLines; LastAddedModelItem=newDupe}
                                                    | option.None->
                                                        let newLines= [possibleNewItem] |> List.append currentContext.Lines
                                                        currentContext.ContextStack.Push(possibleNewItem)
                                                        {currentContext with Lines=newLines; LastAddedModelItem=possibleNewItem}
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="PARENT[^&|^:|^//]*"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 7
                                        let newConnectionModelItemType = ModelItemType.Connection({ConnectionType=ParentChild; LhsId=0; RhsId=0; SourceReference=ctx.newlyCreatedSourceReference})
                                        let possibleNewItem = 
                                            {
                                                Id=getNextItemNumber()
                                                SourceCodeParent=ctx.closestModelParentOfAnyType.Id
                                                ModelParent=0
                                                ItemType=ModelItemType.ModelItem(
                                                                                {
                                                                                    Id=0
                                                                                    SourceCodeParent=ctx.closestModelParentOfAnyType.Id
                                                                                    ModelParent=0
                                                                                    ItemType=ModelItemType.None
                                                                                    Bucket=Buckets.None
                                                                                    Genre=Genres.None
                                                                                    AbstractionLevel=AbstractionLevels.None
                                                                                    TemporalIndicator=TemporalIndicators.None
                                                                                    ItemAnnotation={Notes=[];Questions=[];ToDos=[];WorkHistory=[]}
                                                                                    ModelItemName=ctx.rightHandValue.Trim()
                                                                                    SourceReferences=[ctx.newlyCreatedSourceReference]
                                                                                }
                                                                            )
                                                Bucket=ctx.closestModelParentOfAnyType.Bucket
                                                Genre=ctx.closestModelParentOfAnyType.Genre
                                                AbstractionLevel=AbstractionLevels.Abstract
                                                TemporalIndicator=ctx.closestModelParentOfAnyType.TemporalIndicator
                                                ItemAnnotation={Notes=[];Questions=[];ToDos=[];WorkHistory=[]}
                                                SourceReferences=[ctx.newlyCreatedSourceReference]
                                                ModelItemName=ctx.rightHandValue.Trim()
                                            }
                                        // if no parent, add one (the one we just created)
                                        let possibleParent, newLines = 
                                            match (isThereAnAbstractModelItemAlreadyWithThisNameInThisContext possibleNewItem currentContext) with
                                                | Some x->
                                                    // If there is a parent
                                                    // Need to update the source references for the existing parent
                                                    //replaceAModelItemInAModelItemListById (modelLines:ModelItem list) (newModelItem:ModelItem)
                                                    let oldParentSorceReferences = x.SourceReferences
                                                    let newParentSourceReferences = oldParentSorceReferences |> List.append [ctx.newlyCreatedSourceReference]
                                                    let newParent = {x with SourceReferences=newParentSourceReferences}
                                                    let newLines = replaceAModelItemInAModelItemListById currentContext.Lines newParent 
                                                    x,currentContext.Lines
                                                | option.None->
                                                    let neCtxLines = [possibleNewItem] |> List.append currentContext.Lines
                                                    possibleNewItem,neCtxLines
                                        // no matter what, we need to add a connector from the previous item to the found/created parent
                                        let newConnectionModelItemType = Connection({ConnectionType=ParentChild; LhsId=possibleParent.Id; RhsId=currentContext.LastAddedModelItem.Id; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = {(makeNewModelItemInContext ctx newConnectionModelItemType) with ModelParent=possibleParent.Id} 
                                        let newLinesWithTheConnectionAdded = [newlyCreatedModelItem] |> List.append newLines
                                        // now update the child to point to the parent we've found
                                        let childtWereWorkingOn = newLinesWithTheConnectionAdded |> List.find(fun x->x.Id=currentContext.LastAddedModelItem.Id)
                                        let childWithUpdatedParent = {childtWereWorkingOn with ModelParent=possibleParent.Id}
                                        let linesWithConnectionAndUpdatedKid = replaceAModelItemInAModelItemListById newLinesWithTheConnectionAdded childWithUpdatedParent
                                        {currentContext with Lines=linesWithConnectionAndUpdatedKid; LastAddedModelItem=childWithUpdatedParent}
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="HASA[^&|^:|^//]*"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 4
                                        let newConnectionModelItemType = ModelItemType.Connection({ConnectionType=HasA; LhsId=0; RhsId=0; SourceReference=ctx.newlyCreatedSourceReference})
                                        let possibleNewItem = 
                                            {
                                                Id=getNextItemNumber()
                                                SourceCodeParent=ctx.closestModelParentOfAnyType.Id
                                                ModelParent=currentContext.LastAddedModelItem.Id
                                                ItemType=ModelItemType.ModelItem(
                                                                                {
                                                                                    Id=0
                                                                                    SourceCodeParent=ctx.closestModelParentOfAnyType.Id
                                                                                    ModelParent=0
                                                                                    ItemType=ModelItemType.None
                                                                                    Bucket=Buckets.None
                                                                                    Genre=Genres.None
                                                                                    AbstractionLevel=AbstractionLevels.None
                                                                                    TemporalIndicator=TemporalIndicators.None
                                                                                    ItemAnnotation={Notes=[];Questions=[];ToDos=[];WorkHistory=[]}
                                                                                    ModelItemName=ctx.rightHandValue
                                                                                    SourceReferences=[ctx.newlyCreatedSourceReference]
                                                                                }
                                                                            )
                                                Bucket=ctx.closestModelParentOfAnyType.Bucket
                                                Genre=ctx.closestModelParentOfAnyType.Genre
                                                AbstractionLevel=AbstractionLevels.Abstract
                                                TemporalIndicator=ctx.closestModelParentOfAnyType.TemporalIndicator
                                                ItemAnnotation={Notes=[];Questions=[];ToDos=[];WorkHistory=[]}
                                                SourceReferences=[ctx.newlyCreatedSourceReference]
                                                ModelItemName=ctx.rightHandValue
                                            }
                                        // if no HASA TARGET, add one (the one we just created)
                                        let possibleHasATarget, newLines = 
                                            match (isThereAnAbstractModelItemAlreadyWithThisNameInThisContext possibleNewItem currentContext) with
                                                | Some x->
                                                    // If there is a target for the HASA that already exists
                                                    // Need to update the source references for the existing parent
                                                    //replaceAModelItemInAModelItemListById (modelLines:ModelItem list) (newModelItem:ModelItem)
                                                    let oldTargetSorceReferences = x.SourceReferences
                                                    let newTargetSourceReferences = oldTargetSorceReferences |> List.append [ctx.newlyCreatedSourceReference]
                                                    let newTarget = {x with SourceReferences=newTargetSourceReferences}
                                                    let newLines = replaceAModelItemInAModelItemListById currentContext.Lines newTarget
                                                    x,currentContext.Lines
                                                | option.None->
                                                    let neCtxLines = [possibleNewItem] |> List.append currentContext.Lines
                                                    possibleNewItem,neCtxLines
                                        // no matter what, we need to add a connector from the previous item to the found/created parent
                                        let newConnectionModelItemType = Connection({ConnectionType=HasA; LhsId=currentContext.LastAddedModelItem.Id; RhsId=possibleHasATarget.Id; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = {(makeNewModelItemInContext ctx newConnectionModelItemType) with ModelParent=currentContext.LastAddedModelItem.Id} 
                                        let newLinesWithTheConnectionAdded = [newlyCreatedModelItem] |> List.append newLines
                                        // now update the child to point to the parent we've found
                                        // NOT FOR HASA
                                        //let childtWereWorkingOn = newLinesWithTheConnectionAdded |> List.find(fun x->x.Id=currentContext.LastAddedModelItem.Id)
                                        //let childWithUpdatedParent = {childtWereWorkingOn with ModelParent=possibleParent.Id}
                                        //let linesWithConnectionAndUpdatedKid = replaceAModelItemInAModelItemListById newLinesWithTheConnectionAdded childWithUpdatedParent
                                        //{currentContext with Lines=linesWithConnectionAndUpdatedKid}
                                        {currentContext with Lines=newLinesWithTheConnectionAdded}
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="AFFECTS[^&|^:|^//]*"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 7
                                        let affectsTargets=ctx.rightHandValue.Split([|","|], System.StringSplitOptions.None)
                                        // Go through AFFECTS comma-delimited list. If no AFFECTS TARGET, add one (the one we just created)
                                        let newContext = affectsTargets |> Array.fold(fun (affectCumulator:ProcessContext) (z:string)->
                                                            let newConnectionModelItemType = ModelItemType.Connection({ConnectionType=HasA; LhsId=0; RhsId=0; SourceReference=ctx.newlyCreatedSourceReference})
                                                            let possibleNewItem = 
                                                                {
                                                                    Id=getNextItemNumber()
                                                                    SourceCodeParent=ctx.closestModelParentOfAnyType.Id
                                                                    ModelParent=affectCumulator.LastAddedModelItem.Id
                                                                    ItemType=ModelItemType.ModelItem(
                                                                                                    {
                                                                                                        Id=0
                                                                                                        SourceCodeParent=ctx.closestModelParentOfAnyType.Id
                                                                                                        ModelParent=0
                                                                                                        ItemType=ModelItemType.None
                                                                                                        Bucket=Buckets.None
                                                                                                        Genre=Genres.None
                                                                                                        AbstractionLevel=AbstractionLevels.None
                                                                                                        TemporalIndicator=TemporalIndicators.None
                                                                                                        ItemAnnotation={Notes=[];Questions=[];ToDos=[];WorkHistory=[]}
                                                                                                        ModelItemName=z.Trim()
                                                                                                        SourceReferences=[ctx.newlyCreatedSourceReference]
                                                                                                    }
                                                                                                )
                                                                    Bucket=Buckets.Behavior // AFFECTS has to point to a behavior. All SUPPLs modify behavior tests
                                                                    Genre=ctx.closestModelParentOfAnyType.Genre
                                                                    AbstractionLevel=AbstractionLevels.Abstract
                                                                    TemporalIndicator=ctx.closestModelParentOfAnyType.TemporalIndicator
                                                                    ItemAnnotation={Notes=[];Questions=[];ToDos=[];WorkHistory=[]}
                                                                    SourceReferences=[ctx.newlyCreatedSourceReference]
                                                                    ModelItemName=z.Trim()
                                                                }
                                                            let possibleAffectsTarget, newLines = 
                                                                match (isThereAnAbstractModelItemAlreadyWithThisNameInThisContext possibleNewItem affectCumulator) with
                                                                    | Some x->
                                                                        // If there is a target for the AFFECTS that already exists
                                                                        // Need to update the source references for the existing parent
                                                                        let oldTargetSorceReferences = x.SourceReferences
                                                                        let newTargetSourceReferences = oldTargetSorceReferences |> List.append [ctx.newlyCreatedSourceReference]
                                                                        let newTarget = {x with SourceReferences=newTargetSourceReferences}
                                                                        let newLines = replaceAModelItemInAModelItemListById affectCumulator.Lines newTarget
                                                                        x,newLines
                                                                    | option.None->
                                                                        let newCtxLines = [possibleNewItem] |> List.append affectCumulator.Lines
                                                                        possibleNewItem,newCtxLines

                                                            // no matter what, we need to add a connector from the previous item to the found/created parent
                                                            let newConnectionModelItemType = Connection({ConnectionType=Affects; LhsId=affectCumulator.LastAddedModelItem.Id; RhsId=possibleAffectsTarget.Id; SourceReference=ctx.newlyCreatedSourceReference})
                                                            let newlyCreatedModelItem = {(makeNewModelItemInContext ctx newConnectionModelItemType) with ModelParent=ctx.closestModelParentOfAnyType.Id} 
                                                            let newLinesWithTheConnectionAdded = [newlyCreatedModelItem] |> List.append newLines
                                                            // now update the child to point to the parent we've found
                                                            // NOT FOR HASA
                                                            //let childtWereWorkingOn = newLinesWithTheConnectionAdded |> List.find(fun x->x.Id=currentContext.LastAddedModelItem.Id)
                                                            //let childWithUpdatedParent = {childtWereWorkingOn with ModelParent=possibleParent.Id}
                                                            //let linesWithConnectionAndUpdatedKid = replaceAModelItemInAModelItemListById newLinesWithTheConnectionAdded childWithUpdatedParent
                                                            //{currentContext with Lines=linesWithConnectionAndUpdatedKid}
                                                            {affectCumulator with Lines=newLinesWithTheConnectionAdded}

                                                            ) currentContext
                                        newContext
                                    )
            };
            { // This should be near (next?) to last. Catch any name/value pairs on the line
                SearchDirection=FindFirstMatchExactTokenUsage
                RegexMatch="&[^&|^ |^:]*"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let incomingModelItem=currentContext.ContextStack.Peek()
                                        let possibleSourceCodeParent, possibleModelParent=getParent currentContext lineBeingProcessed
                                        let nameText,valueText =
                                            if tokenMatchText.Contains("=")
                                                then
                                                    let equalsSplit=tokenMatchText.IndexOf("=")
                                                    tokenMatchText.Substring(1, equalsSplit-1), tokenMatchText.Substring(equalsSplit+1)
                                                else
                                                    tokenMatchText,""
                                        let newSourceReference={File=lineBeingProcessed.FileInfo;LineNumber=lineBeingProcessed.LineNumber; LineLevelIndent=lineBeingProcessed.IndentLevel}
                                        let newlyCreatedModelItem =
                                            {
                                            defaultModelItem with
                                                Id=getNextItemNumber()
                                                SourceCodeParent=possibleModelParent.Id
                                                ItemType=NameValueTag({Name=nameText; Value=valueText; SourceReference=newSourceReference})
                                                SourceReferences=[newSourceReference]
                                            }
                                        // smash item with most recent item on list to give it context. Also make it child
                                        let newModelItemUpdatedWithContext = 
                                            if possibleModelParent.Id=0
                                                then
                                                    smashTwoModelItems incomingModelItem newlyCreatedModelItem
                                                else
                                                    smashTwoModelItems possibleModelParent newlyCreatedModelItem
                                        let newLines= [newModelItemUpdatedWithContext] |> List.append currentContext.Lines
                                        {currentContext with Lines=newLines}
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="//.*$"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let ctx = establishTokenHandlingContext lineWithTokenConsumed tokenMatchText currentContext lineBeingProcessed 2
                                        let newNoteModelItemType = Note({Text=ctx.rightHandValue; SourceReference=ctx.newlyCreatedSourceReference})
                                        let newlyCreatedModelItem = makeNewModelItemInContext ctx newNoteModelItemType 
                                        currentContextWithNewLineInIt newlyCreatedModelItem currentContext


                                    )
            }
        |]





    // Compilation Language
    let ItemRelationTokens = [|
        ("HASA ", Buckets.Structure);
        ("CONTAINS ", Buckets.Structure);
        ("WHEN ", Buckets.Behavior);
        ("ASA ", Buckets.Behavior);
        ("INEEDTO ", Buckets.Behavior);
        ("SOTHAT ", Buckets.Behavior);
        ("INITIAL ", Buckets.Behavior);
        ("FINAL ", Buckets.Behavior);
        ("MERGENODE ", Buckets.Behavior);
        ("MERGE ", Buckets.Behavior);
        ("FORK ", Buckets.Behavior);
        ("DO ", Buckets.Behavior);
        ("DATA ", Buckets.Supplemental);
        |]
    let ItemRelationTokenVals = ItemRelationTokens |> Array.map(fun x->
            let a,b = x
            a
        )



    // BEHAVIOR ITEM
    type Actor = NounClause
    type Trigger=VerbClause
    type BusinessContext=VerbNounClause
    // BEHAVIOR ITEM-RELATIONSHIP
    type BehaviorItemRelationship =
        {
            ItemAnnotation:ItemAnnotation list
            NodeType:string
            NodeLHS:string
            NodeRHS:int list
        }

    // STRUCTURE
    type Attribute =
        {
            Title:NounClause
        }
    type Entity =
        {
            Title:NounClause
            Attributes:Attribute list
            Connections:(int*int) list
            AffectedBySupplementals:int list
        }


    type Labels=
        {
            ModelItems:ModelItem list
        }


    // HYPOTHESIS
    type Hypothesis =
        {
            Genre:Genres
            ShortName:string
            Observations:string list
            ExpectedActorsImpacted:Actor list
            ExpectedImpactMetricGiven:string list
            ExpectedSuccessfulResult:string list
            ProposedChangesToTestHypothesis:ModelItem list
            ExpectedExperimentSuspenseTime:string
        }
    let defaultHypothesis = 
        {
            Genre=Genres.Business
            ShortName=""
            Observations=[]
            ExpectedActorsImpacted=[]
            ExpectedImpactMetricGiven=[]
            ExpectedSuccessfulResult=[]
            ProposedChangesToTestHypothesis=[]
            ExpectedExperimentSuspenseTime=""
        }




// MIXED TYPES
    type SVGEntityBox =
        {
            xPos:int
            yPos:int
            width:int
            height:int
            Entity:Entity
        }




      type VertexData<'V> =
        int (* identifier *) *
        'V (* vertex data *)

      type EdgeData<'E> =
        int (* identifier *) *
        int (* priority *) *
        int (* vertex target *) *
        'E (* edge data *)

      (* The graph uses adjacency list notation *)
      type Adjacency<'E> = EdgeData<'E> list

      (* The Vertex type represents the internal structure
         of the graph *)
      type Vertex<'V, 'E> = VertexData<'V> * Adjacency<'E>

      (* A Graph is a Vertex list.  The nextNode allows for
         consistent addressing of nodes *)
      type Graph<'V, 'E> =
        int (* nextNode identifier *) *
        Vertex<'V, 'E> list

      (* Empty graph construction *)
      let empty: Graph<_,_> = (0, [])

      (* Helper methods for getting the data from a Vertex *)
      let vertexId (v:Vertex<_,_>) = v |> fst |> fst
      let vertexData (v:Vertex<_,_>) = v |> fst |> snd
      (* Helper methods for getting the data from an Edge *)
      let edgeId ((x,_,_,_):EdgeData<_>) = x
      let edgePriority ((_,x,_,_):EdgeData<_>) = x
      let edgeTarget ((_,_,x,_):EdgeData<_>) = x
      let edgeData ((_,_,_,x):EdgeData<_>) = x

      (* Getting a vertex from a graph by id *)
      let getVertex v (g:Graph<_, _>) : Vertex<_,_> =
        snd g |> List.find (fun V -> vertexId V = v)
      (* Getting all edges from a graph by a vertex id *)
      let getEdges v (g:Graph<_, _>) =
        g |> getVertex v |> snd

      (* Add a new vertex *)
      let addVertex (v:'V) (g:Graph<'V, _>)
        : (int*Graph<'V,_>) =
          let id = fst g
          let s = snd g
          let newVD : VertexData<_> = (id, v)
          let newA : Adjacency<_> = []
          let newV = (newVD, newA)
          (id, (id + 1, newV::s))

      (* Add a new edge.  Edges include a priority value *)
      let addEdge priority
        (v:int) (v':int) (e:'E) (g:Graph<'V, 'E>)
        : (int*Graph<'V,'E>) =
          let id = fst g
          let s = snd g
          let newE : EdgeData<_> = (id, priority, v', e)
          (id,
            (id + 1,
              s |> List.map (fun V ->
                if (vertexId V) = v then
                  (fst V, newE::(snd V))
                else V)))

      (* The edges aren't sorted by default so this function
         sorts them by priority *)
      let sortEdges (a:Adjacency<_>) =
        a |> List.sortBy edgePriority

      (* Removes an edge from a graph by id *)
      let removeEdge (id:int) (g:Graph<_,_>)
        : Graph<_,_> =
          let next = fst g
          let s = snd g
          (next, s |> List.map ( fun (v, a) ->
            (v, a |> 
                List.filter (fun x -> (edgeId x) <> id)))) 

      (* Removes a vertex from a graph by id and removes
         any related edges *)
      let removeVertex (id:int) (g:Graph<_,_>) 
        : Graph<_,_> =
          let next = fst g
          let s = snd g
          (next, s |> ([] |> List.fold (fun s' (v, a) ->
            if (fst v) = id then s'
            else
              let f = fun x -> ((edgeTarget x) <> id)
              let newA = a |> List.filter f
              let newV = (v, newA)
              newV::s')))        