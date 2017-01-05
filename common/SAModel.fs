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
        | Meta
         static member ToList() =
            [Unknown;None;Behavior;Structure;Supplemental;Meta]
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | None->"None"
            | Behavior->"Behavior"
            | Structure->"Structure"
            | Supplemental->"Supplemental"
            | Meta->"Meta"
    type Genres =
        | Unknown
        | None
        | Business
        | System
         static member ToList() =
            [Unknown;None;Business; System]
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | None->"None"
            | Business->"Business"
            | System->"System"

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
        | AsIs
        | ToBe
         static member ToList() =
            [Unknown;AsIs;ToBe]
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | None->"None"
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
    type incomingLine = {FileNumber:int;FileInfo:System.IO.FileInfo; LineNumber:int; LineText:string}
    type ModelItemType =
        | None
        | Diagram
        | Connection                    // joins up Items
        | Label                         // arbitrary labels for items
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
            | Connection->"Connection"
            | Label->"Label"
            | ModelItem({
                            Id=a
                            Parent=b
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
            Parent:int option
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
    and ProcessContext = {ContextStack:System.Collections.Generic.Stack<ModelItem>; Lines:ModelItem list}
    let defaultModelItem = 
        {
            Id=0
            Parent=option.None
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
        {ContextStack=newContextStack; Lines=[]}

//    let makeEntireLineIntoAnItemModelItem (incomingLineToProcess:incomingLine) (defaultContext:ProcessContext) =
//        let contextParent = defaultContext.ContextStack.Peek()
//        {
//            Id=getNextItemNumber()
//            Parent=Some (contextParent.Id)
//            ItemType=ModelItemType.ModelItem(
//                                            {
//                                                Id=getNextItemNumber()
//                                                Parent=option.None
//                                                ItemType=ModelItemType.None
//                                                Bucket=Buckets.None
//                                                Genre=Genres.None
//                                                AbstractionLevel=AbstractionLevels.None
//                                                TemporalIndicator=TemporalIndicators.None
//                                                ItemAnnotation={Notes=[];Questions=[];ToDos=[];WorkHistory=[]}
//                                                ModelItemName=incomingLineToProcess.LineText
//                                                SourceReferences=
//                                                [{
//                                                    File=incomingLineToProcess.FileInfo; 
//                                                    LineNumber=incomingLineToProcess.LineNumber
//                                                 }]
//                                            }
//                                        )
//            Bucket=contextParent.Bucket
//            Genre=contextParent.Genre
//            AbstractionLevel=contextParent.AbstractionLevel
//            TemporalIndicator=contextParent.TemporalIndicator
//            ItemAnnotation={Notes=[];Questions=[];ToDos=[];WorkHistory=[]}
//            SourceReferences=[{File=incomingLineToProcess.FileInfo; LineNumber=incomingLineToProcess.LineNumber}]
//            ModelItemName=incomingLineToProcess.LineText
//        }
//    let makeEntireLineIntoAnCommentModelItem (incomingLineToProcess:incomingLine) (defaultContext:ProcessContext) =
//        {
//            Id=getNextItemNumber()
//            Parent=Some (defaultContext.ContextStack.Peek().Id)
//            ItemType=ModelItemType.Note({Text=incomingLineToProcess.LineText; SourceReference={File=incomingLineToProcess.FileInfo; LineNumber=incomingLineToProcess.LineNumber}})
//            Bucket=Buckets.None
//            Genre=Genres.None
//            AbstractionLevel=AbstractionLevels.None
//            TemporalIndicator=TemporalIndicators.None
//            ItemAnnotation={Notes=[];Questions=[];ToDos=[];WorkHistory=[]}
//            SourceReferences=[{File=incomingLineToProcess.FileInfo; LineNumber=incomingLineToProcess.LineNumber}]
//            ModelItemName=""
//        }

    let smashTwoModelItems (existingModelItem:ModelItem) (incomingModelItem:ModelItem) =
            let newId=incomingModelItem.Id
            let newParent=if incomingModelItem.Parent.IsSome then incomingModelItem.Parent else existingModelItem.Parent
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
                Parent=newParent
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


    type EasyAMToken = Token<ModelItem, ProcessContext>
    let easyAMTokens:EasyAMToken[] = 
        [|
            {
                SearchDirection=FindLastMatch
                RegexMatch="//.*$"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let incomingModelItem=currentContext.ContextStack.Peek()
                                        let commentValue=tokenMatchText.Substring(2).Trim()
                                        let newSourceReference={File=lineBeingProcessed.FileInfo;LineNumber=lineBeingProcessed.LineNumber}
                                        let newlyCreatedModelItem =
                                            {
                                            defaultModelItem with
                                                Id=getNextItemNumber()
                                                Parent=Some incomingModelItem.Id
                                                ItemType=Note({Text=commentValue; SourceReference=newSourceReference})
                                                SourceReferences=[newSourceReference]
                                            }
                                        let newModelItemUpdatedWithContext = smashTwoModelItems incomingModelItem newlyCreatedModelItem
                                        let newLines= [newModelItemUpdatedWithContext] |> List.append currentContext.Lines
                                        {currentContext with Lines=newLines}
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="NOTE:[^&|^:]*" //+ TokenSeparatorRegex
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let incomingModelItem=currentContext.ContextStack.Peek()
                                        let commentValue=tokenMatchText.Substring(5).Trim()
                                        let newSourceReference={File=lineBeingProcessed.FileInfo;LineNumber=lineBeingProcessed.LineNumber}
                                        let newlyCreatedModelItem =
                                            {
                                            defaultModelItem with
                                                Id=getNextItemNumber()
                                                ItemType=Note({Text=commentValue; SourceReference=newSourceReference})
                                                SourceReferences=[newSourceReference]
                                            }
                                        let newModelItemUpdatedWithContext = smashTwoModelItems incomingModelItem newlyCreatedModelItem
                                        let newLines= [newModelItemUpdatedWithContext] |> List.append currentContext.Lines
                                        {currentContext with Lines=newLines}
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="TODO:[^&|^:]*" //+ TokenSeparatorRegex
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let incomingModelItem=currentContext.ContextStack.Peek()
                                        let commentValue=tokenMatchText.Substring(5).Trim()
                                        let newSourceReference={File=lineBeingProcessed.FileInfo;LineNumber=lineBeingProcessed.LineNumber}
                                        let newlyCreatedModelItem =
                                            {
                                            defaultModelItem with
                                                Id=getNextItemNumber()
                                                ItemType=ToDo({Text=commentValue; SourceReference=newSourceReference})
                                                SourceReferences=[newSourceReference]
                                            }
                                        let newModelItemUpdatedWithContext = smashTwoModelItems incomingModelItem newlyCreatedModelItem
                                        let newLines= [newModelItemUpdatedWithContext] |> List.append currentContext.Lines
                                        {currentContext with Lines=newLines}
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="WORK:[^&|^:]*" //+ TokenSeparatorRegex
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let incomingModelItem=currentContext.ContextStack.Peek()
                                        let commentValue=tokenMatchText.Substring(5).Trim()
                                        let newSourceReference={File=lineBeingProcessed.FileInfo;LineNumber=lineBeingProcessed.LineNumber}
                                        let newlyCreatedModelItem =
                                            {
                                            defaultModelItem with
                                                Id=getNextItemNumber()
                                                ItemType=Work({Text=commentValue; SourceReference=newSourceReference})
                                                SourceReferences=[newSourceReference]
                                            }
                                        let newModelItemUpdatedWithContext = smashTwoModelItems incomingModelItem newlyCreatedModelItem
                                        let newLines= [newModelItemUpdatedWithContext] |> List.append currentContext.Lines
                                        {currentContext with Lines=newLines}
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="MASTER DOMAIN MODEL"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let incomingModelItem=currentContext.ContextStack.Peek()
                                        let contextShiftValue=(tokenMatchText.GetLeft 19).Trim()
                                        let newSourceReference={File=lineBeingProcessed.FileInfo;LineNumber=lineBeingProcessed.LineNumber}
                                        let newlyCreatedModelItem =
                                            {
                                            defaultModelItem with
                                                Id=getNextItemNumber()
                                                ItemType=ContextShift({Text=contextShiftValue; SourceReference=newSourceReference})
                                                Genre=Genres.Business
                                                Bucket=Buckets.Structure
                                                AbstractionLevel=AbstractionLevels.Abstract
                                                SourceReferences=[newSourceReference]
                                            }
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        let newLines= [newlyCreatedModelItem] |> List.append currentContext.Lines
                                        {currentContext with Lines=newLines}
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="MASTER BACKLOG"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let incomingModelItem=currentContext.ContextStack.Peek()
                                        let contextShiftValue=(tokenMatchText.GetLeft 14).Trim()
                                        let newSourceReference={File=lineBeingProcessed.FileInfo;LineNumber=lineBeingProcessed.LineNumber}
                                        let newlyCreatedModelItem =
                                            {
                                            defaultModelItem with
                                                Id=getNextItemNumber()
                                                ItemType=ContextShift({Text=contextShiftValue; SourceReference=newSourceReference})
                                                Genre=Genres.Business
                                                Bucket=Buckets.Behavior
                                                AbstractionLevel=AbstractionLevels.Abstract
                                                SourceReferences=[newSourceReference]
                                            }
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        let newLines= [newlyCreatedModelItem] |> List.append currentContext.Lines
                                        {currentContext with Lines=newLines}
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="MASTER SUPPLEMENTAL MODEL"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let incomingModelItem=currentContext.ContextStack.Peek()
                                        let contextShiftValue=(tokenMatchText.GetLeft 25).Trim()
                                        let newSourceReference={File=lineBeingProcessed.FileInfo;LineNumber=lineBeingProcessed.LineNumber}
                                        let newlyCreatedModelItem =
                                            {
                                            defaultModelItem with
                                                Id=getNextItemNumber()
                                                ItemType=ContextShift({Text=contextShiftValue; SourceReference=newSourceReference})
                                                Genre=Genres.Business
                                                Bucket=Buckets.Supplemental
                                                AbstractionLevel=AbstractionLevels.Abstract
                                                SourceReferences=[newSourceReference]
                                            }
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        let newLines= [newlyCreatedModelItem] |> List.append currentContext.Lines
                                        {currentContext with Lines=newLines}
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="PRODUCT BACKLOG"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let incomingModelItem=currentContext.ContextStack.Peek()
                                        let contextShiftValue=(tokenMatchText.GetLeft 15).Trim()
                                        let newSourceReference={File=lineBeingProcessed.FileInfo;LineNumber=lineBeingProcessed.LineNumber}
                                        let newlyCreatedModelItem =
                                            {
                                            defaultModelItem with
                                                Id=getNextItemNumber()
                                                ItemType=ContextShift({Text=contextShiftValue; SourceReference=newSourceReference})
                                                Genre=Genres.Business
                                                Bucket=Buckets.Behavior
                                                AbstractionLevel=AbstractionLevels.Realized
                                                SourceReferences=[newSourceReference]
                                            }
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        let newLines= [newlyCreatedModelItem] |> List.append currentContext.Lines
                                        {currentContext with Lines=newLines}
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="SPRINT BACKLOG"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let incomingModelItem=currentContext.ContextStack.Peek()
                                        let contextShiftValue=(tokenMatchText.GetLeft 15).Trim()
                                        let newSourceReference={File=lineBeingProcessed.FileInfo;LineNumber=lineBeingProcessed.LineNumber}
                                        let newlyCreatedModelItem =
                                            {
                                            defaultModelItem with
                                                Id=getNextItemNumber()
                                                ItemType=ContextShift({Text=contextShiftValue; SourceReference=newSourceReference})
                                                Genre=Genres.Business
                                                Bucket=Buckets.Behavior
                                                AbstractionLevel=AbstractionLevels.Realized
                                                SourceReferences=[newSourceReference]
                                            }
                                        currentContext.ContextStack.Push newlyCreatedModelItem
                                        let newLines= [newlyCreatedModelItem] |> List.append currentContext.Lines
                                        {currentContext with Lines=newLines}
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="NOTES"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let incomingModelItem=currentContext.ContextStack.Peek()
                                        let contextShiftValue=(tokenMatchText.GetLeft 5).Trim()
                                        let newSourceReference={File=lineBeingProcessed.FileInfo;LineNumber=lineBeingProcessed.LineNumber}
                                        let newlyCreatedModelItem =
                                            {
                                            defaultModelItem with
                                                Id=getNextItemNumber()
                                                ItemType=ContextShift({Text=contextShiftValue; SourceReference=newSourceReference})
                                                Genre=Genres.None
                                                Bucket=Buckets.None
                                                AbstractionLevel=AbstractionLevels.None
                                                SourceReferences=[newSourceReference]
                                            }
                                        currentContext.ContextStack.Clear()
                                        currentContext.ContextStack.Push(defaultModelItem)
                                        let newLines= [newlyCreatedModelItem] |> List.append currentContext.Lines
                                        {currentContext with Lines=newLines}
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="Q:.*"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let incomingModelItem=currentContext.ContextStack.Peek()
                                        let questionValue=tokenMatchText.Substring(2).Trim()
                                        let newSourceReference={File=lineBeingProcessed.FileInfo;LineNumber=lineBeingProcessed.LineNumber}
                                        let newlyCreatedModelItem =
                                            {
                                            defaultModelItem with
                                                Id=getNextItemNumber()
                                                ItemType=Question({Text=questionValue; SourceReference=newSourceReference})
                                                SourceReferences=[newSourceReference]
                                            }
                                        let newModelItemUpdatedWithContext = smashTwoModelItems incomingModelItem newlyCreatedModelItem
                                        let newLines= [newModelItemUpdatedWithContext] |> List.append currentContext.Lines
                                        {currentContext with Lines=newLines}
                                    )
            };
            { // This should be near (next?) to last. Catch any name/value pairs on the line
                SearchDirection=FindFirstMatchExactTokenUsage
                RegexMatch="&[^&|^ |^:]*"
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let incomingModelItem=currentContext.ContextStack.Peek()
                                        let nameText,valueText =
                                            if tokenMatchText.Contains("=")
                                                then
                                                    let equalsSplit=tokenMatchText.IndexOf("=")
                                                    tokenMatchText.Substring(1, equalsSplit-1), tokenMatchText.Substring(equalsSplit+1)
                                                else
                                                    tokenMatchText,""
                                        let newSourceReference={File=lineBeingProcessed.FileInfo;LineNumber=lineBeingProcessed.LineNumber}
                                        let newlyCreatedModelItem =
                                            {
                                            defaultModelItem with
                                                Id=getNextItemNumber()
                                                ItemType=NameValueTag({Name=nameText; Value=valueText; SourceReference=newSourceReference})
                                                SourceReferences=[newSourceReference]
                                            }
                                        // smash item with most recent item on list to give it context. Also make it child
                                        let smashedModelItem = 
                                            if currentContext.Lines.Length>0
                                                then
                                                    let parentItem = currentContext.Lines.[currentContext.Lines.Length-1]
                                                    {(smashTwoModelItems parentItem newlyCreatedModelItem) with Parent=Some parentItem.Id}
                                                else
                                                    newlyCreatedModelItem
                                        let newLines= [smashedModelItem] |> List.append currentContext.Lines
                                        {currentContext with Lines=newLines}
                                    )
            };
            {
                SearchDirection=FindFirstMatch
                RegexMatch="^.*$"  // This runs last. It is the catch-all
                MakeNewModelItemAndUpdateStack=(fun(lineWithTokenConsumed:string, tokenMatchText:string, currentContext:ProcessContext, lineBeingProcessed)->
                                        let newSourceReference={File=lineBeingProcessed.FileInfo;LineNumber=lineBeingProcessed.LineNumber}
                                        let incomingModelItem=currentContext.ContextStack.Peek()
                                        let matchText=tokenMatchText.Trim()
                                        // something there, but notincomingModelItemhing interesting. Either a comment or an item
                                        if incomingModelItem.HasSomeContextToIt
                                            then //it's an item
                                                let newItem = 
                                                    {
                                                        Id=getNextItemNumber()
                                                        Parent=Some (incomingModelItem.Id)
                                                        ItemType=ModelItemType.ModelItem(
                                                                                        {
                                                                                            Id=0
                                                                                            Parent=option.None
                                                                                            ItemType=ModelItemType.None
                                                                                            Bucket=Buckets.None
                                                                                            Genre=Genres.None
                                                                                            AbstractionLevel=AbstractionLevels.None
                                                                                            TemporalIndicator=TemporalIndicators.None
                                                                                            ItemAnnotation={Notes=[];Questions=[];ToDos=[];WorkHistory=[]}
                                                                                            ModelItemName=lineBeingProcessed.LineText.Trim()
                                                                                            SourceReferences=[newSourceReference]
                                                                                        }
                                                                                    )
                                                        Bucket=incomingModelItem.Bucket
                                                        Genre=incomingModelItem.Genre
                                                        AbstractionLevel=incomingModelItem.AbstractionLevel
                                                        TemporalIndicator=incomingModelItem.TemporalIndicator
                                                        ItemAnnotation={Notes=[];Questions=[];ToDos=[];WorkHistory=[]}
                                                        SourceReferences=[newSourceReference]
                                                        ModelItemName=lineBeingProcessed.LineText.Trim()
                                                    }
                                                let newLines= [newItem] |> List.append currentContext.Lines
                                                {currentContext with Lines=newLines}
                                            else //it's a comment
                                                let newItem = 
                                                    {
                                                        Id=getNextItemNumber()
                                                        Parent=Some (incomingModelItem.Id)
                                                        ItemType=ModelItemType.Note({Text=lineBeingProcessed.LineText; SourceReference=newSourceReference})
                                                        Bucket=Buckets.None
                                                        Genre=Genres.None
                                                        AbstractionLevel=AbstractionLevels.None
                                                        TemporalIndicator=TemporalIndicators.None
                                                        ItemAnnotation={Notes=[];Questions=[];ToDos=[];WorkHistory=[]}
                                                        SourceReferences=[newSourceReference]
                                                        ModelItemName=""
                                                    }
                                                let newLines= [newItem] |> List.append currentContext.Lines
                                                {currentContext with Lines=newLines}

                                    )
            }
        |]


    type LanguageTokenMatchType =
        | NoMatch
        | MatchLineContinues
        | MatchLineEnds
        | MatchWithColonBeginLabel
    type LanguageToken =
        {
            TokenText:string
            ExampleItem:ModelItem
        }
    let LanguageTokens = 
        [|
            { TokenText="BUSINESS";                     ExampleItem={defaultModelItem with Genre=Genres.Business}};
            { TokenText="SYSTEM";                       ExampleItem={defaultModelItem with Genre=Genres.System}};
            { TokenText="STRUCTURE";                    ExampleItem={defaultModelItem with Bucket=Buckets.Structure}};
            { TokenText="BEHAVIOR";                     ExampleItem={defaultModelItem with Bucket=Buckets.Behavior}};
            { TokenText="SUPPLEMENTAL";                 ExampleItem={defaultModelItem with Bucket=Buckets.Supplemental}};
            { TokenText="META";                         ExampleItem={defaultModelItem with Bucket=Buckets.Meta}};
            { TokenText="ABSTRACT";                     ExampleItem={defaultModelItem with AbstractionLevel=AbstractionLevels.Abstract}};
            { TokenText="REALIZED";                     ExampleItem={defaultModelItem with AbstractionLevel=AbstractionLevels.Realized}};
            { TokenText="TO-BE";                        ExampleItem={defaultModelItem with TemporalIndicator=TemporalIndicators.ToBe}};
            { TokenText="AS-IS";                        ExampleItem={defaultModelItem with TemporalIndicator=TemporalIndicators.AsIs}};
            // Some combo shortcuts
            { TokenText="MASTER BACKLOG";               ExampleItem={defaultModelItem with TemporalIndicator=TemporalIndicators.ToBe; Genre=Genres.Business; Bucket=Buckets.Behavior; AbstractionLevel=AbstractionLevels.Abstract}};
            { TokenText="PRODUCT BACKLOG";              ExampleItem={defaultModelItem with TemporalIndicator=TemporalIndicators.ToBe; Genre=Genres.Business; Bucket=Buckets.Behavior; AbstractionLevel=AbstractionLevels.Realized}};
            { TokenText="SPRINT BACKLOG";               ExampleItem={defaultModelItem with TemporalIndicator=TemporalIndicators.ToBe; Genre=Genres.Business; Bucket=Buckets.Behavior; AbstractionLevel=AbstractionLevels.Realized}};
            { TokenText="MASTER DOMAIN MODEL";          ExampleItem={defaultModelItem with TemporalIndicator=TemporalIndicators.ToBe; Genre=Genres.Business; Bucket=Buckets.Structure; AbstractionLevel=AbstractionLevels.Abstract}};
            { TokenText="MASTER SUPPLEMENTAL MODEL";    ExampleItem={defaultModelItem with TemporalIndicator=TemporalIndicators.ToBe; Genre=Genres.Business; Bucket=Buckets.Supplemental; AbstractionLevel=AbstractionLevels.Abstract}};
            { TokenText="WORK";                         ExampleItem={defaultModelItem with TemporalIndicator=TemporalIndicators.AsIs; Genre=Genres.Business; Bucket=Buckets.Meta; AbstractionLevel=AbstractionLevels.Realized}};
            { TokenText="Q ";                           ExampleItem=defaultModelItem};
            { TokenText="TODO ";                        ExampleItem=defaultModelItem};
            { TokenText="NOTE ";                        ExampleItem=defaultModelItem}
        |]
    let languageTokenFirstMatch (sText:string) =
        LanguageTokens |> Array.fold(fun (acc:(LanguageTokenMatchType*LanguageToken*int) option) x->
            if sText.Contains(x.TokenText)
                then
                    if acc.IsSome
                        then acc 
                        else
                            let matchType =
                                if sText.ContainsRegex(x.TokenText + "\s+")
                                    then LanguageTokenMatchType.MatchLineContinues
                                    elif sText.ContainsRegex(x.TokenText + "$") then LanguageTokenMatchType.MatchLineEnds
                                    elif sText.ContainsRegex(x.TokenText + ":") then LanguageTokenMatchType.MatchWithColonBeginLabel
                                    else LanguageTokenMatchType.NoMatch
                            if matchType<>LanguageTokenMatchType.NoMatch then Some(matchType, x, sText.IndexOf(x.TokenText)) else acc
                else
                    acc
            ) FSharp.Core.option<LanguageTokenMatchType*LanguageToken*int>.None
    let languageTokenMatches (sText:string) =
        LanguageTokens |> Array.fold(fun (acc:(LanguageTokenMatchType*LanguageToken*int) option list) x->
            if sText.Contains(x.TokenText)
                then
                    let newItem =
                        let matchType =
                            if sText.ContainsRegex(x.TokenText + "\s+")
                                then LanguageTokenMatchType.MatchLineContinues
                                elif sText.ContainsRegex(x.TokenText + "$") then LanguageTokenMatchType.MatchLineEnds
                                elif sText.ContainsRegex(x.TokenText + ":") then LanguageTokenMatchType.MatchWithColonBeginLabel
                                else LanguageTokenMatchType.NoMatch
                        if matchType<>LanguageTokenMatchType.NoMatch
                            then Some(matchType, x, sText.IndexOf(x.TokenText)) 
                            else option<LanguageTokenMatchType*LanguageToken*int>.None
                    if newItem.IsSome then List.append acc [newItem] else acc
                else
                    acc
            ) []
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