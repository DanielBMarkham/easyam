module Main//
    open Types
    open SAModel
    open Utils
    open Persist
    open FParsec
    open GemBox.Spreadsheet

    let defaultBaseOptions = createNewBaseOptions "easyAM" "Compile the Analysis Model." [|"Takes tagged statements created with Structural Analysis, cross-checks and compiles."|] defaultVerbosity
    let defaulSourceDirectory = createNewConfigEntry "S" "Source Directory (Optional)" [|"/S:<path> -> path to the directory having the source files."|] (System.AppDomain.CurrentDomain.BaseDirectory, Some(System.IO.DirectoryInfo(System.AppDomain.CurrentDomain.BaseDirectory)))
    let defaultDestinationDirectory = createNewConfigEntry "D" "Destination Directory (Optional)" [|"/D:<path> -> path to the directory where compiled files will be deployed."|] (System.AppDomain.CurrentDomain.BaseDirectory, Some(System.IO.DirectoryInfo(System.AppDomain.CurrentDomain.BaseDirectory)))
    

    let loadConfigFromCommandLine (args:string []):EasyAMProgramConfig =
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosity, args)
        let newSourceDirectory = ConfigEntry<_>.populateValueFromCommandLine(defaulSourceDirectory, args)
        let newDestinationDirectory = ConfigEntry<_>.populateValueFromCommandLine(defaultDestinationDirectory, args)
        let newConfigBase = {defaultBaseOptions with verbose=newVerbosity}
        { 
            configBase = newConfigBase
            sourceDirectory=newSourceDirectory
            destinationDirectory=newDestinationDirectory
        }
    let getDirectories (opts:EasyAMProgramConfig) = 
        // set up any folders needed by the tool
        let sourceDirectoryInfo = forceDirectoryCreation opts.sourceDirectory
        let destinationDirectoryInfo = forceDirectoryCreation opts.destinationDirectory

        let BehaviorDirectoryInfo = getOrMakeDirectory (destinationDirectoryInfo.FullName + System.IO.Path.DirectorySeparatorChar.ToString() + "Behavior")
        let StructureDirectoryInfo = getOrMakeDirectory (destinationDirectoryInfo.FullName + System.IO.Path.DirectorySeparatorChar.ToString() + "Structure")
        let SupplementalDirectoryInfo = getOrMakeDirectory (destinationDirectoryInfo.FullName + System.IO.Path.DirectorySeparatorChar.ToString() + "Supplemental")
        let MetaDirectoryInfo = getOrMakeDirectory (destinationDirectoryInfo.FullName + System.IO.Path.DirectorySeparatorChar.ToString() + "Meta")
        {
            SourceDirectoryInfo=sourceDirectoryInfo
            DestinationDirectoryInfo=destinationDirectoryInfo
            BehaviorDirectoryInfo=BehaviorDirectoryInfo
            StructureDirectoryInfo=StructureDirectoryInfo
            SupplementalDirectoryInfo=SupplementalDirectoryInfo
            MetaDirectoryInfo=MetaDirectoryInfo
        }
    let allCardinalNumbers = {1..10000}

    let loadInAllIncomingLines (fileList:System.IO.FileInfo[]) =
        let lineDumpForAllFiles = fileList |> Array.mapi(fun i x->
            System.IO.File.ReadAllLines(x.FullName) |> Array.mapi(fun j y->
                {FileNumber=i;FileInfo=x; LineNumber=j; LineText=y}
                )
            )
        lineDumpForAllFiles |> Array.concat
    
//    let smashTwoModelItems (existingModelItem:ModelItem) (incomingModelItem:ModelItem) =
//            let newId=incomingModelItem.Id
//            let newParent=if incomingModelItem.Parent.IsSome then incomingModelItem.Parent else existingModelItem.Parent
//            let newItemType=if ((incomingModelItem.ItemType<>ModelItemType.None)) then incomingModelItem.ItemType else existingModelItem.ItemType
//            let newBucket=if ((incomingModelItem.Bucket<>Buckets.Unknown) && (incomingModelItem.Bucket<>Buckets.None)) then incomingModelItem.Bucket else existingModelItem.Bucket
//            let newGenre=if ((incomingModelItem.Genre<>Genres.Unknown) && (incomingModelItem.Genre<>Genres.None)) then incomingModelItem.Genre else existingModelItem.Genre
//            let newAbstractionLevel=if ((incomingModelItem.AbstractionLevel<>AbstractionLevels.Unknown) && (incomingModelItem.AbstractionLevel<>AbstractionLevels.None)) then incomingModelItem.AbstractionLevel else existingModelItem.AbstractionLevel
//            let newTemporalIndicator=if ((incomingModelItem.TemporalIndicator<>TemporalIndicators.Unknown) && (incomingModelItem.TemporalIndicator<>TemporalIndicators.None)) then incomingModelItem.TemporalIndicator else existingModelItem.TemporalIndicator
//            let newItemAnnotation=incomingModelItem.ItemAnnotation
//            let newSourceReferences=incomingModelItem.SourceReferences
//            let newShortName=incomingModelItem.ShortName
//            {
//                Id=newId
//                Parent=newParent
//                ItemType=newItemType
//                Bucket=newBucket
//                Genre=newGenre
//                AbstractionLevel=newAbstractionLevel
//                TemporalIndicator=newTemporalIndicator
//                ItemAnnotation=newItemAnnotation
//                SourceReferences=newSourceReferences
//                ShortName=newShortName
//            }

//    let smashTokensWithModelItem (tokens:(LanguageTokenMatchType*LanguageToken*int) option list) (modelItem:ModelItem option) (z:incomingLine):ModelItem =
//        let startingModelItem = if modelItem.IsSome then modelItem.Value else defaultModelItem
//        let newModelItem = tokens |> List.fold(fun (acc:ModelItem) x->
//            if x.IsSome
//                then
//                    let tokenMatchType,matchedToken,c=x.Value
//                    let xVal=
//                        matchedToken
//                    let newItemType=
//                        match matchedToken.TokenText with
//                            | "Q:"->
//                                ModelItemType.Question({Text=z.LineText; SourceReference={File=z.FileInfo;LineNumber=z.LineNumber}})
//                            | "NOTE:"->
//                                ModelItemType.Note({Text=z.LineText; SourceReference={File=z.FileInfo;LineNumber=z.LineNumber}})
//                            | "TODO:"->
//                                ModelItemType.TODO({Text=z.LineText; SourceReference={File=z.FileInfo;LineNumber=z.LineNumber}})
//                            |_->
//                                if matchedToken.ExampleItem.ItemType=ModelItemType.None 
//                                    then acc.ItemType 
//                                    else matchedToken.ExampleItem.ItemType
//                    let newBucket=if ( (matchedToken.ExampleItem.Bucket=Buckets.None) || (matchedToken.ExampleItem.Bucket=Buckets.Unknown) ) then acc.Bucket else matchedToken.ExampleItem.Bucket
//                    let newGenre=if ( (matchedToken.ExampleItem.Genre=Genres.None) || (matchedToken.ExampleItem.Genre=Genres.Unknown)) then acc.Genre else matchedToken.ExampleItem.Genre
//                    let newAbstractionLevel=if ( (matchedToken.ExampleItem.AbstractionLevel=AbstractionLevels.None) || (matchedToken.ExampleItem.AbstractionLevel=AbstractionLevels.Unknown)) then acc.AbstractionLevel else matchedToken.ExampleItem.AbstractionLevel
//                    let newTemporalIndicator=if ( (matchedToken.ExampleItem.TemporalIndicator=TemporalIndicators.None) || (matchedToken.ExampleItem.TemporalIndicator=TemporalIndicators.Unknown)) then acc.TemporalIndicator else matchedToken.ExampleItem.TemporalIndicator
//                    { acc with
//                        ItemType=newItemType
//                        Bucket=newBucket
//                        Genre=newGenre
//                        AbstractionLevel=newAbstractionLevel
//                        TemporalIndicator=newTemporalIndicator
//                    }
//                else
//                            acc
//                            ) startingModelItem
//        newModelItem
    let removeNameValueTags (lineWithOnlyLabelRemaining:string) (currentContext:ModelItem) (incomingLine:incomingLine) =
        if lineWithOnlyLabelRemaining.Contains("&")
            then
                let firstDelimiter=lineWithOnlyLabelRemaining.IndexOf("&")
                let newLineWithOnlyLabelRemaining=lineWithOnlyLabelRemaining.GetLeft(firstDelimiter-1)
                let delimterSection = lineWithOnlyLabelRemaining.Substring(firstDelimiter+1)
                let delimiterSections=delimterSection.Split([|"&"|], System.StringSplitOptions.None)
                let newModelItems = delimiterSections |> Array.fold(fun acc x->
                                    let splitByEquals=x.Split([|"="|], System.StringSplitOptions.None)
                                    let lhs, rhs = if splitByEquals.Length>1 then splitByEquals.[0],splitByEquals.[1] else "",""
                                    List.append acc [{currentContext with Id=(Seq.take 1 allCardinalNumbers |> Seq.toArray).[0]; ModelItemName=x; ItemType=NameValueTag({ Name=lhs; Value=rhs; SourceReference={File=incomingLine.FileInfo; LineNumber=incomingLine.LineNumber}})}]
                                    ) []
                newLineWithOnlyLabelRemaining, newModelItems
            else
                lineWithOnlyLabelRemaining, []

    let processIncomingLines incomingLines =        
        let whiteSpaceRegex=new System.Text.RegularExpressions.Regex("^\s+")
        let compiledContext = 
            incomingLines |> Array.fold(fun lineProcessorAccumulator incomingLineBeingProcessed->
            let lastItemProcessed=
                if lineProcessorAccumulator.Lines.Length>0 
                    then
                        lineProcessorAccumulator.Lines.Item(lineProcessorAccumulator.Lines.Length-1) 
                    else 
                        let newSourceReferences=[{File=incomingLineBeingProcessed.FileInfo; LineNumber=incomingLineBeingProcessed.LineNumber}]
                        {defaultModelItem with SourceReferences=newSourceReferences}
            let lastSourceReferenceForLastItemProcessed=if lineProcessorAccumulator.Lines.Length>0 then {File=incomingLineBeingProcessed.FileInfo;LineNumber=incomingLineBeingProcessed.LineNumber} else lastItemProcessed.SourceReferences.Item(lastItemProcessed.SourceReferences.Length-1)
            // if there's a new file from the last time, we zap the stack and start over with context
            let newacc = 
                if ( (lineProcessorAccumulator<>defaultProcessContext) && (lastSourceReferenceForLastItemProcessed.File.FullName<>incomingLineBeingProcessed.FileInfo.FullName))
                    then
                        defaultProcessContext
                    else
                        lineProcessorAccumulator
            let indentLevel, lineWithoutLeadingSpaces = 
                let whiteSpaceMatches = whiteSpaceRegex.Matches(incomingLineBeingProcessed.LineText).toArray
                if whiteSpaceMatches.Length>0
                    then
                        let leadingWhiteSpace = whiteSpaceMatches.[0].Value
                        let tabCount = leadingWhiteSpace.CountOccurences("\t")
                        let spaceCount = leadingWhiteSpace.CountOccurences(" ")
                        ((tabCount + (spaceCount/4)),incomingLineBeingProcessed.LineText.Substring(leadingWhiteSpace.Length))
                    else (0,incomingLineBeingProcessed.LineText)
            let newContext = easyAMTokens |> Array.fold(fun tokenProcessingAccumulator currentTokenWeAreLookingAt->
                                let (incomingLineToParse:string,incomingContext)=tokenProcessingAccumulator
                                match (incomingLineToParse.Length>0),(currentTokenWeAreLookingAt.IsMatch incomingLineToParse) with
                                    | true, true->
                                        let tokenMatchText, lineWithTokenConsumed =currentTokenWeAreLookingAt.ConsumeToken incomingLineToParse
                                        let updatedContext = currentTokenWeAreLookingAt.MakeNewModelItemAndUpdateStack (lineWithTokenConsumed, tokenMatchText,incomingContext, incomingLineBeingProcessed)
                                        (lineWithTokenConsumed, updatedContext)
                                    | false, true-> // empty line matching the catch-all
                                        (incomingLineToParse, incomingContext)
                                    | _, false->
                                        (incomingLineToParse, incomingContext)

                                ) (lineWithoutLeadingSpaces,newacc)
            
            snd newContext
            ) defaultProcessContext
        compiledContext.Lines
    
//    let processIncomingLines incomingLines =        
//        let whiteSpaceRegex=new System.Text.RegularExpressions.Regex("^\s+")
//        let compiledContext = 
//            incomingLines |> Array.fold(fun acc x->
//            let lastItemProcessed=if acc.Lines.Length>0 then Some(acc.Lines.Item(acc.Lines.Length-1)) else option<ModelItem>.None
//            let lastSourceReferenceForLastItemProcessed=if lastItemProcessed.IsNone then option<SourceReference>.None else Some(lastItemProcessed.Value.SourceReferences.Item(lastItemProcessed.Value.SourceReferences.Length-1))
//            // if there's a new file from the last time, we zap the stack and start over with context
//            let newacc = 
//                if ( (acc<>defaultProcessContext) && (lastSourceReferenceForLastItemProcessed.Value.File.FullName<>x.FileInfo.FullName))
//                    then
//                        defaultProcessContext
//                    else
//                        acc
//            let indentLevel, lineWithoutLeadingSpaces = 
//                let whiteSpaceMatches = whiteSpaceRegex.Matches(x.LineText).toArray
//                if whiteSpaceMatches.Length>0
//                    then
//                        let leadingWhiteSpace = whiteSpaceMatches.[0].Value
//                        let tabCount = leadingWhiteSpace.CountOccurences("\t")
//                        let spaceCount = leadingWhiteSpace.CountOccurences(" ")
//                        ((tabCount + (spaceCount/4)),x.LineText.Substring(leadingWhiteSpace.Length))
//                    else (0,x.LineText)
//            let endingComment, lineWithoutBeginningWhitespaceOrEndingComment = 
//                let isThereACommentIndicator = lineWithoutLeadingSpaces.Contains("//")
//                if isThereACommentIndicator
//                    then
//                        let lineCommentBeginsAt = lineWithoutLeadingSpaces.IndexOf("//")
//                        let lineWithoutComment = lineWithoutLeadingSpaces.GetRight(lineCommentBeginsAt)
//                        let lineEndingComment = lineWithoutLeadingSpaces.Substring(lineCommentBeginsAt+3).Trim()
//                        lineWithoutComment, lineEndingComment
//                    else
//                        "", lineWithoutLeadingSpaces
//            let isThereAToken = languageTokenMatches lineWithoutBeginningWhitespaceOrEndingComment
//            if isThereAToken.Length>0
//                then
//                    // smash token list with previous one to make new one
//                    let newModelItemWithTokensAdded=smashTokensWithModelItem isThereAToken (if newacc.ContextStack.Count>0 then Some(newacc.ContextStack.Peek()) else option<ModelItem>.None) x
//                    // push new one on the stack
//                    newacc.ContextStack.Push(newModelItemWithTokensAdded)
//                    // if there are labels on the same line, create a new modelItem for those (including SourceReference) and add
//                    if lineWithoutBeginningWhitespaceOrEndingComment.Contains(":")
//                        then
//                            let onlyGoodTokens = isThereAToken 
//                                                |> List.filter(fun j->j.IsSome) 
//                                                |> List.map(fun j->j.Value) 
//                                                |> List.map(fun (a,b,c)->b)
//                            let removeAllButLabel =
//                                onlyGoodTokens 
//                                    |> List.fold(fun (acc3:string) z->
//                                        acc3.Replace(z.TokenText,"")
//                                        ) (lineWithoutBeginningWhitespaceOrEndingComment.Replace(":", ""))
//                            let trimUpLabel = removeAllButLabel.Trim()
//                            let newName, newNameValueTagsWithoutLabel = (removeNameValueTags trimUpLabel newModelItemWithTokensAdded x)
//                            let newSourceReferences = [{File=x.FileInfo; LineNumber=x.LineNumber}]
//                            let newId2 = (allCardinalNumbers |> Seq.take(1)  |> Seq.toArray).[0]
//                            let newModelItem = { newModelItemWithTokensAdded with Id=newId2; ItemType=ModelItemType.Label;ShortName=newName; SourceReferences=newSourceReferences}
//
//                            let newNameValueLinesAddingSourceReference = newNameValueTagsWithoutLabel |> List.map(fun x->{x with SourceReferences=newSourceReferences; Parent=Some newModelItem.Id})
//                            let newModelItemAndAnyNameValuePairs = List.append [newModelItem] newNameValueLinesAddingSourceReference
//                            let newContextLines = List.append acc.Lines newModelItemAndAnyNameValuePairs
//
//                            {newacc with Lines=newContextLines}
//                        else
//                            newacc
//                else
//                    // if there were no tokens on the line
//                    // If there are any buckets in the stack it's an item
//                    // otherwise it's a comment
//                    let anyBucketsOnTheStack = 
//                        ( (newacc.ContextStack.Count>0) && (newacc.ContextStack.Peek().Bucket<>Buckets.None) && (newacc.ContextStack.Peek().Bucket<>Buckets.Unknown) )
//                    if anyBucketsOnTheStack
//                        then
//                            if lineWithoutBeginningWhitespaceOrEndingComment.Length=0
//                                then
//                                    newacc
//                                else
//                                    let tempNewItem = {(smashTwoModelItems (acc.ContextStack.Peek()) defaultModelItem) with Id=(allCardinalNumbers |> Seq.take(1)  |> Seq.toArray).[0]}
//                                    // pull out any Name/Value pairs from the line
//                                    let newName, newNameValueTagsWithoutLabel = (removeNameValueTags lineWithoutBeginningWhitespaceOrEndingComment tempNewItem x)
//                                    let newSourceReferences = [{File=x.FileInfo; LineNumber=x.LineNumber}]
//                                    let newModelItem = 
//                                        // find any existing root model lines and add new reference. Otherwise add new model line
//                                        let isThereExistingItem = newacc.Lines |> List.tryFind(fun y->y.ShortName=newName)
//                                        if isThereExistingItem.IsSome                           
//                                            then
//                                                { tempNewItem with ItemType=ModelItemType.Item;ShortName=newName; SourceReferences=newSourceReferences}
//                                            else
//                                                { tempNewItem with ItemType=ModelItemType.Item;ShortName=newName; SourceReferences=newSourceReferences}
//
//                                    let newNameValueLinesAddingSourceReference = newNameValueTagsWithoutLabel |> List.map(fun x->{x with SourceReferences=newSourceReferences; Parent=Some newModelItem.Id})
//                                    let newModelItemAndAnyNameValuePairs = List.append [newModelItem] newNameValueLinesAddingSourceReference
//                                    let newContextLines = List.append acc.Lines newModelItemAndAnyNameValuePairs
//
//                                    {newacc with Lines=newContextLines}
//                        else
//                            // no buckets on stack. It's a comment
//                            if lineWithoutBeginningWhitespaceOrEndingComment.Length=0
//                                then newacc
//                                else
//                                    let mostRecentContext=newacc.ContextStack.Peek()
//                                    let newSourceReference =
//                                        {
//                                            File=x.FileInfo
//                                            LineNumber=x.LineNumber
//                                        }
//                                    let newModelItem =
//                                        {
//                                            mostRecentContext with Id=(allCardinalNumbers |> Seq.take(1)  |> Seq.toArray).[0]; ShortName=lineWithoutBeginningWhitespaceOrEndingComment;SourceReferences=[newSourceReference]
//                                        }
//                                    let newContextLines = List.append acc.Lines [newModelItem]
//                                    {newacc with Lines=newContextLines}
//            ) defaultProcessContext
//        compiledContext.Lines

    let doStuff (opts:EasyAMProgramConfig) =
        let programDirectories = getDirectories opts
        let allFiles = System.IO.Directory.EnumerateFiles((fst opts.sourceDirectory.parameterValue), "*.amin", System.IO.SearchOption.AllDirectories)
        let fileList = allFiles |> Seq.toArray |> Array.map(fun x->System.IO.FileInfo(x)) |> Array.sortBy(fun x->x.FullName)
        let incomingLines = loadInAllIncomingLines fileList
        let structuredAnalysisModel = processIncomingLines incomingLines
        System.Console.WriteLine (structuredAnalysisModel.Length.ToString())
        dumpIncomingModel opts structuredAnalysisModel
        ()


    [<EntryPoint>]
    let main argv = 
        try
            let opts = loadConfigFromCommandLine argv //
            commandLinePrintWhileEnter opts.configBase (opts.printThis)
            let outputDirectories = doStuff opts
            commandLinePrintWhileExit opts.configBase
            0
        with
            | :? UserNeedsHelp as hex ->
                defaultBaseOptions.printThis
                0
            | :? System.Exception as ex ->
                System.Console.WriteLine ("Program terminated abnormally " + ex.Message)
                System.Console.WriteLine (ex.StackTrace)
                if ex.InnerException = null
                    then
                        0
                    else
                        System.Console.WriteLine("---   Inner Exception   ---")
                        System.Console.WriteLine (ex.InnerException.Message)
                        System.Console.WriteLine (ex.InnerException.StackTrace)
                        0    