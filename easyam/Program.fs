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

    type incomingLine = {FileNumber:int;FileInfo:System.IO.FileInfo; LineNumber:int; LineText:string}
    let loadInAllIncomingLines (fileList:System.IO.FileInfo[]) =
        let lineDumpForAllFiles = fileList |> Array.mapi(fun i x->
            System.IO.File.ReadAllLines(x.FullName) |> Array.mapi(fun j y->
                {FileNumber=i;FileInfo=x; LineNumber=j; LineText=y}
                )
            )
        lineDumpForAllFiles |> Array.concat
    
    let smashTwoModelItems (existingModelItem:ModelItem) (incomingModelItem:ModelItem) =
            let newId=incomingModelItem.Id
            let newParent=if incomingModelItem.Parent.IsSome then incomingModelItem.Parent else existingModelItem.Parent
            let newItemType=if incomingModelItem.ItemType<>ModelItemType.None then incomingModelItem.ItemType else existingModelItem.ItemType
            let newBucket=if incomingModelItem.Bucket<>Buckets.Unknown then incomingModelItem.Bucket else existingModelItem.Bucket
            let newGenre=if incomingModelItem.Genre<>Genres.Unknown then incomingModelItem.Genre else existingModelItem.Genre
            let newAbstractionLevel=if incomingModelItem.AbstractionLevel<>AbstractionLevels.Unknown then incomingModelItem.AbstractionLevel else existingModelItem.AbstractionLevel
            let newTemporalIndicator=if incomingModelItem.TemporalIndicator<>TemporalIndicators.Unknown then incomingModelItem.TemporalIndicator else existingModelItem.TemporalIndicator
            let newItemAnnotation=incomingModelItem.ItemAnnotation
            let newSourceReferences=incomingModelItem.SourceReferences
            let newShortName=incomingModelItem.ShortName
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
                ShortName=newShortName
            }

    let smashTokensWithModelItem (tokens:(LanguageTokenMatchType*LanguageToken*int) option list) (modelItem:ModelItem option):ModelItem =
        let startingModelItem = if modelItem.IsSome then modelItem.Value else defaultModelItem
        let newModelItem = tokens |> List.fold(fun (acc:ModelItem) x->
            if x.IsSome
                then
                    let xVal=
                        let a,b,c=x.Value
                        b.ExampleItem
                    let newItemType=if xVal.ItemType=ModelItemType.None then acc.ItemType else xVal.ItemType
                    let newBucket=if ( (xVal.Bucket=Buckets.None) || (xVal.Bucket=Buckets.Unknown) ) then acc.Bucket else xVal.Bucket
                    let newGenre=if ( (xVal.Genre=Genres.None) || (xVal.Genre=Genres.Unknown)) then acc.Genre else xVal.Genre
                    let newAbstractionLevel=if ( (xVal.AbstractionLevel=AbstractionLevels.None) || (xVal.AbstractionLevel=AbstractionLevels.Unknown)) then acc.AbstractionLevel else xVal.AbstractionLevel
                    let newTemporalIndicator=if ( (xVal.TemporalIndicator=TemporalIndicators.None) || (xVal.TemporalIndicator=TemporalIndicators.Unknown)) then acc.TemporalIndicator else xVal.TemporalIndicator
                    { acc with
                        ItemType=newItemType
                        Bucket=newBucket
                        Genre=newGenre
                        AbstractionLevel=newAbstractionLevel
                        TemporalIndicator=newTemporalIndicator
                    }
                else
                            acc
                            ) startingModelItem
        newModelItem
    let removeNameValueTags (lineWithOnlyLabelRemaining:string) (currentContext:ModelItem) =
        if lineWithOnlyLabelRemaining.Contains("&")
            then
                let firstDelimiter=lineWithOnlyLabelRemaining.IndexOf("&")
                let newLineWithOnlyLabelRemaining=lineWithOnlyLabelRemaining.GetLeft(firstDelimiter-1)
                let delimterSection = lineWithOnlyLabelRemaining.Substring(firstDelimiter+1)
                let delimiterSections=delimterSection.Split([|"&"|], System.StringSplitOptions.None)
                let newModelItems = delimiterSections |> Array.fold(fun acc x->
                                    List.append acc [{currentContext with Id=(Seq.take 1 allCardinalNumbers |> Seq.toArray).[0]; ShortName=x; ItemType=ModelItemType.NameValuePair}]
                                    ) []
                newLineWithOnlyLabelRemaining, newModelItems
            else
                lineWithOnlyLabelRemaining, []
    type ProcessContext = {ContextStack:System.Collections.Generic.Stack<ModelItem>; Lines:ModelItem list}
    let defaultProcessContext = 
        let newContextStack = new System.Collections.Generic.Stack<ModelItem>()
        newContextStack.Push(defaultModelItem)
        {ContextStack=newContextStack; Lines=[]}
    
    let processIncomingLines incomingLines =        
        let whiteSpaceRegex=new System.Text.RegularExpressions.Regex("^\s+")
        let compiledContext = 
            incomingLines |> Array.fold(fun acc x->
            let lastItemProcessed=if acc.Lines.Length>0 then Some(acc.Lines.Item(acc.Lines.Length-1)) else option<ModelItem>.None
            let lastSourceReferenceForLastItemProcessed=if lastItemProcessed.IsNone then option<SourceReference>.None else Some(lastItemProcessed.Value.SourceReferences.Item(lastItemProcessed.Value.SourceReferences.Length-1))
            // if there's a new file from the last time, we zap the stack and start over with context
            let newacc = 
                if ( (acc<>defaultProcessContext) && (lastSourceReferenceForLastItemProcessed.Value.File.FullName<>x.FileInfo.FullName))
                    then
                        defaultProcessContext
                    else
                        acc
            let indentLevel, lineWithoutLeadingSpaces = 
                let whiteSpaceMatches = whiteSpaceRegex.Matches(x.LineText).toArray
                if whiteSpaceMatches.Length>0
                    then
                        let leadingWhiteSpace = whiteSpaceMatches.[0].Value
                        let tabCount = leadingWhiteSpace.CountOccurences("\t")
                        let spaceCount = leadingWhiteSpace.CountOccurences(" ")
                        ((tabCount + (spaceCount/4)),x.LineText.Remove(leadingWhiteSpace.Length+1))
                    else (0,x.LineText)
            let endingComment, lineWithoutBeginningWhitespaceOrEndingComment = 
                let isThereACommentIndicator = lineWithoutLeadingSpaces.Contains("//")
                if isThereACommentIndicator
                    then
                        let lineCommentBeginsAt = lineWithoutLeadingSpaces.IndexOf("//")
                        let lineWithoutComment = lineWithoutLeadingSpaces.GetRight(lineCommentBeginsAt)
                        let lineEndingComment = lineWithoutLeadingSpaces.Substring(lineCommentBeginsAt+3).Trim()
                        lineWithoutComment, lineEndingComment
                    else
                        "", lineWithoutLeadingSpaces
            let isThereAToken = languageTokenMatches lineWithoutBeginningWhitespaceOrEndingComment
            if isThereAToken.Length>0
                then
                    // smash token list with previous one to make new one
                    let newModelItemWithTokensAdded=smashTokensWithModelItem isThereAToken (if newacc.ContextStack.Count>0 then Some(newacc.ContextStack.Peek()) else option<ModelItem>.None)
                    // push new one on the stack
                    newacc.ContextStack.Push(newModelItemWithTokensAdded)
                    // if there are labels on the same line, create a new modelItem for those (including SourceReference) and add
                    if lineWithoutBeginningWhitespaceOrEndingComment.Contains(":")
                        then
                            let onlyGoodTokens = isThereAToken 
                                                |> List.filter(fun j->j.IsSome) 
                                                |> List.map(fun j->j.Value) 
                                                |> List.map(fun (a,b,c)->b)
                            let removeAllButLabel =
                                onlyGoodTokens 
                                    |> List.fold(fun (acc3:string) z->
                                        acc3.Replace(z.TokenText,"")
                                        ) (lineWithoutBeginningWhitespaceOrEndingComment.Replace(":", ""))
                            let trimUpLabel = removeAllButLabel.Trim()
                            let newName, newNameValueTagsWithoutLabel = removeNameValueTags trimUpLabel newModelItemWithTokensAdded
                            let newSourceReferences = [{File=x.FileInfo; LineNumber=x.LineNumber}]
                            let newId2 = (allCardinalNumbers |> Seq.take(1)  |> Seq.toArray).[0]
                            let newModelItem = { newModelItemWithTokensAdded with Id=newId2; ItemType=ModelItemType.Label;ShortName=newName; SourceReferences=newSourceReferences}

                            let newNameValueLinesAddingSourceReference = newNameValueTagsWithoutLabel |> List.map(fun x->{x with SourceReferences=newSourceReferences; Parent=Some newModelItem.Id})
                            let newModelItemAndAnyNameValuePairs = List.append [newModelItem] newNameValueLinesAddingSourceReference
                            let newContextLines = List.append acc.Lines newModelItemAndAnyNameValuePairs

                            {newacc with Lines=newContextLines}
                        else
                            newacc
                else
                    // if there were no tokens on the line
                    // If there are any buckets in the stack it's an item
                    // otherwise it's a comment
                    let anyBucketsOnTheStack = 
                        ( (newacc.ContextStack.Count>0) && (newacc.ContextStack.Peek().Bucket<>Buckets.None) && (newacc.ContextStack.Peek().Bucket<>Buckets.Unknown) )
                    if anyBucketsOnTheStack
                        then
                            let tempNewItem = {(smashTwoModelItems (acc.ContextStack.Peek()) defaultModelItem) with Id=(allCardinalNumbers |> Seq.take(1)  |> Seq.toArray).[0]}
                            // pull out any Name/Value pairs from the line
                            let newName, newNameValueTagsWithoutLabel = removeNameValueTags lineWithoutBeginningWhitespaceOrEndingComment tempNewItem
                            let newSourceReferences = [{File=x.FileInfo; LineNumber=x.LineNumber}]
                            let newModelItem = 
                                // find any existing root model lines and add new reference. Otherwise add new model line
                                let isThereExistingItem = newacc.Lines |> List.tryFind(fun y->y.ShortName=newName)
                                if isThereExistingItem.IsSome                           
                                    then
                                        { tempNewItem with ItemType=ModelItemType.Item;ShortName=newName; SourceReferences=newSourceReferences}
                                    else
                                        { tempNewItem with ItemType=ModelItemType.Item;ShortName=newName; SourceReferences=newSourceReferences}

                            let newNameValueLinesAddingSourceReference = newNameValueTagsWithoutLabel |> List.map(fun x->{x with SourceReferences=newSourceReferences; Parent=Some newModelItem.Id})
                            let newModelItemAndAnyNameValuePairs = List.append [newModelItem] newNameValueLinesAddingSourceReference
                            let newContextLines = List.append acc.Lines newModelItemAndAnyNameValuePairs

                            {newacc with Lines=newContextLines}
                        else
                            // no buckets on stack. It's a comment
                            if lineWithoutBeginningWhitespaceOrEndingComment.Length=0
                                then newacc
                                else
                                    let mostRecentContext=newacc.ContextStack.Peek()
                                    let newSourceReference =
                                        {
                                            File=x.FileInfo
                                            LineNumber=x.LineNumber
                                        }
                                    let newModelItem =
                                        {
                                            mostRecentContext with Id=(allCardinalNumbers |> Seq.take(1)  |> Seq.toArray).[0]; ShortName=lineWithoutBeginningWhitespaceOrEndingComment;SourceReferences=[newSourceReference]
                                        }
                                    let newContextLines = List.append acc.Lines [newModelItem]
                                    {newacc with Lines=newContextLines}
            ) defaultProcessContext
        compiledContext.Lines

    let doStuff (opts:EasyAMProgramConfig) =
        let programDirectories = getDirectories opts
        let fileList = programDirectories.SourceDirectoryInfo.GetFiles() |> Seq.filter(fun x->
            ((x.Name.GetRight 3).ToUpper() <> "EXE") && ((x.Name.GetRight 3).ToUpper() <> "DLL") && ((x.Name.GetRight 3).ToUpper() <> "XML") && ((x.Name.GetRight 3).ToUpper() <> "PDB") && ((x.Name.GetRight 6).ToUpper() <> "CONFIG") && ((x.Name.GetRight 3).ToUpper() <> "CSV") && ((x.Name.GetRight 3).ToUpper() <> "SVG") && ((x.Attributes.HasFlag(System.IO.FileAttributes.Directory)=false)) ) |> Seq.toArray
        let incomingLines = loadInAllIncomingLines fileList
        let StructuredAnalysisModel = processIncomingLines incomingLines
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