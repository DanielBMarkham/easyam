module Main//
    open Types
    open SAModel
    open Utils
    open Persist
    open FParsec


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
        let whiteSpaceRegex=new System.Text.RegularExpressions.Regex("^\s+")
        let lineDumpForAllFiles = fileList |> Array.mapi(fun i x->
            System.IO.File.ReadAllLines(x.FullName) |> Array.mapi(fun j incomingLineBeingProcessed->
                let indentLevel, lineWithoutLeadingSpaces = 
                    let whiteSpaceMatches = whiteSpaceRegex.Matches(incomingLineBeingProcessed).toArray
                    if whiteSpaceMatches.Length>0
                        then
                            let leadingWhiteSpace = whiteSpaceMatches.[0].Value
                            let tabCount = leadingWhiteSpace.CountOccurences("\t")-1
                            let spaceCount = leadingWhiteSpace.CountOccurences(" ")-1
                            ((tabCount + (spaceCount/4)),incomingLineBeingProcessed.Substring(leadingWhiteSpace.Length))
                        else (0,incomingLineBeingProcessed)
                // Note that we add 1 to j for the line number. Line numbers don't start with zero
                {FileNumber=i;FileInfo=x; LineNumber=j+1; LineText=incomingLineBeingProcessed; IndentLevel=indentLevel; LineWithoutLeadingSpaces=lineWithoutLeadingSpaces}
                )
            )
        lineDumpForAllFiles |> Array.concat
    
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
                                    List.append acc [{currentContext with Id=(Seq.take 1 allCardinalNumbers |> Seq.toArray).[0]; ModelItemName=x; ItemType=NameValueTag({ Name=lhs; Value=rhs; SourceReference={File=incomingLine.FileInfo; LineNumber=incomingLine.LineNumber; LineLevelIndent=incomingLine.IndentLevel}})}]
                                    ) []
                newLineWithOnlyLabelRemaining, newModelItems
            else
                lineWithOnlyLabelRemaining, []

    let processIncomingLines incomingLines =        
        let compiledContext = 
            incomingLines |> Array.fold(fun lineProcessorAccumulator incomingLineBeingProcessed->
            let lastItemInTheContextLineList=
                if lineProcessorAccumulator.Lines.Length>0 
                    then
                        lineProcessorAccumulator.Lines.Item(lineProcessorAccumulator.Lines.Length-1) 
                    else 
                        let newSourceReferences=[{File=incomingLineBeingProcessed.FileInfo; LineNumber=incomingLineBeingProcessed.LineNumber; LineLevelIndent=incomingLineBeingProcessed.IndentLevel}]
                        {defaultModelItem with SourceReferences=newSourceReferences}
            let lastSourceReferenceForLastItemProcessed=
                if lineProcessorAccumulator.Lines.Length=0 
                    then {File=incomingLineBeingProcessed.FileInfo;LineNumber=incomingLineBeingProcessed.LineNumber; LineLevelIndent=incomingLineBeingProcessed.IndentLevel} 
                    else lastItemInTheContextLineList.SourceReferences.Item(lastItemInTheContextLineList.SourceReferences.Length-1)
            // if there's a new file from the last time, we zap the stack and start over with context
            let newacc = 
                //if ( (lineProcessorAccumulator<>defaultProcessContext) && (lastSourceReferenceForLastItemProcessed.File.FullName<>incomingLineBeingProcessed.FileInfo.FullName))
                if (  (lastSourceReferenceForLastItemProcessed.File.FullName<>incomingLineBeingProcessed.FileInfo.FullName))
                    then
                        {defaultProcessContext with Lines=lineProcessorAccumulator.Lines}
                    else
                        lineProcessorAccumulator
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

                                ) (incomingLineBeingProcessed.LineWithoutLeadingSpaces,newacc)
            
            snd newContext
            ) defaultProcessContext
        compiledContext.Lines
    

    let doStuff (opts:EasyAMProgramConfig) =
        let programDirectories = getDirectories opts
        let allFiles = System.IO.Directory.EnumerateFiles((fst opts.sourceDirectory.parameterValue), "*.amin", System.IO.SearchOption.AllDirectories)
        let fileList = allFiles |> Seq.toArray |> Array.map(fun x->System.IO.FileInfo(x)) |> Array.sortBy(fun x->x.FullName)
        let incomingLines = loadInAllIncomingLines fileList
        let structuredAnalysisModel = processIncomingLines incomingLines
        System.Console.WriteLine (structuredAnalysisModel.Length.ToString())
        rawDumpIncomingModel opts structuredAnalysisModel
        compiledDumpIncomingModelAmout opts structuredAnalysisModel
        compiledDumpIncomingModelHtml opts structuredAnalysisModel
        saveMasterIndex opts structuredAnalysisModel
        saveMasterQuestionList opts structuredAnalysisModel
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