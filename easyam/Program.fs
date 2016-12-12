module Main//
    open Types
    open Utils
    open Persist
    open FParsec

    let defaultBaseOptions = createNewBaseOptions "easyAM" "Compile the Analysis Model." [|"Takes tagged statements created with Structural Analysis, cross-checks and compiles."|] defaultVerbosity
    let defaulSourceDirectory = createNewConfigEntry "S" "Source Directory (Optional)" [|"/S:<path> -> path to the directory having the source files."|] (System.AppDomain.CurrentDomain.BaseDirectory, Some(System.IO.DirectoryInfo(System.AppDomain.CurrentDomain.BaseDirectory)))
    let defaultDestinationDirectory = createNewConfigEntry "D" "Destination Directory (Optional)" [|"/D:<path> -> path to the directory where compiled files will be deployed."|] (System.AppDomain.CurrentDomain.BaseDirectory, Some(System.IO.DirectoryInfo(System.AppDomain.CurrentDomain.BaseDirectory)))
    
    let informationTagTokens =[|"STRUCUTRE"; "BEHAVIOR"; "SUPPLEMENTAL"; "META"; "BUSINESS"; "SYSTEM"; "ABSTRACT"; "REALIZED"; "AS-IS"; "TO-BE"|]
    let scopingTokens = [|"NAME"; "ORG"; "DOMAIN"|]
    let commandTokens =[|"HASA"|]

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
    let loadInAllIncomingLines (fileList:System.IO.FileInfo[]) =
        let lineDumpForAllFiles = fileList |> Array.mapi(fun i x->
            System.IO.File.ReadLines(x.FullName) |> Seq.toList  |> List.mapi(fun (j:int) (y:string)->
                {
                    File=Some x
                    LineNumber=j
                    LineType=CompilationLineType.Unknown
                    Scope=""
                    TaggedContext=
                        {
                            Bucket=Buckets.Unknown
                            Genre=Genres.Unkown
                            TemporalIndicator=TemporalIndicators.Unknown
                            AbstractionLevel=AbstractionLevels.Unknown
                        }
                    LineText=y  
                }
            )
        )
        lineDumpForAllFiles |> List.concat

    let sortOutLineTypes (lines:CompilationLine list) =
        let removedEmptyLines = lines |> List.filter(fun x->x.LineText.Length>0)
        let determinedLineType = removedEmptyLines |> List.map(fun x->
            match (x.LineText.ContainsAny informationTagTokens), (x.LineText.ContainsAny scopingTokens), (x.LineText.ContainsAny commandTokens) with
                | true, _, _ ->
                    {x with LineType=CompilationLineType.Context}
                | false, true, _ ->
                    {x with LineType=CompilationLineType.Scoping}
                | false, false, true->
                    {x with LineType=CompilationLineType.Command}
                | false, false, false->
                    {x with LineType=CompilationLineType.Freetext}
            )
        determinedLineType
    // ADDED SCOPE. NEED TO ADD TAGS AND THEN WE'LL BE DONE WITH LOADING INPUT
    let addRunningContext (lines:CompilationLine list):CompilationContext =        
        let k=9
        lines |> List.fold(fun (acc:CompilationContext) x->            
            let newScope:string =
                if x.LineType = CompilationLineType.Scoping
                    then
                        let foundToken = scopingTokens |> Array.find(fun y->x.LineText.Contains(y))
                        let positionWhereAllCapsKeywordStarts = x.LineText.IndexOf(foundToken)
                        x.LineText.Substring(positionWhereAllCapsKeywordStarts + foundToken.Length).Trim()
                    else acc.Scope
            let newListItem =
                 { x with
                    Scope=newScope
                 }
            let newCompilationLines = List.append acc.CompilationLines [newListItem]
            {
                acc with
                    CompilationLines=newCompilationLines
                    Scope=newScope
            }
            ) defaultCompilationContext

    let doStuff (opts:EasyAMProgramConfig) =
        let programDirectories = getDirectories opts
        let fileList = programDirectories.SourceDirectoryInfo.GetFiles() |> Seq.filter(fun x->
            ((x.Name.GetRight 3).ToUpper() <> "EXE") && ((x.Name.GetRight 3).ToUpper() <> "DLL") && ((x.Name.GetRight 3).ToUpper() <> "XML") && ((x.Name.GetRight 3).ToUpper() <> "PDB") && ((x.Name.GetRight 6).ToUpper() <> "CONFIG") && ((x.Attributes.HasFlag(System.IO.FileAttributes.Directory)=false)) ) |> Seq.toArray
        let compilationLines = loadInAllIncomingLines fileList
        let lineTypesAdded = sortOutLineTypes compilationLines
        let lineContextAdded = addRunningContext lineTypesAdded
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