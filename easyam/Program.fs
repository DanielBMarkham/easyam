module Main//
    open Types
    open SAModel
    open Utils
    open Persist
    open FParsec
    open EasyamParsingEngine


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
        let directorySeparatorCharacter = string System.IO.Path.DirectorySeparatorChar
        let BehaviorDirectoryInfo = getOrMakeDirectory (destinationDirectoryInfo.FullName + directorySeparatorCharacter + "Behavior")
        let StructureDirectoryInfo = getOrMakeDirectory (destinationDirectoryInfo.FullName + directorySeparatorCharacter + "Structure")
        let SupplementalDirectoryInfo = getOrMakeDirectory (destinationDirectoryInfo.FullName + directorySeparatorCharacter + "Supplemental")
        let MetaDirectoryInfo = getOrMakeDirectory (destinationDirectoryInfo.FullName + directorySeparatorCharacter + "Meta")
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
        let fileInfosAndContents:(System.IO.FileInfo*string[]) [] = fileList |> Array.map(fun x->
                                    let contentsForTheFile=System.IO.File.ReadAllLines(x.FullName)
                                    (x,contentsForTheFile)
                                    )
        fileInfosAndContents
    
    let doStuff (opts:EasyAMProgramConfig) =
        let programDirectories = getDirectories opts
        let allFiles = System.IO.Directory.EnumerateFiles((fst opts.sourceDirectory.parameterValue), "*.amin", System.IO.SearchOption.AllDirectories)
        let fileList = allFiles |> Seq.toArray |> Array.map(fun x->System.IO.FileInfo(x)) |> Array.sortBy(fun x->x.FullName)
        let listToProcess = loadInAllIncomingLines fileList
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let compilerResult = makeRawModel processedIncomingLines compilerReturn
        printCompilerMessages compilerResult.CompilerMessages
        //let structuredAnalysisModel = processIncomingLines incomingLines
        //System.Console.WriteLine (string structuredAnalysisModel.Length)
        //rawDumpIncomingModel opts structuredAnalysisModel
        //compiledDumpIncomingModelAmout opts structuredAnalysisModel
        //compiledDumpIncomingModelHtml opts structuredAnalysisModel
        //saveMasterIndex opts structuredAnalysisModel
        //saveMasterQuestionList opts structuredAnalysisModel
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
            | ex ->
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