module Main//
    open Types
    open SAModel
    open Utils
    open Persist
    open FParsec
    open EasyamParsingEngine

    let programHelp = [|
                        "Takes tagged statements created with Structural Analysis tags."
                        ;"Cross-checks model and outputs in various formats,"
                        ;"including a canonical format that's organized by tag and"
                        ;"can be used for future input."
                        ;""
                        ;"Example: A simple backlog for an ATM could be created like this:"
                        ;"BEHAVIOR: Deposit Funds, Withdraw Cash, Check Balance,  Transfer Funds"
                        ;"SUPPLEMENTAL: ADA compliant, Touch-sceen enabled, Record images of all users, Provide a full audit trail"
                        ;""
                        ;"SPRINT BACKLOG"
                        ;"  Withdraw Cash Using one click PARENT Withdraw Cash"
                        ;"  ASA Account-holder"
                        ;"  INEEDTO withdraw cash using one click"
                        ;"  SOTHAT I can be on my way quickly"
                        ;""
                        ;"The program will process all files in the source directory ending in .amim"
                        ;"It will produce a canonical (organized) output in the output directory in .amout format"
                        ;""
                        ;"Optional parameters are /S:<source directory> /D:<destination directory> /N:<namespace filter for output>"
                        |]
    let defaultBaseOptions = createNewBaseOptions "easyam" "Command-line analysis model compiler" programHelp defaultVerbosity
    let defaulSourceDirectory = createNewConfigEntry "S" "Source Directory (Optional)" [|"/S:<path> -> path to the directory having the source files."|] (System.AppDomain.CurrentDomain.BaseDirectory, Some(System.IO.DirectoryInfo(System.AppDomain.CurrentDomain.BaseDirectory)))
    let defaultDestinationDirectory = createNewConfigEntry "D" "Destination Directory (Optional)" [|"/D:<path> -> path to the directory where compiled files will be deployed."|] (System.AppDomain.CurrentDomain.BaseDirectory, Some(System.IO.DirectoryInfo(System.AppDomain.CurrentDomain.BaseDirectory)))
    let defaultNamespace = createNewConfigEntry "N" "Namespace (Optional)" [|"/N:<namespace> -> namespace filter to show in output."; "Example: a team's sprint stories may have a namespace of BadgerTeam: Sprint 3"|] ""
    

    let loadConfigFromCommandLine (args:string []):EasyAMProgramConfig =
        if args.Length>0 && (args.[0]="?"||args.[0]="/?"||args.[0]="-?"||args.[0]="--?"||args.[0]="help"||args.[0]="/help"||args.[0]="-help"||args.[0]="--help") then raise (UserNeedsHelp args.[0]) else
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosity, args)
        let newSourceDirectory = ConfigEntry<_>.populateValueFromCommandLine(defaulSourceDirectory, args)
        let newDestinationDirectory = ConfigEntry<_>.populateValueFromCommandLine(defaultDestinationDirectory, args)
        let newNamespace = ConfigEntry<_>.populateValueFromCommandLine(defaultNamespace, args)
        let newConfigBase = {defaultBaseOptions with verbose=newVerbosity}
        { 
            configBase = newConfigBase
            sourceDirectory=newSourceDirectory
            destinationDirectory=newDestinationDirectory
            nameSpace=newNamespace
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
        saveMasterQuestionList (System.AppDomain.CurrentDomain.BaseDirectory) "mql.html" compilerResult
        saveModelGuide (System.AppDomain.CurrentDomain.BaseDirectory + "master-cards.html") compilerResult
        saveCanonicalModel System.AppDomain.CurrentDomain.BaseDirectory compilerResult
        printCompilerMessages compilerResult.CompilerMessages
        ()


    [<EntryPoint>]
    let main argv = 
        try
            let opts = loadConfigFromCommandLine argv
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