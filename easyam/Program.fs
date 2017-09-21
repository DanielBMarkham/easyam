module Main//
    open Types
    open SAModel
    open Lenses
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
                        ;"    ASA Account-holder"
                        ;"    INEEDTO withdraw cash using one click"
                        ;"    SOTHAT I can be on my way quickly"
                        ;"    SCENARIOS:"
                        ;"      Balance not available"
                        ;"      Machine out of cash"
                        ;"        CODE:"
                        ;"          Given that I have an account with the bank"
                        ;"          When I identify myself to the machine"
                        ;"          And I request money"
                        ;"          And the machine is out of money"
                        ;"          Then I am politely notified the machine is out of money"
                        ;"          And pointed to a nearby machine that has funds"
                        ;""
                        ;"The program will process all files in the source directory ending in .amim"
                        ;"It will produce a canonical (organized) output in the output directory in .amout format"
                        ;"It also produces several html reports and generates a gherkin feature file structure for acceptance testing"
                        ;""
                        ;"Optional parameters are /S:<source directory> /D:<destination directory> /O:<output format> /N:<namespace filter for output>"
                        ;"Filter/Sort parameters"
                        ;"  SORT-TAGATT, SORT-THING, SORT-CONVERTO, SORT-ORDER, GENRE, BUCKET, ABSTRACTIONLEVEL, TEMPORAL"
                        ;"  FILTER-TAGATT, FILTER-THING, FILTER-CONVERTTO, FILTER-ORDER, FILTER-FROMVAL, FILTER-TOVAL"
                        |]
    
    let defaultBaseOptions = createNewBaseOptions "easyam" "Command-line analysis model compiler" programHelp defaultVerbosity
    let defaulSourceDirectory = createNewConfigEntry "S" "Source Directory (Optional)" [|"/S:<path> -> path to the directory having the source files."|] (System.AppDomain.CurrentDomain.BaseDirectory, Some(System.IO.DirectoryInfo(System.AppDomain.CurrentDomain.BaseDirectory)))
    let defaultDestinationDirectory = createNewConfigEntry "D" "Destination Directory (Optional)" [|"/D:<path> -> path to the directory where compiled files will be deployed."|] (System.AppDomain.CurrentDomain.BaseDirectory, Some(System.IO.DirectoryInfo(System.AppDomain.CurrentDomain.BaseDirectory)))
    let defaultOutputFormat = createNewConfigEntry "O" "Output Format (Optional)" [|"/O:SINGLEFILE=<path> -> Output everything to a single file"|] ""
    let defaultNamespace = createNewConfigEntry "N" "Namespace (Optional)" [|"/N:<namespace> -> namespace filter to show in output."; "Example: a team's sprint stories may have a namespace of BadgerTeam: Sprint 3"|] ""
    let defaultSortTagOrAtt = createNewConfigEntry "SORT-TAGATT" "Sort based on either a tag or attribute (Optional)" [|"/SORT-TAGATT:<ATT|TAG> -> there will be a sort on either a tag or attribute"; "(rest of sort information is provided in other parameters"; "Example:SORT-TAGATT:ATT"|] TagOrAtt.Tag
    let defaultSortThing = createNewConfigEntry "SORT-THING" "Once you've picked tag or att, the name of the tag or attribute (Optional)" [|"/SORT-THING:<name> -> If you've tagged things in the model, use tag here. There are also a limited number of attributes, like Description, you can use"; "(rest of sort information is provided in other parameters)"; "Example:SORT-THING:Description"|] ""
    let defaultSortConvertTo = createNewConfigEntry "SORT-CONVERTO" "Once you've picked a tag or att, what do I convert it to (Optional)" [|"/SORT-CONVERTTO:<typename> -> typenames supported: Int | Float | Money | DateTime | TimeSpan"; "Failure to provide defaults to string";"(rest of sort information is provided in other parameters)"; "Example:easyam SORT_TAGATT:TAG SORT-THING:Rank SORT-CONVERTO:INT SORT-oRDER:ASC"|] ConvertTo.DontConvert
    let defaultSortOrder = createNewConfigEntry "SORT-ORDER" "Sort order (Optional)" [|"/SORT-ORDER:<ASC/DESC> -> sort output in either ASCending or DESCending order"; "Default is ASCending"; "(rest of sort information is provided in other parameters)"; "Example:easyam SORT_TAGATT:TAG SORT-THING:Rank SORT-CONVERTO:INT SORT-oRDER:ASC"|] SortOrder.Ascending
    let defaultFilterGenre = createNewConfigEntry "GENRE" "Filter for a particular genre (Optional)" [|"/GENRE:<SYSTEM|BUSINESS|META> -> reduces output to a single genre"; "Example:easyam GENRE:BUSINESS"|] Genres.Unknown
    let defaultFilterBucket = createNewConfigEntry "BUCKET" "Filter for a particular bucket (Optional)" [|"/GENRE:<BEHAVIOR|STRUCTURE|SUPPLEMENTAL> -> reduces output to a single bucket"; "Example:easyam BUCKET:BEHAVIOR"|] Buckets.Unknown
    let defaultFilterAbstractionLevel = createNewConfigEntry "ABSTRACTIONLEVEL" "Filter for a particular abstraction level (Optional)" [|"/ABSTRACTIONLEVEL:<Abstract|Realized> -> reduces output to a single abstraction level"; "Example:easyam ABSTRACTIONLEVEL:Abstract"|] AbstractionLevels.Unknown
    let defaultFilterTemporalIndicator = createNewConfigEntry "TEMPORAL" "Filter for a particular temporal indicator (Optional)" [|"/TEMPORAL:<WAS|ASIS|TOBE> -> reduces output to a single temporal indicator"; "Example:easyam TEMPORAL:TOBE"|] TemporalIndicators.Unknown
    let defaultFilterTagOrAtt = createNewConfigEntry "FILTER-TAGATT" "Filter based on either a tag or attribute (Optional)" [|"/FILTER-TAGATT:<ATT|TAG> -> there will be a filter on either a tag or attribute"; "(rest of filter information is provided in other parameters"; "Example:SORT-TAGATT:ATT"|] TagOrAtt.Tag
    let defaultFilterThing = createNewConfigEntry "FILTER-THING" "Once you've picked tag or att, the name of the tag or attribute (Optional)" [|"/FILTER-THING:<name> -> If you've tagged things in the model, use tag here. There are also a limited number of attributes, like Description, you can use"; "(rest of filter information is provided in other parameters)"; "Example outputs only things tags as being in sprint 2: easyam FILTER-THING:Sprint FILTER-TAGATT:TAG FILTER-CONVERTTO:Int FILTER-FROMVAL:1 FILTER-TOVAL:3"|] ""
    let defaultFilterConvertTo = createNewConfigEntry "FILTER-CONVERTO" "Once you've picked a tag or att, what do I convert it to (Optional)" [|"/FILTER-CONVERTTO:<typename> -> typenames supported: Int | Float | Money | DateTime | TimeSpan"; "Failure to provide defaults to string";"(rest of filter information is provided in other parameters)"; "Example outputs only things tags as being in sprint 2: easyam FILTER-THING:Sprint FILTER-TAGATT:TAG FILTER-CONVERTTO:Int FILTER-FROMVAL:1 FILTER-TOVAL:3"|] ConvertTo.DontConvert
    let defaultFilterOrder = createNewConfigEntry "FILTER-ORDER" "Filter order (Optional) UNUSED" [|"/FILTER-ORDER:<ASC/DESC> -> UNUSED"|] SortOrder.Ascending
    let defaultFilterFromVal = createNewConfigEntry "FILTER-FROMVAL" "Filter From Value (Optional)" [|"/FILTER-FROMVAL:<string> -> lower boundary of filter"; "(rest of sort information is provided in other parameters"; "Example outputs only things tags as being in sprint 2: easyam FILTER-THING:Sprint FILTER-TAGATT:TAG FILTER-CONVERTTO:Int FILTER-FROMVAL:1 FILTER-TOVAL:3"|] ""
    let defaultFilterToVal = createNewConfigEntry "FILTER-TOVAL" "Filter To Value (Optional)" [|"/FILTER-TOVAL:<string> -> upper boundary of filter"; "(rest of sort information is provided in other parameters"; "Example outputs only things tags as being in sprint 2: easyam FILTER-THING:Sprint FILTER-TAGATT:TAG FILTER-CONVERTTO:Int FILTER-FROMVAL:1 FILTER-TOVAL:3"|] ""

    let loadConfigFromCommandLine (args:string []):EasyAMProgramConfig =
        if args.Length>0 && (args.[0]="?"||args.[0]="/?"||args.[0]="-?"||args.[0]="--?"||args.[0]="help"||args.[0]="/help"||args.[0]="-help"||args.[0]="--help") then raise (UserNeedsHelp args.[0]) else
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosity, args)
        let newSourceDirectory = ConfigEntry<_>.populateValueFromCommandLine(defaulSourceDirectory, args)
        let newDestinationDirectory = ConfigEntry<_>.populateValueFromCommandLine(defaultDestinationDirectory, args)
        let newConfigBase = {defaultBaseOptions with verbose=newVerbosity}
        let newOutputFormat = ConfigEntry<_>.populateValueFromCommandLine(defaultOutputFormat, args)
        let newNamespace = ConfigEntry<_>.populateValueFromCommandLine(defaultNamespace, args)
        let newSortTaggOrAtt = ConfigEntry<_>.populateValueFromCommandLine(defaultSortTagOrAtt, args)
        let newSortThing = ConfigEntry<_>.populateValueFromCommandLine(defaultSortThing, args)
        let newSortConvertTo = ConfigEntry<_>.populateValueFromCommandLine(defaultSortConvertTo, args)
        let newSortOrder = ConfigEntry<_>.populateValueFromCommandLine(defaultSortOrder, args)
        let newFilterGenre = ConfigEntry<_>.populateValueFromCommandLine(defaultFilterGenre, args)
        let newFilterBucket = ConfigEntry<_>.populateValueFromCommandLine(defaultFilterBucket, args)
        let newFilterAbstractionLevel = ConfigEntry<_>.populateValueFromCommandLine(defaultFilterAbstractionLevel, args)
        let newFilterTemporalIndicator = ConfigEntry<_>.populateValueFromCommandLine(defaultFilterTemporalIndicator, args)
        let newFilterTagOrAtt = ConfigEntry<_>.populateValueFromCommandLine(defaultFilterTagOrAtt, args)
        let newFilterThing = ConfigEntry<_>.populateValueFromCommandLine(defaultFilterThing, args)
        let newFilterConvertTo = ConfigEntry<_>.populateValueFromCommandLine(defaultFilterConvertTo, args)
        let newFilterOrder = ConfigEntry<_>.populateValueFromCommandLine(defaultFilterOrder, args)
        let newFilterFromVal = ConfigEntry<_>.populateValueFromCommandLine(defaultFilterFromVal, args)
        let newFilterToVal = ConfigEntry<_>.populateValueFromCommandLine(defaultFilterToVal, args)
        { 
            configBase = newConfigBase
            sourceDirectory=newSourceDirectory
            destinationDirectory=newDestinationDirectory
            nameSpace=newNamespace
            outputFormat=newOutputFormat
            sortTagOrAtt=newSortTaggOrAtt
            sortThing=newSortThing
            sortConvertTo=newSortConvertTo
            sortOrder=newSortOrder
            filterGenre=newFilterGenre
            filterBucket=newFilterBucket
            filterAbstractionLevel=newFilterAbstractionLevel
            filterTemporalIndicator=newFilterTemporalIndicator
            filterTagOrAtt=newFilterTagOrAtt
            filterThing=newFilterThing
            filterConvertTo=newSortConvertTo
            filterOrder=newFilterOrder
            filterFromVal=newFilterFromVal
            filterToVal=newFilterToVal
        }
    let getDirectories (opts:EasyAMProgramConfig) = 
        // set up any folders needed by the tool
        let sourceDirectoryInfo = forceDirectoryCreation opts.sourceDirectory
        let destinationDirectoryInfo = forceDirectoryCreation opts.destinationDirectory
        let directorySeparatorCharacter = string System.IO.Path.DirectorySeparatorChar
        let BehaviorDirectoryInfo = getOrMakeDirectory (destinationDirectoryInfo.FullName + directorySeparatorCharacter + "Behavior")
        let StructureDirectoryInfo = getOrMakeDirectory (destinationDirectoryInfo.FullName + directorySeparatorCharacter + "Structure")
        let SupplementalDirectoryInfo = getOrMakeDirectory (destinationDirectoryInfo.FullName + directorySeparatorCharacter + "Supplemental")
        let featureDirectoryFullName=destinationDirectoryInfo.FullName + directorySeparatorCharacter + "features"
        // for the features directory, we wipe it every time (for now) to prevent confusion caused by overwrites
        if System.IO.Directory.Exists(featureDirectoryFullName) then System.IO.Directory.Delete(featureDirectoryFullName, true) else ()
        let FeaturesDirectoryInfo = getOrMakeDirectory (featureDirectoryFullName)
        // add standard two Ruby/Cucumber sub-folders
        getOrMakeDirectory(featureDirectoryFullName + directorySeparatorCharacter + "step_definitions") |> ignore
        getOrMakeDirectory(featureDirectoryFullName + directorySeparatorCharacter + "support") |> ignore
        {
            SourceDirectoryInfo=sourceDirectoryInfo
            DestinationDirectoryInfo=destinationDirectoryInfo
            BehaviorDirectoryInfo=BehaviorDirectoryInfo
            StructureDirectoryInfo=StructureDirectoryInfo
            SupplementalDirectoryInfo=SupplementalDirectoryInfo
            FeaturesDirectoryInfo=FeaturesDirectoryInfo
        }
    let allCardinalNumbers = {1..10000}

    let doStuff (opts:EasyAMProgramConfig) =
        // coming in
        let programDirectories = getDirectories opts
        let allFiles = System.IO.Directory.EnumerateFiles(programDirectories.SourceDirectoryInfo.FullName, "*.amin", System.IO.SearchOption.AllDirectories)
        let fileList = allFiles |> Seq.toArray |> Array.map(fun x->System.IO.FileInfo(x)) |> Array.sortBy(fun x->x.FullName)
        let listToProcess = loadInAllIncomingLines fileList
        //processing
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let compilerResult = makeRawModel processedIncomingLines compilerReturn
        let outputModel = applyCommandLineSortAndFilter compilerResult opts
        // heading out
        //
        // TEST SCAFFOLDING FOR GENERIC FUNCTION
        writeOutModel compilerResult.ModelItems outputModel ModelOutputType.AMOUT programDirectories.DestinationDirectoryInfo true ""
        //
        //
        if opts.outputFormat.parameterValue.Contains("SINGLEFILE")
            then
                let targetFilename=
                    let providedFileName=if opts.outputFormat.parameterValue.Contains("=") then (opts.outputFormat.parameterValue.Split([|"="|], System.StringSplitOptions.None)).[1] else ""
                    if providedFileName.Length=0 then "" else providedFileName
                saveAllToOneAMOUTFile programDirectories.DestinationDirectoryInfo.FullName targetFilename outputModel compilerResult
                printCompilerMessages compilerResult.CompilerMessages true
                ()
            else
                saveMasterQuestionList programDirectories.DestinationDirectoryInfo.FullName "mql.html" compilerResult
                saveModelGuide (programDirectories.DestinationDirectoryInfo.FullName + "master-cards.html") compilerResult
                saveProjectBacklog (programDirectories.DestinationDirectoryInfo.FullName + "project-cards.html") compilerResult
                saveCanonicalModel programDirectories.DestinationDirectoryInfo.FullName compilerResult
                saveFeatureFiles programDirectories.FeaturesDirectoryInfo.FullName compilerResult
                printCompilerMessages compilerResult.CompilerMessages false
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