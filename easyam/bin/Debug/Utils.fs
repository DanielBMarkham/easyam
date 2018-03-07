/// All-purpose utilties for all programs
module Utils
    open Types
    open SAModel
    open Lenses
    open System
    open System.Text.RegularExpressions
    open System.Net


    /// Prints out the options for the command before it runs. Detail level is based on verbosity setting
    let commandLinePrintWhileEnter (opts:ConfigBase) fnPrintMe =
                // Entering program command line report
            let nowString = string System.DateTime.Now
            match opts.verbose.parameterValue with
                | Verbosity.Silent ->
                    ()
                | Verbosity.BatchMinimum ->
                    printfn "%s" opts.programName
                | Verbosity.Minimum ->
                    ()
                    //printfn "Begin %s. %s" opts.programName opts.programTagLine
                | Verbosity.BatchNormal ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (nowString)
                | Verbosity.Normal ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (nowString)
                    printfn "Verbosity: Normal (5)" 
                | Verbosity.BatchVerbose ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Verbosity: BatchVerbose (6)"
                    printfn "Begin: %s" (nowString)
                | Verbosity.Verbose ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Verbosity: Verbose (7)"
                    printfn "Begin: %s" (nowString)
                    fnPrintMe()
                    printfn ""
                |_ ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (nowString)
                    fnPrintMe()

    /// Exiting program command line report. Detail level is based on verbosity setting
    let commandLinePrintWhileExit (baseOptions:ConfigBase) =
        let nowString = string System.DateTime.Now
        match baseOptions.verbose.parameterValue with
            | Verbosity.Silent ->
                ()
            | Verbosity.BatchMinimum ->
                ()
            | Verbosity.Minimum ->
                ()
                //printfn "End %s" baseOptions.programName
            | Verbosity.BatchNormal ->
                printfn "End:   %s" (nowString)
            | Verbosity.Normal ->
                printfn "End:   %s" (nowString)
            | Verbosity.BatchVerbose ->
                printfn "End:   %s" (nowString)
            | Verbosity.Verbose ->
                printfn "End:   %s" (nowString)
            |_ ->
                ()

    let defaultVerbosity  =
        {
            commandLineParameterSymbol="V"
            commandLineParameterName="Verbosity"
            parameterHelpText=[|"/V:[0-9]           -> Amount of trace info to report. 0=none, 5=normal, 9=max."|]           
            parameterValue=Verbosity.Minimum
        }

    let createNewBaseOptions programName programTagLine programHelpText verbose =
        {
            programName = programName
            programTagLine = programTagLine
            programHelpText=programHelpText
            verbose = verbose
            interimProgress = {items=new System.Collections.Generic.Dictionary<string, System.Text.StringBuilder>()}
        }

    let createNewConfigEntry commandlineSymbol commandlineParameterName parameterHelpText initialValue =
        {
            commandLineParameterSymbol=commandlineSymbol
            commandLineParameterName=commandlineParameterName
            parameterHelpText=parameterHelpText
            parameterValue=initialValue
        }

    /// Are we running on linux?
    let isLinuxFileSystem =
        let os = Environment.OSVersion
        let platformId = os.Platform
        match platformId with
            | PlatformID.Win32NT | PlatformID.Win32S | PlatformID.Win32Windows | PlatformID.WinCE | PlatformID.Xbox -> false
            | PlatformID.MacOSX | PlatformID.Unix -> true
            | _ ->false
    /// OS-independent file copy from one place to another. Uses shell.
    let copyToDestinationDirectory (localFileName:string) (copyTo:string) =
        if System.IO.File.Exists(localFileName) = false
            then
                ()
            else
                if not isLinuxFileSystem
                    then
                        let systemProc = new System.Diagnostics.Process()
                        systemProc.EnableRaisingEvents<-false
                        systemProc.StartInfo.FileName<-"cmd.exe"
                        systemProc.StartInfo.Arguments<-("/C copy " + localFileName + " " + copyTo)
                        systemProc.Start() |> ignore
                        systemProc.WaitForExit()                
                    else
                        let systemProc = new System.Diagnostics.Process()
                        systemProc.EnableRaisingEvents<-false
                        systemProc.StartInfo.FileName<-"/bin/cp"
                        systemProc.StartInfo.Arguments<-(" " + localFileName + " " + copyTo)
                        //System.Console.WriteLine (systemProc.StartInfo.FileName + systemProc.StartInfo.Arguments)
                        systemProc.Start() |> ignore
                        systemProc.WaitForExit()

                
    let getOrMakeDirectory dirName =
        if System.IO.Directory.Exists dirName
            then System.IO.DirectoryInfo dirName
            else System.IO.Directory.CreateDirectory dirName

    let forceDirectoryCreation (dir:ConfigEntry<DirectoryParm>) =
        if directoryExists dir
            then (snd dir.parameterValue).Value
            else System.IO.Directory.CreateDirectory(fst dir.parameterValue)

    /// Eliminates duplicate items in a list. Items must be comparable
    let removeDuplicates a =
        a |> List.fold(fun acc x->
            let itemCount = (a |> List.filter(fun y->x=y)).Length
            if itemCount>1
                then
                    if acc |> List.exists(fun y->x=y)
                        then
                            acc
                        else
                            List.append acc [x]
                else
                    acc
            ) []
    /// Eliminates duplicates in a list by evaluating the result of a function on each item
    /// Resulting items must be comparable
    let removeDuplicatesBy f a =
        a |> List.fold(fun acc x->
            let itemCount = (a |> List.filter(fun y->f x y)).Length
            if itemCount>1
                then
                    if acc |> List.exists(fun y->f x y)
                        then
                            acc
                        else
                            List.append acc [x]
                else
                    acc
            ) []
    /// Finds only the duplicates in a list. Items must be comparable
    let duplicates a =
        a |> List.fold(fun acc x->
            let itemCount = (a |> List.filter(fun y->x=y)).Length
            if itemCount>1 then List.append acc [x] else acc
            ) []
    /// finds only the duplicates in a list by applying a function to to each item
    /// new items must be comparable
    let duplicatesBy f a =
        a |> List.fold(fun acc x->
            let itemCount = (a |> List.filter(fun y->f x y)).Length
            if itemCount>1 then List.append acc [x] else acc
            ) []
    let prependToDelimitedList (prependString:string) (currentString:string) (newStringItem:string) =
        let prepend = if currentString.Length=0 || (currentString.GetRight 1) = prependString
                        then ""
                        else prependString.ToString()
        if newStringItem.Length=0 then currentString else
            (currentString + prepend + newStringItem)

    /// Create a dummy file in the OS and return a .NET FileInfo object. Used as a mock for testing
    let getFakeFileInfo() = 
        let tempColl = (new System.CodeDom.Compiler.TempFileCollection(System.AppDomain.CurrentDomain.BaseDirectory, false))
        tempColl.AddExtension("bsx") |> ignore
        let rndPrefix = System.IO.Path.GetRandomFileName()
        let tempFileName = System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, (rndPrefix + "_tester.bsx"))
        tempColl.AddFile(tempFileName,false)
        let fs1=System.IO.File.OpenWrite(tempFileName)
        let sw1=new System.IO.StreamWriter(fs1)
        sw1.WriteLine("test")
        sw1.Close()
        fs1.Close()
        let ret=new System.IO.FileInfo(tempFileName)
        tempColl.Delete()
        ret
    // memoize one to reuse
    let dummyFileInfo = getFakeFileInfo()

    /// Takes a list of FileInfo objects and returns a list tuple of the Info object plus all the lines in each file
    let loadInAllIncomingLines (fileList:System.IO.FileInfo[]) =        
        let fileInfosAndContents:(System.IO.FileInfo*string[]) [] = fileList |> Array.map(fun x->
                                    let contentsForTheFile=System.IO.File.ReadAllLines(x.FullName)
                                    (x,contentsForTheFile)
                                    )
        fileInfosAndContents
    /// Takes a completed model and sorts and filters. Used for reporting and custom output
    let applyCommandLineSortAndFilter (compilerResult:CompilerReturn) (opts:EasyAMProgramConfig) = 
        let sortedModel=
            if opts.sortThing.parameterValue.Length>0
                then
                    let sortParm:sortParameterType =
                        {
                            TagOrAtt=opts.sortTagOrAtt.parameterValue
                            Thing=opts.sortThing.parameterValue
                            ConvertTo=opts.sortConvertTo.parameterValue
                            Order=opts.sortOrder.parameterValue
                        }
                    sortModelByOneParameter compilerResult.ModelItems sortParm
                else compilerResult.ModelItems
        let filteredAndSortedModel =
            if opts.filterThing.parameterValue.Length>0
                then 
                    let filterSortParm:sortParameterType =
                        {
                            TagOrAtt=opts.filterTagOrAtt.parameterValue
                            Thing=opts.filterThing.parameterValue
                            ConvertTo=opts.filterConvertTo.parameterValue
                            Order=opts.filterOrder.parameterValue
                        }
                    let filterParm:FilterParmeterType=
                        {
                            Genre=opts.filterGenre.parameterValue
                            Bucket=opts.filterBucket.parameterValue
                            AbstractionLevel=opts.filterAbstractionLevel.parameterValue
                            TemporalIndicator=opts.filterTemporalIndicator.parameterValue
                            CheckValue=filterSortParm
                            FromVal=opts.filterFromVal.parameterValue
                            ToVal=opts.filterToVal.parameterValue
                        }
                    filterModelByOneParameter sortedModel filterParm
                else compilerResult.ModelItems
        filteredAndSortedModel


    