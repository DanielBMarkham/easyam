module Utils
    open Types
    open SAModel
    open Lenses
    open System
    open System.Text.RegularExpressions
    open System.Net


    
    /// Prints out the options for the command. Standard stuff.
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
                    printfn "Verbosity: Normal" 
                | Verbosity.BatchVerbose ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (nowString)
                    fnPrintMe()
                | Verbosity.Verbose ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (nowString)
                    fnPrintMe()
                |_ ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (nowString)
                    fnPrintMe()

    let commandLinePrintWhileExit (baseOptions:ConfigBase) =
            // Exiting program command line report
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

    let isLinuxFileSystem =
        let os = Environment.OSVersion
        let platformId = os.Platform
        match platformId with
            | PlatformID.Win32NT | PlatformID.Win32S | PlatformID.Win32Windows | PlatformID.WinCE | PlatformID.Xbox -> false
            | PlatformID.MacOSX | PlatformID.Unix -> true
            | _ ->false
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

    let duplicatesUnique a =
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
    let duplicatesUniqueBy f a =
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

    let duplicates a =
        a |> List.fold(fun acc x->
            let itemCount = (a |> List.filter(fun y->x=y)).Length
            if itemCount>1 then List.append acc [x] else acc
            ) []
    let duplicatesBy f a =
        a |> List.fold(fun acc x->
            let itemCount = (a |> List.filter(fun y->f x y)).Length
            if itemCount>1 then List.append acc [x] else acc
            ) []

    // Funky code. I need some way to pretend I have a real OS incoming file I'm processing
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
