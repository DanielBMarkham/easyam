module Utils
    open Types
    open SAModel
    open System
    open System.Text.RegularExpressions
    open System.Net
    open GemBox.Spreadsheet

    let commandLinePrintWhileEnter (opts:ConfigBase) fnPrintMe =
                // Entering program command line report
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
                    printfn "Begin: %s" (System.DateTime.Now.ToString())
                | Verbosity.Normal ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (System.DateTime.Now.ToString())
                    printfn "Verbosity: Normal" 
                | Verbosity.BatchVerbose ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (System.DateTime.Now.ToString())
                    fnPrintMe()
                | Verbosity.Verbose ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (System.DateTime.Now.ToString())
                    fnPrintMe()
                |_ ->
                    printfn "%s. %s" opts.programName opts.programTagLine
                    printfn "Begin: %s" (System.DateTime.Now.ToString())
                    fnPrintMe()

    let commandLinePrintWhileExit (baseOptions:ConfigBase) =
            // Exiting program command line report
        match baseOptions.verbose.parameterValue with
            | Verbosity.Silent ->
                ()
            | Verbosity.BatchMinimum ->
                ()
            | Verbosity.Minimum ->
                ()
                //printfn "End %s" baseOptions.programName
            | Verbosity.BatchNormal ->
                printfn "End:   %s" (System.DateTime.Now.ToString())
            | Verbosity.Normal ->
                printfn "End:   %s" (System.DateTime.Now.ToString())
            | Verbosity.BatchVerbose ->
                printfn "End:   %s" (System.DateTime.Now.ToString())
            | Verbosity.Verbose ->
                printfn "End:   %s" (System.DateTime.Now.ToString())
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


    let drawEntityBox (sw:System.IO.StreamWriter) (box:SVGEntityBox) (svgConfig:SVGSetup) =
        sw.WriteLine ("<rect x=\"" + box.xPos.ToString() + "\" y=\"" + box.yPos.ToString() + "\" height=\"" + box.height.ToString() + "\" width=\"" + box.width.ToString() + "\" style=\"stroke:" + svgConfig.EntityBorderColor + "; fill: " + svgConfig.EntityFillColor  + "; fill-opacity: " + svgConfig.EntityFillOpacity  + "\"/>")
        sw.WriteLine ("<text x=\"" + (box.xPos+svgConfig.TextMargin).ToString() + "\" y=\"" + (box.yPos+svgConfig.FontSize+svgConfig.TextMargin).ToString() + "\" font-family=\"Verdana\" font-size=\"" + svgConfig.FontSize.ToString() + "\">")
        sw.WriteLine box.Entity.Title.text
        sw.WriteLine "</text>"
        let dividerYPos = box.yPos + svgConfig.FontSize * 2
        sw.WriteLine ("<line x1=\"" + box.xPos.ToString() + "\" y1=\"" + dividerYPos.ToString() + "\" x2=\"" + (box.xPos + box.width).ToString() + "\" y2=\"" + dividerYPos.ToString() + "\" stroke-width=\"" + svgConfig.EntityBorderWidth + "\" stroke=\"" + svgConfig.EntityBorderColor + "\"/>")
        box.Entity.Attributes |> List.iteri(fun i x->
            let attributeTextYPos = dividerYPos + svgConfig.FontSize + (i * svgConfig.FontSize)
            sw.WriteLine ("<text x=\"" + (box.xPos+svgConfig.TextMargin).ToString() + "\" y=\"" + (attributeTextYPos+svgConfig.TextMargin).ToString() + "\" font-family=\"Verdana\" font-size=\"" + svgConfig.FontSize.ToString() + "\">")
            let attText:string = x.Title.text
            sw.WriteLine (attText)
            sw.WriteLine "</text>"
            )
        ()
    let drawEntityBoxes (sw:System.IO.StreamWriter) (xpos:int) (ypos:int) (width:int) (height:int) (entity:Entity) =
        let newBox =
            {
                xPos=xpos
                yPos=ypos
                width=width
                height=height
                Entity=entity
            }
        drawEntityBox sw newBox defaultSVGSetup

    let csvDumpModelBucketRaw (bucket:Buckets) (compilationList:CompilationLine list) =
        let ef = new ExcelFile()
        let ws = ef.Worksheets.Add(bucket.ToString())
        ws.Cells.[0, 0].Value<-bucket.ToString().ToUpper()

        let labelTitleRow=2
        ws.Cells.[labelTitleRow, 0].Value<-"Labels"
        let labelList = compilationList |> List.filter(fun x->x.LineType=CompilationLineType.Label)
        labelList |> List.iteri(fun i x->
            ws.Cells.[labelTitleRow+i+1, 0].Value<-x.LineText
            )        
        let labelListCount = labelList.Length + 1

        let commandTitleRow=labelTitleRow+labelListCount+1
        ws.Cells.[commandTitleRow, 0].Value<-"Commands"
        let commandList = compilationList |> List.filter(fun x->((x.LineType = CompilationLineType.Command)) && (x.CommandType<>CompilationLineCommands.Comment))
        commandList |> List.iteri(fun i x->
            ws.Cells.[commandTitleRow+i+1, 0].Value<-x.LineText
            )
        let commandListCount = commandList.Length+1


        let questionTitleRow=commandTitleRow+commandListCount+1
        ws.Cells.[questionTitleRow, 0].Value<-"Questions"
        let questionList = compilationList |> List.filter(fun x->((x.LineType=CompilationLineType.Unknown)) && (x.CommandType=CompilationLineCommands.Question))
        questionList |> List.iteri(fun i x->
            ws.Cells.[questionTitleRow+i+1, 0].Value<-x.LineText
            )
        let questionListCount = questionList.Length+1


        let notesTitleRow=questionTitleRow+questionListCount+1
        ws.Cells.[notesTitleRow, 0].Value<-"Notes"
        let notesList = compilationList |> List.filter(fun x->((x.LineType = CompilationLineType.Command)) && (x.CommandType=CompilationLineCommands.Comment))
        notesList |> List.iteri(fun i x->
            ws.Cells.[notesTitleRow+i+1, 0].Value<-x.LineText
            )
        let notesListCount = notesList.Length+1

        let fileName = bucket.ToString() + "-Raw.csv"
        System.IO.File.Delete(fileName)
        ef.Save(fileName)

    ()
    let dumpInputFiles (ctx:CompilationContext) programDirectories = 
        SpreadsheetInfo.SetLicense("FREE-LIMITED-KEY");
        let ef = new ExcelFile()
        let ws = ef.Worksheets.Add("InputFiles")
        ws.Cells.[0, 0].Value<-"InputFileLineList"
        ws.Cells.[1, 0].Value<-"Line Number"
        ws.Cells.[1, 1].Value<-"File Name"
        ws.Cells.[1, 2].Value<-"Line Type"
        ws.Cells.[1, 3].Value<-"Command Type"
        ws.Cells.[1, 4].Value<-"Scope"
        ws.Cells.[1, 5].Value<-"Tagged Context (Raw)"
        ws.Cells.[1, 6].Value<-"Line Text"
        ctx.CompilationLines |> List.iteri(fun i x->
            ws.Cells.[i+2, 0].Value<-i.ToString()
            let filenamecol = if x.File.IsSome then x.File.Value.FullName else ""
            ws.Cells.[i+2, 1].Value<-filenamecol
            ws.Cells.[i+2, 2].Value<-x.LineType.ToString()
            ws.Cells.[i+2, 3].Value<-x.CommandType.ToString()
            ws.Cells.[i+2, 4].Value<-x.Scope
            ws.Cells.[i+2, 5].Value<-x.TaggedContext.ToString()
            ws.Cells.[i+2, 6].Value<-x.LineText
            )
        System.IO.File.Delete("InputFiles.csv")
        ef.Save("InputFiles.csv")

        let ef = new ExcelFile()
        let ws = ef.Worksheets.Add("Open Questions")
        ws.Cells.[0, 0].Value<-"Questions"
        let questions = ctx.CompilationLines |> List.filter(fun x->x.CommandType = CompilationLineCommands.Question)
        questions |> List.iteri(fun i x->
            ws.Cells.[1+i, 0].Value<-x.LineText
            )
        System.IO.File.Delete("questions.csv")
        ef.Save("questions.csv")


    let dumpModelBuckets (domainModel:StructuredAnalysisModel) programDirectories =
        csvDumpModelBucketRaw Buckets.Unknown domainModel.Unknown
        csvDumpModelBucketRaw Buckets.Behavior domainModel.BehaviorModel
        csvDumpModelBucketRaw Buckets.Structure domainModel.StructureModel
        csvDumpModelBucketRaw Buckets.Supplemental domainModel.SupplementalModel



//        let ef = new ExcelFile()
//        let ws = ef.Worksheets.Add("Unknown")
//        ws.Cells.[0, 0].Value<-"Unknown"
//        let questions = ctx.CompilationLines |> List.filter(fun x->x.TaggedContext.Bucket=Buckets.Unknown)
//        questions |> List.iteri(fun i x->
//            ws.Cells.[1+i, 0].Value<-x.LineText
//            )
//        System.IO.File.Delete("Unknown.csv")
//        ef.Save("Unknown.csv")


//        let ef = new ExcelFile()
//        let ws = ef.Worksheets.Add("Behavior")
//        ws.Cells.[0, 0].Value<-"Behavior"
//        let questions = ctx.CompilationLines |> List.filter(fun x->x.TaggedContext.Bucket=Buckets.Behavior)
//        questions |> List.iteri(fun i x->
//            ws.Cells.[1+i, 0].Value<-x.LineText
//            )
//        System.IO.File.Delete("Behavior.csv")
//        ef.Save("Behavior.csv")
//
//
//        let ef = new ExcelFile()
//        let ws = ef.Worksheets.Add("Structure")
//        ws.Cells.[0, 0].Value<-"Structure"
//        let questions = ctx.CompilationLines |> List.filter(fun x->x.TaggedContext.Bucket=Buckets.Structure)
//        questions |> List.iteri(fun i x->
//            ws.Cells.[1+i, 0].Value<-x.LineText
//            )
//        System.IO.File.Delete("Structure.csv")
//        ef.Save("Structure.csv")
//
//
//        let ef = new ExcelFile()
//        let ws = ef.Worksheets.Add("Meta")
//        ws.Cells.[0, 0].Value<-"Meta"
//        let questions = ctx.CompilationLines |> List.filter(fun x->x.TaggedContext.Bucket=Buckets.Meta)
//        questions |> List.iteri(fun i x->
//            ws.Cells.[1+i, 0].Value<-x.LineText
//            )
//        System.IO.File.Delete("Meta.csv")
//        ef.Save("Meta.csv")


//        ()

//    let createDomainModelDiagram (model:StructureModel) (fileName:string) =
//        // entity box metrics
//        let gridSize =  (int)(Math.Sqrt((float)model.Entities.Length) + 0.5)
//        let maxEntityNameLength = (model.Entities |> List.maxBy(fun x->x.Title.text.Length)).Title.text.Length
//        let maxNumberOfEntityAttributes = (model.Entities |> List.maxBy(fun x->x.Attributes.Length)).Attributes.Length
//
//        let entityBoxHeight = (maxNumberOfEntityAttributes + 4) * defaultSVGSetup.FontSize
//        let entityBoxWidth = (maxEntityNameLength + 4) * defaultSVGSetup.FontSize
//        let entityBoxHorizSpacer = 10
//        let entityBoxVertSpacer = 10
//
//        System.IO.File.Delete(fileName)
//        let svgOutputFile = new System.IO.StreamWriter(fileName)
//        svgOutputFile.WriteLine "<svg  xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">" 
//        svgOutputFile.WriteLine ""
//        svgOutputFile.WriteLine ""
//        model.Entities |> List.iteri(fun i x->
//            let ycol = i%gridSize
//            let xcol = (int)i/gridSize
//            let colYPos = entityBoxVertSpacer + (ycol * entityBoxHeight + entityBoxVertSpacer) + (defaultSVGSetup.FontSize*ycol)
//            let colXPos = entityBoxHorizSpacer + (xcol * entityBoxWidth + entityBoxHorizSpacer) + (defaultSVGSetup.FontSize*xcol)
//            drawEntityBoxes svgOutputFile colXPos colYPos entityBoxWidth entityBoxHeight x
//            )
//        svgOutputFile.WriteLine ""
//        svgOutputFile.WriteLine ""
//        svgOutputFile.WriteLine "</svg>"
//        svgOutputFile.Flush()
//        svgOutputFile.Close()
//        ()
