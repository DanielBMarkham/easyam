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
            sw.WriteLine x.text
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
    let layoutEntityModel (model:StructureModel) =
        // Need some way to display a Directed Acyclic Graph (DAG)
        // So that users can make sense of it. Doesn't have to be pretty
        // HACKETY HACK HACK
        model.Entities |> List.map(fun x->
            ()
            )
        ()
    let createDomainModelDiagram (model:StructureModel) (fileName:string) =
        // entity box metrics
        let gridSize =  (int)(Math.Sqrt((float)model.Entities.Length) + 0.5)
        let maxEntityNameLength = (model.Entities |> List.maxBy(fun x->x.Title.text.Length)).Title.text.Length
        let maxNumberOfEntityAttributes = (model.Entities |> List.maxBy(fun x->x.Attributes.Length)).Attributes.Length

        let entityBoxHeight = (maxNumberOfEntityAttributes + 4) * defaultSVGSetup.FontSize
        let entityBoxWidth = (maxEntityNameLength + 4) * defaultSVGSetup.FontSize
        let entityBoxHorizSpacer = 10
        let entityBoxVertSpacer = 10

        System.IO.File.Delete(fileName)
        let svgOutputFile = new System.IO.StreamWriter(fileName)
        svgOutputFile.WriteLine "<svg  xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">" 
        svgOutputFile.WriteLine ""
        svgOutputFile.WriteLine ""
        model.Entities |> List.iteri(fun i x->
            let ycol = i%gridSize
            let xcol = (int)i/gridSize
            let colYPos = entityBoxVertSpacer + (ycol * entityBoxHeight + entityBoxVertSpacer) + (defaultSVGSetup.FontSize*ycol)
            let colXPos = entityBoxHorizSpacer + (xcol * entityBoxWidth + entityBoxHorizSpacer) + (defaultSVGSetup.FontSize*xcol)
            drawEntityBoxes svgOutputFile colXPos colYPos entityBoxWidth entityBoxHeight x
            )
        svgOutputFile.WriteLine ""
        svgOutputFile.WriteLine ""
        svgOutputFile.WriteLine "</svg>"
        svgOutputFile.Flush()
        svgOutputFile.Close()
        ()
    let dumpCSVs (ctx:CompilationContext) = 
        SpreadsheetInfo.SetLicense("FREE-LIMITED-KEY");
        let ef = new ExcelFile()
        let ws = ef.Worksheets.Add("Domain")
        ws.Cells.[0, 0].Value<-"Domain Statements"
        let domainStatements = ctx.CompilationLines |> List.filter(fun x->
            ( (x.CommandType=CompilationLineCommands.Hasa) || (x.CommandType=CompilationLineCommands.Contains))
            && (x.TaggedContext.Bucket=Buckets.Structure)
            && (x.TaggedContext.AbstractionLevel=AbstractionLevels.Abstract)
            && (x.TaggedContext.Genre=Genres.Business))
        domainStatements |> List.iteri(fun i x->
            ws.Cells.[1+i, 0].Value<-x.LineText
            )
        ws.Cells.[domainStatements.Length+1, 0].Value<-"Domain Nouns"
        System.IO.File.Delete("domain.csv")
        ef.Save("domain.csv")

        let ef = new ExcelFile()
        let ws = ef.Worksheets.Add("Open Questions")
        ws.Cells.[0, 0].Value<-"Questions"
        let questions = ctx.CompilationLines |> List.filter(fun x->x.CommandType = CompilationLineCommands.Question)
        questions |> List.iteri(fun i x->
            ws.Cells.[1+i, 0].Value<-x.LineText
            )
        System.IO.File.Delete("questions.csv")
        ef.Save("questions.csv")

        ()
