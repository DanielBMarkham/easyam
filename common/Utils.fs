module Utils
    open Types
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
    let drawEntityBox (sw:System.IO.StreamWriter) (xpos:int) (ypos:int) (width:int) (height:int) (fontSize:int) (entity:Entity) =
        let textMargin = (int)((float)fontSize/2.0)
        sw.WriteLine ("<rect x=\"" + xpos.ToString() + "\" y=\"" + ypos.ToString() + "\" height=\"" + height.ToString() + "\" width=\"" + width.ToString() + "\" style=\"stroke:#ff0000; fill: #dddddd\"/>")
        sw.WriteLine ("<text x=\"" + (xpos+textMargin).ToString() + "\" y=\"" + (ypos+fontSize+textMargin).ToString() + "\" font-family=\"Verdana\" font-size=\"" + fontSize.ToString() + "\">")
        sw.WriteLine entity.Title.text
        sw.WriteLine "</text>"
        let dividerYPos = ypos + fontSize * 2
        sw.WriteLine ("<line x1=\"" + xpos.ToString() + "\" y1=\"" + dividerYPos.ToString() + "\" x2=\"" + (xpos + width).ToString() + "\" y2=\"" + dividerYPos.ToString() + "\" stroke-width=\"2\" stroke=\"black\"/>")
        entity.Attributes |> List.iteri(fun i x->
            let attributeTextYPos = dividerYPos + fontSize + (i * fontSize)
            sw.WriteLine ("<text x=\"" + (xpos+textMargin).ToString() + "\" y=\"" + (attributeTextYPos+textMargin).ToString() + "\" font-family=\"Verdana\" font-size=\"" + fontSize.ToString() + "\">")
            sw.WriteLine x.text
            sw.WriteLine "</text>"
            )
        ()
    let createDomainModelDiagram (model:StructureModel) (fileName:string) =
        // entity box metrics
        let gridSize =  (int)(Math.Sqrt((float)model.Entities.Length) + 0.5)
        let maxEntityNameLength = (model.Entities |> List.maxBy(fun x->x.Title.text.Length)).Title.text.Length
        let maxNumberOfEntityAttributes = (model.Entities |> List.maxBy(fun x->x.Attributes.Length)).Attributes.Length
        let entityFontSize=12
        let entityBoxHeight = (maxNumberOfEntityAttributes + 4) *entityFontSize
        let entityBoxWidth = (maxEntityNameLength + 4) * entityFontSize
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
            let colYPos = entityBoxVertSpacer + (ycol * entityBoxHeight + entityBoxVertSpacer) + (entityFontSize*ycol)
            let colXPos = entityBoxHorizSpacer + (xcol * entityBoxWidth + entityBoxHorizSpacer) + (entityFontSize*xcol)
            drawEntityBox svgOutputFile colXPos colYPos entityBoxWidth entityBoxHeight entityFontSize x
            )
//        model.Entities |> List.iteri(fun i x->
//            let newY = 10 + i *110
//            let newYString = newY.ToString()
//            svgOutputFile.WriteLine ("<rect x=\"10\" y=\"" + newYString + "\" height=\"100\" width=\"100\" style=\"stroke:#ff0000; fill: #dddddd\"/>")
//            
//            let newYtext = 34 + i *110
//            svgOutputFile.WriteLine ("<text x=\"20\" y=\"" + newYtext.ToString() + "\" font-family=\"Verdana\" font-size=\"12\">")
//            svgOutputFile.WriteLine x.Title.text
//            svgOutputFile.WriteLine "</text>"
//
//            let newYDividerLine = 60 + i *110
//            svgOutputFile.WriteLine ("<line x1=\"10\" y1=\"" + newYDividerLine.ToString() + "\" x2=\"110\" y2=\"" + newYDividerLine.ToString() + "\" stroke-width=\"2\" stroke=\"black\"/>")
//            x.Attributes |> List.iteri(fun j z->
//                let newYAttributeText = 84 + i *110 + j * 12
//                svgOutputFile.WriteLine ("<text x=\"20\" y=\"" + newYAttributeText.ToString() + "\" font-family=\"Verdana\" font-size=\"12\">")
//                svgOutputFile.WriteLine z.text
//                svgOutputFile.WriteLine "</text>"
//                )
//            )
        svgOutputFile.WriteLine ""
        svgOutputFile.WriteLine ""
        svgOutputFile.WriteLine "</svg>"
        svgOutputFile.Flush()
        svgOutputFile.Close()
        ()
