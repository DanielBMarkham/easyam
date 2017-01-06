module Utils
    open Types
    open SAModel
    open System
    open System.Text.RegularExpressions
    open System.Net


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

    let makeContextAString (genre:Genres) (bucket:Buckets) (temporalIndicator:TemporalIndicators) (abstractionLevel:AbstractionLevels) =
        genre.ToString() + " " + bucket.ToString() + " " + abstractionLevel.ToString() + " " + temporalIndicator.ToString()
    let writeATableCell (sb:System.Text.StringBuilder) s =
        sb.Append ("    <td>" + s + "</td>\n") |> ignore
    let filterModelItemsByTag (modelItems:ModelItem list) (genre:Genres) (abstractionLevel:AbstractionLevels) (temporalIndicator:TemporalIndicators) (bucket:Buckets) =
        modelItems |> List.filter(fun x->
            ((x.Genre=genre) || (genre=Genres.Unknown))
            && ((x.AbstractionLevel=abstractionLevel) || (abstractionLevel=AbstractionLevels.Unknown))
            && ((x.Bucket=bucket) || (bucket=Buckets.Unknown))
            && ((x.TemporalIndicator=temporalIndicator) || (temporalIndicator=TemporalIndicators.Unknown))
            )
    let compiledDumpIncomingModel (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) =
        let fileName="Business-Behavior-Abstract-ToBe.amout"
        System.IO.File.Delete(fileName)
        let sw = System.IO.File.CreateText(fileName)
        sw.WriteLine "BUSINESS BEHAVIOR ABSTRACT TO-BE"
        let businessBehaviorAbstractToBeItems = filterModelItemsByTag modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior
        let masterUserStoryTitles = businessBehaviorAbstractToBeItems |> List.iteri(fun i x->
            match x.ItemType with
                |ModelItemType.ModelItem(_)->
                    sw.WriteLine (x.ModelItemName)
                |_ ->()
            )
        sw.Flush()
        sw.Close()
        // STRUCT
        let fileName="Business-Structure-Abstract-ToBe.amout"
        System.IO.File.Delete(fileName)
        let sw = System.IO.File.CreateText(fileName)
        sw.WriteLine "BUSINESS STRUCTURE ABSTRACT TO-BE"
        let businessBehaviorAbstractToBeItems = filterModelItemsByTag modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Structure
        let masterUserStoryTitles = businessBehaviorAbstractToBeItems |> List.iteri(fun i x->
            match x.ItemType with
                |ModelItemType.ModelItem(_)->
                    sw.WriteLine (x.ModelItemName)
                |_ ->()
            )
        sw.Flush()
        sw.Close()
        // SUPPL
        let fileName="Business-Supplemental-Abstract-ToBe.amout"
        System.IO.File.Delete(fileName)
        let sw = System.IO.File.CreateText(fileName)
        sw.WriteLine "BUSINESS SUPPLEMENTAL ABSTRACT TO-BE"
        let businessBehaviorAbstractToBeItems = filterModelItemsByTag modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Supplemental
        let masterUserStoryTitles = businessBehaviorAbstractToBeItems |> List.iteri(fun i x->
            match x.ItemType with
                |ModelItemType.ModelItem(_)->
                    sw.WriteLine (x.ModelItemName)
                |_ ->()
            )
        sw.Flush()
        sw.Close()
        ()
    let rawDumpIncomingModel (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) =
        let fileName="incomingLines-DEBUG.html"
        System.IO.File.Delete(fileName)
        let sw = System.IO.File.CreateText(fileName)
        sw.WriteLine "<html>"
        sw.WriteLine "<head>"
        sw.WriteLine "</head>"
        sw.WriteLine "<body>"
        sw.WriteLine "<table><thead><tr>"
        sw.WriteLine ("<td>" + "Item Number" + "</td>")
        sw.WriteLine ("<td>" + "Id Number" + "</td>")
        sw.WriteLine ("<td>" + "Parent" + "</td>")
        sw.WriteLine ("<td>" + "Context" + "</td>")
        sw.WriteLine ("<td>" + "Item Type" + "</td>")
        sw.WriteLine ("<td>" + "ShortName" + "</td>")
        sw.WriteLine ("<td>" + "Reference" + "</td>")
        sw.WriteLine ("<td>" + "Reference Line Number" + "</td>")
        sw.WriteLine "</tr></thead>"
        modelItems |> List.iteri(fun i x->
            let sb=new System.Text.StringBuilder(4096)
            sb.Append("<tr>\n") |> ignore
            sb.Append ("<td>" + i.ToString() + "</td>") |> ignore
            writeATableCell sb (x.Id.ToString())
            writeATableCell sb (if x.Parent.IsSome then x.Parent.Value.ToString() else "")
            writeATableCell sb (makeContextAString x.Genre x.Bucket x.TemporalIndicator x.AbstractionLevel)
            writeATableCell sb (x.ItemType.ToString())
            writeATableCell sb (x.ModelItemName)
            writeATableCell sb (x.SourceReferences.Item(x.SourceReferences.Length-1).File.FullName)
            let referenceLineNumber= x.SourceReferences.Item(x.SourceReferences.Length-1).LineNumber.ToString()
            writeATableCell sb (referenceLineNumber)
            sb.Append ("</tr>\n") |> ignore
            sw.WriteLine (sb.ToString())
            )
        sw.WriteLine"</body>"
        sw.WriteLine"</html>"
        sw.Flush()
        sw.Close()


