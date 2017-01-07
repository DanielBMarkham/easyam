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
    let writeModelItemDetailHtmlTableHead (swItemDetailTextFileWriter:System.IO.StreamWriter) =
        swItemDetailTextFileWriter.WriteLine "<table><thead><tr>"
        swItemDetailTextFileWriter.WriteLine ("<td>" + "Item Number" + "</td>")
        swItemDetailTextFileWriter.WriteLine ("<td>" + "Id Number" + "</td>")
        swItemDetailTextFileWriter.WriteLine ("<td>" + "Parent" + "</td>")
        swItemDetailTextFileWriter.WriteLine ("<td>" + "Context" + "</td>")
        swItemDetailTextFileWriter.WriteLine ("<td>" + "Item Type" + "</td>")
        swItemDetailTextFileWriter.WriteLine ("<td>" + "ShortName" + "</td>")
        swItemDetailTextFileWriter.WriteLine ("<td>" + "Reference" + "</td>")
        swItemDetailTextFileWriter.WriteLine ("<td>" + "Reference Line Number" + "</td>")
        swItemDetailTextFileWriter.WriteLine "</tr></thead>"
    let writeModelItemDetailHtmlTableDetail (swItemDetailTextFileWriter:System.IO.StreamWriter) (iterationNumber:int) (modelItem:ModelItem) =
        let sb=new System.Text.StringBuilder(4096)
        sb.Append("<tr>\n") |> ignore
        sb.Append ("<td>" + iterationNumber.ToString() + "</td>") |> ignore
        writeATableCell sb (modelItem.Id.ToString())
        writeATableCell sb (if modelItem.Parent.IsSome then modelItem.Parent.Value.ToString() else "")
        writeATableCell sb (makeContextAString modelItem.Genre modelItem.Bucket modelItem.TemporalIndicator modelItem.AbstractionLevel)
        writeATableCell sb (modelItem.ItemType.ToString())
        writeATableCell sb (modelItem.ModelItemName)
        writeATableCell sb (modelItem.SourceReferences.Item(modelItem.SourceReferences.Length-1).File.FullName)
        let referenceLineNumber= modelItem.SourceReferences.Item(modelItem.SourceReferences.Length-1).LineNumber.ToString()
        writeATableCell sb (referenceLineNumber)
        sb.Append ("</tr>\n") |> ignore
        swItemDetailTextFileWriter.WriteLine (sb.ToString())
    let saveDetailedViewHtml (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) (selectedItem:ModelItem) =
        let itemDetailFileName=selectedItem.ModelItemName + ".html"
        System.IO.File.Delete(itemDetailFileName)
        let swItemDetailTextFileWriter = System.IO.File.CreateText(itemDetailFileName)
        swItemDetailTextFileWriter.WriteLine "<html>"
        swItemDetailTextFileWriter.WriteLine "<head>"
        swItemDetailTextFileWriter.WriteLine ("<title>" + selectedItem.ModelItemName + "</title>")
        swItemDetailTextFileWriter.WriteLine "</head>"
        swItemDetailTextFileWriter.WriteLine "<body>"
        swItemDetailTextFileWriter.WriteLine ("<h2>" + selectedItem.ToModelHeading + "</h2>")
        swItemDetailTextFileWriter.WriteLine ("<h1>" + selectedItem.ModelItemName + "</h1>")
        swItemDetailTextFileWriter.WriteLine ()
        swItemDetailTextFileWriter.WriteLine ("<h3><a name='ItemDetail'>Item Detail</h3>")
        swItemDetailTextFileWriter.WriteLine ("<h3><a name='Notes'>Notes</h3>")
        swItemDetailTextFileWriter.WriteLine ("<h3><a name='Questions'>Questions</h3>")
        swItemDetailTextFileWriter.WriteLine ("<h3><a name='ToDo'>To-Do</h3>")
        swItemDetailTextFileWriter.WriteLine ("<h3><a name='Work'>Work</h3>")
        swItemDetailTextFileWriter.WriteLine ("<h3><a name='Realizations'>Realizations</h3>")
        swItemDetailTextFileWriter.WriteLine ("<h3>" + "DEBUG: Incoming Data Dump" + "</h3>")
        writeModelItemDetailHtmlTableHead swItemDetailTextFileWriter
        writeModelItemDetailHtmlTableDetail swItemDetailTextFileWriter 0 selectedItem
        let itemsThatAreRelatedToThisItem = 
            modelItems |> List.filter(fun y->
                y.Parent.IsSome && y.Parent.Value=selectedItem.Id
                )
        itemsThatAreRelatedToThisItem |> List.iteri(fun i j->
            writeModelItemDetailHtmlTableDetail swItemDetailTextFileWriter i j
            )
        swItemDetailTextFileWriter.WriteLine "</table></body></html>"
        swItemDetailTextFileWriter.Flush()
        swItemDetailTextFileWriter.Close()
    let saveMasterIndex =
        let fileName= "index.html"
        System.IO.File.Delete(fileName)
        let sw = System.IO.File.CreateText(fileName)
        sw.WriteLine "<html>"
        sw.WriteLine "<head>"
        sw.WriteLine ("<title>Our Universe</title>")
        sw.WriteLine "</head>"
        sw.WriteLine "<body>"
        sw.WriteLine ("<h1>Our Universe</h1>")
        sw.WriteLine ("<h2>Overview</h2>")
        sw.WriteLine ("<h3><a href='Business Behavior Abstract To-Be.html'>Benefits We Offer Our Clients</a></h3>")
        sw.WriteLine ("<h3><a href='Business Structure Abstract To-Be.html'>Definitions, Terms, and Systems</h3>")
        sw.WriteLine ("<h3><a href='Business Supplemental Abstract To-Be.html'>Rules We Do Our Work By</h3>")
        sw.WriteLine ("<h2>How Things Have Went So Far</h2>")
        sw.WriteLine ("<h3>Completed Work</h3>")
        sw.WriteLine ("<h2>Where We Are Now</h2>")
        sw.WriteLine ("<h3>Open Bugs</h3>")
        sw.WriteLine ("<h3>Work In Progress</h3>")
        sw.WriteLine ("<h2>Where We're Planning To Go</h2>")
        sw.WriteLine ("<h3>Release Plan</h3>")
        sw.WriteLine ("<h3>Roadmap</h3>")
        sw.WriteLine ()
        sw.WriteLine "</body></html>"
        sw.Flush()
        sw.Close()
    let saveDetailedViewAmout (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) (selectedItem:ModelItem) =
        let itemDetailFileName=selectedItem.ModelItemName + ".amout"
        System.IO.File.Delete(itemDetailFileName)
        let swItemDetailTextFileWriter = System.IO.File.CreateText(itemDetailFileName)
        swItemDetailTextFileWriter.WriteLine selectedItem.ToModelHeading
        swItemDetailTextFileWriter.WriteLine("    " + selectedItem.ModelItemName)
        let itemsThatAreRelatedToThisItem = 
            modelItems |> List.filter(fun y->
                y.Parent.IsSome && y.Parent.Value=selectedItem.Id
                )
        swItemDetailTextFileWriter.WriteLine("        NOTES")
        itemsThatAreRelatedToThisItem |> List.iter(fun k->
            match k.ItemType with
                |ModelItemType.Note(a) as n->
                    swItemDetailTextFileWriter.WriteLine("            " + a.Text )
                |_->()
            )
        swItemDetailTextFileWriter.WriteLine("        QUESTIONS")
        itemsThatAreRelatedToThisItem |> List.iter(fun k->
            match k.ItemType with
                |ModelItemType.Question(a) as n->
                    swItemDetailTextFileWriter.WriteLine("            " + a.Text )
                |_->()
            )
        swItemDetailTextFileWriter.WriteLine("        TODO")
        itemsThatAreRelatedToThisItem |> List.iter(fun k->
            match k.ItemType with
                |ModelItemType.ToDo(a) as n->
                    swItemDetailTextFileWriter.WriteLine("            " + a.Text )
                |_->()
            )
        swItemDetailTextFileWriter.WriteLine("        WORK")
        itemsThatAreRelatedToThisItem |> List.iter(fun k->
            match k.ItemType with
                |ModelItemType.Work(a) as n->
                    swItemDetailTextFileWriter.WriteLine("            " + a.Text )
                |_->()
            )
        swItemDetailTextFileWriter.Flush()
        swItemDetailTextFileWriter.Close()

    let compiledDumpIncomingModelHtml (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) =
        let topLevelItemsAndRelated = modelItems |> List.filter(fun x->x.Genre=Genres.Business && x.Bucket=Buckets.Behavior && x.AbstractionLevel=AbstractionLevels.Abstract && x.TemporalIndicator=TemporalIndicators.ToBe)
        let sectionHeading="Business Behavior Abstract To-Be"
        let itemDetailFileName=sectionHeading + ".html"
        System.IO.File.Delete(itemDetailFileName)
        let swItemDetailTextFileWriter = System.IO.File.CreateText(itemDetailFileName)
        swItemDetailTextFileWriter.WriteLine "<html>"
        swItemDetailTextFileWriter.WriteLine "<head>"
        swItemDetailTextFileWriter.WriteLine ("<title>" + sectionHeading + "</title>")
        swItemDetailTextFileWriter.WriteLine "</head>"
        swItemDetailTextFileWriter.WriteLine "<body>"
        swItemDetailTextFileWriter.WriteLine ("<h1>" + sectionHeading + " (" + topLevelItemsAndRelated.Length.ToString() + ")</h1>")
        swItemDetailTextFileWriter.WriteLine ()
        swItemDetailTextFileWriter.WriteLine ("</body></html>")
        let topLevelItems = topLevelItemsAndRelated |> List.filter(fun x->match x.ItemType with |ModelItemType.ModelItem(_)->true |_->false)
        //swItemDetailTextFileWriter.WriteLine "<ul>"
        topLevelItems |> List.iter(fun y->
            let itemChildren = modelItems |> List.filter(fun j->j.Parent.IsSome&&j.Parent.Value=y.Id)
            let modelItemDetailHtmlFileName= y.ModelItemName + ".html"
            swItemDetailTextFileWriter.WriteLine("<h2><a href='" + modelItemDetailHtmlFileName + "'>" + y.ModelItemName + "</a> (" + itemChildren.Length.ToString() + ")</h2>")
            swItemDetailTextFileWriter.WriteLine "<table>"
            swItemDetailTextFileWriter.WriteLine ("<tr><td><a href='" + modelItemDetailHtmlFileName + "#ItemDetail'>Item Detail</a></td>")
            swItemDetailTextFileWriter.WriteLine "<td></td>"
            swItemDetailTextFileWriter.WriteLine ("<td><a href='" + modelItemDetailHtmlFileName + "#Notes'>Notes</a></td>")
            swItemDetailTextFileWriter.WriteLine "<td></td>"
            swItemDetailTextFileWriter.WriteLine ("<td><a href='" + modelItemDetailHtmlFileName + "#Questions'>Questions</a></td>")
            swItemDetailTextFileWriter.WriteLine "<td></td></tr>"
            swItemDetailTextFileWriter.WriteLine ("<tr><td><a href='" + modelItemDetailHtmlFileName + "#ToDo'>To-Dos</a></td>")
            swItemDetailTextFileWriter.WriteLine "<td></td>"
            swItemDetailTextFileWriter.WriteLine ("<td><a href='" + modelItemDetailHtmlFileName + "#Work'>Work</a></td>")
            swItemDetailTextFileWriter.WriteLine "<td></td>"
            swItemDetailTextFileWriter.WriteLine ("<td><a href='" + modelItemDetailHtmlFileName + "#Realizations'>Realizations</a></td>")
            swItemDetailTextFileWriter.WriteLine "<td></td>"
            swItemDetailTextFileWriter.WriteLine "</tr></table>"
            )
        //swItemDetailTextFileWriter.WriteLine "</ul>"
        swItemDetailTextFileWriter.Flush()
        swItemDetailTextFileWriter.Close()
        ()
    let compiledDumpIncomingModelAmout (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) =
        let fileName="Business-Behavior-Abstract-ToBe.amout"
        System.IO.File.Delete(fileName)
        let sw = System.IO.File.CreateText(fileName)
        sw.WriteLine "BUSINESS BEHAVIOR ABSTRACT TO-BE"
        let businessBehaviorAbstractToBeItems = filterModelItemsByTag modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior
        let masterUserStoryTitles = businessBehaviorAbstractToBeItems |> List.iteri(fun i x->
            match x.ItemType with
                |ModelItemType.ModelItem(_)->
                    sw.WriteLine (x.ModelItemName)
                    saveDetailedViewHtml opts modelItems x
                    saveDetailedViewAmout opts modelItems x
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
                    saveDetailedViewHtml opts modelItems x
                    saveDetailedViewAmout opts modelItems x
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
        writeModelItemDetailHtmlTableHead sw
        modelItems |> List.iteri(fun i x->
            writeModelItemDetailHtmlTableDetail sw i x
            )
        sw.WriteLine"</table></body>"
        sw.WriteLine"</html>"
        sw.Flush()
        sw.Close()


