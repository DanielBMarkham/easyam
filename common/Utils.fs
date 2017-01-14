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
    /// writeHtmlBeginAndHead (sw:System.IO.StreamWriter) (title:string) (description:string) (author:string) (fileShortName:string)
    let writeHtmlBeginAndHead (sw:System.IO.StreamWriter) (title:string) (description:string) (author:string) (fileShortName:string) =
        sw.WriteLine("<!DOCTYPE html> ")
        sw.WriteLine("<html lang='en'> ")
        sw.WriteLine("    <head> ")
        sw.WriteLine("        <meta http-equiv='content-type' content='text/html; charset=UTF-8'>  ")
        sw.WriteLine("        <meta charset='utf-8'> ")
        sw.WriteLine("        <link rel='SHORTCUT ICON' href='favico.png'/> ")
        sw.WriteLine("        <title>" + title + "</title> ")
        sw.WriteLine("        <meta itemprop='name' content='" + title + "'> ")
        sw.WriteLine("        <meta itemprop='description' content='" + description + "'> ")
        sw.WriteLine("        <meta name='description' content='" + description + "' /> ")
        sw.WriteLine("        <meta itemprop='image' content='" + fileShortName + ".png'> ")
        sw.WriteLine("        <meta name='author' content='" + author + "' /> ")
        sw.WriteLine("        <meta name='viewport' content='width=device-width, initial-scale=1, maximum-scale=1'> ")
        sw.WriteLine("        <link href='main.css' type='text/css' rel='stylesheet'> ")
        sw.WriteLine("        <link href='" + fileShortName + ".css' type='text/css' rel='stylesheet'> ")
        sw.WriteLine("    </head> ")

    type ItemReport =
        {
            ItemDetail:ModelItem list
            Notes:ModelItem list
            Questions:ModelItem list
            ToDos:ModelItem List
            Work:ModelItem list
            Realizations:ModelItem list
            Feedback:ModelItem list
        }
        with member self.AllTogether =
                List.concat [self.ItemDetail; self.Notes; self.Questions; self.ToDos; self.Work; self.Realizations; self.Feedback]
    let createItemReport (modelItems:ModelItem list) (selectedModelItem:ModelItem) =
        let collectedNotes= modelItems |> List.filter(fun x->
            match x.ItemType with
                |Note(_)->x.ModelParent=selectedModelItem.Id
                |_->false
            )
        let collectedQuestions= modelItems |> List.filter(fun x->
            match x.ItemType with
                |Question(_)->x.ModelParent=selectedModelItem.Id
                |_->false
            )
        let collectedToDos= modelItems |> List.filter(fun x->
            match x.ItemType with
                |ToDo(_)->x.ModelParent=selectedModelItem.Id
                |_->false
            )
        let collectedWork= modelItems |> List.filter(fun x->
            match x.ItemType with
                |Work(_)->x.ModelParent=selectedModelItem.Id
                |_->false
            )
        let collectedRealizations= modelItems |> List.filter(fun x->
            match x.ItemType with
                |ModelItem(_)->x.ModelParent=selectedModelItem.Id
                |_->false
            )
        {
            ItemDetail=[]
            Notes=collectedNotes
            Questions=collectedQuestions
            ToDos=collectedToDos
            Work=collectedWork
            Realizations=collectedRealizations
            Feedback=[]
        }
    let makeContextAString (genre:Genres) (bucket:Buckets) (temporalIndicator:TemporalIndicators) (abstractionLevel:AbstractionLevels) =
        genre.ToString() + " " + bucket.ToString() + " " + abstractionLevel.ToString() + " " + temporalIndicator.ToString()
    let writeATableCell (sb:System.Text.StringBuilder) s =
        sb.Append ("    <td>" + s + "</td>\n") |> ignore
    let getModelItemById (modelItems:ModelItem list) (id:int) =
        modelItems |> List.find(fun x->x.Id=id)
    let filterModelItemsByTag (modelItems:ModelItem list) (genre:Genres) (abstractionLevel:AbstractionLevels) (temporalIndicator:TemporalIndicators) (bucket:Buckets) =
        modelItems |> List.filter(fun x->
            ((x.Genre=genre) || (genre=Genres.Unknown))
            && ((x.AbstractionLevel=abstractionLevel) || (abstractionLevel=AbstractionLevels.Unknown))
            && ((x.Bucket=bucket) || (bucket=Buckets.Unknown))
            && ((x.TemporalIndicator=temporalIndicator) || (temporalIndicator=TemporalIndicators.Unknown))
            )
    let filterModelItemsByNOTType (modelItemTypeName:string) (modelItems:ModelItem list) =
        let modelItemTypeNameLength = modelItemTypeName.Length
        modelItems |> List.filter(fun x->
            let indexedItemTypeName = x.ItemType.ToString()
            let xItemTypeString = x.ItemType.ToString()
            let xItemTypeStringLength =xItemTypeString.Length
            (xItemTypeStringLength<modelItemTypeNameLength) 
            || ((xItemTypeString.GetLeft modelItemTypeNameLength) <> modelItemTypeName)
            )
    let filterModelItemsByType (modelItemTypeName:string) (modelItems:ModelItem list) =
        let modelItemTypeNameLength = modelItemTypeName.Length
        modelItems |> List.filter(fun x->
            let indexedItemTypeName = x.ItemType.ToString()
            let xItemTypeString = x.ItemType.ToString()
            let xItemTypeStringLength =xItemTypeString.Length
            (xItemTypeStringLength>modelItemTypeNameLength) 
            && ((xItemTypeString.GetLeft modelItemTypeNameLength) = modelItemTypeName)
            )
    let filterModelItemsByTagAndType (modelItems:ModelItem list) (genre:Genres) (abstractionLevel:AbstractionLevels) (temporalIndicator:TemporalIndicators) (bucket:Buckets) (modelItemTypeName:string):ModelItem list=
        let filteredByTags = filterModelItemsByTag modelItems genre abstractionLevel temporalIndicator bucket
        filteredByTags |> filterModelItemsByType  modelItemTypeName
    let itemsThatAreRelatedToThisItem (thisItem:ModelItem) (modelItems:ModelItem list)= 
        modelItems |> List.filter(fun y->
            y.ModelParent=thisItem.Id
            )
    let getAllRootItemsForTags (modelItems:ModelItem list) (genre:Genres) (abstractionLevel:AbstractionLevels) (temporalIndicator:TemporalIndicators) (bucket:Buckets) =
        let itemsFilteredByTag = filterModelItemsByTag modelItems genre abstractionLevel temporalIndicator bucket
        let modelItemsWithContextShiftParentsOrNoParent = itemsFilteredByTag |> List.filter(fun x->
            match x.SourceCodeParent with
                |0->true // parent is empty
                |parent->
                    match (modelItems|>List.tryFind(fun y->y.Id=parent)) with
                        | Some realParent->
                            match realParent.ItemType with
                                | ModelItemType.ContextShift(ctx)->true // parent is a context shift, which makes this a root item
                                |_->false // parent is anything else
                        | option.None->true // parent doesn't exist
            )
        let rootItemsOfAnyType = modelItemsWithContextShiftParentsOrNoParent |> List.filter(fun x->match x.ItemType with |ModelItemType.ContextShift(_)->false|ModelItemType.ModelItem(_)->true|_->false)
        rootItemsOfAnyType
    let getModelItemRootItems (modelItems:ModelItem list) (genre:Genres) (abstractionLevel:AbstractionLevels) (temporalIndicator:TemporalIndicators) (bucket:Buckets) =
        let rootItemsOfAnyType = getAllRootItemsForTags modelItems genre abstractionLevel temporalIndicator bucket
        let rootItemsOfModelItemType = rootItemsOfAnyType |> List.filter(fun x->
            match x.ItemType,x.SourceCodeParent with
                |ModelItemType.ModelItem(_), myParent->true
                |_,_ ->false
            )
        rootItemsOfModelItemType

    let getRootLevelUnattachedSubItems (modelItems:ModelItem list) (genre:Genres) (abstractionLevel:AbstractionLevels) (temporalIndicator:TemporalIndicators) (bucket:Buckets) (modelItemTypeName:string):ModelItem list=
        let allItems=filterModelItemsByTagAndType modelItems genre abstractionLevel temporalIndicator bucket modelItemTypeName
        //let allQuestions = modelItems |> List.filter(fun x->match x.ItemType with |Question(q)->true |_->false)
        // has no parent or the parent is a context shift
        let unattachedItemsWithoutTagFilter = allItems |> List.filter(fun x->
            match x.SourceCodeParent with
                | 0->false
                | id->
                    let parent = getModelItemById modelItems id
                    let parentItemType=parent.ItemType.ToString()
                    let lengthOk=parentItemType.Length>13
                    let leftSide=parentItemType.GetLeft 13
                    let leftSideMatches=leftSide = "CONTEXT SHIFT"
                    (lengthOk && leftSideMatches)
            )
        let unattachedItems = filterModelItemsByTag unattachedItemsWithoutTagFilter Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior
        unattachedItems

    let writeModelItemDetailHtmlTableHead (swItemDetailTextFileWriter:System.IO.StreamWriter) =
        swItemDetailTextFileWriter.WriteLine "<table><thead><tr>"
        swItemDetailTextFileWriter.WriteLine ("<td>" + "Item Number" + "</td>")
        swItemDetailTextFileWriter.WriteLine ("<td>" + "Id Number" + "</td>")
        swItemDetailTextFileWriter.WriteLine ("<td>" + "Source<br/>Code</br>Parent" + "</td>")
        swItemDetailTextFileWriter.WriteLine ("<td>" + "Model</br>Parent" + "</td>")
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
        writeATableCell sb (if modelItem.SourceCodeParent<>0 then modelItem.SourceCodeParent.ToString() else "")
        writeATableCell sb (if modelItem.ModelParent<>0 then modelItem.ModelParent.ToString() else "")
        writeATableCell sb (makeContextAString modelItem.Genre modelItem.Bucket modelItem.TemporalIndicator modelItem.AbstractionLevel)
        writeATableCell sb (modelItem.ItemType.ToString())
        writeATableCell sb (modelItem.ModelItemName)
        writeATableCell sb (modelItem.SourceReferences.Item(modelItem.SourceReferences.Length-1).File.FullName)
        let referenceLineNumber= modelItem.SourceReferences.Item(modelItem.SourceReferences.Length-1).LineNumber.ToString()
        writeATableCell sb (referenceLineNumber)
        sb.Append ("</tr>\n") |> ignore
        swItemDetailTextFileWriter.WriteLine (sb.ToString())
    let saveDetailedViewHtml (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) (selectedItem:ModelItem) =
        let relatedItems = createItemReport modelItems selectedItem //itemsThatAreRelatedToThisItem selectedItem modelItems
        let itemDetailFileName=selectedItem.ToFileName + ".html"
        System.IO.File.Delete(itemDetailFileName)
        let swItemDetailTextFileWriter = System.IO.File.CreateText(itemDetailFileName)
        writeHtmlBeginAndHead swItemDetailTextFileWriter selectedItem.ModelItemName (selectedItem.ModelItemName + ": Detailed View") "EasyAM" selectedItem.ToFileName
        swItemDetailTextFileWriter.WriteLine "<body>"
        swItemDetailTextFileWriter.WriteLine ("<h2>" + selectedItem.ToModelHeading + "</h2>")
        swItemDetailTextFileWriter.WriteLine ("<h1>" + selectedItem.ModelItemName + "</h1>")
        swItemDetailTextFileWriter.WriteLine ()
        if relatedItems.ItemDetail.Length>0
            then
                swItemDetailTextFileWriter.WriteLine ("<h3><a name='ItemDetail'>Item Detail</h3>")
            else ()
        if relatedItems.Notes.Length>0
            then
                swItemDetailTextFileWriter.WriteLine ("<h3><a name='Notes'>Notes</h3>")
                swItemDetailTextFileWriter.WriteLine("           <ul> ")
                relatedItems.Notes |> List.iter(fun k->
                    match k.ItemType with
                        |ModelItemType.Note(a) as n->
                            swItemDetailTextFileWriter.WriteLine("               <li> " + a.Text + "</li>")
                        |_->()
                    )
                swItemDetailTextFileWriter.WriteLine("           </ul> ")
            else ()
        if relatedItems.Questions.Length>0
            then
                swItemDetailTextFileWriter.WriteLine ("<h3><a name='Questions'>Questions</h3>")
                swItemDetailTextFileWriter.WriteLine("           <ul> ")
                relatedItems.Questions |> List.iter(fun k->
                    match k.ItemType with
                        |ModelItemType.Question(a) as n->
                            swItemDetailTextFileWriter.WriteLine("               <li> " + a.Text + "</li>")
                        |_->()
                    )
                swItemDetailTextFileWriter.WriteLine("           </ul> ")
            else ()
        if relatedItems.ToDos.Length>0
            then
                swItemDetailTextFileWriter.WriteLine ("<h3><a name='ToDo'>To-Do</h3>")
                swItemDetailTextFileWriter.WriteLine("           <ul> ")
                relatedItems.ToDos |> List.iter(fun k->
                    match k.ItemType with
                        |ModelItemType.ToDo(a) as n->
                            swItemDetailTextFileWriter.WriteLine("               <li> " + a.Text + "</li>")
                        |_->()
                    )
                swItemDetailTextFileWriter.WriteLine("           </ul> ")
            else ()
        if relatedItems.Work.Length>0
            then
                swItemDetailTextFileWriter.WriteLine ("<h3><a name='Work'>Work</h3>")
                swItemDetailTextFileWriter.WriteLine("           <ul> ")
                relatedItems.Work |> List.iter(fun k->
                    match k.ItemType with
                        |ModelItemType.Work(a) as n->
                            swItemDetailTextFileWriter.WriteLine("               <li> " + a.Text + "</li>")
                        |_->()
                    )
                swItemDetailTextFileWriter.WriteLine("           </ul> ")
            else ()
        if relatedItems.Realizations.Length>0
            then
                swItemDetailTextFileWriter.WriteLine ("<h3><a name='Realizations'>Realizations</h3>")
                swItemDetailTextFileWriter.WriteLine("           <ul> ")
                relatedItems.Realizations |> List.iter(fun k->
                    match k.ItemType with
                        |ModelItemType.ModelItem(a) as n->
                            swItemDetailTextFileWriter.WriteLine("               <li> " + a.ModelItemName + " <a href='" + a.ToFileName + ".html'>Related</a></li>")
                        |_->()
                    )
                swItemDetailTextFileWriter.WriteLine("           </ul> ")
            else ()
        if relatedItems.Feedback.Length>0
            then
                swItemDetailTextFileWriter.WriteLine ("<h3><a name='Feedback'>Feedback</h3>")
            else ()
        swItemDetailTextFileWriter.WriteLine("</table>")
        swItemDetailTextFileWriter.WriteLine("<p><a href='" + (selectedItem.ToFileName  + ".amout") + "'>Code</a></p>")
        let k=selectedItem.ToAbbreviatedModelHeading
        swItemDetailTextFileWriter.WriteLine ("<h3>" + "DEBUG: Incoming Data Dump" + "</h3>")
        writeModelItemDetailHtmlTableHead swItemDetailTextFileWriter
        writeModelItemDetailHtmlTableDetail swItemDetailTextFileWriter 0 selectedItem
        modelItems |> List.iteri(fun i j->
            if j.ModelParent=selectedItem.Id
                then writeModelItemDetailHtmlTableDetail swItemDetailTextFileWriter i j
                else ()
            )
        swItemDetailTextFileWriter.WriteLine "</body></html>"
        swItemDetailTextFileWriter.Flush()
        swItemDetailTextFileWriter.Close()
    let saveMasterIndex opts modelItems =
        let fileName= "index.html"
        System.IO.File.Delete(fileName)
        let sw = System.IO.File.CreateText(fileName)
        writeHtmlBeginAndHead sw "Our Universe" "Everything We are Working On" "EasyAM" "index"
        sw.WriteLine "<body>"
        sw.WriteLine ("<h1>Our Universe</h1>")
        let unassignedQuestions = modelItems |> List.filter(fun x->
            match x.ItemType with
                |Question(_)->
                    ( (x.Genre=Genres.None) || (x.Genre=Genres.Unknown))
                            && ((x.AbstractionLevel=AbstractionLevels.None) || (x.AbstractionLevel=AbstractionLevels.Unknown))
                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
                            && ((x.Bucket=Buckets.None) || (x.Bucket=Buckets.Unknown))
                |_->false
            )
        if unassignedQuestions.Length>0
            then
                sw.WriteLine ("<h2>Questions</h2>")
                unassignedQuestions |> List.iter(fun x->match x.ItemType with |Question(q)->sw.WriteLine("<p>" + q.Text + "</p>") |_->())   
            else ()
        sw.WriteLine ("<h2>Overview</h2>")
        sw.WriteLine ("<h3><a href='business-behavior-abstract-to-be.html'>Benefits We Offer Our Clients</a></h3>")
        sw.WriteLine ("<h3><a href='business-structure-abstract-to-be.html'>Definitions, Terms, and Systems</h3>")
        sw.WriteLine ("<h3><a href='business-supplemental-abstract-to-be.html'>Rules We Do Our Work By</h3>")
        sw.WriteLine ("<h2>How Things Have Went So Far</h2>")
        sw.WriteLine ("<h3>Completed Work</h3>")
        sw.WriteLine ("<h2>Where We Are Now</h2>")
        sw.WriteLine ("<h3><a href='questions.html'>What We Need To Know To Do Our Job</a></h3>")
        sw.WriteLine ("<h3>Open Bugs</h3>")
        sw.WriteLine ("<h3>Work In Progress</h3>")
        sw.WriteLine ("<h2>Where We're Planning To Go</h2>")
        sw.WriteLine ("<h3>Release Plan</h3>")
        sw.WriteLine ("<h3>Roadmap</h3>")
        let unassignedNotes = modelItems |> List.filter(fun x->
            match x.ItemType with
                |Note(_)->
                    ( (x.Genre=Genres.None) || (x.Genre=Genres.Unknown))
                            && ((x.AbstractionLevel=AbstractionLevels.None) || (x.AbstractionLevel=AbstractionLevels.Unknown))
                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
                            && ((x.Bucket=Buckets.None) || (x.Bucket=Buckets.Unknown))
                |_->false
            )
        if unassignedNotes.Length>0
            then
                sw.WriteLine ("<h2>Notes</h2>")
                unassignedNotes |> List.iter(fun x->match x.ItemType with |Note(q)->sw.WriteLine("<p>" + q.Text + "</p>") |_->())   
            else ()
        sw.WriteLine ()
        sw.WriteLine "</body></html>"
        sw.Flush()
        sw.Close()

    let saveMasterQuestionList (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) =
        let fileName= "questions.html"
        let allQuestions = modelItems |> List.filter(fun x->match x.ItemType with |Question(_)->true |_->false)
        let unassignedBusinessQuestions = allQuestions |> List.filter(fun x->
                    ( (x.Genre=Genres.Business))
                            && ((x.AbstractionLevel=AbstractionLevels.None) || (x.AbstractionLevel=AbstractionLevels.Unknown))
                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
                            && ((x.Bucket=Buckets.None) || (x.Bucket=Buckets.Unknown)))
        let unassignedBusinessBehaviorQuestions = allQuestions |> List.filter(fun x->
                    ( (x.Genre=Genres.Business))
                            && ((x.AbstractionLevel=AbstractionLevels.None) || (x.AbstractionLevel=AbstractionLevels.Unknown))
                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
                            && ((x.Bucket=Buckets.Behavior) ))
        let unassignedBusinessStructureQuestions = allQuestions |> List.filter(fun x->
                    ( (x.Genre=Genres.Business))
                            && ((x.AbstractionLevel=AbstractionLevels.None) || (x.AbstractionLevel=AbstractionLevels.Unknown))
                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
                            && ((x.Bucket=Buckets.Structure) ))
        let unassignedBusinessSupplementalQuestions = allQuestions |> List.filter(fun x->
                    ( (x.Genre=Genres.Business))
                            && ((x.AbstractionLevel=AbstractionLevels.None) || (x.AbstractionLevel=AbstractionLevels.Unknown))
                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
                            && ((x.Bucket=Buckets.Supplemental) ))
        let unassignedBusinessBehaviorRealizedQuestions = allQuestions |> List.filter(fun x->
                    ( (x.Genre=Genres.Business))
                            && ((x.AbstractionLevel=AbstractionLevels.Realized) )
                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
                            && ((x.Bucket=Buckets.Behavior) ))
        let unassignedBusinessStructureRealizedQuestions = allQuestions |> List.filter(fun x->
                    ( (x.Genre=Genres.Business))
                            && ((x.AbstractionLevel=AbstractionLevels.Realized))
                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
                            && ((x.Bucket=Buckets.Structure) ))
        let unassignedBusinessSupplementalRealizedQuestions = allQuestions |> List.filter(fun x->
                    ( (x.Genre=Genres.Business))
                            && ((x.AbstractionLevel=AbstractionLevels.None))
                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
                            && ((x.Bucket=Buckets.Supplemental) ))
        System.IO.File.Delete(fileName)
        let sw = System.IO.File.CreateText(fileName)
        writeHtmlBeginAndHead sw "What We Need To Know" "Questions We Need Answers To" "EasyAM" "questions"
        sw.WriteLine "<body>"
        sw.WriteLine ("<h1>What we need to know</h1>")
        sw.WriteLine ("<h2>Business-Related Questions</h2>")
        sw.WriteLine ("<h3>Overall Business Questions</h3>")
        sw.WriteLine ("<ul>")
        unassignedBusinessQuestions |> List.iteri(fun i x->
            match x.ItemType with
                |Question(q)->
                    sw.WriteLine ("<li>" + q.Text + " <a href='" + ("business-behavior-abstract-to-be" + ".amout") + "'>(Related material):</a>" + "</li>")
                |_->()
            )
        sw.WriteLine ("</ul>")
        sw.WriteLine ("<h3>Questions about the way in general people do the kinds of things we want to help folks with</h3>")
        let allModelItemsForThisSection = getModelItemRootItems modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior
        let allQuestions = filterModelItemsByTagAndType modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior "QUESTION"
        allModelItemsForThisSection |> List.iteri(fun i x->
            let questionsForThisModelItem = allQuestions |> List.filter(fun y->
                match y.SourceCodeParent with |0->false |parent->parent=x.Id)
            match questionsForThisModelItem.Length with
                |0->()
                |_->
                    sw.WriteLine("<li>For " + x.ModelItemName + " <a href='" + (x.ToFileName + ".amout") + "'>(Related material):</a>")
                    sw.WriteLine ("<ul>")
                    questionsForThisModelItem |> List.iteri(fun j y->
                        match y.ItemType with
                            |Question(q)->sw.WriteLine ("<li>" + q.Text + "</li>")
                            |_->()
                        )
                    sw.WriteLine ("</ul>")            
                    sw.WriteLine("</li>")
            )
        sw.WriteLine ("<h3>Questions about specific ways of doing things our target audience does that we're going to help them with</h3>")
        let allModelItemsForThisSection = getModelItemRootItems modelItems Genres.Business AbstractionLevels.Realized TemporalIndicators.ToBe Buckets.Behavior
        allModelItemsForThisSection |> List.iteri(fun i x->
            let questionsForThisModelItem = allQuestions |> List.filter(fun y->
                match y.SourceCodeParent with |0->false |parent->parent=x.Id)
            match questionsForThisModelItem.Length with
                |0->()
                |_->
                    sw.WriteLine("<li>For " + x.ModelItemName + " <a href='" + (x.ToFileName + ".amout") + "'>(Related material):</a>")
                    sw.WriteLine ("<ul>")
                    questionsForThisModelItem |> List.iteri(fun j y->
                        match y.ItemType with
                            |Question(q)->sw.WriteLine ("<li>" + q.Text + "</li>")
                            |_->()
                        )
                    sw.WriteLine ("</ul>")            
                    sw.WriteLine("</li>")
            )

        sw.WriteLine ("<h2>Overall Technology Capability And Functional Flow (Not Structure) Questions</h2>")
        sw.WriteLine ("<h3>Questions about the general functional flow our technology uses to help folks</h3>")
        sw.WriteLine ("<h3>Questions about the specific technology flow we use in a certain context to help folks</h3>")

        sw.WriteLine ()
        sw.WriteLine ("<h3>" + "DEBUG: Incoming Data Dump" + "</h3>")
        writeModelItemDetailHtmlTableHead sw
        allQuestions |> List.iteri(fun i j->
            writeModelItemDetailHtmlTableDetail sw i j
            )
        sw.WriteLine "</body></html>"
        sw.Flush()
        sw.Close()

    let saveDetailedViewAmout (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) (selectedItem:ModelItem) =
        let relatedItems = createItemReport  modelItems selectedItem
        let itemDetailFileName=selectedItem.ToFileName + ".amout"
        System.IO.File.Delete(itemDetailFileName)
        let swItemDetailTextFileWriter = System.IO.File.CreateText(itemDetailFileName)
        swItemDetailTextFileWriter.WriteLine selectedItem.ToModelHeading
        swItemDetailTextFileWriter.WriteLine("    " + selectedItem.ModelItemName)
        swItemDetailTextFileWriter.WriteLine("        NOTES")
        relatedItems.Notes |> List.iter(fun k->
            match k.ItemType with
                |ModelItemType.Note(a) as n->
                    swItemDetailTextFileWriter.WriteLine("            " + a.Text )
                |_->()
            )
        swItemDetailTextFileWriter.WriteLine("        QUESTIONS")
        relatedItems.Questions |> List.iter(fun k->
            match k.ItemType with
                |ModelItemType.Question(a) as n->
                    swItemDetailTextFileWriter.WriteLine("            " + a.Text )
                |_->()
            )
        swItemDetailTextFileWriter.WriteLine("        TODO")
        relatedItems.ToDos |> List.iter(fun k->
            match k.ItemType with
                |ModelItemType.ToDo(a) as n->
                    swItemDetailTextFileWriter.WriteLine("            " + a.Text )
                |_->()
            )
        swItemDetailTextFileWriter.WriteLine("        WORK")
        relatedItems.Work |> List.iter(fun k->
            match k.ItemType with
                |ModelItemType.Work(a) as n->
                    swItemDetailTextFileWriter.WriteLine("            " + a.Text )
                |_->()
            )
        swItemDetailTextFileWriter.WriteLine("")
        swItemDetailTextFileWriter.WriteLine("        REALIZATIONS")
        relatedItems.Realizations |> List.iter(fun k->
            match k.ItemType with
                |ModelItemType.ModelItem(a) as n->
                    swItemDetailTextFileWriter.WriteLine("            " + a.ModelItemName )
                |_->()
            )
        swItemDetailTextFileWriter.WriteLine("")
        swItemDetailTextFileWriter.WriteLine("        FEEDBACK")

        swItemDetailTextFileWriter.Flush()
        swItemDetailTextFileWriter.Close()
    let saveCompiledModelSectionHtml (opts:Types.EasyAMProgramConfig) (bucketToSave:Buckets) (genreToSave:Genres) (modelItems:ModelItem list) =
        let topLevelItemsAndRelated = filterModelItemsByTag modelItems genreToSave AbstractionLevels.Abstract TemporalIndicators.ToBe bucketToSave
        //modelItems |> List.filter(fun x->x.Genre=Genres.Business && x.Bucket=Buckets.Behavior && x.AbstractionLevel=AbstractionLevels.Abstract && x.TemporalIndicator=TemporalIndicators.ToBe)
        let sectionHeading=genreToSave.ToString() + " " + bucketToSave.ToString() + " " + "Abstract To-Be"  //"Business Behavior Abstract To-Be"
        let itemDetailFileName=sectionHeading.CovertIntoOSSafeFileName + ".html"
        System.IO.File.Delete(itemDetailFileName)
        let swItemDetailTextFileWriter = System.IO.File.CreateText(itemDetailFileName)
        writeHtmlBeginAndHead swItemDetailTextFileWriter sectionHeading "Section Heading" "EasyAM" itemDetailFileName
        swItemDetailTextFileWriter.WriteLine "<body>"
        swItemDetailTextFileWriter.WriteLine ("<h1>" + sectionHeading + " (" + topLevelItemsAndRelated.Length.ToString() + ")</h1>")
        swItemDetailTextFileWriter.WriteLine ()
        swItemDetailTextFileWriter.WriteLine ("</body></html>")
        let topLevelItems = topLevelItemsAndRelated |> List.filter(fun x->match x.ItemType with |ModelItemType.ModelItem(_)->true |_->false)
        topLevelItems |> List.iter(fun y->
            //let itemChildren = modelItems |> List.filter(fun j->j.SourceCodeParent<>0&&j.SourceCodeParent=y.Id)
            let itemChildren = createItemReport modelItems y
            let itemChildrenCount = itemChildren.Feedback.Length + itemChildren.ItemDetail.Length + itemChildren.Notes.Length + itemChildren.Questions.Length + itemChildren.Realizations.Length + itemChildren.ToDos.Length + itemChildren.Work.Length
            let modelItemDetailHtmlFileName= y.ToFileName + ".html"
            swItemDetailTextFileWriter.WriteLine("<h2><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "'>" + y.ModelItemName + "</a> (" + itemChildrenCount.ToString() + ")</h2>")
            swItemDetailTextFileWriter.WriteLine "<table>"
            swItemDetailTextFileWriter.WriteLine ("<tr><td><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "#ItemDetail'>Item Detail</a> (" + itemChildren.ItemDetail.Length.ToString() + ")</td>")
            swItemDetailTextFileWriter.WriteLine "<td></td>"
            swItemDetailTextFileWriter.WriteLine ("<td><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "#Notes'>Notes</a> (" + itemChildren.Notes.Length.ToString() + ")</td>")
            swItemDetailTextFileWriter.WriteLine "<td></td>"
            swItemDetailTextFileWriter.WriteLine ("<td><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "#Questions'>Questions</a> (" + itemChildren.Questions.Length.ToString() + ")</td>")
            swItemDetailTextFileWriter.WriteLine "<td></td></tr>"
            swItemDetailTextFileWriter.WriteLine ("<tr><td><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "#ToDo'>To-Dos</a> (" + itemChildren.ToDos.Length.ToString() + ")</td>")
            swItemDetailTextFileWriter.WriteLine "<td></td>"
            swItemDetailTextFileWriter.WriteLine ("<td><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "#Work'>Work</a> (" + itemChildren.Work.Length.ToString() + ")</td>")
            swItemDetailTextFileWriter.WriteLine "<td></td>"
            swItemDetailTextFileWriter.WriteLine ("<td><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "#Realizations'>Realizations</a> (" + itemChildren.Realizations.Length.ToString() + ")</td>")
            swItemDetailTextFileWriter.WriteLine "<td></td>"
            swItemDetailTextFileWriter.WriteLine ("<td><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "#Realizations'>Feedback</a> (" + itemChildren.Feedback.Length.ToString() + ")</td>")
            swItemDetailTextFileWriter.WriteLine "</tr></table>"
            )
        swItemDetailTextFileWriter.WriteLine()
        swItemDetailTextFileWriter.WriteLine("<p><a href='" + (sectionHeading.CovertIntoOSSafeFileName + ".amout") + "'>Code</a></p>")
        // Debug code
        swItemDetailTextFileWriter.WriteLine ("<h3>" + "DEBUG: Incoming Data Dump" + "</h3>")
        writeModelItemDetailHtmlTableHead swItemDetailTextFileWriter
        topLevelItems |> List.iteri(fun i j->
            writeModelItemDetailHtmlTableDetail swItemDetailTextFileWriter i j
            )

        swItemDetailTextFileWriter.WriteLine "</table></body></html>"
        swItemDetailTextFileWriter.Flush()
        swItemDetailTextFileWriter.Close()
        ()

    let compiledDumpIncomingModelHtml (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) =
        saveCompiledModelSectionHtml opts Buckets.Behavior Genres.Business modelItems
        saveCompiledModelSectionHtml opts Buckets.Structure Genres.Business modelItems
        saveCompiledModelSectionHtml opts Buckets.Supplemental Genres.Business modelItems

        saveCompiledModelSectionHtml opts Buckets.Behavior Genres.System modelItems
        saveCompiledModelSectionHtml opts Buckets.Structure Genres.System modelItems
        saveCompiledModelSectionHtml opts Buckets.Supplemental Genres.System modelItems

        saveCompiledModelSectionHtml opts Buckets.Behavior Genres.Meta modelItems
        saveCompiledModelSectionHtml opts Buckets.Structure Genres.Meta modelItems
        saveCompiledModelSectionHtml opts Buckets.Supplemental Genres.Meta modelItems

    let saveCompiledModelSection (bucketToSave:Buckets) (genreToSave:Genres) (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) =
        let fileName=(genreToSave.ToString().ToUpper() + "-" + bucketToSave.ToString() +  "-ABSTRACT-TO-BE").CovertIntoOSSafeFileName + ".amout"
        let fileTitle=genreToSave.ToString().ToUpper() + " " + bucketToSave.ToString().ToUpper() +  " ABSTRACT TO-BE"
        System.IO.File.Delete(fileName)
        let sw = System.IO.File.CreateText(fileName)
        sw.WriteLine fileTitle
        let rootModelItems = getModelItemRootItems modelItems genreToSave AbstractionLevels.Abstract TemporalIndicators.ToBe bucketToSave 
        rootModelItems |> List.iteri(fun i x->
            sw.WriteLine ("    " + x.ModelItemName)
            saveDetailedViewHtml opts modelItems x
            saveDetailedViewAmout opts modelItems x
            )
        let rootItems = getAllRootItemsForTags modelItems genreToSave AbstractionLevels.Abstract TemporalIndicators.ToBe bucketToSave 
        sw.WriteLine ""
        sw.WriteLine "    NOTES  //Notes not attached to any specific item"
        let rootNotes = getRootLevelUnattachedSubItems modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior "NOTE"
        rootNotes |> List.iteri(fun i x->match x.ItemType with |Note(n)->sw.WriteLine ("        " + n.Text)|_->())
        sw.WriteLine "    QUESTIONS  //Questions not attached to any specific item"
        let rootQuestions = getRootLevelUnattachedSubItems modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior "QUESTION"
        rootQuestions |> List.iteri(fun i x->match x.ItemType with |Question(q)->sw.WriteLine ("        " + q.Text)|_->())
        sw.WriteLine "    TODO  //To-do items not attached to any specific item"
        let rootToDos = getRootLevelUnattachedSubItems modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior "TODO"
        rootToDos |> List.iteri(fun i x->match x.ItemType with |ToDo(td)->sw.WriteLine ("        " + td.Text)|_->())
        sw.WriteLine "    WORK  //Work not attached to any specific item"
        let rootWork = getRootLevelUnattachedSubItems modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior "WORK"
        rootWork |> List.iteri(fun i x->match x.ItemType with |Work(w)->sw.WriteLine ("        " + w.Text)|_->())
        sw.Flush()
        sw.Close()

    let compiledDumpIncomingModelAmout (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) =
        saveCompiledModelSection Buckets.Behavior Genres.Business opts modelItems
        saveCompiledModelSection Buckets.Behavior Genres.System opts modelItems
        saveCompiledModelSection Buckets.Behavior Genres.Meta opts modelItems

        saveCompiledModelSection Buckets.Structure Genres.Business opts modelItems
        saveCompiledModelSection Buckets.Structure Genres.System opts modelItems
        saveCompiledModelSection Buckets.Structure Genres.Meta opts modelItems

        saveCompiledModelSection Buckets.Supplemental Genres.Business opts modelItems
        saveCompiledModelSection Buckets.Supplemental Genres.System opts modelItems
        saveCompiledModelSection Buckets.Supplemental Genres.Meta opts modelItems

    let rawDumpIncomingModel (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) =
        let fileName="incoming-lines-DEBUG" 
        System.IO.File.Delete(fileName + ".html")
        let sw = System.IO.File.CreateText(fileName + ".html")
        writeHtmlBeginAndHead sw "Incoming File Dump" "Incoming files processed by line for the EasyAM program" "EasyAM" fileName
        sw.WriteLine "<body>"
        writeModelItemDetailHtmlTableHead sw
        modelItems |> List.iteri(fun i x->
            writeModelItemDetailHtmlTableDetail sw i x
            )
        sw.WriteLine"</table></body>"
        sw.WriteLine"</html>"
        sw.Flush()
        sw.Close()


