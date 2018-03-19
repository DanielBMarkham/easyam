/// Persistence functions. Stuff dealing with IO
module Persist
    open System
    open System.Text.RegularExpressions
    open System.Net
    open Types
    open SAModel
    open Lenses
    open Utils


    let writeJoinDetails (sw:System.IO.TextWriter) (detailList:(string*string []) list) =
        if detailList.Length=0 then () else
            sw.wt 5 "<table class='joinDetails')>"
            detailList |> List.iter(fun x->
                let joinList = (snd x)
                if joinList.Length=0 then () else
                    let firstItem = joinList.[0]
                    sw.wt 6 "<tr>"
                    sw.wt 6 ("<td>" + (fst x) + "</td>")
                    sw.wt 6 ("<td>" + firstItem + "</td>")
                    sw.wt 6 "</tr>"
                    joinList |> Array.iteri(fun i y->
                        if i=0 then () else
                            sw.wt 6 "<tr>"
                            sw.wt 6 "<td></td>"
                            sw.wt 6 ("<td>" + y + "</td>")
                            sw.wt 6 "</tr>"
                        )
            )
            sw.wt 5 "</table>"
    let writeItemAttributes (sw:System.IO.TextWriter) (modelItems:ModelItem []) (x:ModelItem)  = 
        match x.Location.Bucket with
                | Buckets.Behavior->
                    let triggers=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Trigger)
                    let actors=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Actor)
                    let verbnouns=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Goal)
                    let contexts=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.BusinessContext)
                    let arr =[|("WHEN: ", triggers);("ASA: ", actors);("INEEDTO: ", verbnouns);("SOTHAT: ", contexts)|]
                    let arrFiltered = arr |> Array.filter(fun y->(fst y).Length>0)
                    let arrFilteredDesc = arrFiltered |> Array.map(fun y->(fst y),(snd y) |>Array.map(fun z->z.Description)) |> Array.toList
                    writeJoinDetails sw arrFilteredDesc
                | Buckets.Structure->
                    let contains=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Contains)
                    let arr=[|"CONTAINS: ", contains|]
                    let arrFiltered = arr |> Array.filter(fun y->(fst y).Length>0)
                    let arrFilteredDesc = arrFiltered |> Array.map(fun y->(fst y),(snd y) |>Array.map(fun z->z.Description)) |> Array.toList
                    writeJoinDetails sw arrFilteredDesc
                | Buckets.Supplemental->
                    let becauses=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Because)
                    let whenevers=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Whenever)
                    let ithastobethats=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.ItHasToBeThat)
                    let arr=[|("BECAUSE: ", becauses); ("WHENEVER: ", whenevers); ("ITHASTOBETHAT: ", ithastobethats)|]
                    let arrFiltered = arr |> Array.filter(fun y->(snd y).Length>0)
                    let arrFilteredDesc = arrFiltered |> Array.map(fun y->(fst y),(snd y) |>Array.map(fun z->z.Description)) |> Array.toList
                    writeJoinDetails sw arrFilteredDesc
                | Buckets.None->()
                | Buckets.Unknown->()
    let writeItemJoins (sw:System.IO.TextWriter) (modelItems:ModelItem []) (x:ModelItem)  = 
        let getModelItemDescForId (id:int) = 
            (modelItems|> Array.map(fun y->modelItems|>Array.tryFind(fun z->z.Id=id))).[0].Value.Description
        let ig = match x.Location.Bucket with
                    | Buckets.Behavior->
                        let isAffectedBy = (getAffectedBy x) 
                        let affectedByEx = 
                            if ((getALL_MUS modelItems).IsNone)
                                then
                                    let ret=isAffectedBy|>Array.map(fun z->getModelItemDescForId z.TargetId)
                                    ret
                                else
                                    if x.Description<>"ALL"
                                        then
                                            let allIsAffectedBy = getAffectedBy (getALL_MUS modelItems).Value
                                            let temp=(allIsAffectedBy |> Array.append isAffectedBy)
                                            let ret=temp|>Array.map(fun z->getModelItemDescForId z.TargetId)
                                            ret
                                        else
                                            let allIsAffectedBy = getAffectedBy (getALL_MUS modelItems).Value
                                            let ret=allIsAffectedBy|>Array.map(fun z->getModelItemDescForId z.TargetId)
                                            ret
                        let usesEntities = (getUses x)
                        let usesEntitiesEx = usesEntities|>Array.map(fun z->getModelItemDescForId z.TargetId)
                        writeJoinDetails sw [("AFFECTEDBY: ", affectedByEx); ("USES: ",usesEntitiesEx)]
                    | Buckets.Structure->
                        let isUsedBy= (getUsedBy x)
                        let isUsedByEx=isUsedBy|>Array.map(fun z->getModelItemDescForId z.TargetId)
                        let hasA = (getHasA x)
                        let hasAEx = hasA |>Array.map(fun z->getModelItemDescForId z.TargetId)
                        writeJoinDetails sw [("USEDBY: ", isUsedByEx); ("HASA: ", hasAEx)]
                    | Buckets.Supplemental->
                        let affects= (getAffects x)
                        let affectsEx=affects|>Array.map(fun z->getModelItemDescForId z.TargetId)
                        writeJoinDetails sw [("AFFECTS: ", affectsEx)]
                    | Buckets.None->()
                    | Buckets.Unknown->()
        ()
    let writeMasterItemDetail (sw:System.IO.TextWriter) (modelItems:ModelItem []) (x:ModelItem) = 
        sw.wt 5 ("<div class='master-item'>")
        let itemAlphaPrefix = match x.Location.Bucket with |Buckets.Structure->"MDE" |Buckets.Supplemental->"MSU" |Buckets.Behavior->"MUS" |_->"XXX"
        let itemNumber= "  (" + itemAlphaPrefix+(string x.Id) + ") "
        sw.wt 5 ("<h2><span class='item-title'>" + x.Description + "</span><span class='item-number'>" + itemNumber + "</span></h2>")
        let hasAParent = x.Relations|>Array.exists(fun z->z.ModelJoinType=ModelJoin.Parent)
        if hasAParent
            then
                let itemsParentId=(x.Relations|>Array.find(fun z->z.ModelJoinType=ModelJoin.Parent)).TargetId
                let itemsParent=modelItems|>Array.find(fun z->z.Id=itemsParentId)
                sw.wt 5 ("<h2><span class='item-title'>" + ("PARENT: " + itemsParent.Description) + "</span><span class='item-number'>" + (itemAlphaPrefix + itemsParent.Description) + "</span></h2>")
            else ()
        writeItemAttributes sw modelItems x
        getNotes x |> Array.iter(fun z->
            sw.wt 5 ("<p class='notes'>" + z.AnnotationText + "</p>")
            )
        let questions = getQuestions x
        let questionSummary = 
            if questions.Length=0 
                then "no open questions"
                else (string questions.Length + " open question(s)")
        let todos = getToDos x
        let todoSummary = if todos.Length>0 then (string todos.Length + " open to-do item(s)") else ""
        let annotationSummary= match questionSummary.Length, todoSummary.Length with
                                                                    |0,0->""
                                                                    |0,_->"This has " + todoSummary
                                                                    |_,0->"This has " + questionSummary
                                                                    |_,_->"This has " + todoSummary + " and " + questionSummary
        if annotationSummary.Length>0 then (sw.wt 4 ("<p class='annotation-summary'>" + annotationSummary + "</p>")) else ()
        sw.wt 4 "<div class='question-list'><ul>"
        questions |> Array.iteri(fun i x->
            sw.wt 4 ("<li>" + x.AnnotationText + "</li>")
            )
        sw.wt 4 "</ul></div>"
        if x.Relations.Length>0
            then
                sw.wt 5 ("<h3>Cross-References</h3>")
                writeItemJoins sw modelItems x
            else ()
        sw.wt 4 ("</div>")
    let writeMinimalHtmlHead (sw:System.IO.TextWriter) (title:string) (styleSheets:string list) =
        sw.wt 0 "<!DOCTYPE html>"
        sw.wt 0 "<html lang='en'>"
        sw.wt 1 "<head>"
        sw.wt 2 "<meta charset='utf-8'>"
        sw.wt 2 "<meta http-equiv='X-UA-Compatible' content='IE=edge'>"
        sw.wt 2 "<meta name='viewport' content='width=device-width, initial-scale=1'>"
        sw.wt 2 "<!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->"
        sw.wt 2 ("<title>" + title + "</title>")
        styleSheets |> List.iter(fun x->
            sw.wt 2 ("<link href='" + x + "' rel='stylesheet'>")
            )
        sw.wt 1 "</head>"
    // Temp function to test saving that given a compiler return, saves a test html file
    let saveModelGuide (fileName:string) (compilerStatus:CompilerReturn) =
        let modeltems = compilerStatus.ModelItems
        let sw = System.IO.File.CreateText(fileName)
        writeMinimalHtmlHead sw "Analysis Model Masters" ["model-master.css"]
        sw.wt 1 "<body>"
        sw.wt 2 "<div id='content'>"
        sw.wt 3 "<div class='bucket-div'>"
        sw.wt 4 "<h1>Master User Stories</h1>"
        getMasterUserStories compilerStatus.ModelItems |> Array.iteri(fun i x->
            writeMasterItemDetail sw compilerStatus.ModelItems x
            )
        sw.wt 3 "</div'>"
        sw.wt 3 "<div class='bucket-div'>"
        sw.wt 4 "<h1>Master Domain Model</h1>"
        getMasterDomainEntities compilerStatus.ModelItems |> Array.iteri(fun i x->
            writeMasterItemDetail sw compilerStatus.ModelItems x
            )
        sw.wt 3 "</div'>"
        sw.wt 3 "<div class='bucket-div'>"
        sw.wt 4 "<h1>Master Supplemental List</h1>"
        getMasterSupplementals compilerStatus.ModelItems |> Array.filter(fun x->x.Location.Bucket=Buckets.Supplemental) |> Array.iteri(fun i x->
            writeMasterItemDetail sw compilerStatus.ModelItems x
            )
        sw.wt 3 "</div'>"
        sw.wt 2 "</div>"
        sw.wt 1 "</body>"
        sw.Flush()
        sw.Close()
        ()
    let saveProjectBacklog (fileName:string) (compilerStatus:CompilerReturn) =
        let modeltems = compilerStatus.ModelItems
        let sw = System.IO.File.CreateText(fileName)
        writeMinimalHtmlHead sw "Project Backlog" ["model-master.css"]
        sw.wt 1 "<body>"
        sw.wt 2 "<div id='content'>"
        sw.wt 3 "<div class='bucket-div'>"
        sw.wt 4 "<h1>Project User Stories</h1>"
        let projectUserStories = (getProjectUserStories compilerStatus.ModelItems) |> sortModelByOneParameter {TagOrAtt=Tag; Thing="Rank"; ConvertTo=Int; Order=Ascending}
        projectUserStories |> Array.iteri(fun i x->
            writeMasterItemDetail sw compilerStatus.ModelItems x
            )
        sw.wt 3 "</div'>"
        sw.wt 3 "<div class='bucket-div'>"
        sw.wt 4 "<h1>Project Domain Model</h1>"
        getProjectDomainEntities compilerStatus.ModelItems |> Array.iteri(fun i x->
            writeMasterItemDetail sw compilerStatus.ModelItems x
            )
        sw.wt 3 "</div'>"
        sw.wt 3 "<div class='bucket-div'>"
        sw.wt 4 "<h1>Project Supplemental List</h1>"
        getProjectSupplementals compilerStatus.ModelItems |> Array.filter(fun x->x.Location.Bucket=Buckets.Supplemental) |> Array.iteri(fun i x->
            writeMasterItemDetail sw compilerStatus.ModelItems x
            )
        sw.wt 3 "</div'>"
        sw.wt 2 "</div>"
        sw.wt 1 "</body>"
        sw.Flush()
        sw.Close()
        ()
    let centerGherkinCommentText (s:string) = 
        if s.Length<78 then ("#" + s.PadBoth 78) else ("#" + s)
    let centerEasyAMCommentText (s:string) = 
        if s.Length<77 then ("//" + s.PadBoth 77) else ("//" + s)
    let writeCompilerReportAmoutFormat (fullFileName:string) (compilerStatus:CompilerReturn) = 
        let sb= new System.Text.StringBuilder(65535)
        sb.wl (centerEasyAMCommentText "EASYAM REPORT")
        sb.wl (centerEasyAMCommentText ("Model Generation: " + string DateTime.Now))
        sb.wl (centerEasyAMCommentText "")
        sb.wl ("//    Compiler Messages")
        sb.wl ("//    -----------------")
        compilerStatus.CompilerMessages |> Array.iteri(fun i x->
            sb.wl (("//  " + x.SourceFileShort + ":" + (string x.SourceLineBegin) + ": " + (string x.MessageType) + ": " + x.Message))
            )
        sb.wl (centerEasyAMCommentText "")
        sb.wl ("//    Model Summary")
        sb.wl ("//    -------------")
        let MUS=getMasterUserStories compilerStatus.ModelItems
        let MDE=getMasterDomainEntities compilerStatus.ModelItems
        let MSP=getMasterSupplementals compilerStatus.ModelItems
        let itemSummary (title:string) (items:ModelItem []) =
            sb.wl ("//    Total " + title + ": " + string items.Length)
            sb.wl ("//    Total open questions: " + string (getTotalQuestionCount items))
            sb.wl ("//    Total notes: " + string (getTotalNoteCount items))
            sb.wl ("//    Total to-do items: " + string (getTotalToDoCount items))
            sb.wl ("//    Total work items: " + string (getTotalWorkCount items))
            sb.wl ("//")
        itemSummary "Master User Stories" MUS
        itemSummary "Master Domain Entities" MDE
        itemSummary "Master Supplementals" MSP
        sb.wl ("//")
        System.IO.File.WriteAllText(fullFileName, string sb)

    let writeItemAnnotationDetailForMasterPage (sb:System.Text.StringBuilder) (annotationsArray:ModelItemAnnotation []) (title:string) (beginIndentLevel:int) = 
        let titleIndent = new System.String(' ', beginIndentLevel*2)
        let detailIndent = new System.String(' ', (beginIndentLevel+1)*2)
        if annotationsArray.Length=0 then ()
            else 
                sb.wl (titleIndent + title)
                annotationsArray|>Array.iteri(fun j y->sb.wl (detailIndent + y.AnnotationText))
    let writeAllItemAnnotationDetailForMasterPage (sb:System.Text.StringBuilder) (annotationAndTitleArray:(string*ModelItemAnnotation []) [])=
        annotationAndTitleArray|> Array.iteri(fun i x->
            let annotationsArray=(snd x)
            let title=(fst x)
            writeItemAnnotationDetailForMasterPage sb annotationsArray title 2
            )

    let writeItemAttributeDetailForMasterPage (sb:System.Text.StringBuilder) (attributesArray:ModelItemAttribute []) (attributesTitle:string) (includeAttributeAnnotations:bool) =
        if attributesArray.Length=0 then ()
            else
                sb.wl ("    " + attributesTitle)
                attributesArray|>Array.iteri(fun i x->
                    sb.wl ("      " + x.Description)
                    if includeAttributeAnnotations 
                        then
                            let notes=getAttributeAnnoationsForAType x Note
                            let questions=getAttributeAnnoationsForAType x Question
                            let todo=getAttributeAnnoationsForAType x ToDo
                            let work=getAttributeAnnoationsForAType x Work
                            writeItemAnnotationDetailForMasterPage sb notes "NOTES: " 4
                            writeItemAnnotationDetailForMasterPage sb questions "QUESTIONS: " 4
                            writeItemAnnotationDetailForMasterPage sb todo "TO-DO: " 4
                            writeItemAnnotationDetailForMasterPage sb work "WORK: " 4
                        else ()
                    )
    let writeAllItemAttributeDetailsForMasterPage (sb:System.Text.StringBuilder) (attributesAndTitles:(string*ModelItemAttribute []) []) (includeAttributeAnnotations:bool) =
        attributesAndTitles|>Array.iteri(fun i x->
            let title=(fst x)
            let attributesArray=(snd x)
            writeItemAttributeDetailForMasterPage sb attributesArray title includeAttributeAnnotations
        )
    let writeMasterUseCasesAmoutFormat (fullFileName:string) (compilerStatus:CompilerReturn) =
        let sb= new System.Text.StringBuilder(65535)
        sb.wl (centerEasyAMCommentText "MASTER USE CASES")
        sb.wl (centerEasyAMCommentText ("Model Generation: " + string DateTime.Now))
        sb.wl (centerEasyAMCommentText "")
        sb.wl ("")
        sb.wl ("BUSINESS BEHAVIOR ABSTRACT TO-BE")
        let MUS=getMasterUserStories compilerStatus.ModelItems

        MUS |> Array.iteri(fun i x->
            sb.wl ("  " + x.Description)
            
            let triggers=getTriggers x
            let actors=getActors x
            let goals=getGoals x
            let contexts=getBusinessContexts x 
            writeAllItemAttributeDetailsForMasterPage sb [|("WHEN: ",triggers);("ASA: ",actors);("INEEDTO: ",goals);("SOTHAT: ",contexts)|] false
            
            let notes=getNotes x
            let questions=getQuestions x 
            let todo=getToDos x 
            let work=getWork x
            writeAllItemAnnotationDetailForMasterPage sb [|("NOTES: ", notes);("QUESTIONS: ", questions);("TO-DO: ", todo);("WORK: ", work)|]

            sb.wl ("")
            sb.wl ("")
            )
        System.IO.File.WriteAllText(fullFileName, string sb)
    let writeMasterSupplementalsAmoutFormat (fullFileName:string) (compilerStatus:CompilerReturn) =
        let sb= new System.Text.StringBuilder(65535)
        sb.wl (centerEasyAMCommentText "MASTER SUPPLEMENTALS")
        sb.wl (centerEasyAMCommentText ("Model Generation: " + string DateTime.Now))
        sb.wl (centerEasyAMCommentText "")
        sb.wl ("")
        sb.wl ("BUSINESS SUPPLEMENTAL ABSTRACT TO-BE")
        let msp=getMasterSupplementals compilerStatus.ModelItems
        msp|>Array.iteri(fun i x->
            sb.wl ("  " + x.Description)
            let because=getBecauses x
            let whenever=getWhenevers x
            let ithastobethat=getItHasToBeThats x
            writeAllItemAttributeDetailsForMasterPage sb [|("BECAUSE: ",because);("WHENEVER: ",whenever);("ITHASTOBETHAT: ",ithastobethat)|] false

            let notes=getNotes x
            let questions=getQuestions x
            let todo=getToDos x
            let work=getWork x
            writeAllItemAnnotationDetailForMasterPage sb [|("NOTES: ", notes);("QUESTIONS: ", questions);("TO-DO: ", todo);("WORK: ", work)|]

            sb.wl ("")
            sb.wl ("")
            )
        System.IO.File.WriteAllText(fullFileName, string sb)
    
    let writeMasterDomainEntitiesAmoutFormat (fullFileName:string) (compilerStatus:CompilerReturn) =
        let sb= new System.Text.StringBuilder(65535)
        sb.wl (centerEasyAMCommentText "MASTER DOMAIN ENTITIES")
        sb.wl (centerEasyAMCommentText ("Model Generation: " + string DateTime.Now))
        sb.wl (centerEasyAMCommentText "")
        sb.wl ("")
        sb.wl ("BUSINESS STRUCTURE ABSTRACT TO-BE")
        let msp=getMasterDomainEntities compilerStatus.ModelItems
        msp|>Array.iteri(fun i x->
            sb.wl ("  " + x.Description)
            let contains=getContains x
            writeAllItemAttributeDetailsForMasterPage sb [|("CONTAINS: ",contains)|] false

            // For structure, there's not much to show. So we'll also show joins
            let hasa=x.Relations|>Array.filter(fun z->z.ModelJoinType=ModelJoin.HasA)
            if hasa.Length=0 then ()
                else
                    sb.wl ("    HASA:")
                    hasa|>Array.iteri(fun j y->
                        let hasaItem=compilerStatus.ModelItems|>Array.find(fun z->z.Id=y.TargetId)
                        sb.wl ("      " + hasaItem.Description)
                        )
            
            let notes=getNotes x
            let questions=getQuestions x
            let todo=getToDos x
            let work=getWork x
            writeAllItemAnnotationDetailForMasterPage sb [|("NOTES: ", notes);("QUESTIONS: ", questions);("TO-DO: ", todo);("WORK: ", work)|]

            sb.wl ("")
            sb.wl ("")
            )
        System.IO.File.WriteAllText(fullFileName, string sb)
    let writeItemRelations (sb:System.Text.StringBuilder) (modelItems:ModelItem []) (x:ModelItem) =
        let ig = match x.Location.Bucket with
                    | Buckets.Behavior->
                        let isAffectedBy = (getAffectedBy x) 
                        let affectedByEx = 
                            if ((getALL_MUS modelItems).IsNone)
                                then
                                    let ret=isAffectedBy|>Array.map(fun z->tryFindModelItemDescriptionForId modelItems z.TargetId)
                                    ret
                                else
                                    let allIsAffectedBy = getAffectedBy (getALL_MUS modelItems).Value
                                    let temp=if x.Description.Trim()<>"ALL" then (allIsAffectedBy |> Array.append isAffectedBy) else allIsAffectedBy
                                    let ret=temp|>Array.map(fun z->tryFindModelItemDescriptionForId modelItems z.TargetId)
                                    ret
                        if affectedByEx.Length=0 then () else
                            sb.wl("    AFFECTEDBY: ")
                            affectedByEx|>Array.iter(fun z->
                                if z.IsSome then sb.wl ("      " + z.Value.Description) else ()
                                )
                            sb.wl("")
                        let usesEntities = (getUses x)
                        let usesEntitiesEx = usesEntities|>Array.map(fun z->tryFindModelItemDescriptionForId modelItems z.TargetId)
                        if usesEntitiesEx.Length=0 then () else
                            sb.wl("    USES: ")
                            usesEntitiesEx|>Array.iter(fun z->
                                if z.IsSome then sb.wl ("      " + z.Value.Description) else()
                                )
                    | Buckets.Structure->
                        let isUsedBy= (getUsedBy x)
                        let isUsedByEx=isUsedBy|>Array.map(fun z->tryFindModelItemDescriptionForId modelItems z.TargetId)
                        let hasA = (getHasA x)
                        let hasAEx = hasA |>Array.map(fun z->tryFindModelItemDescriptionForId modelItems z.TargetId)
                        if hasAEx.Length=0 then () else
                            sb.wl("    HASA: ")
                            hasAEx|>Array.iter(fun z->
                                if z.IsSome then sb.wl ("      " + z.Value.Description) else ()
                                )
                    | Buckets.Supplemental->
                        let affects= (getAffects x)
                        let affectsEx=affects|>Array.map(fun z->tryFindModelItemDescriptionForId modelItems z.TargetId)
                        if affectsEx.Length=0 then () else
                            sb.wl("    AFFECTS: ")
                            affectsEx|>Array.iter(fun z->
                               if z.IsSome then sb.wl ("      " + z.Value.Description) else()
                                )
                    | Buckets.None->()
                    | Buckets.Unknown->()
        ()
    let writeDetailHeader (sb:System.Text.StringBuilder) (title:string) (itemDescription:string) = 
        sb.wl (centerEasyAMCommentText title)
        sb.wl (centerEasyAMCommentText (itemDescription.ToUpper()))
        sb.wl (centerEasyAMCommentText ("Model Generation: " + string DateTime.Now))
        sb.wl (centerEasyAMCommentText "")
        sb.wl ("")
        
    let saveCanonicalModel (directoryPath:string) (compilerStatus:CompilerReturn) =
        // Master Pages
        let reportFileName=System.IO.Path.Combine([|directoryPath;"report.amout"|]) 
        writeCompilerReportAmoutFormat reportFileName compilerStatus
        
        let MusFileName = System.IO.Path.Combine([|directoryPath;"mus.amout"|]) 
        writeMasterUseCasesAmoutFormat MusFileName compilerStatus

        let MspFileName = System.IO.Path.Combine([|directoryPath;"msp.amout"|])
        writeMasterSupplementalsAmoutFormat MspFileName compilerStatus

        let MdeFileName = System.IO.Path.Combine([|directoryPath;"mde.amout"|])
        writeMasterDomainEntitiesAmoutFormat MdeFileName compilerStatus

        let writeDetailAnnotations (sb:System.Text.StringBuilder) (x:ModelItem) =
            let notes=getNotes x
            let questions=getQuestions x 
            let todo=getToDos x 
            let work=getWork x
            writeAllItemAnnotationDetailForMasterPage sb [|("NOTES: ", notes);("QUESTIONS: ", questions);("TODO: ", todo);("WORK: ", work)|]
            sb.wl ("")
        // Detail Pages
        let MUS=getMasterUserStories compilerStatus.ModelItems
        MUS|>Array.iteri(fun i x->
            let sb= new System.Text.StringBuilder(65535)
            let musTempFileName= "mus-" + (x.Description.ToSafeFileName() + ".amout")
            let musDetailFileName=System.IO.Path.Combine([|directoryPath;musTempFileName|]) 
            writeDetailHeader sb "MASTER USER STORY " x.Description
            sb.wl ("MASTER USER STORY: " + x.Description)
            let triggers=getTriggers x
            let actors=getActors x
            let goals=getGoals x
            let contexts=getBusinessContexts x 
            writeAllItemAttributeDetailsForMasterPage sb [|("WHEN: ",triggers);("ASA: ",actors);("INEEDTO: ",goals);("SOTHAT: ",contexts)|] true
            sb.wl ("")
            writeDetailAnnotations sb x 
            writeItemRelations sb compilerStatus.ModelItems x


            System.IO.File.WriteAllText(musDetailFileName, string sb)        
            )

        let MDE=getMasterDomainEntities compilerStatus.ModelItems
        MDE|>Array.iteri(fun i x->
            let sb= new System.Text.StringBuilder(65535)
            let mdeTempFileName= "mde-" + (x.Description.ToSafeFileName() + ".amout")
            let mdeDetailFileName=System.IO.Path.Combine([|directoryPath;mdeTempFileName|]) 
            writeDetailHeader sb "MASTER DOMAIN MODEL - ENTITY" x.Description
            sb.wl("BUSINESS STRUCTURE ABSTRACT TO-BE: " + x.Description)
            let contains=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Contains)
            writeAllItemAttributeDetailsForMasterPage sb [|("CONTAINS: ",contains)|] true
            sb.wl ("")
            writeDetailAnnotations sb x 
            writeItemRelations sb compilerStatus.ModelItems x
            System.IO.File.WriteAllText(mdeDetailFileName, string sb)        
            )

        let MSP=getMasterSupplementals compilerStatus.ModelItems
        MSP|>Array.iteri(fun i x->
            let sb= new System.Text.StringBuilder(65535)
            let mspTempFileName= "msp-" + (x.Description.ToSafeFileName() + ".amout")
            let mspDetailFileName=System.IO.Path.Combine([|directoryPath;mspTempFileName|]) 
            writeDetailHeader sb "MASTER SUPPLEMENTAL " x.Description
            sb.wl("BUSINESS SUPPLEMENTAL ABSTRACT TO-BE: " + x.Description)
            let because=getBecauses x
            let whenever=getWhenevers x
            let ithastobethat=getItHasToBeThats x
            writeAllItemAttributeDetailsForMasterPage sb [|("BECAUSE: ",because);("WHENEVER: ",whenever);("ITHASTOBETHAT: ",ithastobethat)|] true
            sb.wl ("")
            writeDetailAnnotations sb x 
            writeItemRelations sb compilerStatus.ModelItems x
            System.IO.File.WriteAllText(mspDetailFileName, string sb)        
            )
    let getQuestionsAndSubQuestionsForAnItem (modelItems:ModelItem []) (x:ModelItem) =
        let itemQuestions = (getQuestions x) |> Array.map(fun y->(x.Description,y.AnnotationText))
        let itemAttributes= x.Attributes|>Array.map(fun y->(x.Description, y))
        let itemAttributeAnnotationQuestions= itemAttributes|> Array.map(fun z->
            let annotationQuestions=(getAttributeQuestions (snd z)) |> Array.map(fun a->a.AnnotationText)
            (fst z), (snd z).Description, annotationQuestions)
        let itemQuestionsPrep = itemQuestions|>Array.map(fun y->(fst y), "", [|(snd y)|])
        let ret = itemAttributeAnnotationQuestions |> Array.append itemQuestionsPrep |>Array.filter(fun (a,b,c)->c.Length>0)
        ret
    let writeOutQuestionsForAnItemHtmlTableFormat (sw:System.IO.StreamWriter) (modelItems:ModelItem []) (compilerStatus:CompilerReturn) = 
        sw.wt 4 "<table>"
        modelItems |>Array.iteri(fun i y->
            let quests=getQuestionsAndSubQuestionsForAnItem compilerStatus.ModelItems y
            quests|>Array.iteri(fun j z->
                let itemDesc, attribDesc, attribQuestions = z 
                if attribQuestions.Length=0 then () else
                    attribQuestions|>Array.iter(fun a->
                        sw.wt 5 "<tr>"
                        sw.wt 6 ("<td>" + itemDesc + "</td>")
                        sw.wt 6 ("<td>" + attribDesc + "</td>")
                        sw.wt 6 ("<td>" + a + "</td>")
                        sw.wt 5 "</tr>"
                        )
                )
            )
        sw.wt 4 "</table>"

    let saveMasterQuestionList (directoryPath:string) (fileName:string) (compilerStatus:CompilerReturn) =
        let fileName=System.IO.Path.Combine([|directoryPath;fileName|]) 
        let sw = System.IO.File.CreateText(fileName)
        writeMinimalHtmlHead sw "Master Question List" ["mql.css"]
        sw.wt 1 "<body>"
        sw.wt 2 "<div id='content'>"
        sw.wt 4 "<h1>Master Question List</h1>"
        sw.wt 3 "<div class='bucket-div'>"
        sw.wt 4 "<h2>Master User Stories</h2>"
        let MUS = getMasterUserStories compilerStatus.ModelItems
        writeOutQuestionsForAnItemHtmlTableFormat sw MUS compilerStatus
        sw.wt 3 "</div>"

        sw.wt 3 "<div class='bucket-div'>"
        sw.wt 4 "<h2>Master Supplementals</h2>"
        let MSP=getMasterSupplementals compilerStatus.ModelItems
        writeOutQuestionsForAnItemHtmlTableFormat sw MSP compilerStatus
        sw.wt 3 "</div>"

        sw.wt 3 "<div class='bucket-div'>"
        sw.wt 4 "<h2>Master Domain Model</h2>"
        let MDE=getMasterDomainEntities compilerStatus.ModelItems
        writeOutQuestionsForAnItemHtmlTableFormat sw MDE compilerStatus
        sw.wt 3 "</div>"

        sw.wt 2 "</div>"
        sw.Flush()
        sw.Close()        
        ()


    let writeIndentedTextDetailsWithAHeading (sb:System.Text.StringBuilder) (startingIndentLevel:int) (indentBy:int) (title:string) (itemList:string []) =
        if itemList.Length=0 then () else
            let titleIndent = new System.String(' ', startingIndentLevel*indentBy)
            let detailIndent = new System.String(' ', (startingIndentLevel+1)*indentBy)
            if title="" then
                itemList |> Array.iter(fun x->
                    sb.wl (titleIndent + x))           
                else
                    let newTitle=if itemList.Length>1 then title + "s" else title
                    sb.wl (titleIndent + newTitle)
                    itemList |> Array.iter(fun x->
                        sb.wl (detailIndent + x))
    /// beta function that saves a gherkin feature file based on the model
    let saveFeatureFile (fileName:string) (musItem:ModelItem) (compilerStatus:CompilerReturn) =
        let sb= new System.Text.StringBuilder(65535)
        let triggers=getTriggers musItem
        let actors=getActors musItem
        let goals=getGoals musItem
        let contexts=getBusinessContexts musItem
        let scenarios=getScenarios musItem
        let notes=getNotes musItem
        let questions=getQuestions musItem
        let todos=getToDos
        let works=getWork

        sb.wl (centerGherkinCommentText "GHERKIN FEATURE FILE")
        sb.wl (centerGherkinCommentText musItem.Description)
        sb.wl (centerGherkinCommentText ("Model Generation: " + string DateTime.Now))
        sb.wl (centerGherkinCommentText "")
        sb.wl ("")
        // kick out any MSPs (and their notes) that impact this story
        let applicableMSPs=getAllSupplementalsThatAffectThisUserStory compilerStatus.ModelItems musItem Business Abstract 
        sb.wl ("")
        sb.wl ("#  MASTER SUPPLEMENTALS TO CONSIDER WHEN WRITING ACCEPTANCE CRITERIA")
        applicableMSPs|>Array.iter(fun y->
            sb.wl ("#  " + y.Description)
            y.Annotations|>Array.filter(fun z->z.AnnotationType=Note)|>Array.iter(fun z->
                sb.wl ("#    " + z.AnnotationText)
                )
            )
        // kick out any Business Realized Supplmentals (and their notes) that impact this story
        let applicableBRSPs=getAllSupplementalsThatAffectThisUserStory compilerStatus.ModelItems musItem Business Realized 
        sb.wl ("")
        sb.wl ("#  BUSINESS REALIZED SUPPLEMENTALS TO CONSIDER WHEN WRITING ACCEPTANCE CRITERIA")
        applicableBRSPs|>Array.iter(fun y->
            sb.wl ("#  " + y.Description)
            y.Annotations|>Array.filter(fun z->z.AnnotationType=Note)|>Array.iter(fun z->
                sb.wl ("#    " + z.AnnotationText)
                )
            )
        // Finally, kick out any System Abstract Supplmentals (and their notes) that impact this story
        // Kick out the even if it's only ALL items. After all, this is a System Test harness. System Abstract applies
        let applicableSASPs=getAllSupplementalsThatAffectThisUserStory compilerStatus.ModelItems musItem System Abstract
        sb.wl ("")
        sb.wl ("#  SYSTEM ABSTRACT SUPPLEMENTALS TO CONSIDER WHEN WRITING ACCEPTANCE CRITERIA")
        applicableSASPs|>Array.iter(fun y->
            sb.wl ("#  " + y.Description)
            y.Annotations|>Array.filter(fun z->z.AnnotationType=Note)|>Array.iter(fun z->
                sb.wl ("#    " + z.AnnotationText)
                )
            )
        sb.wl ("")
        sb.wl ("Feature: " + musItem.Description)
        if scenarios.Length>0
            then
                scenarios|>Array.iter(fun z->
                sb.wl ("")
                sb.wl ("Scenario: " + z.Description)
                    )
            else ()
        writeIndentedTextDetailsWithAHeading sb 1 2 "Trigger" (triggers|>Array.map(fun x->x.Description))
        writeIndentedTextDetailsWithAHeading sb 1 2 "Actor" (actors|>Array.map(fun x->x.Description))
        writeIndentedTextDetailsWithAHeading sb 1 2 "Goal" (goals|>Array.map(fun x->x.Description))
        writeIndentedTextDetailsWithAHeading sb 1 2 "Business Context" (contexts|>Array.map(fun x->x.Description))
        sb.wl ("")
        writeIndentedTextDetailsWithAHeading sb 1 2 "" ((getNotes musItem) |> Array.map(fun z->z.AnnotationText))
        sb.wl ("")
        writeIndentedTextDetailsWithAHeading sb 1 2 "Open question" ((getQuestions musItem) |> Array.map(fun z->z.AnnotationText))
        sb.wl ("")
        writeIndentedTextDetailsWithAHeading sb 1 2 "Diagram" ((getDiagrams musItem) |> Array.map(fun z->z.AnnotationText))
        sb.wl ("")
        let subAnnotations=getAllSubAnnotations musItem
        if subAnnotations.Length>0 then (sb.wl (string subAnnotations.Length + " additional annotations not included here")) else ()
        scenarios|>Array.iter(fun x->
            sb.wl ("Scenario: " + x.Description)
            let scenarioNotes=(getAttributeNotes x) |> Array.map(fun z->z.AnnotationText)
            writeIndentedTextDetailsWithAHeading sb 1 2 "" scenarioNotes
            sb.wl ("")
            let scenarioQuestions=(getAttributeNotes x) |> Array.map(fun z->z.AnnotationText)
            writeIndentedTextDetailsWithAHeading sb 1 2 "Open question" scenarioQuestions
            sb.wl ("")
            let scenarioDiagrams=(getAttributeNotes x) |> Array.map(fun z->z.AnnotationText)
            writeIndentedTextDetailsWithAHeading sb 1 2 "Diagram" scenarioDiagrams
            sb.wl ("")
            )
        //
        let getSupplementalsAffectingAParticularUserStory modelItems usItem =
            getAffectedByItems modelItems usItem
        
        let getRelatedSupplementals modelItems usItem =  
            let forTheItem=getSupplementalsAffectingAParticularUserStory modelItems usItem
            let allMUS=getALL_MUS modelItems
            let forTheALLUS=
                if allMUS.IsNone then [||]
                    else getSupplementalsAffectingAParticularUserStory modelItems allMUS.Value
            [forTheItem;forTheALLUS] |> Array.concat

        let relatedSupplementals = getRelatedSupplementals compilerStatus.ModelItems musItem
        relatedSupplementals|>Array.iter(fun x->sb.wl x.Description)

        System.IO.File.WriteAllText(fileName,sb.ToString())
        ()
    /// beat function to create gherkin feature files
    /// used as a sample model extension to begin to prove out functionality
    let saveFeatureFiles (directoryPath:string) (compilerStatus:CompilerReturn) =
        // Master User Stories make folders
        // BUSINESS ABSTRACT REALIZED make files
        // Project User Stories go in sections in the file
        // Scenarios continue to split up the sections
        // Code goes in the scenarios (which creates steps)
        let mus=getMasterUserStories compilerStatus.ModelItems
        let turnAModelItemIntoAFullFeatureFileName modelItem directoryFullName =
            let shortName=modelItem.Description.ToSafeFileName() + ".feature"
            System.IO.Path.Combine([|directoryFullName;shortName|])
        mus |> Array.iteri(fun i x->
            let directoryNameForMUS=System.IO.Path.Combine([|directoryPath;x.Description.ToSafeFileName()|])
            let targetFeatureFileDirectory=System.IO.Directory.CreateDirectory(directoryNameForMUS)
            let musFileName=turnAModelItemIntoAFullFeatureFileName x targetFeatureFileDirectory.FullName
            //let shortFileName=x.Description.ToSafeFileName() + ".feature"
            //let fileName=System.IO.Path.Combine([|targetFeatureFileDirectory.FullName;shortFileName|])
            // first save the mus feature file, if there's anything in there
            saveFeatureFile musFileName x compilerStatus
            // now save all the kids, the business behavior realized
            let kidRelations=x.Relations|>Array.filter(fun y->y.ModelJoinType=ModelJoin.Child)
            let kids=kidRelations|>Array.map(fun y->getModelItemById compilerStatus.ModelItems y.TargetId)
            kids|>Array.iteri(fun j y->
                let usFileName=turnAModelItemIntoAFullFeatureFileName y targetFeatureFileDirectory.FullName
                saveFeatureFile usFileName y compilerStatus
                )
            )

    let saveAllToOneAMOUTFile (directoryPath:string) (fileName:string) (modelToOutput:ModelItem []) (currentCompilerStatus:CompilerReturn) =
        let fullFileName=System.IO.Path.Combine([|directoryPath; fileName|])
        let sb=new System.Text.StringBuilder(65535)
        writeDetailHeader sb "SINGLE FILE MODEL OUTPUT" "EASYAM Analysis Modeling Language Compiler"
        if fileName.Length>0 then System.IO.File.WriteAllText(fullFileName, sb.ToString()) else System.Console.Write(sb.ToString())
    let saveAllTagsToOneCSVFile (directoryPath:string) (fileName:string) (modelToOutput:ModelItem []) (currentCompilerStatus:CompilerReturn) =
        let behavior=(getBehavior modelToOutput) |> Array.sortBy(fun x->x.Description)
        let supplementals=(getSupplementals modelToOutput) |> Array.sortBy(fun x->x.Description)
        let structure=(getStructure modelToOutput) |> Array.sortBy(fun x->x.Description)
        let organizedModel = [|structure;supplementals;behavior|] |> Array.concat
        let fullFileName=System.IO.Path.Combine([|directoryPath; fileName|])
        let sb=new System.Text.StringBuilder(65535)
        //writeDetailHeader sb "SINGLE FILE MODEL OUTPUT" "EASYAM Analysis Modeling Language Compiler"
        organizedModel |> Array.iteri(fun i x->
            sb.Append ("\"" + x.Description + "\"") |> ignore
            x.Tags |> Array.iteri(fun j y->
                sb.Append "," |> ignore
                sb.Append ("\"" + y.Key + "\"") |> ignore
                sb.Append "," |> ignore
                sb.Append ("\"" + y.Value + "\"") |> ignore
                )
            sb.Append System.Environment.NewLine |> ignore
            )
        if fileName.Length>0 then System.IO.File.WriteAllText(fullFileName, sb.ToString()) else System.Console.Write(sb.ToString())

    let osLineEnd=System.Environment.NewLine
    let writeHtmlHead (sb:System.Text.StringBuilder) (title:string) (styleSheets:string list) =
        sb.wt 0 "<!DOCTYPE html>"
        sb.wt 0 "<html lang='en'>"
        sb.wt 1 "<head>"
        sb.wt 2 "<meta charset='utf-8'>"
        sb.wt 2 "<meta http-equiv='X-UA-Compatible' content='IE=edge'>"
        sb.wt 2 "<meta name='viewport' content='width=device-width, initial-scale=1'>"
        sb.wt 2 "<!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->"
        sb.wt 2 ("<title>" + title + "</title>")
        styleSheets |> List.iter(fun x->
            sb.wt 2 ("<link href='" + x + "' rel='stylesheet'>")
            )
        sb.wt 1 "</head>"
    let createLineWithLanguageCommentPrefix (outputFormat:ModelOutputType) (stringToProcess:string) =
        match outputFormat with 
            | AMOUT-> "//" + stringToProcess + osLineEnd
            | HTML->"<!--" + stringToProcess + "-->" + osLineEnd
            | TEXT->stringToProcess + osLineEnd
            | CSV->stringToProcess + osLineEnd
            | GHERKIN->"#" + stringToProcess + osLineEnd
    let centerWithPrefixAndSuffix (len:int) (prefix:string) (suffix:string) (str:string) =
        let remainingSize = len-(prefix.Length+suffix.Length)
        let paddedString=str.PadBoth remainingSize
        prefix+paddedString
    let writeHeader (sb:System.Text.StringBuilder) (headerFormat:ModelOutputType) (title:string) (description:string) =
        match headerFormat with 
            | AMOUT->
                sb.wl(centerWithPrefixAndSuffix 80 "//" "" title)
                sb.wl(centerWithPrefixAndSuffix 80 "//" "" (description))
                sb.wl(centerWithPrefixAndSuffix 80 "//" ""  ("Model Generation: " + string DateTime.Now))
                sb.wl(centerWithPrefixAndSuffix 80 "//"  "" "")
                sb.wl("")
            | HTML->
                sb.wl(centerWithPrefixAndSuffix 80 "<!--" "-->" title)
                sb.wl(centerWithPrefixAndSuffix 80 "<!--" "-->" (description))
                sb.wl(centerWithPrefixAndSuffix 80 "<!--" "-->"  ("Model Generation: " + string DateTime.Now))
                sb.wl(centerWithPrefixAndSuffix 80 "<!--"  "-->" "")
                sb.wl("")
                writeHtmlHead sb title  ["model-master.css"]
            | TEXT->
                sb.wl(centerWithPrefixAndSuffix 80 "" "" title)
                sb.wl(centerWithPrefixAndSuffix 80 "" "" (description))
                sb.wl(centerWithPrefixAndSuffix 80 "" ""  ("Model Generation: " + string DateTime.Now))
                sb.wl(centerWithPrefixAndSuffix 80 ""  "" "")
                sb.wl("")
            | CSV->
                sb.wl(title)
                sb.wl(description)
                sb.wl(("Model Generation: " + string DateTime.Now))
                sb.wl("")
                sb.wl("")
            | GHERKIN->
                sb.wl(centerWithPrefixAndSuffix 80 "#" "" title)
                sb.wl(centerWithPrefixAndSuffix 80 "#" "" (description))
                sb.wl(centerWithPrefixAndSuffix 80 "#" ""  ("Model Generation: " + string DateTime.Now))
                sb.wl(centerWithPrefixAndSuffix 80 "#"  "" "")
                sb.wl("")

    type reportPointerSortType =
        {
            TemporalIndicator:TemporalIndicators
            Genre:Genres
            AbstractionLevel:AbstractionLevels
        }
    let outputSortOrder =
        [|
             {TemporalIndicator=Was;Genre=Business;AbstractionLevel=Abstract}
            ;{TemporalIndicator=Was;Genre=System;AbstractionLevel=Abstract}
            ;{TemporalIndicator=Was;Genre=Meta;AbstractionLevel=Abstract}
            ;{TemporalIndicator=Was;Genre=Business;AbstractionLevel=Realized}
            ;{TemporalIndicator=Was;Genre=System;AbstractionLevel=Realized}
            ;{TemporalIndicator=Was;Genre=Meta;AbstractionLevel=Realized}
            ;{TemporalIndicator=AsIs;Genre=Business;AbstractionLevel=Abstract}
            ;{TemporalIndicator=AsIs;Genre=System;AbstractionLevel=Abstract}
            ;{TemporalIndicator=AsIs;Genre=Meta;AbstractionLevel=Abstract}
            ;{TemporalIndicator=AsIs;Genre=Business;AbstractionLevel=Realized}
            ;{TemporalIndicator=AsIs;Genre=System;AbstractionLevel=Realized}
            ;{TemporalIndicator=AsIs;Genre=Meta;AbstractionLevel=Realized}
            ;{TemporalIndicator=ToBe;Genre=Business;AbstractionLevel=Abstract}
            ;{TemporalIndicator=ToBe;Genre=System;AbstractionLevel=Abstract}
            ;{TemporalIndicator=ToBe;Genre=Meta;AbstractionLevel=Abstract}
            ;{TemporalIndicator=ToBe;Genre=Business;AbstractionLevel=Realized}
            ;{TemporalIndicator=ToBe;Genre=System;AbstractionLevel=Realized}
            ;{TemporalIndicator=ToBe;Genre=Meta;AbstractionLevel=Realized}
        |]

    let outputFilterExistsInPlanningToDisplay (planningToDisplay:ModelDetailItemType []) (outputFilters:ModelDetailItemType []) = 
        let isEverythingSet = outputFilters|> Array.exists(fun x->x=ModelDetailItemType.Everything)
        let mtch=planningToDisplay |> Array.fold(fun acc x->acc || (outputFilters|>Array.exists(fun y->y=x))) false
        mtch || isEverythingSet
    let writeJoinDetail (sb:System.Text.StringBuilder) (referenceModel:ModelItem []) (outputFormat:ModelOutputType) (modelItem:ModelItem) (detailLevel:ModelDetailLevelFlags) (beginningIndent:int) =
        let tagExists = detailLevel |> Array.exists(fun x->x=Everything)
        let planningToOutput = 
            [| 
                JoinsALL; JoinsParent; JoinsChild; JoinsAffects; JoinsAffectedBy; 
                JoinsUses; JoinsUsedBy; JoinsHasA; JoinsIsOwnedByA; JoinsSourceReferencesALL; JoinsTagsALL
            |]
        if outputFilterExistsInPlanningToDisplay planningToOutput detailLevel
            then 
                let sortedJoins=modelItem.Relations |> Array.sortBy(fun x->x.ModelJoinType) |> Array.groupBy(fun x->x.ModelJoinType)
                // only show targets of the join that are in this level
                // otherwise it messes with scope on readback, putting things in higher places than when they started
                let sortedJoinsOnlyOnThisLevel = sortedJoins |> Array.map(fun x->
                    let joinType=fst x 
                    let joinsForThisJoinType=snd x 
                    let filteredJoinsOnlyOnThisLevel=joinsForThisJoinType|>Array.filter(fun y->
                        let targetItem=getModelItemById referenceModel y.TargetId
                        let locationIsTheSame=
                            (targetItem.Location.Genre=modelItem.Location.Genre) && (targetItem.Location.AbstractionLevel=modelItem.Location.AbstractionLevel) && (targetItem.Location.TemporalIndicator=modelItem.Location.TemporalIndicator)
                        locationIsTheSame
                        )
                    (joinType,filteredJoinsOnlyOnThisLevel)
                    )
                match outputFormat with
                    | AMOUT->
                        //sb.wt 1 modelItem.Description
                        let sortedJoinTypes=sortedJoinsOnlyOnThisLevel|>Array.map(fun x->(fst x)) |> Array.toList
                        ModelJoin.ToList()|>List.iter(fun y->
                            if (List.contains y sortedJoinTypes)
                                then 
                                    sb.wt 2 ((string y).ToUpper())
                                    let joinsToDisplay = sortedJoinsOnlyOnThisLevel|>Array.filter(fun z->(fst z)=y) |> Array.map(fun z->(snd z)) |> Array.concat
                                    joinsToDisplay |> Array.iter(fun z->
                                        let targetItem = referenceModel|>Array.find(fun a->a.Id=z.TargetId)
                                        sb.wt 3 (targetItem.Description)
                                        )
                                else ()
                            )
                    | CSV->
                        sb.Append(string modelItem.Id + ",\"" + modelItem.Description + "\"") |> ignore
                    | GHERKIN->
                        ()
                    | HTML->
                        sb.wt 1 ("<div class='modelItem'>")
                    | TEXT->
                        //sb.wt 1 modelItem.Description
                        ()
            else ()
        ()

    let writeAnnotations (sb:System.Text.StringBuilder) (outputFormat:ModelOutputType)  (annotationsArray:ModelItemAnnotation []) (beginningIndent:int) =
        if annotationsArray.Length=0 then () else
        sb.wt beginningIndent (annotationsArray.[0].AnnotationType.ToModelOutputSectionHeading(outputFormat))
        match outputFormat with
            | AMOUT | TEXT->
                annotationsArray |> Array.iteri(fun i x->
                    sb.wt (beginningIndent+1) x.AnnotationText
                    )
            | CSV->
                ()
            | GHERKIN->
                ()
            | HTML->
                annotationsArray |> Array.iteri(fun i x->
                    sb.wt 3 ("<div class='annotationText'>" + x.AnnotationText + "</div> <!-- annotationText -->")
                    )
    let writeAnnotationsGroup (sb:System.Text.StringBuilder) (referenceModel:ModelItem []) (outputFormat:ModelOutputType) (annotationGroup:(ModelItemAnnotation []*bool) []) (beginningIndent:int) =
        let groupToDisplay=annotationGroup|>Array.filter(fun x->(snd x)=true)
        if groupToDisplay.Length=0 then () else
        groupToDisplay |> Array.iteri(fun i x->
            writeAnnotations sb outputFormat (fst x) beginningIndent
            )
        ()
    let setupAttributeAnnotationGroup (attrib:ModelItemAttribute) (detailLevel:ModelDetailLevelFlags) =
        let notes=getAttributeNotes attrib
        let questions=getAttributeQuestions attrib
        let todos=getAttributeToDos attrib
        let works=getAttributeWork attrib
        let code=getAttributeCode attrib
        let diagrams=getAttributeDiagrams attrib
        let defects=getAttributeDefects attrib
        let notesDisplayFlags= [| AttributeAnnotationsALL; AttributeAnnotationsNote|]
        let displayNotes=outputFilterExistsInPlanningToDisplay notesDisplayFlags detailLevel
        let questionsDisplayFlags= [| AttributeAnnotationsALL; AttributeAnnotationsQuestion|]
        let displayQuestions=outputFilterExistsInPlanningToDisplay questionsDisplayFlags detailLevel
        let todosDisplayFlags= [| AttributeAnnotationsALL; AttributeAnnotationsToDo|]
        let displayTodos=outputFilterExistsInPlanningToDisplay questionsDisplayFlags detailLevel
        let workDisplayFlags= [| AttributeAnnotationsALL; AttributeAnnotationsWork|]
        let displaywork=outputFilterExistsInPlanningToDisplay workDisplayFlags detailLevel
        let codeDisplayFlags= [| AttributeAnnotationsALL; AttributeAnnotationsCode|]
        let displayCode=outputFilterExistsInPlanningToDisplay codeDisplayFlags detailLevel
        let diagramDisplayFlags= [| AttributeAnnotationsALL; AttributeAnnotationsDiagram|]
        let displayDiagram=outputFilterExistsInPlanningToDisplay diagramDisplayFlags detailLevel
        let defectDisplayFlags= [| AttributeAnnotationsALL; AttributeAnnotationsDefect|]
        let displayDefect=outputFilterExistsInPlanningToDisplay defectDisplayFlags detailLevel
        let annotationGroup=[|(notes, displayNotes);(questions,displayQuestions);(todos,displayTodos);(works,displaywork);(code,displayCode);(diagrams,displayDiagram);(defects,displayDefect)|]
        annotationGroup
    let setupModelItemAnnotationGroup (modelItem:ModelItem) (detailLevel:ModelDetailLevelFlags) =
        let notes=getNotes modelItem
        let questions=getQuestions modelItem
        let todos=getToDos modelItem
        let works=getWork modelItem
        let code=getCode modelItem
        let diagrams=getDiagrams modelItem
        let defects=getDefects modelItem
        let notesDisplayFlags= [| AnnotationsALL; AnnotationsNote|]
        let displayNotes=outputFilterExistsInPlanningToDisplay notesDisplayFlags detailLevel
        let questionsDisplayFlags= [| AnnotationsALL; AnnotationsQuestion|]
        let displayQuestions=outputFilterExistsInPlanningToDisplay questionsDisplayFlags detailLevel
        let todosDisplayFlags= [| AnnotationsALL; AnnotationsToDo|]
        let displayTodos=outputFilterExistsInPlanningToDisplay questionsDisplayFlags detailLevel
        let workDisplayFlags= [| AnnotationsALL; AnnotationsWork|]
        let displaywork=outputFilterExistsInPlanningToDisplay workDisplayFlags detailLevel
        let codeDisplayFlags= [|AnnotationsALL; AnnotationsCode|]
        let displayCode=outputFilterExistsInPlanningToDisplay codeDisplayFlags detailLevel
        let diagramDisplayFlags= [| AnnotationsALL; AnnotationsDiagram|]
        let displayDiagram=outputFilterExistsInPlanningToDisplay diagramDisplayFlags detailLevel
        let defectDisplayFlags= [| AnnotationsALL; AnnotationsDefect|]
        let displayDefect=outputFilterExistsInPlanningToDisplay defectDisplayFlags detailLevel
        let annotationGroup=[|(notes, displayNotes);(questions,displayQuestions);(todos,displayTodos);(works,displaywork);(code,displayCode);(diagrams,displayDiagram);(defects,displayDefect)|]
        annotationGroup

    let writeAttributeGroup (sb:System.Text.StringBuilder) (referenceModel:ModelItem []) (outputFormat:ModelOutputType) (modelItem:ModelItem) (detailLevel:ModelDetailLevelFlags)  (attributeGroups:ModelItemAttribute [] []) =
        if attributeGroups.Length=0 then () else
        attributeGroups |> Array.iteri(fun i x->
            if x.Length=0 then () else
                sb.wt 2 (x.[0].AttributeType.ToModelOutputSectionHeading(outputFormat))

                match outputFormat with
                    | AMOUT | TEXT->
                        x |> Array.iteri(fun i x->
                            sb.wt 3 x.Description
                            let attributeAnnotationsToDisplay=setupAttributeAnnotationGroup x detailLevel
                            writeAnnotationsGroup sb referenceModel outputFormat attributeAnnotationsToDisplay 4
                            )
                    | CSV->
                        ()
                    | GHERKIN->
                        ()
                    | HTML->
                        x |> Array.iteri(fun i x->
                            sb.wt 3 ("<div class='attributeTitle'>" + x.Description + "</div> <!-- attributeTitle -->")
                            )
            )
        ()
    let writeItemAttributes2 (sb:System.Text.StringBuilder) (referenceModel:ModelItem []) (outputFormat:ModelOutputType) (modelItem:ModelItem) (detailLevel:ModelDetailLevelFlags) =
        if modelItem.Attributes.Length=0 then () else
        let triggers=getTriggers modelItem
        let actors=getActors modelItem
        let goals=getGoals modelItem
        let businessContexts=getBusinessContexts modelItem
        let scenarios=getScenarios modelItem
        let contains=getContains modelItem
        let becauses=getBecauses modelItem
        let whenevers=getWhenevers modelItem
        let itHasToBeThats=getItHasToBeThats modelItem
        let planningToOutput = 
            [| AttributesALL; AttributesTrigger; AttributesActor; AttributesGoal; AttributesBusinessContext; AttributesScenario
            ; AttributesContains; AttributesBecause; AttributesWhenever; AttributesItHasToBeThat
            ; AttributeAnnotationsALL; AttributeAnnotationsNote; AttributeAnnotationsQuestion; AttributeAnnotationsToDo
            ; AttributeAnnotationsWork; AttributeAnnotationsDiagram; AttributeAnnotationsCode; AttributeAnnotationsDefect|]
        if outputFilterExistsInPlanningToDisplay planningToOutput detailLevel
            then
                let attributeGroupsToOutput =
                    match modelItem.Location.Bucket with 
                        |Behavior->[|triggers; actors; goals; businessContexts; scenarios|]
                        |Structure->[|contains|]
                        |Supplemental->[|becauses;whenevers;itHasToBeThats|]
                        |Buckets.None|Buckets.Unknown->[||]
                writeAttributeGroup sb referenceModel outputFormat modelItem detailLevel attributeGroupsToOutput
            else ()
        ()
    let wrteItemAnnotations (sb:System.Text.StringBuilder) (referenceModel:ModelItem []) (outputFormat:ModelOutputType) (modelItem:ModelItem) (detailLevel:ModelDetailLevelFlags) =
        if modelItem.Annotations.Length=0 then () else
        let tagExists = detailLevel |> Array.exists(fun x->x=Everything)
        let notes=getNotes modelItem
        let questions=getQuestions modelItem
        let todos=getToDos modelItem
        let works=getWork modelItem
        let code=getCode modelItem
        let diagrams=getDiagrams modelItem
        let defects=getDefects modelItem
        let annoationGroup=[|notes;questions;todos;works;code;diagrams;defects|]
        let planningToOutput = 
            [| AnnotationsALL; AnnotationsNote; AnnotationsQuestion; 
               AnnotationsToDo; AnnotationsWork; AnnotationsDiagram; AnnotationsCode; AnnotationsDefect
            |]
        if outputFilterExistsInPlanningToDisplay planningToOutput detailLevel
            then
                if outputFilterExistsInPlanningToDisplay [|Everything; AnnotationsALL; Item; AnnotationsNote|] detailLevel
                    then writeAnnotations sb outputFormat notes 2
                    else ()
                if outputFilterExistsInPlanningToDisplay [|Everything; AnnotationsALL; Item; AnnotationsQuestion|] detailLevel
                    then writeAnnotations sb outputFormat questions 2
                    else ()
                if outputFilterExistsInPlanningToDisplay [|Everything; AnnotationsALL; Item; AnnotationsToDo|] detailLevel
                    then writeAnnotations sb outputFormat todos 2
                    else ()
                if outputFilterExistsInPlanningToDisplay [|Everything; AnnotationsALL; Item; AnnotationsWork|] detailLevel
                    then writeAnnotations sb outputFormat works 2
                    else ()
                if outputFilterExistsInPlanningToDisplay [|Everything; AnnotationsALL; Item; AnnotationsCode|] detailLevel
                    then writeAnnotations sb outputFormat code 2
                    else ()
                if outputFilterExistsInPlanningToDisplay [|Everything; AnnotationsALL; Item; AnnotationsDiagram|] detailLevel
                    then writeAnnotations sb outputFormat diagrams 2
                    else ()
                if outputFilterExistsInPlanningToDisplay [|Everything; AnnotationsALL; Item; AnnotationsDefect|] detailLevel
                    then writeAnnotations sb outputFormat defects 2
                    else ()
            else ()
        ()

    let writeItemDetail (sb:System.Text.StringBuilder) (referenceModel:ModelItem []) (outputFormat:ModelOutputType) (modelItem:ModelItem) (detailLevel:ModelDetailLevelFlags) =
        match outputFormat with
            | AMOUT->
                sb.wt 1 modelItem.Description
                writeJoinDetail sb referenceModel outputFormat modelItem detailLevel 2
                wrteItemAnnotations sb referenceModel outputFormat modelItem detailLevel
                writeItemAttributes2 sb referenceModel outputFormat modelItem detailLevel
            | CSV->
                sb.Append(string modelItem.Id + ",\"" + modelItem.Description + "\"") |> ignore
                writeJoinDetail sb referenceModel outputFormat modelItem detailLevel 2
                wrteItemAnnotations sb referenceModel outputFormat modelItem detailLevel
                writeItemAttributes2 sb referenceModel outputFormat modelItem detailLevel
            | GHERKIN->
                ()
                writeJoinDetail sb referenceModel outputFormat modelItem detailLevel 2
                wrteItemAnnotations sb referenceModel outputFormat modelItem detailLevel
                writeItemAttributes2 sb referenceModel outputFormat modelItem detailLevel
            | HTML->
                sb.wt 1 ("<div class='modelItem'>")
                sb.wt 2 ("<h1>" + modelItem.Description + "</h1>")
                writeJoinDetail sb referenceModel outputFormat modelItem detailLevel 2
                wrteItemAnnotations sb referenceModel outputFormat modelItem detailLevel
                writeItemAttributes2 sb referenceModel outputFormat modelItem detailLevel

                sb.wt 1 ("</div> <!-- modelItem -->")
            | TEXT->
                sb.wt 1 modelItem.Description
                writeJoinDetail sb referenceModel outputFormat modelItem detailLevel 2
                wrteItemAnnotations sb referenceModel outputFormat modelItem detailLevel
                writeItemAttributes2 sb referenceModel outputFormat modelItem detailLevel
    let writeOutModel (referenceModel:ModelItem []) (outputModel:ModelItem []) (outputFormat:ModelOutputType) (destinationDirectory:System.IO.DirectoryInfo) (singleFile:bool) (singleFileName:string) =
        
        let sbCumulative=new System.Text.StringBuilder(65535)
        if singleFile
            then
                writeHeader sbCumulative outputFormat "SINGLE FILE MODEL OUTPUT" "EASYAM Analysis Modeling Language Compiler"
                outputSortOrder |> Array.iteri(fun i x->
                    let itemsForThisSegment=getAllItemsForTemporalGenreAbstraction referenceModel x.TemporalIndicator x.Genre x.AbstractionLevel
                    let entities=getStructure itemsForThisSegment
                    let supplementals=getSupplementals itemsForThisSegment
                    let behavior=getBehavior itemsForThisSegment
                    let makeSectionTitle (title:string) = ((string x.Genre).ToUpper() + " " + title + " " + (string x.AbstractionLevel).ToUpper() + " " + (string x.TemporalIndicator).ToUpper())
                    if entities.Length>0
                        then 
                            //sbCumulative.wl (string x.Genre + " " + "STRUCTURE" + " " + string x.AbstractionLevel + " " + string x.TemporalIndicator)
                            sbCumulative.wl (makeSectionTitle "STRUCTURE")
                            entities |> Array.iteri(fun k z->writeItemDetail sbCumulative referenceModel outputFormat z [|Everything|])
                        else ()
                    if supplementals.Length>0
                        then 
                            sbCumulative.wl (makeSectionTitle "SUPPLEMENTAL")
                            //sbCumulative.wl (string x.Genre + " " + "SUPPLEMENTAL" + " " + string x.AbstractionLevel + " " + string x.TemporalIndicator)
                            supplementals |> Array.iteri(fun k z->writeItemDetail sbCumulative referenceModel outputFormat z [|Everything|])
                        else ()
                    if behavior.Length>0
                        then 
                            sbCumulative.wl (makeSectionTitle "BEHAVIOR")
                            //sbCumulative.wl (string x.Genre + " " + "BEHAVIOR" + " " + string x.AbstractionLevel + " " + string x.TemporalIndicator)
                            behavior |> Array.iteri(fun k z->writeItemDetail sbCumulative referenceModel outputFormat z [|Everything|])
                        else ()
                    )
                sbCumulative.wl ""
                sbCumulative.wl ""
                if singleFileName="" then System.Console.Write(sbCumulative) else System.IO.File.WriteAllText(singleFileName,string sbCumulative)

            else
                outputSortOrder |> Array.iteri(fun i x->
                    let itemsForThisSegment=getAllItemsForTemporalGenreAbstraction referenceModel x.TemporalIndicator x.Genre x.AbstractionLevel
                    itemsForThisSegment |> Array.iteri(fun j y->
                        let itemDetailFileName=y.Description.ToSafeFileName() + match outputFormat with AMOUT->".amout" | HTML->".html" |TEXT->".txt" |CSV->".csv" |GHERKIN->".feature"
                        let fullFileName=System.IO.Path.Combine(destinationDirectory.FullName,itemDetailFileName)
                        let sbFile=new System.Text.StringBuilder(65535)
                        writeItemDetail sbFile referenceModel outputFormat y [|Everything|]
                        System.IO.File.WriteAllText(singleFileName,string sbFile)
                        )
                    )
        ()


