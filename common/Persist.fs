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
    let writeItemAttributes (sw:System.IO.TextWriter) (modelItems:ModelItem2 []) (x:ModelItem2)  = 
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
    let writeItemJoins (sw:System.IO.TextWriter) (modelItems:ModelItem2 []) (x:ModelItem2)  = 
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
    let writeMasterItemDetail (sw:System.IO.TextWriter) (modelItems:ModelItem2 []) (x:ModelItem2) = 
        sw.wt 5 ("<div class='master-item'>")
        let itemAlphaPrefix = match x.Location.Bucket with |Buckets.Structure->"MDE" |Buckets.Supplemental->"MSU" |Buckets.Behavior->"MUS" |_->"XXX"
        let itemNumber= "  (" + itemAlphaPrefix+(string x.Id) + ") "
        //wl sw 5 ("<h2>" + x.Description + "</h2>")
        sw.wt 5 ("<h2><span class='item-title'>" + x.Description + "</span><span class='item-number'>" + itemNumber + "</span></h2>")
        let hasAParent = x.Relations|>Array.exists(fun z->z.ModelJoinType=ModelJoin.Parent)
        if hasAParent
            then
                let itemsParentId=(x.Relations|>Array.find(fun z->z.ModelJoinType=ModelJoin.Parent)).TargetId
                let itemsParent=modelItems|>Array.find(fun z->z.Id=itemsParentId)
                sw.wt 5 ("<h2><span class='item-title'>" + ("PARENT: " + itemsParent.Description) + "</span><span class='item-number'>" + (itemAlphaPrefix + itemsParent.Description) + "</span></h2>")
            else ()
        writeItemAttributes sw modelItems x
        x.Annotations |> Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Note) |> Array.iter(fun z->
            sw.wt 5 ("<p class='notes'>" + (snd z) + "</p>")
            )
        let questions = getQuestions x //x.Annotations|> Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Question)
        let questionSummary = if questions.Length>0 then (string questions.Length + " open question(s)") else ""
        let todos = getToDos x //x.Annotations|> Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.ToDo)
        let todoSummary = if todos.Length>0 then (string todos.Length + " open to-do item(s)") else ""
        let annotationSummary= match questionSummary.Length, todoSummary.Length with
                                                                    |0,0->""
                                                                    |0,_->"This has " + todoSummary
                                                                    |_,0->"This has " + questionSummary
                                                                    |_,_->"This has " + todoSummary + " and " + questionSummary
        if annotationSummary.Length>0 then (sw.wt 4 ("<p class='annotation-summary'>" + annotationSummary + "</p>")) else ()
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
        //wl sw 2 "<link rel='icon' href='images/rotating-star.gif' type='image/gif' >"
        //wl sw 2 "<link rel='canonical' href='http://danielbmarkham.com' />"
        sw.wt 2 ("<title>" + title + "</title>")
        styleSheets |> List.iter(fun x->
            sw.wt 2 ("<link href='" + x + "' rel='stylesheet'>")
            )
        //wl sw 2 "<link href='css/bootstrap.min.css' rel='stylesheet'>"
        //wl sw 2 "<link rel='stylesheet' href='font-awesome/css/font-awesome.min.css'>"
        //wl sw 2 "<link rel='stylesheet' href='css/bootstrap-social.css'>"
        //wl sw 2 "<link rel='stylesheet' href='css/main.css'>"
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
        sw.wt 4 "<h1>Master User Stories</h1>"
        getProjectUserStories compilerStatus.ModelItems |> Array.iteri(fun i x->
            writeMasterItemDetail sw compilerStatus.ModelItems x
            )
        sw.wt 3 "</div'>"
        sw.wt 3 "<div class='bucket-div'>"
        sw.wt 4 "<h1>Master Domain Model</h1>"
        getProjectDomainEntities compilerStatus.ModelItems |> Array.iteri(fun i x->
            writeMasterItemDetail sw compilerStatus.ModelItems x
            )
        sw.wt 3 "</div'>"
        sw.wt 3 "<div class='bucket-div'>"
        sw.wt 4 "<h1>Master Supplemental List</h1>"
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
        let itemSummary (title:string) (items:ModelItem2 []) =
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

    let writeItemAnnotationDetailForMasterPage (sb:System.Text.StringBuilder) (annotationsArray:(ANNOTATION_TOKEN_TYPE*string)[]) (title:string) (beginIndentLevel:int) = 
        let titleIndent = new System.String(' ', beginIndentLevel*2)
        let detailIndent = new System.String(' ', (beginIndentLevel+1)*2)
        if annotationsArray.Length=0 then ()
            else 
                sb.wl (titleIndent + title)
                annotationsArray|>Array.iteri(fun j y->sb.wl (detailIndent + (snd y)))
    let writeAllItemAnnotationDetailForMasterPage (sb:System.Text.StringBuilder) (annotationAndTitleArray:(string*(ANNOTATION_TOKEN_TYPE*string) []) [])=
        annotationAndTitleArray|> Array.iteri(fun i x->
            let annotationsArray=(snd x)
            let title=(fst x)
            writeItemAnnotationDetailForMasterPage sb annotationsArray title 2
            )

    let writeItemAttributeDetailForMasterPage (sb:System.Text.StringBuilder) (attributesArray:ModelItem2Attribute []) (attributesTitle:string) (includeAttributeAnnotations:bool) =
        if attributesArray.Length=0 then ()
            else
                sb.wl ("    " + attributesTitle)
                attributesArray|>Array.iteri(fun i x->
                    sb.wl ("      " + x.Description)
                    if includeAttributeAnnotations 
                        then
                            let notes=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Note)
                            let questions=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Question)
                            let todo=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.ToDo)
                            let work=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Work)
                            writeItemAnnotationDetailForMasterPage sb notes "NOTES: " 4
                            writeItemAnnotationDetailForMasterPage sb questions "QUESTIONS: " 4
                            writeItemAnnotationDetailForMasterPage sb todo "TO-DO: " 4
                            writeItemAnnotationDetailForMasterPage sb work "WORK: " 4
                        else ()
                    )
    let writeAllItemAttributeDetailsForMasterPage (sb:System.Text.StringBuilder) (attributesAndTitles:(string*ModelItem2Attribute []) []) (includeAttributeAnnotations:bool) =
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
            
            let triggers=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Trigger)
            let actors=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Actor)
            let goals=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Goal)
            let contexts=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.BusinessContext)
            writeAllItemAttributeDetailsForMasterPage sb [|("WHEN: ",triggers);("ASA: ",actors);("INEEDTO: ",goals);("SOTHAT: ",contexts)|] false
            
            let notes=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Note)
            let questions=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Question)
            let todo=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.ToDo)
            let work=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Work)
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
            let because=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Because)
            let whenever=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Whenever)
            let ithastobethat=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.ItHasToBeThat)
            writeAllItemAttributeDetailsForMasterPage sb [|("BECAUSE: ",because);("WHENEVER: ",whenever);("ITHASTOBETHAT: ",ithastobethat)|] false

            
            let notes=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Note)
            let questions=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Question)
            let todo=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.ToDo)
            let work=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Work)
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
            let contains=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Contains)
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
            
            let notes=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Note)
            let questions=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Question)
            let todo=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.ToDo)
            let work=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Work)
            writeAllItemAnnotationDetailForMasterPage sb [|("NOTES: ", notes);("QUESTIONS: ", questions);("TO-DO: ", todo);("WORK: ", work)|]

            sb.wl ("")
            sb.wl ("")
            )
        System.IO.File.WriteAllText(fullFileName, string sb)
    let writeItemRelations (sb:System.Text.StringBuilder) (modelItems:ModelItem2 []) (x:ModelItem2) =
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
                                    let allIsAffectedBy = getAffectedBy (getALL_MUS modelItems).Value
                                    let temp=if x.Description.Trim()<>"ALL" then (allIsAffectedBy |> Array.append isAffectedBy) else allIsAffectedBy
                                    let ret=temp|>Array.map(fun z->getModelItemDescForId z.TargetId)
                                    ret
                        if affectedByEx.Length=0 then () else
                            sb.wl("    AFFECTEDBY: ")
                            affectedByEx|>Array.iter(fun z->
                                sb.wl ("      " + z)
                                )
                            sb.wl("")
                        let usesEntities = (getUses x)
                        let usesEntitiesEx = usesEntities|>Array.map(fun z->getModelItemDescForId z.TargetId)
                        if usesEntitiesEx.Length=0 then () else
                            sb.wl("    USES: ")
                            usesEntitiesEx|>Array.iter(fun z->
                                sb.wl ("      " + z)
                                )
                    | Buckets.Structure->
                        let isUsedBy= (getUsedBy x)
                        let isUsedByEx=isUsedBy|>Array.map(fun z->getModelItemDescForId z.TargetId)
                        let hasA = (getHasA x)
                        let hasAEx = hasA |>Array.map(fun z->getModelItemDescForId z.TargetId)
                        if hasAEx.Length=0 then () else
                            sb.wl("    HASA: ")
                            hasAEx|>Array.iter(fun z->
                                sb.wl ("      " + z)
                                )
                    | Buckets.Supplemental->
                        let affects= (getAffects x)
                        let affectsEx=affects|>Array.map(fun z->getModelItemDescForId z.TargetId)
                        if affectsEx.Length=0 then () else
                            sb.wl("    AFFECTS: ")
                            affectsEx|>Array.iter(fun z->
                                sb.wl ("      " + z)
                                )
                    | Buckets.None->()
                    | Buckets.Unknown->()
        ()
        
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

        let writeDetailHeader (sb:System.Text.StringBuilder) (title:string) (itemDescription:string) = 
            sb.wl (centerEasyAMCommentText title)
            sb.wl (centerEasyAMCommentText (itemDescription.ToUpper()))
            sb.wl (centerEasyAMCommentText ("Model Generation: " + string DateTime.Now))
            sb.wl (centerEasyAMCommentText "")
            sb.wl ("")
        let writeDetailAnnotations (sb:System.Text.StringBuilder) (x:ModelItem2) =
            let notes=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Note)
            let questions=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Question)
            let todo=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.ToDo)
            let work=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Work)
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
            let triggers=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Trigger)
            let actors=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Actor)
            let goals=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Goal)
            let contexts=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.BusinessContext)
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
            let because=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Because)
            let whenever=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.Whenever)
            let ithastobethat=x.Attributes|>Array.filter(fun y->y.AttributeType=ModelAttributeTypes.ItHasToBeThat)
            writeAllItemAttributeDetailsForMasterPage sb [|("BECAUSE: ",because);("WHENEVER: ",whenever);("ITHASTOBETHAT: ",ithastobethat)|] true
            sb.wl ("")
            writeDetailAnnotations sb x 
            writeItemRelations sb compilerStatus.ModelItems x
            System.IO.File.WriteAllText(mspDetailFileName, string sb)        
            )
    let getQuestionsAndSubQuestionsForAnItem (modelItems:ModelItem2 []) (x:ModelItem2) =
        let itemQuestions = (getQuestions x) |> Array.map(fun y->(x.Description,(snd y)))
        let itemAttributes=x.Attributes|>Array.map(fun y->(x.Description, y))
        let itemAttributeAnnotationQuestions= itemAttributes|> Array.map(fun z->
            let annotationQuestions=(snd z).Annotations|>Array.filter(fun a->(fst a)=ANNOTATION_TOKEN_TYPE.Question) |> Array.map(fun a->(snd a))
            (fst z), (snd z).Description, annotationQuestions)
        let itemQuestionsPrep = itemQuestions|>Array.map(fun y->(fst y), "", [|(snd y)|])
        let ret = itemAttributeAnnotationQuestions |> Array.append itemQuestionsPrep |>Array.filter(fun (a,b,c)->c.Length>0)
        ret
    let writeOutQuestionsForAnItemHtmlTableFormat (sw:System.IO.StreamWriter) (modelItems:ModelItem2 []) (compilerStatus:CompilerReturn) = 
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


    //let writeJoinDetails (sw:System.IO.TextWriter) (detailList:(string*string []) list) =
    //    if detailList.Length=0 then () else
    //        wl sw 5 "<table class='joinDetails')>"
    //        detailList |> List.iter(fun x->
    //            let joinList = (snd x)
    //            if joinList.Length=0 then () else
    //                let firstItem = joinList.[0]
    //                wl sw 6 "<tr>"
    //                wl sw 6 ("<td>" + (fst x) + "</td>")
    //                wl sw 6 ("<td>" + firstItem + "</td>")
    //                wl sw 6 "</tr>"
    //                joinList |> Array.iteri(fun i y->
    //                    if i=0 then () else
    //                        wl sw 6 "<tr>"
    //                        wl sw 6 "<td></td>"
    //                        wl sw 6 ("<td>" + y + "</td>")
    //                        wl sw 6 "</tr>"
    //                    )
    //        )
    //        wl sw 5 "</table>"

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
    
    let saveFeatureFile (fileName:string) (musItem:ModelItem2) (compilerStatus:CompilerReturn) =
        let sb= new System.Text.StringBuilder(65535)
        let triggers=getTriggers musItem
        let actors=getActors musItem
        let goals=getGoals musItem
        let contexts=getContexts musItem
        let scenarios=getScenarios musItem
        let notes=getNotes musItem
        let questions=getQuestions musItem
        let todos=getToDos
        let works=getWorks

        sb.wl (centerGherkinCommentText "GHERKIN FEATURE FILE")
        sb.wl (centerGherkinCommentText musItem.Description)
        sb.wl (centerGherkinCommentText ("Model Generation: " + string DateTime.Now))
        sb.wl (centerGherkinCommentText "")
        sb.wl ("")
        sb.wl ("Feature: " + musItem.Description)
        writeIndentedTextDetailsWithAHeading sb 1 2 "Trigger" (triggers|>Array.map(fun x->x.Description))
        writeIndentedTextDetailsWithAHeading sb 1 2 "Actor" (actors|>Array.map(fun x->x.Description))
        writeIndentedTextDetailsWithAHeading sb 1 2 "Goal" (goals|>Array.map(fun x->x.Description))
        writeIndentedTextDetailsWithAHeading sb 1 2 "Business Context" (contexts|>Array.map(fun x->x.Description))
        sb.wl ("")
        writeIndentedTextDetailsWithAHeading sb 1 2 "" (musItem.Annotations|>Array.filter(fun x->(fst x)=ANNOTATION_TOKEN_TYPE.Note)|>Array.map(fun x->(snd x)))
        sb.wl ("")
        writeIndentedTextDetailsWithAHeading sb 1 2 "Open question" (musItem.Annotations|>Array.filter(fun x->(fst x)=ANNOTATION_TOKEN_TYPE.Question)|>Array.map(fun x->(snd x)))
        sb.wl ("")
        writeIndentedTextDetailsWithAHeading sb 1 2 "Diagram" (musItem.Annotations|>Array.filter(fun x->(fst x)=ANNOTATION_TOKEN_TYPE.Diagram)|>Array.map(fun x->(snd x)))
        sb.wl ("")
        let subAnnotations=getAllSubAnnotations musItem
        if subAnnotations.Length>0 then (sb.wl (string subAnnotations.Length + " additional annotations not included here")) else ()
        scenarios|>Array.iter(fun x->
            sb.wl ("Scenario: " + x.Description)
            let scenarioNotes=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Note)|>Array.map(fun y->(snd y))
            writeIndentedTextDetailsWithAHeading sb 1 2 "" scenarioNotes
            sb.wl ("")
            let scenarioQuestions=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Question)|>Array.map(fun y->(snd y))
            writeIndentedTextDetailsWithAHeading sb 1 2 "Open question" scenarioQuestions
            sb.wl ("")
            let scenarioDiagrams=x.Annotations|>Array.filter(fun y->(fst y)=ANNOTATION_TOKEN_TYPE.Diagram)|>Array.map(fun y->(snd y))
            writeIndentedTextDetailsWithAHeading sb 1 2 "Diagram" scenarioDiagrams
            sb.wl ("")
            )
        //
        let getSupplementalsAffectingAParticularUserStory modelItems usItem =
            let affectedBy = (getAffectedBy usItem)
            affectedBy|> Array.map(fun y->(modelItems|>Array.find(fun z->z.Id=y.TargetId)))
        
        let getRelatedSupplementals modelItems usItem =  
            let forTheItem=getSupplementalsAffectingAParticularUserStory modelItems usItem
            let forTheALLUS=getSupplementalsAffectingAParticularUserStory modelItems (getALL_MUS modelItems).Value
            [forTheItem;forTheALLUS] |> Array.concat

        let relatedSupplementals = getRelatedSupplementals compilerStatus.ModelItems musItem
        relatedSupplementals|>Array.iter(fun x->sb.wl x.Description)

        System.IO.File.WriteAllText(fileName,sb.ToString())
        ()

    let saveFeatureFiles (directoryPath:string) (compilerStatus:CompilerReturn) =
        let mus=getMasterUserStories compilerStatus.ModelItems
        mus |> Array.iteri(fun i x->
            let shortFileName=x.Description.ToSafeFileName() + ".feature"
            let directoryName=System.IO.Path.Combine([|directoryPath;x.Description.ToSafeFileName()|])
            let targetFeatureFileDirectory=System.IO.Directory.CreateDirectory(directoryName)
            let fileName=System.IO.Path.Combine([|targetFeatureFileDirectory.FullName;shortFileName|])
            saveFeatureFile fileName x compilerStatus
            )

//    let drawEntityBox (sw:System.IO.StreamWriter) (box:SVGEntityBox) (svgConfig:SVGSetup) =
//        sw.WriteLine ("<rect x=\"" + box.xPos.ToString() + "\" y=\"" + box.yPos.ToString() + "\" height=\"" + box.height.ToString() + "\" width=\"" + box.width.ToString() + "\" style=\"stroke:" + svgConfig.EntityBorderColor + "; fill: " + svgConfig.EntityFillColor  + "; fill-opacity: " + svgConfig.EntityFillOpacity  + "\"/>")
//        sw.WriteLine ("<text x=\"" + string (box.xPos+svgConfig.TextMargin) + "\" y=\"" + string (box.yPos+svgConfig.FontSize+svgConfig.TextMargin) + "\" font-family=\"Verdana\" font-size=\"" + svgConfig.FontSize.ToString() + "\">")
//        sw.WriteLine box.Entity.Title.text
//        sw.WriteLine "</text>"
//        let dividerYPos = box.yPos + svgConfig.FontSize * 2
//        sw.WriteLine ("<line x1=\"" + box.xPos.ToString() + "\" y1=\"" + dividerYPos.ToString() + "\" x2=\"" + string (box.xPos + box.width) + "\" y2=\"" + string dividerYPos + "\" stroke-width=\"" + svgConfig.EntityBorderWidth + "\" stroke=\"" + svgConfig.EntityBorderColor + "\"/>")
//        box.Entity.Attributes |> List.iteri(fun i x->
//            let attributeTextYPos = dividerYPos + svgConfig.FontSize + (i * svgConfig.FontSize)
//            sw.WriteLine ("<text x=\"" + string (box.xPos+svgConfig.TextMargin) + "\" y=\"" + string (attributeTextYPos+svgConfig.TextMargin) + "\" font-family=\"Verdana\" font-size=\"" + string svgConfig.FontSize + "\">")
//            let attText:string = x.Title.text
//            sw.WriteLine (attText)
//            sw.WriteLine "</text>"
//            )
//        ()
//    let drawEntityBoxes (sw:System.IO.StreamWriter) (xpos:int) (ypos:int) (width:int) (height:int) (entity:Entity) =
//        let newBox =
//            {
//                xPos=xpos
//                yPos=ypos
//                width=width
//                height=height
//                Entity=entity
//            }
//        drawEntityBox sw newBox defaultSVGSetup
//    /// writeHtmlBeginAndHead (sw:System.IO.StreamWriter) (title:string) (description:string) (author:string) (fileShortName:string)
//    let writeHtmlBeginAndHead (sw:System.IO.StreamWriter) (title:string) (description:string) (author:string) (fileShortName:string) =
//        sw.WriteLine("<!DOCTYPE html> ")
//        sw.WriteLine("<html lang='en'> ")
//        sw.WriteLine("    <head> ")
//        sw.WriteLine("        <meta http-equiv='content-type' content='text/html; charset=UTF-8'>  ")
//        sw.WriteLine("        <meta charset='utf-8'> ")
//        sw.WriteLine("        <link rel='SHORTCUT ICON' href='favico.png'/> ")
//        sw.WriteLine("        <title>" + title + "</title> ")
//        sw.WriteLine("        <meta itemprop='name' content='" + title + "'> ")
//        sw.WriteLine("        <meta itemprop='description' content='" + description + "'> ")
//        sw.WriteLine("        <meta name='description' content='" + description + "' /> ")
//        sw.WriteLine("        <meta itemprop='image' content='" + fileShortName + ".png'> ")
//        sw.WriteLine("        <meta name='author' content='" + author + "' /> ")
//        sw.WriteLine("        <meta name='viewport' content='width=device-width, initial-scale=1, maximum-scale=1'> ")
//        sw.WriteLine("        <link href='main.css' type='text/css' rel='stylesheet'> ")
//        sw.WriteLine("        <link href='" + fileShortName + ".css' type='text/css' rel='stylesheet'> ")
//        sw.WriteLine("    </head> ")
//    [<NoComparison>]
//    type ItemReport =
//        {
//            ItemQualifiers:(ItemConnectorType*ModelItem) list
//            ItemDetail:ModelItem list
//            Notes:ModelItem list
//            Questions:ModelItem list
//            ToDos:ModelItem List
//            Work:ModelItem list
//            Realizations:ModelItem list
//            Feedback:ModelItem list
//        }
//        with member self.AllTogether =
//                List.concat [self.ItemDetail; self.Notes; self.Questions; self.ToDos; self.Work; self.Realizations; self.Feedback]
//    let createItemReport (modelItems:ModelItem list) (selectedModelItem:ModelItem) =
//        // add any item qualifiers (modelItem juniors) that apply
//        let qualifierConnectionList = modelItems |> List.choose(fun z->
//            match z.ItemType with
//                |Connection(c)->
//                    match c.ConnectionType with
//                        |ParentChild->option.None
//                        |_->
//                            if c.LhsId=selectedModelItem.Id
//                                then
//                                    let targetItem = getModelItemById modelItems c.RhsId                                 
//                                    Some(c.ConnectionType, targetItem)
//                                else option.None
//                |_->option.None
//            )
////        let sortedQualifierConnection = qualifierConnectionList |> Seq.groupBy(fun z->
////            (fst z))
////        sortedqualifierconnection |> seq.iter(fun y->
////            switemdetailtextfilewriter.write("        " + (fst y).tostring().toupper() + " ")
////            //(snd y) |> seq.iter(fun z->switemdetailtextfilewriter.write(" " + (snd z).modelitemname))
////            let listofstringstojoin = (snd y) |> seq.map(fun z->(snd z).modelitemname)
////            let linetowrite = string.concat ", " listofstringstojoin
////            switemdetailtextfilewriter.writeline(linetowrite)
////            )

//        let collectedNotes= modelItems |> List.filter(fun x->
//            match x.ItemType with
//                |Note(_)->x.ModelParent=selectedModelItem.Id
//                |_->false
//            )
//        let collectedQuestions= modelItems |> List.filter(fun x->
//            match x.ItemType with
//                |Question(_)->x.ModelParent=selectedModelItem.Id
//                |_->false
//            )
//        let collectedToDos= modelItems |> List.filter(fun x->
//            match x.ItemType with
//                |ToDo(_)->x.ModelParent=selectedModelItem.Id
//                |_->false
//            )
//        let collectedWork= modelItems |> List.filter(fun x->
//            match x.ItemType with
//                |Work(_)->x.ModelParent=selectedModelItem.Id
//                |_->false
//            )
//        let collectedRealizations= modelItems |> List.filter(fun x->
//            match x.ItemType with
//                |ModelItem(_)->x.ModelParent=selectedModelItem.Id
//                |_->false
//            )
//        {
//            ItemQualifiers=qualifierConnectionList
//            ItemDetail=[]
//            Notes=collectedNotes
//            Questions=collectedQuestions
//            ToDos=collectedToDos
//            Work=collectedWork
//            Realizations=collectedRealizations
//            Feedback=[]
//        }
//    let makeContextAString (genre:Genres) (bucket:Buckets) (temporalIndicator:TemporalIndicators) (abstractionLevel:AbstractionLevels) =
//        genre.ToString() + " " + bucket.ToString() + " " + abstractionLevel.ToString() + " " + temporalIndicator.ToString()
//    let writeATableCell (sb:System.Text.StringBuilder) s =
//        sb.Append ("    <td>" + s + "</td>\n") |> ignore
////    let getModelItemById (modelItems:ModelItem list) (id:int) =
////        modelItems |> List.find(fun x->x.Id=id)
//    let filterModelItemsByTag (modelItems:ModelItem list) (genre:Genres) (abstractionLevel:AbstractionLevels) (temporalIndicator:TemporalIndicators) (bucket:Buckets) =
//        modelItems |> List.filter(fun x->
//            ((x.Genre=genre) || (genre=Genres.Unknown))
//            && ((x.AbstractionLevel=abstractionLevel) || (abstractionLevel=AbstractionLevels.Unknown))
//            && ((x.Bucket=bucket) || (bucket=Buckets.Unknown))
//            && ((x.TemporalIndicator=temporalIndicator) || (temporalIndicator=TemporalIndicators.Unknown))
//            )
//    let filterModelItemsByNOTType (modelItemTypeName:string) (modelItems:ModelItem list) =
//        let modelItemTypeNameLength = modelItemTypeName.Length
//        modelItems |> List.filter(fun x->
//            let indexedItemTypeName = x.ItemType.ToString()
//            let xItemTypeString = x.ItemType.ToString()
//            let xItemTypeStringLength =xItemTypeString.Length
//            (xItemTypeStringLength<modelItemTypeNameLength) 
//            || ((xItemTypeString.GetLeft modelItemTypeNameLength) <> modelItemTypeName)
//            )
//    let filterModelItemsByType (modelItemTypeName:string) (modelItems:ModelItem list) =
//        let modelItemTypeNameLength = modelItemTypeName.Length
//        modelItems |> List.filter(fun x->
//            let indexedItemTypeName = x.ItemType.ToString()
//            let xItemTypeString = x.ItemType.ToString()
//            let xItemTypeStringLength =xItemTypeString.Length
//            (xItemTypeStringLength>modelItemTypeNameLength) 
//            && ((xItemTypeString.GetLeft modelItemTypeNameLength) = modelItemTypeName)
//            )
//    let filterModelItemsByTagAndType (modelItems:ModelItem list) (genre:Genres) (abstractionLevel:AbstractionLevels) (temporalIndicator:TemporalIndicators) (bucket:Buckets) (modelItemTypeName:string):ModelItem list=
//        let filteredByTags = filterModelItemsByTag modelItems genre abstractionLevel temporalIndicator bucket
//        filteredByTags |> filterModelItemsByType  modelItemTypeName
//    let itemsThatAreRelatedToThisItem (thisItem:ModelItem) (modelItems:ModelItem list)= 
//        modelItems |> List.filter(fun y->
//            y.ModelParent=thisItem.Id
//            )
//    let getAllRootItemsForTags (modelItems:ModelItem list) (genre:Genres) (abstractionLevel:AbstractionLevels) (temporalIndicator:TemporalIndicators) (bucket:Buckets) =
//        let itemsFilteredByTag = filterModelItemsByTag modelItems genre abstractionLevel temporalIndicator bucket
//        let modelItemsWithContextShiftParentsOrNoParent = itemsFilteredByTag |> List.filter(fun x->
//            match x.SourceCodeParent with
//                |0->true // parent is empty
//                |parent->
//                    match (modelItems|>List.tryFind(fun y->y.Id=parent)) with
//                        | Some realParent->
//                            match realParent.ItemType with
//                                | ModelItemType.ContextShift(ctx)->true // parent is a context shift, which makes this a root item
//                                |_->false // parent is anything else
//                        | option.None->true // parent doesn't exist
//            )
//        let rootItemsOfAnyType = modelItemsWithContextShiftParentsOrNoParent |> List.filter(fun x->match x.ItemType with |ModelItemType.ContextShift(_)->false|ModelItemType.ModelItem(_)->true|_->false)
//        rootItemsOfAnyType
//    let getModelItemRootItems (modelItems:ModelItem list) (genre:Genres) (abstractionLevel:AbstractionLevels) (temporalIndicator:TemporalIndicators) (bucket:Buckets) =
//        let rootItemsOfAnyType = getAllRootItemsForTags modelItems genre abstractionLevel temporalIndicator bucket
//        let rootItemsOfModelItemType = rootItemsOfAnyType |> List.filter(fun x->
//            match x.ItemType,x.SourceCodeParent with
//                |ModelItemType.ModelItem(_), myParent->true
//                |_,_ ->false
//            )
//        rootItemsOfModelItemType

//    let getRootLevelUnattachedSubItems (modelItems:ModelItem list) (genre:Genres) (abstractionLevel:AbstractionLevels) (temporalIndicator:TemporalIndicators) (bucket:Buckets) (modelItemTypeName:string):ModelItem list=
//        let allItems=filterModelItemsByTagAndType modelItems genre abstractionLevel temporalIndicator bucket modelItemTypeName
//        //let allQuestions = modelItems |> List.filter(fun x->match x.ItemType with |Question(q)->true |_->false)
//        // has no parent or the parent is a context shift
//        let unattachedItemsWithoutTagFilter = allItems |> List.filter(fun x->
//            match x.SourceCodeParent with
//                | 0->false
//                | id->
//                    let parent = getModelItemById modelItems id
//                    let parentItemType=parent.ItemType.ToString()
//                    let lengthOk=parentItemType.Length>13
//                    let leftSide=parentItemType.GetLeft 13
//                    let leftSideMatches=leftSide = "CONTEXT SHIFT"
//                    (lengthOk && leftSideMatches)
//            )
//        let unattachedItems = filterModelItemsByTag unattachedItemsWithoutTagFilter Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior
//        unattachedItems

//    let writeModelItemDetailHtmlTableHead (swItemDetailTextFileWriter:System.IO.StreamWriter) =
//        swItemDetailTextFileWriter.WriteLine "<table><thead><tr>"
//        swItemDetailTextFileWriter.WriteLine ("<td>" + "Item Number" + "</td>")
//        swItemDetailTextFileWriter.WriteLine ("<td>" + "Id Number" + "</td>")
//        swItemDetailTextFileWriter.WriteLine ("<td>" + "Source<br/>Code</br>Parent" + "</td>")
//        swItemDetailTextFileWriter.WriteLine ("<td>" + "Model</br>Parent" + "</td>")
//        swItemDetailTextFileWriter.WriteLine ("<td>" + "Context" + "</td>")
//        swItemDetailTextFileWriter.WriteLine ("<td>" + "Item Type" + "</td>")
//        swItemDetailTextFileWriter.WriteLine ("<td>" + "ShortName" + "</td>")
//        swItemDetailTextFileWriter.WriteLine ("<td>" + "Reference" + "</td>")
//        swItemDetailTextFileWriter.WriteLine ("<td>" + "Reference Line Number" + "</td>")
//        swItemDetailTextFileWriter.WriteLine "</tr></thead>"
//    let writeModelItemDetailHtmlTableDetail (swItemDetailTextFileWriter:System.IO.StreamWriter) (iterationNumber:int) (modelItem:ModelItem) =
//        let sb=new System.Text.StringBuilder(4096)
//        sb.Append("<tr>\n") |> ignore
//        sb.Append ("<td>" + iterationNumber.ToString() + "</td>") |> ignore
//        writeATableCell sb (modelItem.Id.ToString())
//        writeATableCell sb (if modelItem.SourceCodeParent<>0 then modelItem.SourceCodeParent.ToString() else "")
//        writeATableCell sb (if modelItem.ModelParent<>0 then modelItem.ModelParent.ToString() else "")
//        writeATableCell sb (makeContextAString modelItem.Genre modelItem.Bucket modelItem.TemporalIndicator modelItem.AbstractionLevel)
//        writeATableCell sb (modelItem.ItemType.ToString())
//        writeATableCell sb (modelItem.ModelItemName)
//        writeATableCell sb (modelItem.SourceReferences.Item(modelItem.SourceReferences.Length-1).File.FullName)
//        let referenceLineNumber= modelItem.SourceReferences.Item(modelItem.SourceReferences.Length-1).LineNumber.ToString()
//        writeATableCell sb (referenceLineNumber)
//        sb.Append ("</tr>\n") |> ignore
//        swItemDetailTextFileWriter.WriteLine (sb.ToString())
//    let saveDetailedViewHtml (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) (selectedItem:ModelItem) =
//        let relatedItems = createItemReport modelItems selectedItem //itemsThatAreRelatedToThisItem selectedItem modelItems
//        let itemDetailFileName=selectedItem.ToFileName + ".html"
//        System.IO.File.Delete(itemDetailFileName)
//        let swItemDetailTextFileWriter = System.IO.File.CreateText(itemDetailFileName)
//        writeHtmlBeginAndHead swItemDetailTextFileWriter selectedItem.ModelItemName (selectedItem.ModelItemName + ": Detailed View") "EasyAM" selectedItem.ToFileName
//        swItemDetailTextFileWriter.WriteLine "<body>"
//        swItemDetailTextFileWriter.WriteLine ("<h2>" + selectedItem.ToModelHeading + "</h2>")
//        swItemDetailTextFileWriter.WriteLine ("<h1>" + selectedItem.ModelItemName + "</h1>")

//        swItemDetailTextFileWriter.WriteLine("<p>")
//        let sortedQualifierConnection = relatedItems.ItemQualifiers |> Seq.groupBy(fun z->
//            (fst z))
//        sortedQualifierConnection |> Seq.iter (fun y->
//            swItemDetailTextFileWriter.Write("        " + (fst y).ToString().ToUpper() + " ")
//            let listofstringstojoin = (snd y) |> Seq.map(fun z->(snd z).ModelItemName)
//            let linetowrite = String.concat ", " listofstringstojoin
//            swItemDetailTextFileWriter.WriteLine(linetowrite)
//            )
//        swItemDetailTextFileWriter.WriteLine("</p>")

//        swItemDetailTextFileWriter.WriteLine ()
//        if relatedItems.ItemDetail.Length>0
//            then
//                swItemDetailTextFileWriter.WriteLine ("<h3><a name='ItemDetail'>Item Detail</h3>")
//            else ()
//        if relatedItems.Notes.Length>0
//            then
//                swItemDetailTextFileWriter.WriteLine ("<h3><a name='Notes'>Notes</h3>")
//                swItemDetailTextFileWriter.WriteLine("           <ul> ")
//                relatedItems.Notes |> List.iter(fun k->
//                    match k.ItemType with
//                        |ModelItemType.Note(a) as n->
//                            swItemDetailTextFileWriter.WriteLine("               <li> " + a.Text + "</li>")
//                        |_->()
//                    )
//                swItemDetailTextFileWriter.WriteLine("           </ul> ")
//            else ()
//        if relatedItems.Questions.Length>0
//            then
//                swItemDetailTextFileWriter.WriteLine ("<h3><a name='Questions'>Questions</h3>")
//                swItemDetailTextFileWriter.WriteLine("           <ul> ")
//                relatedItems.Questions |> List.iter(fun k->
//                    match k.ItemType with
//                        |ModelItemType.Question(a) as n->
//                            swItemDetailTextFileWriter.WriteLine("               <li> " + a.Text + "</li>")
//                        |_->()
//                    )
//                swItemDetailTextFileWriter.WriteLine("           </ul> ")
//            else ()
//        if relatedItems.ToDos.Length>0
//            then
//                swItemDetailTextFileWriter.WriteLine ("<h3><a name='ToDo'>To-Do</h3>")
//                swItemDetailTextFileWriter.WriteLine("           <ul> ")
//                relatedItems.ToDos |> List.iter(fun k->
//                    match k.ItemType with
//                        |ModelItemType.ToDo(a) as n->
//                            swItemDetailTextFileWriter.WriteLine("               <li> " + a.Text + "</li>")
//                        |_->()
//                    )
//                swItemDetailTextFileWriter.WriteLine("           </ul> ")
//            else ()
//        if relatedItems.Work.Length>0
//            then
//                swItemDetailTextFileWriter.WriteLine ("<h3><a name='Work'>Work</h3>")
//                swItemDetailTextFileWriter.WriteLine("           <ul> ")
//                relatedItems.Work |> List.iter(fun k->
//                    match k.ItemType with
//                        |ModelItemType.Work(a) as n->
//                            swItemDetailTextFileWriter.WriteLine("               <li> " + a.Text + "</li>")
//                        |_->()
//                    )
//                swItemDetailTextFileWriter.WriteLine("           </ul> ")
//            else ()
//        if relatedItems.Realizations.Length>0
//            then
//                swItemDetailTextFileWriter.WriteLine ("<h3><a name='Realizations'>Realizations</h3>")
//                swItemDetailTextFileWriter.WriteLine("           <ul> ")
//                relatedItems.Realizations |> List.iter(fun k->
//                    match k.ItemType with
//                        |ModelItemType.ModelItem(a) as n->
//                            swItemDetailTextFileWriter.WriteLine("               <li> " + a.ModelItemName + " <a href='" + k.ToFileName + ".html'>Related</a></li>")
//                        |_->()
//                    )
//                swItemDetailTextFileWriter.WriteLine("           </ul> ")
//            else ()
//        let associatedSupplementalConnections = modelItems |> List.filter(fun z->
//                match z.ItemType with
//                    | Connection(c)->c.RhsId=selectedItem.Id && c.ConnectionType=Affects
//                    |_->false
//            )
//        if associatedSupplementalConnections.Length>0
//            then
//                let associatedSupplementalItems=associatedSupplementalConnections |> List.map(fun z->
//                    getModelItemById modelItems z.ModelParent
//                    )
//                swItemDetailTextFileWriter.WriteLine ("<h3><a name='Supplementals'>Supplementals</h3>")
//                swItemDetailTextFileWriter.WriteLine("           <ul> ")
//                associatedSupplementalItems |> List.iter(fun k->
//                    match k.ItemType with
//                        |ModelItemType.ModelItem(a) as n->
//                            swItemDetailTextFileWriter.WriteLine("               <li> " + a.ModelItemName + " <a href='" + k.ToFileName + ".html'>Related</a></li>")
//                        |_->()
//                    )
//                swItemDetailTextFileWriter.WriteLine("           </ul> ")
//            else ()
//        if relatedItems.Feedback.Length>0
//            then
//                swItemDetailTextFileWriter.WriteLine ("<h3><a name='Feedback'>Feedback</h3>")
//            else ()
//        swItemDetailTextFileWriter.WriteLine("</table>")
//        swItemDetailTextFileWriter.WriteLine("<p><a href='" + (selectedItem.ToFileName  + ".amout") + "'>Code</a></p>")
//        let k=selectedItem.ToAbbreviatedModelHeading
//        swItemDetailTextFileWriter.WriteLine ("<h3>" + "DEBUG: Incoming Data Dump" + "</h3>")
//        writeModelItemDetailHtmlTableHead swItemDetailTextFileWriter
//        writeModelItemDetailHtmlTableDetail swItemDetailTextFileWriter 0 selectedItem
//        modelItems |> List.iteri(fun i j->
//            if j.ModelParent=selectedItem.Id
//                then writeModelItemDetailHtmlTableDetail swItemDetailTextFileWriter i j
//                else ()
//            )
//        swItemDetailTextFileWriter.WriteLine "</body></html>"
//        swItemDetailTextFileWriter.Flush()
//        swItemDetailTextFileWriter.Close()
//    let saveMasterIndex opts modelItems =
//        let fileName= "index.html"
//        System.IO.File.Delete(fileName)
//        let sw = System.IO.File.CreateText(fileName)
//        writeHtmlBeginAndHead sw "Our Universe" "Everything We are Working On" "EasyAM" "index"
//        sw.WriteLine "<body>"
//        sw.WriteLine ("<h1>Our Universe</h1>")
//        let unassignedQuestions = modelItems |> List.filter(fun x->
//            match x.ItemType with
//                |Question(_)->
//                    ( (x.Genre=Genres.None) || (x.Genre=Genres.Unknown))
//                            && ((x.AbstractionLevel=AbstractionLevels.None) || (x.AbstractionLevel=AbstractionLevels.Unknown))
//                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
//                            && ((x.Bucket=Buckets.None) || (x.Bucket=Buckets.Unknown))
//                |_->false
//            )
//        if unassignedQuestions.Length>0
//            then
//                sw.WriteLine ("<h2>Questions</h2>")
//                unassignedQuestions |> List.iter(fun x->match x.ItemType with |Question(q)->sw.WriteLine("<p>" + q.Text + "</p>") |_->())   
//            else ()
//        sw.WriteLine ("<h2>Overview</h2>")
//        sw.WriteLine ("<h3><a href='business-behavior-abstract-to-be.html'>Benefits We Offer Our Clients</a></h3>")
//        sw.WriteLine ("<h3><a href='business-structure-abstract-to-be.html'>Definitions, Terms, and Systems</h3>")
//        sw.WriteLine ("<h3><a href='business-supplemental-abstract-to-be.html'>Rules We Do Our Work By</h3>")
//        sw.WriteLine ("<h2>How Things Have Went So Far</h2>")
//        sw.WriteLine ("<h3>Completed Work</h3>")
//        sw.WriteLine ("<h2>Where We Are Now</h2>")
//        sw.WriteLine ("<h3><a href='questions.html'>What We Need To Know To Do Our Job</a></h3>")
//        sw.WriteLine ("<h3>Open Bugs</h3>")
//        sw.WriteLine ("<h3>Work In Progress</h3>")
//        sw.WriteLine ("<h2>Where We're Planning To Go</h2>")
//        sw.WriteLine ("<h3>Release Plan</h3>")
//        sw.WriteLine ("<h3>Roadmap</h3>")
//        let unassignedNotes = modelItems |> List.filter(fun x->
//            match x.ItemType with
//                |Note(_)->
//                    ( (x.Genre=Genres.None) || (x.Genre=Genres.Unknown))
//                            && ((x.AbstractionLevel=AbstractionLevels.None) || (x.AbstractionLevel=AbstractionLevels.Unknown))
//                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
//                            && ((x.Bucket=Buckets.None) || (x.Bucket=Buckets.Unknown))
//                |_->false
//            )
//        if unassignedNotes.Length>0
//            then
//                sw.WriteLine ("<h2>Notes</h2>")
//                unassignedNotes |> List.iter(fun x->match x.ItemType with |Note(q)->sw.WriteLine("<p>" + q.Text + "</p>") |_->())   
//            else ()
//        sw.WriteLine ()
//        sw.WriteLine "</body></html>"
//        sw.Flush()
//        sw.Close()

//    let saveMasterQuestionList (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) =
//        let fileName= "questions.html"
//        let allQuestions = modelItems |> List.filter(fun x->match x.ItemType with |Question(_)->true |_->false)
//        let unassignedBusinessQuestions = allQuestions |> List.filter(fun x->
//                    ( (x.Genre=Genres.Business))
//                            && ((x.AbstractionLevel=AbstractionLevels.None) || (x.AbstractionLevel=AbstractionLevels.Unknown))
//                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
//                            && ((x.Bucket=Buckets.None) || (x.Bucket=Buckets.Unknown)))
//        let unassignedBusinessBehaviorQuestions = allQuestions |> List.filter(fun x->
//                    ( (x.Genre=Genres.Business))
//                            && ((x.AbstractionLevel=AbstractionLevels.None) || (x.AbstractionLevel=AbstractionLevels.Unknown))
//                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
//                            && ((x.Bucket=Buckets.Behavior) ))
//        let unassignedBusinessStructureQuestions = allQuestions |> List.filter(fun x->
//                    ( (x.Genre=Genres.Business))
//                            && ((x.AbstractionLevel=AbstractionLevels.None) || (x.AbstractionLevel=AbstractionLevels.Unknown))
//                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
//                            && ((x.Bucket=Buckets.Structure) ))
//        let unassignedBusinessSupplementalQuestions = allQuestions |> List.filter(fun x->
//                    ( (x.Genre=Genres.Business))
//                            && ((x.AbstractionLevel=AbstractionLevels.None) || (x.AbstractionLevel=AbstractionLevels.Unknown))
//                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
//                            && ((x.Bucket=Buckets.Supplemental) ))
//        let unassignedBusinessBehaviorRealizedQuestions = allQuestions |> List.filter(fun x->
//                    ( (x.Genre=Genres.Business))
//                            && ((x.AbstractionLevel=AbstractionLevels.Realized) )
//                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
//                            && ((x.Bucket=Buckets.Behavior) ))
//        let unassignedBusinessStructureRealizedQuestions = allQuestions |> List.filter(fun x->
//                    ( (x.Genre=Genres.Business))
//                            && ((x.AbstractionLevel=AbstractionLevels.Realized))
//                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
//                            && ((x.Bucket=Buckets.Structure) ))
//        let unassignedBusinessSupplementalRealizedQuestions = allQuestions |> List.filter(fun x->
//                    ( (x.Genre=Genres.Business))
//                            && ((x.AbstractionLevel=AbstractionLevels.None))
//                            && ((x.TemporalIndicator=TemporalIndicators.None) || (x.TemporalIndicator=TemporalIndicators.Unknown))
//                            && ((x.Bucket=Buckets.Supplemental) ))
//        System.IO.File.Delete(fileName)
//        let sw = System.IO.File.CreateText(fileName)
//        writeHtmlBeginAndHead sw "What We Need To Know" "Questions We Need Answers To" "EasyAM" "questions"
//        sw.WriteLine "<body>"
//        sw.WriteLine ("<h1>What we need to know</h1>")
//        sw.WriteLine ("<h2>Business-Related Questions</h2>")
//        sw.WriteLine ("<h3>Overall Business Questions</h3>")
//        sw.WriteLine ("<ul>")
//        unassignedBusinessQuestions |> List.iteri(fun i x->
//            match x.ItemType with
//                |Question(q)->
//                    sw.WriteLine ("<li>" + q.Text + " <a href='" + ("business-behavior-abstract-to-be" + ".amout") + "'>(Related material):</a>" + "</li>")
//                |_->()
//            )
//        sw.WriteLine ("</ul>")
//        sw.WriteLine ("<h3>Questions about the way in general people do the kinds of things we want to help folks with</h3>")
//        let allModelItemsForThisSection = getModelItemRootItems modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior
//        let allQuestions = filterModelItemsByTagAndType modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior "QUESTION"
//        allModelItemsForThisSection |> List.iteri(fun i x->
//            let questionsForThisModelItem = allQuestions |> List.filter(fun y->
//                match y.SourceCodeParent with |0->false |parent->parent=x.Id)
//            match questionsForThisModelItem.Length with
//                |0->()
//                |_->
//                    sw.WriteLine("<li>For " + x.ModelItemName + " <a href='" + (x.ToFileName + ".amout") + "'>(Related material):</a>")
//                    sw.WriteLine ("<ul>")
//                    questionsForThisModelItem |> List.iteri(fun j y->
//                        match y.ItemType with
//                            |Question(q)->sw.WriteLine ("<li>" + q.Text + "</li>")
//                            |_->()
//                        )
//                    sw.WriteLine ("</ul>")            
//                    sw.WriteLine("</li>")
//            )
//        sw.WriteLine ("<h3>Questions about specific ways of doing things our target audience does that we're going to help them with</h3>")
//        let allModelItemsForThisSection = getModelItemRootItems modelItems Genres.Business AbstractionLevels.Realized TemporalIndicators.ToBe Buckets.Behavior
//        allModelItemsForThisSection |> List.iteri(fun i x->
//            let questionsForThisModelItem = allQuestions |> List.filter(fun y->
//                match y.SourceCodeParent with |0->false |parent->parent=x.Id)
//            match questionsForThisModelItem.Length with
//                |0->()
//                |_->
//                    sw.WriteLine("<li>For " + x.ModelItemName + " <a href='" + (x.ToFileName + ".amout") + "'>(Related material):</a>")
//                    sw.WriteLine ("<ul>")
//                    questionsForThisModelItem |> List.iteri(fun j y->
//                        match y.ItemType with
//                            |Question(q)->sw.WriteLine ("<li>" + q.Text + "</li>")
//                            |_->()
//                        )
//                    sw.WriteLine ("</ul>")            
//                    sw.WriteLine("</li>")
//            )

//        sw.WriteLine ("<h2>Overall Technology Capability And Functional Flow (Not Structure) Questions</h2>")
//        sw.WriteLine ("<h3>Questions about the general functional flow our technology uses to help folks</h3>")
//        sw.WriteLine ("<h3>Questions about the specific technology flow we use in a certain context to help folks</h3>")

//        sw.WriteLine ()
//        sw.WriteLine ("<h3>" + "DEBUG: Incoming Data Dump" + "</h3>")
//        writeModelItemDetailHtmlTableHead sw
//        allQuestions |> List.iteri(fun i j->
//            writeModelItemDetailHtmlTableDetail sw i j
//            )
//        sw.WriteLine "</body></html>"
//        sw.Flush()
//        sw.Close()

//    let saveDetailedViewAmout (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) (selectedItem:ModelItem) =
//        let relatedItems = createItemReport  modelItems selectedItem
//        let itemDetailFileName=selectedItem.ToFileName + ".amout"
//        System.IO.File.Delete(itemDetailFileName)
//        let swItemDetailTextFileWriter = System.IO.File.CreateText(itemDetailFileName)
//        swItemDetailTextFileWriter.WriteLine selectedItem.ToModelHeading
//        swItemDetailTextFileWriter.Write("    " + selectedItem.ModelItemName)
//        // add a parent in, if applicable
//        if selectedItem.AbstractionLevel=AbstractionLevels.Realized
//            then
//                if selectedItem.ModelParent <> 0
//                    then
//                        let parent = getModelItemById modelItems selectedItem.ModelParent
//                        swItemDetailTextFileWriter.WriteLine(" PARENT " + parent.ModelItemName)
//                        else swItemDetailTextFileWriter.WriteLine("#### MISSING PARENT ####")
//            else swItemDetailTextFileWriter.WriteLine()
////        // add any item qualifiers (modelItem juniors) that apply
//        let sortedQualifierConnection = relatedItems.ItemQualifiers |> Seq.groupBy(fun z->
//            (fst z))
//        sortedQualifierConnection |> Seq.iter (fun y->
//            swItemDetailTextFileWriter.Write("        " + (fst y).ToString().ToUpper() + " ")
//            let listofstringstojoin = (snd y) |> Seq.map(fun z->(snd z).ModelItemName)
//            let linetowrite = String.concat ", " listofstringstojoin
//            swItemDetailTextFileWriter.WriteLine(linetowrite)
//            )

//        swItemDetailTextFileWriter.WriteLine("")
//        swItemDetailTextFileWriter.WriteLine("        NOTES")
//        relatedItems.Notes |> List.iter(fun k->
//            match k.ItemType with
//                |ModelItemType.Note(a) as n->
//                    swItemDetailTextFileWriter.WriteLine("            " + a.Text )
//                |_->()
//            )
//        swItemDetailTextFileWriter.WriteLine("        QUESTIONS")
//        relatedItems.Questions |> List.iter(fun k->
//            match k.ItemType with
//                |ModelItemType.Question(a) as n->
//                    swItemDetailTextFileWriter.WriteLine("            " + a.Text )
//                |_->()
//            )
//        swItemDetailTextFileWriter.WriteLine("        TODO")
//        relatedItems.ToDos |> List.iter(fun k->
//            match k.ItemType with
//                |ModelItemType.ToDo(a) as n->
//                    swItemDetailTextFileWriter.WriteLine("            " + a.Text )
//                |_->()
//            )
//        swItemDetailTextFileWriter.WriteLine("        WORK")
//        relatedItems.Work |> List.iter(fun k->
//            match k.ItemType with
//                |ModelItemType.Work(a) as n->
//                    swItemDetailTextFileWriter.WriteLine("            " + a.Text )
//                |_->()
//            )
//        swItemDetailTextFileWriter.WriteLine("")
//        swItemDetailTextFileWriter.WriteLine("        REALIZATIONS")
//        relatedItems.Realizations |> List.iter(fun k->
//            match k.ItemType with
//                |ModelItemType.ModelItem(a) as n->
//                    swItemDetailTextFileWriter.WriteLine("            " + a.ModelItemName )
//                |_->()
//            )
//        swItemDetailTextFileWriter.WriteLine("")
//        swItemDetailTextFileWriter.WriteLine("        FEEDBACK")

//        swItemDetailTextFileWriter.Flush()
//        swItemDetailTextFileWriter.Close()
//    let saveCompiledModelSectionHtml (opts:Types.EasyAMProgramConfig) (bucketToSave:Buckets) (genreToSave:Genres) (modelItems:ModelItem list) =
//        let topLevelItemsAndRelated = filterModelItemsByTag modelItems genreToSave AbstractionLevels.Abstract TemporalIndicators.ToBe bucketToSave
//        //modelItems |> List.filter(fun x->x.Genre=Genres.Business && x.Bucket=Buckets.Behavior && x.AbstractionLevel=AbstractionLevels.Abstract && x.TemporalIndicator=TemporalIndicators.ToBe)
//        let sectionHeading=genreToSave.ToString() + " " + bucketToSave.ToString() + " " + "Abstract To-Be"  //"Business Behavior Abstract To-Be"
//        let itemDetailFileName=sectionHeading.CovertIntoOSSafeFileName + ".html"
//        System.IO.File.Delete(itemDetailFileName)
//        let swItemDetailTextFileWriter = System.IO.File.CreateText(itemDetailFileName)
//        writeHtmlBeginAndHead swItemDetailTextFileWriter sectionHeading "Section Heading" "EasyAM" itemDetailFileName
//        swItemDetailTextFileWriter.WriteLine "<body>"
//        swItemDetailTextFileWriter.WriteLine ("<h1>" + sectionHeading + " (" + string topLevelItemsAndRelated.Length + ")</h1>")
//        swItemDetailTextFileWriter.WriteLine ()
//        swItemDetailTextFileWriter.WriteLine ("</body></html>")
//        let topLevelItems = topLevelItemsAndRelated |> List.filter(fun x->match x.ItemType with |ModelItemType.ModelItem(_)->true |_->false)
//        topLevelItems |> List.iter(fun y->
//            //let itemChildren = modelItems |> List.filter(fun j->j.SourceCodeParent<>0&&j.SourceCodeParent=y.Id)
//            let itemChildren = createItemReport modelItems y
//            let itemChildrenCount = itemChildren.Feedback.Length + itemChildren.ItemDetail.Length + itemChildren.Notes.Length + itemChildren.Questions.Length + itemChildren.Realizations.Length + itemChildren.ToDos.Length + itemChildren.Work.Length
//            let modelItemDetailHtmlFileName= y.ToFileName + ".html"
//            swItemDetailTextFileWriter.WriteLine("<h2><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "'>" + y.ModelItemName + "</a> (" + itemChildrenCount.ToString() + ")</h2>")
//            swItemDetailTextFileWriter.WriteLine "<table>"
//            swItemDetailTextFileWriter.WriteLine ("<tr><td><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "#ItemDetail'>Item Detail</a> (" + string itemChildren.ItemDetail.Length + ")</td>")
//            swItemDetailTextFileWriter.WriteLine "<td></td>"
//            swItemDetailTextFileWriter.WriteLine ("<td><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "#Notes'>Notes</a> (" + string itemChildren.Notes.Length + ")</td>")
//            swItemDetailTextFileWriter.WriteLine "<td></td>"
//            swItemDetailTextFileWriter.WriteLine ("<td><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "#Questions'>Questions</a> (" + string itemChildren.Questions.Length + ")</td>")
//            swItemDetailTextFileWriter.WriteLine "<td></td></tr>"
//            swItemDetailTextFileWriter.WriteLine ("<tr><td><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "#ToDo'>To-Dos</a> (" + string itemChildren.ToDos.Length + ")</td>")
//            swItemDetailTextFileWriter.WriteLine "<td></td>"
//            swItemDetailTextFileWriter.WriteLine ("<td><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "#Work'>Work</a> (" + string itemChildren.Work.Length + ")</td>")
//            swItemDetailTextFileWriter.WriteLine "<td></td>"
//            swItemDetailTextFileWriter.WriteLine ("<td><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "#Realizations'>Realizations</a> (" + string itemChildren.Realizations.Length + ")</td>")
//            swItemDetailTextFileWriter.WriteLine "<td></td>"
//            swItemDetailTextFileWriter.WriteLine ("<td><a href='" + modelItemDetailHtmlFileName.CovertIntoOSSafeFileName + "#Realizations'>Feedback</a> (" + string itemChildren.Feedback.Length + ")</td>")
//            swItemDetailTextFileWriter.WriteLine "</tr></table>"
//            )
//        swItemDetailTextFileWriter.WriteLine()
//        swItemDetailTextFileWriter.WriteLine("<p><a href='" + (sectionHeading.CovertIntoOSSafeFileName + ".amout") + "'>Code</a></p>")
//        // Debug code
//        swItemDetailTextFileWriter.WriteLine ("<h3>" + "DEBUG: Incoming Data Dump" + "</h3>")
//        writeModelItemDetailHtmlTableHead swItemDetailTextFileWriter
//        topLevelItems |> List.iteri(fun i j->
//            writeModelItemDetailHtmlTableDetail swItemDetailTextFileWriter i j
//            )

//        swItemDetailTextFileWriter.WriteLine "</table></body></html>"
//        swItemDetailTextFileWriter.Flush()
//        swItemDetailTextFileWriter.Close()
//        ()

//    let compiledDumpIncomingModelHtml (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) =
//        saveCompiledModelSectionHtml opts Buckets.Behavior Genres.Business modelItems
//        saveCompiledModelSectionHtml opts Buckets.Structure Genres.Business modelItems
//        saveCompiledModelSectionHtml opts Buckets.Supplemental Genres.Business modelItems

//        saveCompiledModelSectionHtml opts Buckets.Behavior Genres.System modelItems
//        saveCompiledModelSectionHtml opts Buckets.Structure Genres.System modelItems
//        saveCompiledModelSectionHtml opts Buckets.Supplemental Genres.System modelItems

//        saveCompiledModelSectionHtml opts Buckets.Behavior Genres.Meta modelItems
//        saveCompiledModelSectionHtml opts Buckets.Structure Genres.Meta modelItems
//        saveCompiledModelSectionHtml opts Buckets.Supplemental Genres.Meta modelItems

//    let saveCompiledModelSection (bucketToSave:Buckets) (genreToSave:Genres) (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) =
//        let fileName=(genreToSave.ToString().ToUpper() + "-" + bucketToSave.ToString() +  "-ABSTRACT-TO-BE").CovertIntoOSSafeFileName + ".amout"
//        let fileTitle=genreToSave.ToString().ToUpper() + " " + bucketToSave.ToString().ToUpper() +  " ABSTRACT TO-BE"
//        System.IO.File.Delete(fileName)
//        let sw = System.IO.File.CreateText(fileName)
//        sw.WriteLine fileTitle
//        let abstractRootModelItems = getModelItemRootItems modelItems genreToSave AbstractionLevels.Abstract TemporalIndicators.ToBe bucketToSave 
//        abstractRootModelItems |> List.iteri(fun i x->
//            sw.WriteLine ("    " + x.ModelItemName)
//            saveDetailedViewHtml opts modelItems x
//            saveDetailedViewAmout opts modelItems x
//            )
//        let realizedRootModelItems = getModelItemRootItems modelItems genreToSave AbstractionLevels.Realized TemporalIndicators.ToBe bucketToSave 
//        realizedRootModelItems |> List.iteri(fun i x->
//            sw.WriteLine ("    " + x.ModelItemName)
//            saveDetailedViewHtml opts modelItems x
//            saveDetailedViewAmout opts modelItems x
//            )

//        let rootItems = getAllRootItemsForTags modelItems genreToSave AbstractionLevels.Abstract TemporalIndicators.ToBe bucketToSave 
//        sw.WriteLine ""
//        sw.WriteLine "    NOTES  //Notes not attached to any specific item"
//        let rootNotes = getRootLevelUnattachedSubItems modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior "NOTE"
//        rootNotes |> List.iteri(fun i x->match x.ItemType with |Note(n)->sw.WriteLine ("        " + n.Text)|_->())
//        sw.WriteLine "    QUESTIONS  //Questions not attached to any specific item"
//        let rootQuestions = getRootLevelUnattachedSubItems modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior "QUESTION"
//        rootQuestions |> List.iteri(fun i x->match x.ItemType with |Question(q)->sw.WriteLine ("        " + q.Text)|_->())
//        sw.WriteLine "    TODO  //To-do items not attached to any specific item"
//        let rootToDos = getRootLevelUnattachedSubItems modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior "TODO"
//        rootToDos |> List.iteri(fun i x->match x.ItemType with |ToDo(td)->sw.WriteLine ("        " + td.Text)|_->())
//        sw.WriteLine "    WORK  //Work not attached to any specific item"
//        let rootWork = getRootLevelUnattachedSubItems modelItems Genres.Business AbstractionLevels.Abstract TemporalIndicators.ToBe Buckets.Behavior "WORK"
//        rootWork |> List.iteri(fun i x->match x.ItemType with |Work(w)->sw.WriteLine ("        " + w.Text)|_->())
//        sw.Flush()
//        sw.Close()

//    let compiledDumpIncomingModelAmout (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) =
//        saveCompiledModelSection Buckets.Behavior Genres.Business opts modelItems
//        saveCompiledModelSection Buckets.Behavior Genres.System opts modelItems
//        saveCompiledModelSection Buckets.Behavior Genres.Meta opts modelItems

//        saveCompiledModelSection Buckets.Structure Genres.Business opts modelItems
//        saveCompiledModelSection Buckets.Structure Genres.System opts modelItems
//        saveCompiledModelSection Buckets.Structure Genres.Meta opts modelItems

//        saveCompiledModelSection Buckets.Supplemental Genres.Business opts modelItems
//        saveCompiledModelSection Buckets.Supplemental Genres.System opts modelItems
//        saveCompiledModelSection Buckets.Supplemental Genres.Meta opts modelItems

    //let rawDumpIncomingModel (opts:Types.EasyAMProgramConfig) (modelItems:ModelItem list) =
    //    let fileName="incoming-lines-DEBUG" 
    //    System.IO.File.Delete(fileName + ".html")
    //    let sw = System.IO.File.CreateText(fileName + ".html")
    //    writeHtmlBeginAndHead sw "Incoming File Dump" "Incoming files processed by line for the EasyAM program" "EasyAM" fileName
    //    sw.WriteLine "<body>"
    //    writeModelItemDetailHtmlTableHead sw
    //    modelItems |> List.iteri(fun i x->
    //        writeModelItemDetailHtmlTableDetail sw i x
    //        )
    //    sw.WriteLine"</table></body>"
    //    sw.WriteLine"</html>"
    //    sw.Flush()
    //    sw.Close()




