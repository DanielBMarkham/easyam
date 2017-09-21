module Lenses
    open Types
    open SAModel

    // Model Lenses
    let getModelItemDescForId (modelItems:ModelItem []) (id:int) = 
        (modelItems|> Array.map(fun y->modelItems|>Array.tryFind(fun z->z.Id=id))).[0].Value.Description
    let getModelItemForId (modelItems:ModelItem []) (id:int) = 
        (modelItems|> Array.map(fun y->modelItems|>Array.tryFind(fun z->z.Id=id))).[0]

    let gettAllForABucket (modelItems:ModelItem []) (bucket:Buckets)=modelItems |> Array.filter(fun x->x.Location.Bucket=bucket)
    let getSupplementals (modelItems:ModelItem []) = gettAllForABucket modelItems Buckets.Supplemental
    let getStructure (modelItems:ModelItem []) = gettAllForABucket modelItems Buckets.Structure
    let getBehavior (modelItems:ModelItem []) = gettAllForABucket modelItems Buckets.Behavior

    let getTopLevelItems (modelItems:ModelItem []) =
        modelItems |> Array.filter(fun x->x.Location.AbstractionLevel=AbstractionLevels.Abstract && x.Location.Genre=Genres.Business && x.Location.TemporalIndicator=TemporalIndicators.ToBe) |> Array.sortBy(fun x->x.Description)
    let getMasterUserStories (modelItems:ModelItem []) = 
        (getTopLevelItems modelItems) |> Array.filter(fun x->x.Location.Bucket=Buckets.Behavior)
    let getALL_MUS (modelItems:ModelItem []) = 
        let allItem = getMasterUserStories modelItems |> Array.tryFind(fun x->x.Description="ALL")
        allItem
    let getMasterDomainEntities (modelItems:ModelItem []) = 
        (getTopLevelItems modelItems) |> Array.filter(fun x->x.Location.Bucket=Buckets.Structure)
    let getMasterSupplementals (modelItems:ModelItem []) = 
        (getTopLevelItems modelItems) |> Array.filter(fun x->x.Location.Bucket=Buckets.Supplemental)

    let getProjectLevelItems (modelItems:ModelItem []) =
        modelItems |> Array.filter(fun x->x.Location.AbstractionLevel=AbstractionLevels.Realized && x.Location.Genre=Genres.Business && x.Location.TemporalIndicator=TemporalIndicators.ToBe)
    let getProjectUserStories (modelItems:ModelItem []) = 
        (getProjectLevelItems modelItems) |> Array.filter(fun x->x.Location.Bucket=Buckets.Behavior)
    let getALL_ProjectUserStories (modelItems:ModelItem []) = 
        let allItem = getProjectUserStories modelItems |> Array.tryFind(fun x->x.Description="ALL")
        allItem
    let getProjectDomainEntities (modelItems:ModelItem []) = 
        (getProjectLevelItems modelItems) |> Array.filter(fun x->x.Location.Bucket=Buckets.Structure)
    let getProjectSupplementals (modelItems:ModelItem []) = 
        (getProjectLevelItems modelItems) |> Array.filter(fun x->x.Location.Bucket=Buckets.Supplemental)


    let getTotalAnnotationCount (items:ModelItem []) (annotationType:AnnotationTokenType) = 
        items|>Array.sumBy(fun x->x.Annotations|>Array.filter(fun y->y.AnnotationType=annotationType)|>Array.length)
    let getTotalNoteCount (items:ModelItem []) = getTotalAnnotationCount items AnnotationTokenType.Note
    let getTotalToDoCount (items:ModelItem []) = getTotalAnnotationCount items AnnotationTokenType.ToDo
    let getTotalWorkCount (items:ModelItem []) = getTotalAnnotationCount items AnnotationTokenType.Work
    let getTotalQuestionCount (items:ModelItem []) = getTotalAnnotationCount items AnnotationTokenType.Question


    //
    // Individual Item Lenses
    let getAllJoinsForAModelItem (modelItem:ModelItem) (joinType:ModelJoin) = 
        modelItem.Relations|>Array.filter(fun x->x.ModelJoinType=joinType)
    let getAffectedBy (modelItem:ModelItem) = getAllJoinsForAModelItem modelItem AffectedBy
    let getAffects (modelItem:ModelItem) = getAllJoinsForAModelItem modelItem Affects
    let getUses (modelItem:ModelItem) = getAllJoinsForAModelItem modelItem Uses
    let getUsedBy (modelItem:ModelItem) = getAllJoinsForAModelItem modelItem UsedBy
    let getHasA (modelItem:ModelItem) = getAllJoinsForAModelItem modelItem HasA
    let getIsOwnedBy (modelItem:ModelItem) = getAllJoinsForAModelItem modelItem IsOwnedByA
    let getChildren (modelItem:ModelItem) =getAllJoinsForAModelItem modelItem Child
    let getParent (modelItem:ModelItem)= getAllJoinsForAModelItem modelItem Parent

    let getAllAnnotationsForAModelItem (modelItem:ModelItem) (annotationType:AnnotationTokenType)=
        modelItem.Annotations|>Array.filter(fun x->x.AnnotationType=annotationType)
    let getNotes (modelItem:ModelItem) = getAllAnnotationsForAModelItem modelItem Note
    let getQuestions (modelItem:ModelItem) = getAllAnnotationsForAModelItem modelItem Question
    let getToDos (modelItem:ModelItem) = getAllAnnotationsForAModelItem modelItem ToDo
    let getWork (modelItem:ModelItem) = getAllAnnotationsForAModelItem modelItem Work
    let getDiagrams (modelItem:ModelItem) = getAllAnnotationsForAModelItem modelItem Diagram
    let getCode (modelItem:ModelItem) = getAllAnnotationsForAModelItem modelItem Code
    let getDefects (modelItem:ModelItem) = getAllAnnotationsForAModelItem modelItem Defect

    let getAttributeAnnoationsForAType (attribute:ModelItemAttribute) (annotationType:AnnotationTokenType):ModelItemAnnotation []=
        attribute.Annotations|>Array.filter(fun x->x.AnnotationType=annotationType)
    let getAttributeNotes (attribute:ModelItemAttribute) = getAttributeAnnoationsForAType attribute AnnotationTokenType.Note
    let getAttributeQuestions (attribute:ModelItemAttribute) = getAttributeAnnoationsForAType attribute AnnotationTokenType.Question 
    let getAttributeToDos (attribute:ModelItemAttribute) = getAttributeAnnoationsForAType attribute AnnotationTokenType.ToDo 
    let getAttributeWork (attribute:ModelItemAttribute) = getAttributeAnnoationsForAType attribute AnnotationTokenType.Work 
    let getAttributeDiagrams (attribute:ModelItemAttribute) = getAttributeAnnoationsForAType attribute AnnotationTokenType.Diagram 
    let getAttributeCode (attribute:ModelItemAttribute) = getAttributeAnnoationsForAType attribute AnnotationTokenType.Code 
    let getAttributeDefects (attribute:ModelItemAttribute) = getAttributeAnnoationsForAType attribute AnnotationTokenType.Defect


    let getOpenItems (modelItem:ModelItem) = 
        (getQuestions modelItem) |> Array.append (getToDos modelItem)


    let getAllAttributesForAModelItem (modelItem:ModelItem) (attributeType:ModelAttributeTypes) =
        modelItem.Attributes |> Array.filter(fun x->x.AttributeType=attributeType)
    let getTriggers modelItem = getAllAttributesForAModelItem modelItem Trigger
    let getActors modelItem = getAllAttributesForAModelItem modelItem  Actor
    let getGoals modelItem = getAllAttributesForAModelItem modelItem   Goal
    let getBusinessContexts modelItem = getAllAttributesForAModelItem modelItem  BusinessContext
    let getScenarios modelItem = getAllAttributesForAModelItem modelItem  Scenario
    let getBecauses modelItem = getAllAttributesForAModelItem modelItem  Because
    let getWhenevers modelItem = getAllAttributesForAModelItem modelItem  Whenever
    let getItHasToBeThats modelItem = getAllAttributesForAModelItem modelItem ItHasToBeThat
    let getContains modelItem = getAllAttributesForAModelItem modelItem Contains 

    let getAllSubAnnotations (modelItem:ModelItem):ModelItemAnnotation [] =
        modelItem.Attributes |> Array.map(fun x->x.Annotations) |> Array.concat
    let getAllNoteSubAnnotations (modelItem:ModelItem) = (getAllSubAnnotations modelItem) |> Array.filter(fun x->x.AnnotationType=AnnotationTokenType.Note)
    let getAllQuestionSubAnnotations (modelItem:ModelItem) = (getAllSubAnnotations modelItem) |> Array.filter(fun x->x.AnnotationType=AnnotationTokenType.Question)
    let getAllToDosSubAnnotations (modelItem:ModelItem) = (getAllSubAnnotations modelItem) |> Array.filter(fun x->x.AnnotationType=AnnotationTokenType.ToDo)
    let getAllWorkSubAnnotations (modelItem:ModelItem) = (getAllSubAnnotations modelItem) |> Array.filter(fun x->x.AnnotationType=AnnotationTokenType.Work)
    let getAllDiagramsSubAnnotations (modelItem:ModelItem) = (getAllSubAnnotations modelItem) |> Array.filter(fun x->x.AnnotationType=AnnotationTokenType.Diagram)
    let getAllCodeSubAnnotations (modelItem:ModelItem) = (getAllSubAnnotations modelItem) |> Array.filter(fun x->x.AnnotationType=AnnotationTokenType.Code)
    let getAllDefectSubAnnotations (modelItem:ModelItem) = (getAllSubAnnotations modelItem) |> Array.filter(fun x->x.AnnotationType=AnnotationTokenType.Defect)

    // Item to Model Lenses
    let getAllJoinItemsForAModelItem (referenceModel:ModelItem []) (modelItem:ModelItem) (joinType:ModelJoin) =
        let allJoinsForModelItem=getAllJoinsForAModelItem modelItem joinType
        allJoinsForModelItem |> Array.collect(fun x->
            let itemFoundInModel=referenceModel|>Array.exists(fun y->y.Id=x.TargetId)
            if itemFoundInModel=false
                then [||] 
                else 
                    let lookupItem=referenceModel|>Array.find(fun y->y.Id=x.TargetId)
                    [|lookupItem|])
    let getAffectedByItems (referenceModel:ModelItem []) (modelItem:ModelItem) = getAllJoinItemsForAModelItem referenceModel modelItem AffectedBy
    let getAffectsItems (referenceModel:ModelItem []) (modelItem:ModelItem) = getAllJoinItemsForAModelItem referenceModel modelItem Affects
    let getUsesItems (referenceModel:ModelItem []) (modelItem:ModelItem) = getAllJoinItemsForAModelItem referenceModel modelItem Uses
    let getUsedByItems (referenceModel:ModelItem [])  (modelItem:ModelItem)=getAllJoinItemsForAModelItem referenceModel modelItem UsedBy
    let getHasAItems (referenceModel:ModelItem [])  (modelItem:ModelItem)=getAllJoinItemsForAModelItem referenceModel modelItem HasA
    let getIsOwnedByItems(referenceModel:ModelItem [])  (modelItem:ModelItem)= getAllJoinItemsForAModelItem referenceModel modelItem IsOwnedByA
    let getChildrenItems (referenceModel:ModelItem [])  (modelItem:ModelItem)=getAllJoinItemsForAModelItem referenceModel modelItem Child
    let getParentItems (referenceModel:ModelItem [])  (modelItem:ModelItem)=getAllJoinItemsForAModelItem referenceModel modelItem Parent


    let rec GetAllDescendentItems (referenceModel:ModelItem []) (modelItem:ModelItem) =
        getChildrenItems referenceModel modelItem |> Array.fold(fun acc x->
            let nextLevelDownChildren=GetAllDescendentItems referenceModel x
            [|x|] |>Array.append nextLevelDownChildren |> Array.append acc
            ) [||]
    let rec GetAllParentalItems (referenceModel:ModelItem []) (modelItem:ModelItem) =
        getParentItems referenceModel modelItem |> Array.fold(fun acc x->
            let nextLevelUpParents=GetAllParentalItems referenceModel x
            [|x|] |>Array.append nextLevelUpParents |> Array.append acc
            ) [||]


    let getTheRightThingToCheck (modelItem:ModelItem) (tagOrAttName:TagOrAtt) (thingToInspect:string) =
        match tagOrAttName with
            |Tag->
                if thingToInspect="" then "" else
                let tagExistsOnThisItem=modelItem.Tags |> Array.exists(fun x->x.Key=thingToInspect)
                if tagExistsOnThisItem=false then ""
                    else
                        let ret=(modelItem.Tags|>Array.find(fun x->x.Key=thingToInspect))
                        ret.Value
            |Att->
                if thingToInspect="" then "" else
                match thingToInspect with
                    |"Description"->modelItem.Description
                    |"Id"->modelItem.Id.ToString()
                    |"NumberOfQuestions"->string (getQuestions modelItem).Length
                    |"NumberOfToDos"->string (getToDos modelItem).Length
                    |"OpenItems"->string (getOpenItems modelItem).Length
                    |"AmountOfWork"->string (getWork modelItem).Length
                    |"NumberOfAttributes"->string (modelItem.Attributes).Length
                    |"NumberOfAnnotations"->string (modelItem.Annotations).Length
                    |"NumberOfRelations"->string (modelItem.Relations).Length
                    |"NumberOfSourceReferences"->string (modelItem.SourceReferences).Length
                    |_->""
    let sortModelByOneParameter (incomingModelItems:ModelItem []) (sortParameter:sortParameterType) =
        let initialSort=incomingModelItems |> Array.sortWith(fun (a:ModelItem) (b:ModelItem)->
            let itemToSortA=getTheRightThingToCheck a sortParameter.TagOrAtt sortParameter.Thing
            let itemToSortB=getTheRightThingToCheck b sortParameter.TagOrAtt sortParameter.Thing
            match sortParameter.ConvertTo with
                |ConvertTo.DontConvert->itemToSortA.CompareTo itemToSortB
                |DateTime->
                    let aConverted=if fst (System.DateTime.TryParse(itemToSortA))=true then System.DateTime.Parse(itemToSortA) else System.DateTime.MinValue
                    let bConverted=if fst (System.DateTime.TryParse(itemToSortB))=true then System.DateTime.Parse(itemToSortB) else System.DateTime.MinValue
                    aConverted.CompareTo bConverted
                |TimeSpan->
                    let aConverted=if fst (System.TimeSpan.TryParse(itemToSortA))=true then System.TimeSpan.Parse(itemToSortA) else System.TimeSpan.MinValue
                    let bConverted=if fst (System.TimeSpan.TryParse(itemToSortB))=true then System.TimeSpan.Parse(itemToSortB) else System.TimeSpan.MinValue
                    aConverted.CompareTo bConverted
                |Float->
                    let aConverted=if fst (System.Double.TryParse(itemToSortA))=true then System.Double.Parse(itemToSortA) else System.Double.MinValue
                    let bConverted=if fst (System.Double.TryParse(itemToSortB))=true then System.Double.Parse(itemToSortB) else System.Double.MinValue
                    aConverted.CompareTo bConverted
                |Int->
                    let aConverted=if fst (System.Int64.TryParse(itemToSortA))=true then System.Int64.Parse(itemToSortA) else System.Int64.MinValue
                    let bConverted=if fst (System.Int64.TryParse(itemToSortB))=true then System.Int64.Parse(itemToSortB) else System.Int64.MinValue
                    aConverted.CompareTo bConverted
                |Money->
                    let aConverted=if fst (System.Decimal.TryParse(itemToSortA))=true then System.Decimal.Parse(itemToSortA) else System.Decimal.MinValue
                    let bConverted=if fst (System.Decimal.TryParse(itemToSortB))=true then System.Decimal.Parse(itemToSortB) else System.Decimal.MinValue
                    aConverted.CompareTo bConverted
            )
        let ret = if sortParameter.Order = SortOrder.Ascending then initialSort else initialSort |> Array.rev
        ret
    let modelItemHasTagBetweenTwoValues (modelItem:ModelItem) (valueToCheck:sortParameterType) (fromVal:string) (toVal:string):bool = 
            let thingToCheck=getTheRightThingToCheck modelItem valueToCheck.TagOrAtt valueToCheck.Thing
            let tagExistsOnThisItem modelItem tagName =modelItem.Tags |> Array.exists(fun x->x.Key=tagName)
            if valueToCheck.TagOrAtt=Att && fromVal="" && toVal="" then true else
            if valueToCheck.TagOrAtt=Tag && fromVal="" && toVal="" && (tagExistsOnThisItem modelItem valueToCheck.Thing) then true else
            match valueToCheck.ConvertTo with
                |ConvertTo.DontConvert->
                    let comparedToFromVal=thingToCheck.CompareTo fromVal
                    let comparedToToVal=thingToCheck.CompareTo toVal
                    comparedToFromVal>0 && comparedToToVal<0
                |DateTime->
                    let convertedThing=if fst (System.DateTime.TryParse(thingToCheck))=true then System.DateTime.Parse(thingToCheck) else System.DateTime.MinValue
                    let convertedFromVal=if fst (System.DateTime.TryParse(fromVal))=true then System.DateTime.Parse(fromVal) else System.DateTime.MinValue
                    let convertedToVal=if fst (System.DateTime.TryParse(toVal))=true then System.DateTime.Parse(toVal) else System.DateTime.MinValue
                    let comparedToFromVal=convertedThing.CompareTo convertedFromVal
                    let comparedToToVal=convertedThing.CompareTo convertedToVal
                    comparedToFromVal>0 && comparedToToVal<0
                |TimeSpan->
                    let convertedThing=if fst (System.TimeSpan.TryParse(thingToCheck))=true then System.TimeSpan.Parse(thingToCheck) else System.TimeSpan.MinValue
                    let convertedFromVal=if fst (System.TimeSpan.TryParse(fromVal))=true then System.TimeSpan.Parse(fromVal) else System.TimeSpan.MinValue
                    let convertedToVal=if fst (System.TimeSpan.TryParse(toVal))=true then System.TimeSpan.Parse(toVal) else System.TimeSpan.MinValue
                    let comparedToFromVal=convertedThing.CompareTo convertedFromVal
                    let comparedToToVal=convertedThing.CompareTo convertedToVal
                    comparedToFromVal>0 && comparedToToVal<0
                |Float->
                    let convertedThing=if fst (System.Double.TryParse(thingToCheck))=true then System.Double.Parse(thingToCheck) else System.Double.MinValue
                    let convertedFromVal=if fst (System.Double.TryParse(fromVal))=true then System.Double.Parse(fromVal) else System.Double.MinValue
                    let convertedToVal=if fst (System.Double.TryParse(toVal))=true then System.Double.Parse(toVal) else System.Double.MinValue
                    let comparedToFromVal=convertedThing.CompareTo convertedFromVal
                    let comparedToToVal=convertedThing.CompareTo convertedToVal
                    comparedToFromVal>0 && comparedToToVal<0
                |Int->
                    let convertedThing=if fst (System.Int64.TryParse(thingToCheck))=true then System.Int64.Parse(thingToCheck) else System.Int64.MinValue
                    let convertedFromVal=if fst (System.Int64.TryParse(fromVal))=true then System.Int64.Parse(fromVal) else System.Int64.MinValue
                    let convertedToVal=if fst (System.Int64.TryParse(toVal))=true then System.Int64.Parse(toVal) else System.Int64.MinValue
                    let comparedToFromVal=convertedThing.CompareTo convertedFromVal
                    let comparedToToVal=convertedThing.CompareTo convertedToVal
                    comparedToFromVal>0 && comparedToToVal<0
                |Money->
                    let convertedThing=if fst (System.Decimal.TryParse(thingToCheck))=true then System.Decimal.Parse(thingToCheck) else System.Decimal.MinValue
                    let convertedFromVal=if fst (System.Decimal.TryParse(fromVal))=true then System.Decimal.Parse(fromVal) else System.Decimal.MinValue
                    let convertedToVal=if fst (System.Decimal.TryParse(toVal))=true then System.Decimal.Parse(toVal) else System.Decimal.MinValue
                    let comparedToFromVal=convertedThing.CompareTo convertedFromVal
                    let comparedToToVal=convertedThing.CompareTo convertedToVal
                    comparedToFromVal>0 && comparedToToVal<0

    let filterModelByOneParameter (incomingModelItems:ModelItem []) (filterParameter:FilterParmeterType)=
        let allOfTheAMTagsAreUnknown=filterParameter.Bucket=Buckets.Unknown&&filterParameter.AbstractionLevel=AbstractionLevels.Unknown&&filterParameter.TemporalIndicator=TemporalIndicators.Unknown&&filterParameter.Genre=Genres.Unknown
        let filterByAMTags=incomingModelItems |> Array.filter(fun x->
            let matchesBucket = if filterParameter.Bucket=Buckets.Unknown then true else x.Location.Bucket=filterParameter.Bucket
            let matchesGenre = if filterParameter.Genre=Genres.Unknown then true else x.Location.Genre=filterParameter.Genre
            let matchesTemporal = if filterParameter.TemporalIndicator=TemporalIndicators.Unknown then true else x.Location.TemporalIndicator=filterParameter.TemporalIndicator
            let matchesAbstraction = if filterParameter.AbstractionLevel=AbstractionLevels.Unknown then true else x.Location.AbstractionLevel=filterParameter.AbstractionLevel
            let matchesModelParms = matchesBucket && matchesGenre && matchesTemporal && matchesAbstraction
            if allOfTheAMTagsAreUnknown=true then true else matchesBucket&&matchesGenre&&matchesTemporal&&matchesAbstraction
            )
        if filterParameter.CheckValue.Thing="" then filterByAMTags else
        let filterByCheckVal=filterByAMTags |> Array.filter(fun x->
            modelItemHasTagBetweenTwoValues x filterParameter.CheckValue filterParameter.FromVal filterParameter.ToVal            
            )
        filterByCheckVal

    // common shortcuts
    let getAllBehaviorForASprintTagValue (modelItems:ModelItem []) (sprint:string) =
        let sprintFloat = if fst (System.Double.TryParse(sprint))=true then System.Double.Parse(sprint) else System.Double.MinValue
        let checkParameter =
            {
                TagOrAtt=Tag
                Thing="Sprint"
                ConvertTo=Float
                Order=Ascending
            }
        let filterParameter =
            {
                Genre=Genres.Unknown
                Bucket=Buckets.Behavior
                AbstractionLevel=AbstractionLevels.Unknown
                TemporalIndicator=TemporalIndicators.Unknown
                CheckValue=checkParameter
                FromVal=string (sprintFloat-0.001)
                ToVal=string (sprintFloat+0.001)
            }
        let ret = filterModelByOneParameter modelItems filterParameter
        ret

    let sortByRankTag (modelItems:ModelItem [])=
        let sortParameter =
            {
                TagOrAtt=Tag
                Thing="Rank"
                ConvertTo=Int
                Order=Ascending
            }
        let ret = sortModelByOneParameter modelItems sortParameter
        ret
    let sortByDescription (modelItems:ModelItem [])=
        let sortParameter =
            {
                TagOrAtt=Att
                Thing="Description"
                ConvertTo=ConvertTo.DontConvert
                Order=Ascending
            }
        let ret = sortModelByOneParameter modelItems sortParameter
        ret
    let getAllItemsForTemporalGenreAbstraction (referenceModel:ModelItem []) (temporal:TemporalIndicators) (genre:Genres) (abst:AbstractionLevels) =
        let checkParameter =
            {
                TagOrAtt=Tag
                Thing=""
                ConvertTo=DontConvert
                Order=Ascending
            }
        let filterParameter =
            {
                Genre=genre
                Bucket=Buckets.Unknown
                AbstractionLevel=abst
                TemporalIndicator=temporal
                CheckValue=checkParameter
                FromVal=""
                ToVal=""
            }
        let ret = filterModelByOneParameter referenceModel filterParameter
        ret

