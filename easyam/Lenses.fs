module Lenses
    open Types
    open SAModel

    let getTopLevelItems (modelItems:ModelItem2 []) =
        modelItems |> Array.filter(fun x->x.Location.AbstractionLevel=AbstractionLevels.Abstract && x.Location.Genre=Genres.Business && x.Location.TemporalIndicator=TemporalIndicators.ToBe) |> Array.sortBy(fun x->x.Description)
    let getMasterUserStories (modelItems:ModelItem2 []) = 
        (getTopLevelItems modelItems) |> Array.filter(fun x->x.Location.Bucket=Buckets.Behavior)
    let getALL_MUS (modelItems:ModelItem2 []) = 
        let allItem = getMasterUserStories modelItems |> Array.tryFind(fun x->x.Description="ALL")
        allItem
    let getMasterDomainEntities (modelItems:ModelItem2 []) = 
        (getTopLevelItems modelItems) |> Array.filter(fun x->x.Location.Bucket=Buckets.Structure)
    let getMasterSupplementals (modelItems:ModelItem2 []) = 
        (getTopLevelItems modelItems) |> Array.filter(fun x->x.Location.Bucket=Buckets.Supplemental)
    let getNotes (modelItem:ModelItem2) =
        modelItem.Annotations |> Array.filter(fun x->(fst x)=ANNOTATION_TOKEN_TYPE.Note)
    let getQuestions (modelItem:ModelItem2) =
        modelItem.Annotations |> Array.filter(fun x->(fst x)=ANNOTATION_TOKEN_TYPE.Question)
    let getToDos (modelItem:ModelItem2) =
        modelItem.Annotations |> Array.filter(fun x->(fst x)=ANNOTATION_TOKEN_TYPE.ToDo)
    let getOpenItems (modelItem:ModelItem2) = 
        (getQuestions modelItem) |> Array.append (getToDos modelItem)
    let getWorks (modelItem:ModelItem2) =
        modelItem.Annotations |> Array.filter(fun x->(fst x)=ANNOTATION_TOKEN_TYPE.Work)
    let getDiagrams (modelItem:ModelItem2) =
        modelItem.Annotations |> Array.filter(fun x->(fst x)=ANNOTATION_TOKEN_TYPE.Diagram)

    let getAllSubAnnotations (modelItem:ModelItem2) =
        modelItem.Attributes |> Array.map(fun x->x.Annotations) |> Array.concat

    let getAffectedBy (modelItem:ModelItem2) =
        modelItem.Relations|>Array.filter(fun x->x.ModelJoinType=ModelJoin.AffectedBy)
    let getAffects (modelItem:ModelItem2) =
        modelItem.Relations|>Array.filter(fun x->x.ModelJoinType=ModelJoin.Affects)
    let getUses (modelItem:ModelItem2) =
        modelItem.Relations|>Array.filter(fun x->x.ModelJoinType=ModelJoin.Uses)
    let getUsedBy (modelItem:ModelItem2) =
        modelItem.Relations|>Array.filter(fun x->x.ModelJoinType=ModelJoin.UsedBy)
    let getHasA (modelItem:ModelItem2) =
        modelItem.Relations|>Array.filter(fun x->x.ModelJoinType=ModelJoin.HasA)
    let getIsOwnedByA (modelItem:ModelItem2) =
        modelItem.Relations|>Array.filter(fun x->x.ModelJoinType=ModelJoin.IsOwnedByA)

    let getTriggers (modelItem:ModelItem2) =
        if modelItem.Location.Bucket<>Buckets.Behavior then Array.empty else
        modelItem.Attributes|>Array.filter(fun x->x.AttributeType=ModelAttributeTypes.Trigger)
    let getActors (modelItem:ModelItem2) =
        if modelItem.Location.Bucket<>Buckets.Behavior then Array.empty else
        modelItem.Attributes|>Array.filter(fun x->x.AttributeType=ModelAttributeTypes.Actor)
    let getGoals (modelItem:ModelItem2) =
        if modelItem.Location.Bucket<>Buckets.Behavior then Array.empty else
        modelItem.Attributes|>Array.filter(fun x->x.AttributeType=ModelAttributeTypes.Goal)
    let getContexts (modelItem:ModelItem2) =
        if modelItem.Location.Bucket<>Buckets.Behavior then Array.empty else
        modelItem.Attributes|>Array.filter(fun x->x.AttributeType=ModelAttributeTypes.BusinessContext)
    let getScenarios (modelItem:ModelItem2) =
        if modelItem.Location.Bucket<>Buckets.Behavior then Array.empty else
        modelItem.Attributes|>Array.filter(fun x->x.AttributeType=ModelAttributeTypes.Scenario)



    let getProjectLevelItems (modelItems:ModelItem2 []) =
        modelItems |> Array.filter(fun x->x.Location.AbstractionLevel=AbstractionLevels.Realized && x.Location.Genre=Genres.Business && x.Location.TemporalIndicator=TemporalIndicators.ToBe)
    let getProjectUserStories (modelItems:ModelItem2 []) = 
        (getProjectLevelItems modelItems) |> Array.filter(fun x->x.Location.Bucket=Buckets.Behavior)
    let getALL_ProjectUserStories (modelItems:ModelItem2 []) = 
        let allItem = getProjectUserStories modelItems |> Array.tryFind(fun x->x.Description="ALL")
        allItem
    let getProjectDomainEntities (modelItems:ModelItem2 []) = 
        (getProjectLevelItems modelItems) |> Array.filter(fun x->x.Location.Bucket=Buckets.Structure)
    let getProjectSupplementals (modelItems:ModelItem2 []) = 
        (getProjectLevelItems modelItems) |> Array.filter(fun x->x.Location.Bucket=Buckets.Supplemental)


    let getTotalAnnotationCount (items:ModelItem2 []) (annotationType:ANNOTATION_TOKEN_TYPE) = 
        items|>Array.sumBy(fun x->x.Annotations|>Array.filter(fun y->(fst y)=annotationType)|>Array.length)
    let getTotalNoteCount (items:ModelItem2 []) = getTotalAnnotationCount items ANNOTATION_TOKEN_TYPE.Note
    let getTotalToDoCount (items:ModelItem2 []) = getTotalAnnotationCount items ANNOTATION_TOKEN_TYPE.ToDo
    let getTotalWorkCount (items:ModelItem2 []) = getTotalAnnotationCount items ANNOTATION_TOKEN_TYPE.Work
    let getTotalQuestionCount (items:ModelItem2 []) = getTotalAnnotationCount items ANNOTATION_TOKEN_TYPE.Question


    let getTheRightThingToCheck (modelItem:ModelItem2) (tagOrAttName:TagOrAtt) (thingToInspect:string) =
        match tagOrAttName with
            |Tag->
                if thingToInspect="" then "" else
                let tagExistsOnThisItem=modelItem.Tags |> Array.exists(fun x->x.Key=thingToInspect)
                if tagExistsOnThisItem=false then "" else (modelItem.Tags|>Array.find(fun x->x.Key=thingToInspect)).Value
            |Att->
                if thingToInspect="" then "" else
                match thingToInspect with
                    |"Description"->modelItem.Description
                    |"Id"->modelItem.Id.ToString()
                    |"NumberOfQuestions"->(getQuestions modelItem).Length.ToString()
                    |"NumberOfToDos"->(getToDos modelItem).Length.ToString()
                    |"OpenItems"->(getOpenItems modelItem).Length.ToString()
                    |"AmountOfWork"->(getWorks modelItem).Length.ToString()
                    |"NumberOfAttributes"->(modelItem.Attributes).Length.ToString()
                    |"NumberOfAnnotations"->(modelItem.Annotations).Length.ToString()
                    |"NumberOfRelations"->(modelItem.Relations).Length.ToString()
                    |"NumberOfSourceReferences"->(modelItem.SourceReferences).Length.ToString()
                    |_->""
    let sortModelByOneParameter (incomingModelItems:ModelItem2 []) (sortParameter:sortParameterType) =
        let initialSort=incomingModelItems |> Array.sortWith(fun (a:ModelItem2) (b:ModelItem2)->
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
    let modelItemHasTagBetweenTwoValues (modelItem:ModelItem2) (valueToCheck:sortParameterType) (fromVal:string) (toVal:string):bool = 
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

    let filterModelByOneParameter (incomingModelItems:ModelItem2 []) (filterParameter:FilterParmeterType)=
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
    let getAllBehaviorForASprintTagValue (modelItems:ModelItem2 []) (sprint:string) =
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
                FromVal=(sprintFloat-0.001).ToString()
                ToVal=(sprintFloat+0.001).ToString()
            }
        let ret = filterModelByOneParameter modelItems filterParameter
        ret

    let sortByRankTag (modelItems:ModelItem2 [])=
        let sortParameter =
            {
                TagOrAtt=Tag
                Thing="Rank"
                ConvertTo=Int
                Order=Ascending
            }
        let ret = sortModelByOneParameter modelItems sortParameter
        ret
    let sortByDescription (modelItems:ModelItem2 [])=
        let sortParameter =
            {
                TagOrAtt=Att
                Thing="Description"
                ConvertTo=ConvertTo.DontConvert
                Order=Ascending
            }
        let ret = sortModelByOneParameter modelItems sortParameter
        ret


