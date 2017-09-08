module Lenses
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
