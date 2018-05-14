module EasyAMParser

type UserState = string

let ModelItemIntegerFactory = 
    let counter = ref 0
    fun () -> 
        counter.Value <- !counter + 1
        !counter
let getNextModelItemNumber()=ModelItemIntegerFactory()

type JoinType =
    |Parent     |Child      |Affects    |AffectedBy
    |Uses       |UsedBy     |HasA       |IsOwnedByA
    static member ToList()=[Parent;Child;Affects;AffectedBy;Uses;UsedBy;HasA;IsOwnedByA]
    override self.ToString() =
        match self with
            |Parent->"Parent"           |Child->"Child "
            |Affects->"Affects"         |AffectedBy->"AffectedBy"
            |Uses->"Uses"               |UsedBy->"UsedBy"
            |HasA->"HasA"               |IsOwnedByA->"IsOwnedByA"
    member x.GetReverse =
        match x with 
            |Parent->Child              |Child->Parent
            |Affects->AffectedBy        |AffectedBy->Affects
            |Uses->UsedBy               |UsedBy->Uses
            |HasA->IsOwnedByA           |IsOwnedByA->HasA
type Genres =
    |Business   |System     |Meta
    static member ToList()=[Business;System;Meta]
    override self.ToString() =
        match self with 
            |Business->"Business"       |System->"System"
            |Meta->"Meta"
type Buckets =
    |Behavior   |Structure  |Supplemental
    static member ToList()=[Behavior;Structure;Supplemental]
    override self.ToString() =
        match self with 
            |Behavior->"Behavior"       |Structure->"Structure"
            |Supplemental->"Supplemental"
type TemporalIndicators =
    |Was        |AsIs       |ToBe
    static member ToList()=[Was;AsIs;ToBe]
    override self.ToString() =
        match self with 
            |Was->"Was"                 |AsIs->"AsIs"
            |ToBe->"ToBe"
type AbstractionLevels =    
    |Abstract   |Realized
    static member ToList()=[Abstract;Realized]
    override self.ToString() =
        match self with 
            |Abstract->"Abstract"       |Realized->"Realized"
type AnnotationType =       
    |Note       |Question   |ToDo       |Work 
    |Diagram    |Code       |Defect
    static member ToList()=[Note;Question;ToDo;Work;Diagram;Code;Defect]
    override self.ToString() =
        match self with 
            |Note->"Note"               |Question->"Question"
            |ToDo->"ToDo"               |Work->"Work"
            |Diagram->"Diagram"         |Code->"Code"
            |Defect->"Defect"
type AttributeType =
    |Trigger    |Actor      |Goal       |BusinessContext 
    |Scenario   |Contains   |Because    |Whenever 
    |ItHasToBeThat
    static member ToList()=[Trigger;Actor; Goal; BusinessContext; Scenario;Contains;Because;Whenever;ItHasToBeThat]
    override self.ToString() =
        match self with 
            |Trigger->"Trigger"     |Actor->"Actor"     |Goal->"Goal"
            |BusinessContext->"BusinessContext"         |Scenario->"Scenario"
            |Contains->"Contains"   |Because->"Because" |Whenever->"Whenever"
            |ItHasToBeThat->"ItHasToBeThat"
let areTheseTwoKeyValueStringArraysEqual (array1:System.Collections.Generic.KeyValuePair<string,string> []) (array2:System.Collections.Generic.KeyValuePair<string,string> []) =
    if array1.Length<>array2.Length then false else
    if array1.Length=0 then true else
    let itemsAreEqual (item1:System.Collections.Generic.KeyValuePair<string,string>) (item2:System.Collections.Generic.KeyValuePair<string,string>) = (item1.Key=item2.Key) && (item1.Value=item2.Value)
    let array1FoundIn2 = array1|>Array.fold(fun acc x->array2|>Array.exists(fun y->(itemsAreEqual y x)) && acc) true
    let array2FoundIn1 = array2|>Array.fold(fun acc x->array1|>Array.exists(fun y->(itemsAreEqual y x)) && acc) true
    (array1FoundIn2 && array2FoundIn1)
//[<CustomEquality;CustomComparison>]
//type TagList = 
//    {Tags:System.Collections.Generic.KeyValuePair<string,string>[]}
//    override x.GetHashCode()=hash x

[<CustomEquality;CustomComparison>]
type ModelCore = 
    {id:int; Namespace:string; ItemText:string; Tags:System.Collections.Generic.KeyValuePair<string,string>[]}
    override x.GetHashCode()=hash x
    override x.Equals(yobj)=
        match yobj with 
            | :? ModelCore as y->
                (x.id=y.id && x.ItemText = y.ItemText && x.Namespace=y.Namespace && (areTheseTwoKeyValueStringArraysEqual x.Tags  y.Tags))
            |_->false
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with 
                | :? ModelCore as y->
                    compare
                        (x.id, x.Namespace, x.ItemText)
                        (y.id, y.Namespace, y.ItemText)
                |_-> invalidArg "yobj" "cannot compare value of different types"

type CommentDetail = { Core:ModelCore}
type ModelItemRelation = { Core:ModelCore; Type:JoinType; SourceId:int; TargetId:int}
type ModelItemDetail =
    {
        Core:ModelCore        
        ParentId:int
        InHDDMode:bool
        Bucket:Buckets
        Genre:Genres
        AbstractionLevel:AbstractionLevels
        TemporalIndicator:TemporalIndicators
     }
type AttributeDetail =
    {
        Core:ModelCore
        AttributeType:AttributeType
        ModelItemId:int
    }
type AnnotationDetail =
    {
        Core:ModelCore
        AnnotationType:AnnotationType
        ModelItemId:int
    }
type AttributeAnnotationDetail =
    {
        Core:ModelCore
        AnnotationType:AnnotationType
        AttributeId:int
        AttributeType:AttributeType 
        ModelItemId:int
    }


    

