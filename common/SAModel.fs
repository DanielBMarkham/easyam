module SAModel
    open Types

//// REFACTOR
//// Everything entered in has three things: an origin, a tagged context, and free text (markup)
//// There are four types of models: Structure, Behavior, Supplemental, and Meta
//// Each model is structured so: 
////      Model Business Abstract
////          Item(s) [Diagrams: Item/Relationship list]
////              Model Business Realized
////                  Item(s) [Diagrams: Item/Relationship list]
////                      Model System Abstract
////                          Items(s) [Diagrams: Item/Relationship list]
////                              Model System Realized
////                                  Item(s) [Diagrams: Item/Relationship list]
//// HYPOTHESIS->Item (title format) -> Desired change list of behavior, structure, and suppl (at any level)
////
//// Any one of these can have notes (freetext tagged to something), questions, TODOs, and name-value tags
//// Items can be unique to that layer -- or simply refer to an ancestor item up the tree
//// Items have a short name, a long name, and a detailed name (super long)? Ids are INTERNAL-ONLY,
//// we force people organize their shared mental models around the English Language, not around codes and numbers
//// Nesting is implied by whitespace or colons, eg BUSINESS BEHAVIOR: Do Dishes
////
//// compilation rules:
////      no more than 40 items per layer (error)
////      no behavior nouns not mentioned in structure (and vice versa) (warning)
////      

//    let IntegerFactory = 
//        let counter = ref 0
//        fun () -> 
//            counter.Value <- !counter + 1
//            !counter
//    let getNextItemNumber()=IntegerFactory()
//    // Structured Analysis Model Super Types
    type Buckets =
        | Unknown
        | None
        | Behavior
        | Structure
        | Supplemental
         static member ToList() =
            [Unknown;None;Behavior;Structure;Supplemental]
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | None->"None"
            | Behavior->"Behavior"
            | Structure->"Structure"
            | Supplemental->"Supplemental"
    type Genres =
        | Unknown
        | None
        | Business
        | System
        | Meta
         static member ToList() =
            [Unknown;None;Business; System; Meta]
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | None->"None"
            | Business->"Business"
            | System->"System"
            | Meta->"Meta"

    type AbstractionLevels = 
        | Unknown
        | None
        | Abstract
        | Realized
         static member ToList() =
            [Unknown;None;Abstract;Realized]
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | None->"None"
            | Abstract->"Abstract"
            | Realized->"Realized"
    type TemporalIndicators =
        | Unknown
        | None
        | Was
        | AsIs
        | ToBe
         static member ToList() =
            [Unknown;None;Was;AsIs;ToBe]
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | None->"None"
            | Was->"Was"
            | AsIs->"As-Is"
            | ToBe->"To-Be"


//    // HYPOTHESIS
//    [<NoComparison>]
//    type Hypothesis =
//        {
//            Genre:Genres
//            ShortName:string
//            Observations:string list
//            ExpectedActorsImpacted:Actor list
//            ExpectedImpactMetricGiven:string list
//            ExpectedSuccessfulResult:string list
//            ProposedChangesToTestHypothesis:ModelItem list
//            ExpectedExperimentSuspenseTime:string
//        }
//    let defaultHypothesis = 
//        {
//            Genre=Genres.Business
//            ShortName=""
//            Observations=[]
//            ExpectedActorsImpacted=[]
//            ExpectedImpactMetricGiven=[]
//            ExpectedSuccessfulResult=[]
//            ProposedChangesToTestHypothesis=[]
//            ExpectedExperimentSuspenseTime=""
//        }




//// MIXED TYPES
//    type SVGEntityBox =
//        {
//            xPos:int
//            yPos:int
//            width:int
//            height:int
//            Entity:Entity
//        }

//    type VertexData<'V> =
//        int (* identifier *) *
//        'V (* vertex data *)

//    type EdgeData<'E> =
//        int (* identifier *) *
//        int (* priority *) *
//        int (* vertex target *) *
//        'E (* edge data *)

//    (* The graph uses adjacency list notation *)
//    type Adjacency<'E> = EdgeData<'E> list

//    (* The Vertex type represents the internal structure
//        of the graph *)
//    type Vertex<'V, 'E> = VertexData<'V> * Adjacency<'E>

//    (* A Graph is a Vertex list.  The nextNode allows for
//        consistent addressing of nodes *)
//    type Graph<'V, 'E> =
//        int (* nextNode identifier *) *
//        Vertex<'V, 'E> list

//    (* Empty graph construction *)
//    let empty: Graph<_,_> = (0, [])

//    (* Helper methods for getting the data from a Vertex *)
//    let vertexId (v:Vertex<_,_>) = v |> fst |> fst
//    let vertexData (v:Vertex<_,_>) = v |> fst |> snd
//    (* Helper methods for getting the data from an Edge *)
//    let edgeId ((x,_,_,_):EdgeData<_>) = x
//    let edgePriority ((_,x,_,_):EdgeData<_>) = x
//    let edgeTarget ((_,_,x,_):EdgeData<_>) = x
//    let edgeData ((_,_,_,x):EdgeData<_>) = x

//    (* Getting a vertex from a graph by id *)
//    let getVertex v (g:Graph<_, _>) : Vertex<_,_> =
//        snd g |> List.find (fun V -> vertexId V = v)
//    (* Getting all edges from a graph by a vertex id *)
//    let getEdges v (g:Graph<_, _>) =
//        g |> getVertex v |> snd

//    (* Add a new vertex *)
//    let addVertex (v:'V) (g:Graph<'V, _>)
//        : (int*Graph<'V,_>) =
//            let id = fst g
//            let s = snd g
//            let newVD : VertexData<_> = (id, v)
//            let newA : Adjacency<_> = []
//            let newV = (newVD, newA)
//            (id, (id + 1, newV::s))

//    (* Add a new edge.  Edges include a priority value *)
//    let addEdge priority
//        (v:int) (v':int) (e:'E) (g:Graph<'V, 'E>)
//        : (int*Graph<'V,'E>) =
//            let id = fst g
//            let s = snd g
//            let newE : EdgeData<_> = (id, priority, v', e)
//            (id,
//                (id + 1,
//                    s |> List.map (fun V ->
//                    if (vertexId V) = v then
//                        (fst V, newE::(snd V))
//                    else V)))

//    (* The edges aren't sorted by default so this function
//        sorts them by priority *)
//    let sortEdges (a:Adjacency<_>) =
//        a |> List.sortBy edgePriority

//    (* Removes an edge from a graph by id *)
//    let removeEdge (id:int) (g:Graph<_,_>)
//        : Graph<_,_> =
//            let next = fst g
//            let s = snd g
//            (next, s |> List.map ( fun (v, a) ->
//            (v, a |> 
//                List.filter (fun x -> (edgeId x) <> id)))) 

//    (* Removes a vertex from a graph by id and removes
//        any related edges *)
//    let removeVertex (id:int) (g:Graph<_,_>) 
//        : Graph<_,_> =
//            let next = fst g
//            let s = snd g
//            (next, s |> ([] |> List.fold (fun s' (v, a) ->
//            if (fst v) = id then s'
//            else
//                let f = fun x -> ((edgeTarget x) <> id)
//                let newA = a |> List.filter f
//                let newV = (v, newA)
//                newV::s')))        