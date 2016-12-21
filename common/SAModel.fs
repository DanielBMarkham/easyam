module SAModel
    open Types

    type NounClause =
        {
            text:string
        }
    type VerbClause =
        {
            text:string
        }
    type VerbNounClause = VerbClause*NounClause
    type Actor = NounClause

    type Genres =
        | Unknown
        | Business
        | System
         static member ToList() =
            [Unknown; Business; System]
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | Business->"Business"
            | System->"System"

    type Buckets =
        | Unknown
        | Behavior
        | Structure
        | Supplemental
        | Meta
         static member ToList() =
            [Unknown;Behavior;Structure;Supplemental;Meta]
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | Behavior->"Behavior"
            | Structure->"Structure"
            | Supplemental->"Supplemental"
            | Meta->"Meta"
    type AbstractionLevels = 
        | Unknown
        | Abstract
        | Realized
         static member ToList() =
            [Unknown;Abstract;Realized]
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | Abstract->"Abstract"
            | Realized->"Realized"
    type TemporalIndicators =
        | Unknown
        | AsIs
        | ToBe
         static member ToList() =
            [Unknown;AsIs;ToBe]
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | AsIs->"As-Is"
            | ToBe->"To-Be"
    type InformationTag =
        {
            Genre:Genres
            Bucket:Buckets
            AbstractionLevel:AbstractionLevels
            TemporalIndicator:TemporalIndicators
        }
         member self.ToString2() =
            let genre = if self.Genre=Genres.Unknown then "" else self.Genre.ToString()
            let bucket = if self.Bucket=Buckets.Unknown then "" else self.Bucket.ToString()
            let abstractionlevel = if self.AbstractionLevel=AbstractionLevels.Unknown then "" else self.AbstractionLevel.ToString()
            let temporalindicator = if self.TemporalIndicator=TemporalIndicators.Unknown then "" else self.TemporalIndicator.ToString()
            genre + " " + bucket + " " + abstractionlevel + " " + temporalindicator.ToString()
         override self.ToString() =
            self.Genre.ToString() + " " + self.Bucket.ToString() + " " + self.AbstractionLevel.ToString() + " " + self.TemporalIndicator.ToString()
    let defaultInformationTag=
        {
            Genre=Genres.Unknown
            Bucket=Buckets.Unknown
            AbstractionLevel=AbstractionLevels.Unknown
            TemporalIndicator=TemporalIndicators.Unknown
        }

// INCOMING FILE TYPES
    let informationTagTokens =[|"STRUCUTRE"; "BEHAVIOR"; "SUPPLEMENTAL"; "META"; "BUSINESS"; "SYSTEM"; "ABSTRACT"; "REALIZED"; "AS-IS"; "TO-BE"|]
    let scopingTokens = [|
        ("NAME: ", None); 
        ("ORG: ", None); 
        ("DOMAIN: ", None); 
        ("US: ", Some(Buckets.Behavior)); 
        ("USER STORY: ", Some(Buckets.Behavior)); 
        ("SUPPL: ",  Some(Buckets.Supplemental)); 
        ("ENTITY: ", Some(Buckets.Structure)); 
        ("META: ",  Some(Buckets.Meta))|]
    let scopingTokenVals = scopingTokens |> Array.map(fun x->fst x)

    let commandTokens =[|"Q: "; "//"|]
    type BucketTokenType =
        | LTOR
        | Declarative
    let bucketTokens = [|
        ("HASA ", Buckets.Structure, LTOR);
        ("CONTAINS ", Buckets.Structure, LTOR);
        ("WHEN ", Buckets.Behavior, Declarative);
        ("ASA ", Buckets.Behavior, Declarative);
        ("INEEDTO ", Buckets.Behavior, Declarative);
        ("SOTHAT ", Buckets.Behavior, Declarative);
        ("INITIAL ", Buckets.Behavior, Declarative);
        ("FINAL ", Buckets.Behavior, Declarative);
        ("MERGENODE ", Buckets.Behavior, Declarative);
        ("MERGE ", Buckets.Behavior, Declarative);
        ("FORK ", Buckets.Behavior, Declarative);
        ("DO ", Buckets.Behavior, Declarative);
        ("DATA ", Buckets.Supplemental, Declarative);
        |]
    let bucketTokenVals = bucketTokens |> Array.map(fun x->
        let a,b,c = x
        a
        )
    type ProgramDirectories =
        {
            SourceDirectoryInfo:System.IO.DirectoryInfo
            DestinationDirectoryInfo:System.IO.DirectoryInfo
            BehaviorDirectoryInfo:System.IO.DirectoryInfo
            StructureDirectoryInfo:System.IO.DirectoryInfo
            SupplementalDirectoryInfo:System.IO.DirectoryInfo
            MetaDirectoryInfo:System.IO.DirectoryInfo
        }
    type ProgramInputFiles = 
        {
            Files:System.IO.FileInfo list
        }
    type CompilationLineCommands =
        | NoCommand
        | Unknown
        | Comment
        | Hasa
        | Contains
        | Question
        | Label
         override self.ToString() =
          match self with
            | NoCommand->"No Command"
            | Unknown->"Unknown"
            | Comment->"Comment"
            | Hasa->"HasA"
            | Contains->"Contains"
            | Question->"Question"
            | Label->"Label"

    type CompilationLineType =
        | Unknown
        | Scoping
        | Context
        | Command
        | Freetext
        | Label
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | Scoping->"Scoping"
            | Context->"Context"
            | Command->"Command"
            | FreeText->"FreeText"
            | Label->"Label"

    type CompilationLine =
        {
            File:System.IO.FileInfo option
            LineNumber:int
            LineType:CompilationLineType
            CommandType:CompilationLineCommands
            Scope:string
            TaggedContext:InformationTag
            LineText:string
        }
    let defaultCompilationLine =
        {
            File=None
            LineNumber=0
            Scope=""
            LineType=CompilationLineType.Unknown
            CommandType=NoCommand
            TaggedContext=defaultInformationTag
            LineText=""
        }
    type CompilationContext =
        {
            CompilationLines:CompilationLine list
            State:InformationTag
            Scope:string
            CurrentFile:string
        }
    let defaultCompilationContext =
        {
            CompilationLines = []
            State=defaultInformationTag
            Scope=""
            CurrentFile=""
        }

    // HYPOTHESIS
    type Hypothesis =
        {
            ExpectedChange:string
            ExpectedImpactMetric:string
            ExpectedActorsImpacted:string
            ExpectedImpactAmount:string
            ExpectedTimeUntilImpactObserved:string
            MightWorkBecause:string list
        }
    let defaultHypothesis =
        {
            ExpectedChange=""
            ExpectedImpactMetric=""
            ExpectedActorsImpacted=""
            ExpectedImpactAmount=""
            ExpectedTimeUntilImpactObserved=""
            MightWorkBecause=[]
        }


    // True for all models
    type SourceFileReference =
        {
            File:System.IO.FileInfo
            Line:int
        }

    // STRUCTURE
    type Attribute =
        {
            Title:NounClause
            SourceFileReferences:SourceFileReference list
        }
    type Entity =
        {
            Id:int
            ParentId:int option
            Title:NounClause
            Attributes:Attribute list
            Connections:(int*int) list
            SourceFileReferences:SourceFileReference list
            AffectedBySupplementals:int list
        }
    type StructureLayer =
        {
            CompilationLines:CompilationLine list
            Questions:string list
            Notes:string list
            Entities:Entity list
        }
    let defaultStructureLayer = 
        {
            CompilationLines=[]
            Entities=[]
            Questions=[]
            Notes=[]
        }
    type StructureModelType =
        {
            Input:StructureLayer
            Root:StructureLayer
            AbstractBusinessEntitiesToBe: StructureLayer
            RealizedBusinessEntitiesToBe: StructureLayer
            AbstractSystemEntitiesToBe: StructureLayer
            RealizedSystemEntitiesToBe: StructureLayer
            AbstractBusinessEntitiesAsIs: StructureLayer
            RealizedBusinessEntitiesAsIs: StructureLayer
            AbstractSystemEntitiesAsIs: StructureLayer
            RealizedSystemEntitiesAsIs: StructureLayer
        }
    let defaultStructureModel =
        {
            Input=defaultStructureLayer
            Root=defaultStructureLayer
            AbstractBusinessEntitiesToBe=defaultStructureLayer
            RealizedBusinessEntitiesToBe=defaultStructureLayer
            AbstractSystemEntitiesToBe=defaultStructureLayer
            RealizedSystemEntitiesToBe=defaultStructureLayer
            AbstractBusinessEntitiesAsIs=defaultStructureLayer
            RealizedBusinessEntitiesAsIs=defaultStructureLayer
            AbstractSystemEntitiesAsIs=defaultStructureLayer
            RealizedSystemEntitiesAsIs=defaultStructureLayer
        }

    // BEHAVIOR
    type USTrigger=VerbNounClause
    type USActor=VerbNounClause
    type USGoal=VerbNounClause
    type USContext=VerbNounClause
    type USUserStoryTitle =
        {
            Trigger:USTrigger
            Actor:USActor
            Goal:USGoal
            Context:USContext
        }
    type UserStory =
        {
            Id:int
            ParentId:int option
            USUserStoryTitle:USUserStoryTitle
            DiagramSteps:string list
            SourceFileReferences:SourceFileReference list
            AffectedBySupplementals:int list
        }
    type BehaviorLayer =
        {
            CompilationLines:CompilationLine list
            Questions:string list
            Notes:string list
            UserStoryList:UserStory list
        }
    let defaultBehaviorLayer =
        {
            CompilationLines=[]
            Questions=[]
            Notes=[]
            UserStoryList=[]
        }
    type BehaviorModelType =
        {
            Input:BehaviorLayer
            Root:BehaviorLayer
            AbstractBusinessUserStoriesToBe: BehaviorLayer
            RealizedBusinessUserStoriesToBe: BehaviorLayer
            AbstractSystemUserStoriesToBe: BehaviorLayer
            RealizedSystemUserStoriesToBe: BehaviorLayer
            AbstractBusinessUserStoriesAsIs: BehaviorLayer
            RealizedBusinessUserStoriesAsIs: BehaviorLayer
            AbstractSystemUserStoriesAsIs: BehaviorLayer
            RealizedSystemUserStoriesAsIs: BehaviorLayer
        }
    let defaultBehaviorModel =
        {
            Input=defaultBehaviorLayer
            Root=defaultBehaviorLayer
            AbstractBusinessUserStoriesToBe= defaultBehaviorLayer
            RealizedBusinessUserStoriesToBe= defaultBehaviorLayer
            AbstractSystemUserStoriesToBe= defaultBehaviorLayer
            RealizedSystemUserStoriesToBe= defaultBehaviorLayer
            AbstractBusinessUserStoriesAsIs= defaultBehaviorLayer
            RealizedBusinessUserStoriesAsIs= defaultBehaviorLayer
            AbstractSystemUserStoriesAsIs= defaultBehaviorLayer
            RealizedSystemUserStoriesAsIs= defaultBehaviorLayer
        }


    // Supplementals
    type Supplemental =
        {
            Id:int
            ParentId:int option
            SourceFileReferences:SourceFileReference list
            EntityReferences:int list
            BehaviorReferences:int list
        }
    type SupplementalLayer =
        {
            CompilationLines:CompilationLine list
            Questions:string list
            Notes:string list
            Supplementals:Supplemental list
        }
    let defaultSupplementalLayer =
        {
            CompilationLines=[]
            Questions=[]
            Notes=[]
            Supplementals=[]
        }
    type SupplementalModelType =
        {
            Input:SupplementalLayer
            Root:SupplementalLayer
            AbstractBusinessUserStoriesToBe: SupplementalLayer
            RealizedBusinessUserStoriesToBe: SupplementalLayer
            AbstractSystemUserStoriesToBe: SupplementalLayer
            RealizedSystemUserStoriesToBe: SupplementalLayer
            AbstractBusinessUserStoriesAsIs: SupplementalLayer
            RealizedBusinessUserStoriesAsIs: SupplementalLayer
            AbstractSystemUserStoriesAsIs: SupplementalLayer
            RealizedSystemUserStoriesAsIs: SupplementalLayer
        }
    let defaultSupplementalModel =
        {
            Input= defaultSupplementalLayer
            Root= defaultSupplementalLayer
            AbstractBusinessUserStoriesToBe= defaultSupplementalLayer
            RealizedBusinessUserStoriesToBe= defaultSupplementalLayer
            AbstractSystemUserStoriesToBe= defaultSupplementalLayer
            RealizedSystemUserStoriesToBe= defaultSupplementalLayer
            AbstractBusinessUserStoriesAsIs= defaultSupplementalLayer
            RealizedBusinessUserStoriesAsIs= defaultSupplementalLayer
            AbstractSystemUserStoriesAsIs= defaultSupplementalLayer
            RealizedSystemUserStoriesAsIs= defaultSupplementalLayer
        }

    // META
    type MetaItem =
        {
            Id:int
            ParentId:int option
            Text:string
        }

    type MetaLayer =
        {
            CompilationLines:CompilationLine list
            Questions:string list
            Notes:string list
            MetaItems:MetaItem list
        }
    let defaultMetaLayer =
        {
            CompilationLines=[]
            Questions=[]
            Notes=[]
            MetaItems=[]
        }
    type MetaModelType =
        {
            Input:MetaLayer
            Root:MetaLayer
        }
    let defaultMetaModel =
        {
            Input=defaultMetaLayer
            Root=defaultMetaLayer
        }
        

    type StructuredAnalysisModel =
        {
            StructureModel:StructureModelType
            BehaviorModel:BehaviorModelType
            SupplementalModel:SupplementalModelType
            MetaModel:MetaModelType
            Unknown:CompilationLine list
        }
    let defaultStructuredAnalysisModel = {BehaviorModel=defaultBehaviorModel; StructureModel=defaultStructureModel; SupplementalModel=defaultSupplementalModel; MetaModel=defaultMetaModel; Unknown=[]}



// MIXED TYPES
    type SVGEntityBox =
        {
            xPos:int
            yPos:int
            width:int
            height:int
            Entity:Entity
        }




      type VertexData<'V> =
        int (* identifier *) *
        'V (* vertex data *)

      type EdgeData<'E> =
        int (* identifier *) *
        int (* priority *) *
        int (* vertex target *) *
        'E (* edge data *)

      (* The graph uses adjacency list notation *)
      type Adjacency<'E> = EdgeData<'E> list

      (* The Vertex type represents the internal structure
         of the graph *)
      type Vertex<'V, 'E> = VertexData<'V> * Adjacency<'E>

      (* A Graph is a Vertex list.  The nextNode allows for
         consistent addressing of nodes *)
      type Graph<'V, 'E> =
        int (* nextNode identifier *) *
        Vertex<'V, 'E> list

      (* Empty graph construction *)
      let empty: Graph<_,_> = (0, [])

      (* Helper methods for getting the data from a Vertex *)
      let vertexId (v:Vertex<_,_>) = v |> fst |> fst
      let vertexData (v:Vertex<_,_>) = v |> fst |> snd
      (* Helper methods for getting the data from an Edge *)
      let edgeId ((x,_,_,_):EdgeData<_>) = x
      let edgePriority ((_,x,_,_):EdgeData<_>) = x
      let edgeTarget ((_,_,x,_):EdgeData<_>) = x
      let edgeData ((_,_,_,x):EdgeData<_>) = x

      (* Getting a vertex from a graph by id *)
      let getVertex v (g:Graph<_, _>) : Vertex<_,_> =
        snd g |> List.find (fun V -> vertexId V = v)
      (* Getting all edges from a graph by a vertex id *)
      let getEdges v (g:Graph<_, _>) =
        g |> getVertex v |> snd

      (* Add a new vertex *)
      let addVertex (v:'V) (g:Graph<'V, _>)
        : (int*Graph<'V,_>) =
          let id = fst g
          let s = snd g
          let newVD : VertexData<_> = (id, v)
          let newA : Adjacency<_> = []
          let newV = (newVD, newA)
          (id, (id + 1, newV::s))

      (* Add a new edge.  Edges include a priority value *)
      let addEdge priority
        (v:int) (v':int) (e:'E) (g:Graph<'V, 'E>)
        : (int*Graph<'V,'E>) =
          let id = fst g
          let s = snd g
          let newE : EdgeData<_> = (id, priority, v', e)
          (id,
            (id + 1,
              s |> List.map (fun V ->
                if (vertexId V) = v then
                  (fst V, newE::(snd V))
                else V)))

      (* The edges aren't sorted by default so this function
         sorts them by priority *)
      let sortEdges (a:Adjacency<_>) =
        a |> List.sortBy edgePriority

      (* Removes an edge from a graph by id *)
      let removeEdge (id:int) (g:Graph<_,_>)
        : Graph<_,_> =
          let next = fst g
          let s = snd g
          (next, s |> List.map ( fun (v, a) ->
            (v, a |> 
                List.filter (fun x -> (edgeId x) <> id)))) 

      (* Removes a vertex from a graph by id and removes
         any related edges *)
      let removeVertex (id:int) (g:Graph<_,_>) 
        : Graph<_,_> =
          let next = fst g
          let s = snd g
          (next, s |> ([] |> List.fold (fun s' (v, a) ->
            if (fst v) = id then s'
            else
              let f = fun x -> ((edgeTarget x) <> id)
              let newA = a |> List.filter f
              let newV = (v, newA)
              newV::s')))        