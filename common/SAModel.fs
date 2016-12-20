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
         override self.ToString() =
          match self with
            | Unknown->"Unknown"
            | Abstract->"Abstract"
            | Realized->"Realized"
    type TemporalIndicators =
        | Unknown
        | AsIs
        | ToBe
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

    let commandTokens =[|"Q:"; "//"|]
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
//    type Statement =
//        {
//            Tag:InformationTag
//            StatementText:string
//        }
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
//    type DomainConnection =
//        {
//            SourceEntity:NounClause
//            DestinationEntity:NounClause
//        }
    type Attribute =
        {
            Title:NounClause
        }
    type Entity =
        {
            Id:int
            Title:NounClause
            Attributes:Attribute list
            Connections:(int*int) list
            ParentId:int option
        }



    type SourceFileReference =
        {
            FileName:string
            Line:int
        }
    type ModelBaseEntry =
        {
            id:int
            SourceFileReferences:SourceFileReference list
            Questions:string list
            Notes:string list
        }
    type UserStoryTitle =
        {
            Trigger:VerbNounClause
            Actor:NounClause
            Goal:VerbNounClause
            Context:VerbNounClause
        }
    type SupplementalTitle =
        {
            Title:string
        }
    type StructuralDiagramTitle =
        {
            Title:string
        }
    type BehaviorLayer =
        {
            CompilationLines:CompilationLine list
            Entities:Entity list
            Questions:string list
            Notes:string list
        }
//    type BucketBase =
//        {
//            Bucket:Buckets
//            CompilationLines:CompilationLine list
//        }
    type BehaviorModelType =
        {
            Root:BehaviorLayer
            AbstractBusinessEntitiesToBe: BehaviorLayer
            RealizedBusinessEntitiesToBe: BehaviorLayer
            AbstractSystemEntitiesToBe: BehaviorLayer
            RealizedSystemEntitiesToBe: BehaviorLayer
            AbstractBusinessEntitiesAsIs: BehaviorLayer
            RealizedBusinessEntitiesAsIs: BehaviorLayer
            AbstractSystemEntitiesAsIs: BehaviorLayer
            RealizedSystemEntitiesAsIs: BehaviorLayer
        }
    type StructuredAnalysisModel =
        {
            BehaviorModel:CompilationLine list
            StructureModel:CompilationLine list
            SupplementalModel:CompilationLine list
            MetaModel:CompilationLine list
            Unknown:CompilationLine list
        }
    let defaultStructuredAnalysisModel = {BehaviorModel=[]; StructureModel=[]; SupplementalModel=[]; MetaModel=[]; Unknown=[]}



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