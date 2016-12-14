module SAModel
    open Types

// INCOMING FILE TYPES
    let informationTagTokens =[|"STRUCUTRE"; "BEHAVIOR"; "SUPPLEMENTAL"; "META"; "BUSINESS"; "SYSTEM"; "ABSTRACT"; "REALIZED"; "AS-IS"; "TO-BE"|]
    let scopingTokens = [|"NAME"; "ORG"; "DOMAIN"|]
    let commandTokens =[|"HASA"; "CONTAINS"; "Q:"; "//"|]

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
        | Unkown
        | Business
        | System
    type Buckets =
        | Unknown
        | Behavior
        | Structure
        | Supplemental
        | Meta
    type AbstractionLevels = 
        | Unknown
        | Abstract
        | Realized
    type TemporalIndicators =
        | Unknown
        | AsIs
        | ToBe
    type InformationTag =
        {
            Genre:Genres
            Bucket:Buckets
            AbstractionLevel:AbstractionLevels
            TemporalIndicator:TemporalIndicators
        }
    let defaultInformationTag=
        {
            Genre=Genres.Unkown
            Bucket=Buckets.Unknown
            AbstractionLevel=AbstractionLevels.Unknown
            TemporalIndicator=TemporalIndicators.Unknown
        }
    type Statement =
        {
            Tag:InformationTag
            StatementText:string
        }
    type ProgramOutputDirectories =
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
    type CompilationLineType =
        | Unknown
        | Scoping
        | Context
        | Command
        | Freetext
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
    type DomainConnection =
        {
            SourceEntity:NounClause
            DestinationEntity:NounClause
        }
    type Entity =
        {
            Title:NounClause
            Attributes:NounClause list
            Connections:NounClause list
        }
    type StructureModel =
        {
            Entities:Entity list
            DomainConnections:DomainConnection list
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
    type StructuralModelTitle =
        {
            Title:string
        }
//    type ModelBase =
//        {
//            DOWOP:string   
//        }
    type StructuredAnalysisModel =
        {
            BehaviorModel:string
            StructuralModel:string
            SupplementalModel:string
            Meta:string
        }




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