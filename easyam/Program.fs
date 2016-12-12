module Main//
    open Types
    open Utils
    open Persist
    open FParsec
    open GemBox.Spreadsheet

    let defaultBaseOptions = createNewBaseOptions "easyAM" "Compile the Analysis Model." [|"Takes tagged statements created with Structural Analysis, cross-checks and compiles."|] defaultVerbosity
    let defaulSourceDirectory = createNewConfigEntry "S" "Source Directory (Optional)" [|"/S:<path> -> path to the directory having the source files."|] (System.AppDomain.CurrentDomain.BaseDirectory, Some(System.IO.DirectoryInfo(System.AppDomain.CurrentDomain.BaseDirectory)))
    let defaultDestinationDirectory = createNewConfigEntry "D" "Destination Directory (Optional)" [|"/D:<path> -> path to the directory where compiled files will be deployed."|] (System.AppDomain.CurrentDomain.BaseDirectory, Some(System.IO.DirectoryInfo(System.AppDomain.CurrentDomain.BaseDirectory)))
    
    let informationTagTokens =[|"STRUCUTRE"; "BEHAVIOR"; "SUPPLEMENTAL"; "META"; "BUSINESS"; "SYSTEM"; "ABSTRACT"; "REALIZED"; "AS-IS"; "TO-BE"|]
    let scopingTokens = [|"NAME"; "ORG"; "DOMAIN"|]
    let commandTokens =[|"HASA"; "Q:"|]

    let loadConfigFromCommandLine (args:string []):EasyAMProgramConfig =
        let newVerbosity =ConfigEntry<_>.populateValueFromCommandLine(defaultVerbosity, args)
        let newSourceDirectory = ConfigEntry<_>.populateValueFromCommandLine(defaulSourceDirectory, args)
        let newDestinationDirectory = ConfigEntry<_>.populateValueFromCommandLine(defaultDestinationDirectory, args)
        let newConfigBase = {defaultBaseOptions with verbose=newVerbosity}
        { 
            configBase = newConfigBase
            sourceDirectory=newSourceDirectory
            destinationDirectory=newDestinationDirectory
        }
    let getDirectories (opts:EasyAMProgramConfig) = 
        // set up any folders needed by the tool
        let sourceDirectoryInfo = forceDirectoryCreation opts.sourceDirectory
        let destinationDirectoryInfo = forceDirectoryCreation opts.destinationDirectory

        let BehaviorDirectoryInfo = getOrMakeDirectory (destinationDirectoryInfo.FullName + System.IO.Path.DirectorySeparatorChar.ToString() + "Behavior")
        let StructureDirectoryInfo = getOrMakeDirectory (destinationDirectoryInfo.FullName + System.IO.Path.DirectorySeparatorChar.ToString() + "Structure")
        let SupplementalDirectoryInfo = getOrMakeDirectory (destinationDirectoryInfo.FullName + System.IO.Path.DirectorySeparatorChar.ToString() + "Supplemental")
        let MetaDirectoryInfo = getOrMakeDirectory (destinationDirectoryInfo.FullName + System.IO.Path.DirectorySeparatorChar.ToString() + "Meta")
        {
            SourceDirectoryInfo=sourceDirectoryInfo
            DestinationDirectoryInfo=destinationDirectoryInfo
            BehaviorDirectoryInfo=BehaviorDirectoryInfo
            StructureDirectoryInfo=StructureDirectoryInfo
            SupplementalDirectoryInfo=SupplementalDirectoryInfo
            MetaDirectoryInfo=MetaDirectoryInfo
        }
    let loadInAllIncomingLines (fileList:System.IO.FileInfo[]) =
        let lineDumpForAllFiles = fileList |> Array.mapi(fun i x->
            System.IO.File.ReadLines(x.FullName) |> Seq.toList  |> List.mapi(fun (j:int) (y:string)->
                {
                    File=Some x
                    LineNumber=j
                    LineType=CompilationLineType.Unknown
                    CommandType=CompilationLineCommands.Unknown
                    Scope=""
                    TaggedContext=
                        {
                            Bucket=Buckets.Unknown
                            Genre=Genres.Unkown
                            TemporalIndicator=TemporalIndicators.Unknown
                            AbstractionLevel=AbstractionLevels.Unknown
                        }
                    LineText=y  
                }
            )
        )
        lineDumpForAllFiles |> List.concat

    let sortOutLineTypes (lines:CompilationLine list) =
        let removedEmptyLines = lines |> List.filter(fun x->x.LineText.Length>0)
        let determinedLineType = removedEmptyLines |> List.map(fun x->
            match (x.LineText.ContainsAny informationTagTokens), (x.LineText.ContainsAny scopingTokens), (x.LineText.ContainsAny commandTokens) with
                | true, _, _ ->
                    {x with LineType=CompilationLineType.Context}
                | false, true, _ ->
                    {x with LineType=CompilationLineType.Scoping}
                | false, false, true->
                    let foundToken = commandTokens |> Array.find(fun y->x.LineText.Contains(y))
                    let newCommandType = match foundToken with
                                                | "HASA"->CompilationLineCommands.Hasa
                                                | "Q:"->CompilationLineCommands.Question
                                                |_ ->CompilationLineCommands.Unknown
                    {x with CommandType=newCommandType}
                | false, false, false->
                    {x with LineType=CompilationLineType.Freetext}
            )
        determinedLineType

    let addRunningContext (lines:CompilationLine list):CompilationContext =        
        let k=9
        lines |> List.fold(fun (acc:CompilationContext) x->
            // when the file changes, reset the context
            let newacc = 
                if x.File.IsSome && x.File.Value.FullName <> acc.CurrentFile
                    then 
                        {acc with CurrentFile=x.File.Value.FullName; State = defaultInformationTag}
                    else acc
            let newCompilationContext = 
                match x.LineType with
                    | CompilationLineType.Scoping->
                            let foundToken = scopingTokens |> Array.find(fun y->x.LineText.Contains(y))
                            let positionWhereAllCapsKeywordStarts = x.LineText.IndexOf(foundToken)
                            let newScope = x.LineText.Substring(positionWhereAllCapsKeywordStarts + foundToken.Length).Trim()
                            {newacc with Scope= newScope}
                    | CompilationLineType.Context->
                            let lineWords = x.LineText.Split([|" "|],System.StringSplitOptions.None)
                            let newContext = lineWords |> Array.fold(fun (acc:CompilationContext) x->
                                                                            match x with 
                                                                                        | "BEHAVIOR"->{acc with State={acc.State with Bucket=Buckets.Behavior}}
                                                                                        | "STRUCTURE"->{acc with State={acc.State with Bucket=Buckets.Structure}}
                                                                                        | "SUPPLEMENTAL"->{acc with State={acc.State with Bucket=Buckets.Supplemental}}
                                                                                        | "META"->{acc with State={acc.State with Bucket=Buckets.Meta}}
                                                                                        | "BUSINESS"->{acc with State={acc.State with Genre=Genres.Business}}
                                                                                        | "SYSTEM"->{acc with State={acc.State with Genre=Genres.System}}
                                                                                        | "ABSTRACT"->{acc with State={acc.State with AbstractionLevel=AbstractionLevels.Abstract}}
                                                                                        | "REALIZED"->{acc with State={acc.State with AbstractionLevel=AbstractionLevels.Realized}}
                                                                                        | "AS-IS"->{acc with State={acc.State with TemporalIndicator=TemporalIndicators.AsIs}}
                                                                                        | "TO-BE"->{acc with State={acc.State with TemporalIndicator=TemporalIndicators.ToBe}}
                                                                                        |_ ->acc

                                                                            ) newacc
                            newContext
                    | CompilationLineType.Command->
                        newacc
                    | CompilationLineType.Freetext->
                        newacc
                    |_->newacc

            let newListItem =
                 { x with
                    Scope=newCompilationContext.Scope
                    TaggedContext=newCompilationContext.State
                 }
            let newCompilationLines = List.append newacc.CompilationLines [newListItem]
            {
                newCompilationContext with
                    CompilationLines=newCompilationLines
            }
            ) defaultCompilationContext
    let dumpCSVs (ctx:CompilationContext) = 
        SpreadsheetInfo.SetLicense("FREE-LIMITED-KEY");
        let ef = new ExcelFile()
        let ws = ef.Worksheets.Add("Domain")
        ws.Cells.[0, 0].Value<-"Domain Statements"
        let domainStatements = ctx.CompilationLines |> List.filter(fun x->
            (x.CommandType=CompilationLineCommands.Hasa)
            && (x.TaggedContext.Bucket=Buckets.Structure)
            && (x.TaggedContext.Genre=Genres.Business))
        domainStatements |> List.iteri(fun i x->
            ws.Cells.[1+i, 0].Value<-x.LineText
            )
        ef.Save("domain.csv")

        let ef = new ExcelFile()
        let ws = ef.Worksheets.Add("Open Questions")
        ws.Cells.[0, 0].Value<-"Questions"
        let questions = ctx.CompilationLines |> List.filter(fun x->x.CommandType = CompilationLineCommands.Question)
        questions |> List.iteri(fun i x->
            ws.Cells.[1+i, 0].Value<-x.LineText
            )
        System.IO.File.Delete("questions.csv")
        ef.Save("questions.csv")

        let svgOutputFile = new System.IO.StreamWriter("out.svg")
        svgOutputFile.WriteLine "<svg  xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">" 
        svgOutputFile.WriteLine ""
        svgOutputFile.WriteLine ""
        svgOutputFile.WriteLine "<rect x=\"10\" y=\"10\" height=\"100\" width=\"100\" style=\"stroke:#ff0000; fill: #0000ff\"/>"
        svgOutputFile.WriteLine ""
        svgOutputFile.WriteLine ""
        svgOutputFile.WriteLine "</svg>"
        svgOutputFile.Flush()
        svgOutputFile.Close()
        ()

    let doStuff (opts:EasyAMProgramConfig) =
        let programDirectories = getDirectories opts
        let fileList = programDirectories.SourceDirectoryInfo.GetFiles() |> Seq.filter(fun x->
            ((x.Name.GetRight 3).ToUpper() <> "EXE") && ((x.Name.GetRight 3).ToUpper() <> "DLL") && ((x.Name.GetRight 3).ToUpper() <> "XML") && ((x.Name.GetRight 3).ToUpper() <> "PDB") && ((x.Name.GetRight 6).ToUpper() <> "CONFIG") && ((x.Attributes.HasFlag(System.IO.FileAttributes.Directory)=false)) ) |> Seq.toArray
        let compilationLines = loadInAllIncomingLines fileList
        let lineTypesAdded = sortOutLineTypes compilationLines
        let lineContextAdded = addRunningContext lineTypesAdded
        dumpCSVs lineContextAdded
        ()


    [<EntryPoint>]
    let main argv = 
        try
            let opts = loadConfigFromCommandLine argv //
            commandLinePrintWhileEnter opts.configBase (opts.printThis)
            let outputDirectories = doStuff opts
            commandLinePrintWhileExit opts.configBase
            0
        with
            | :? UserNeedsHelp as hex ->
                defaultBaseOptions.printThis
                0
            | :? System.Exception as ex ->
                System.Console.WriteLine ("Program terminated abnormally " + ex.Message)
                System.Console.WriteLine (ex.StackTrace)
                if ex.InnerException = null
                    then
                        0
                    else
                        System.Console.WriteLine("---   Inner Exception   ---")
                        System.Console.WriteLine (ex.InnerException.Message)
                        System.Console.WriteLine (ex.InnerException.StackTrace)
                        0    