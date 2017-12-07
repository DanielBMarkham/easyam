/// Universal types and type modifications for all programs
module Types
    open System.Text.RegularExpressions
    open System.Collections
    open System.Collections.Generic
    open System

    /// for option types, gimme either the value or a default I'll provide
    let inline (|?) (a: 'a option) b = if a.IsSome then a.Value else b
    /// for .NET nullable types, gimme the value or the default I'll provide
    let inline (|??) (a: 'a Nullable) b = if a.HasValue then a.Value else b


    type 'a ``[]`` with         
        member x.randomItem = 
            let rnd = new System.Random()
            let idx = rnd.Next(x.Length)
            x.[idx]
    type System.Random with
        /// Generates an infinite sequence of random numbers within the given range.
        member this.GetValues(minValue, maxValue) =
            Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))

    type System.String with
        member x.ContainsAny (possibleMatches:string[]) =
            let ret = possibleMatches |> Array.tryFind(fun y->
                x.Contains(y)
                )
            ret.IsSome
        member x.ContainsAnyRegex(possibleRegexMatches:string[]) =
            let ret = possibleRegexMatches |> Array.tryFind(fun y->
                let rg = new System.Text.RegularExpressions.Regex(y)
                rg.IsMatch(x)
                )
            ret.IsSome
        member x.ContainsRegex(regexMatch:string) =
            let rg = new System.Text.RegularExpressions.Regex(regexMatch)
            rg.IsMatch(x)
        member x.ReplaceWithRegex (regexMatchString:string) (replacementString:string) = 
            System.Text.RegularExpressions.Regex.Replace(x, regexMatchString, replacementString)
        member x.ReplaceAny (charactersToReplace:char []) (characterToUse:char) =
            let sb = new System.Text.StringBuilder(x)
            let newString = charactersToReplace |> Array.fold(fun (acc:System.Text.StringBuilder) x->
                            acc.Replace(x,characterToUse)
                            ) sb
            newString.ToString()
        member x.CountOccurences (token:string) = 
            let mts = x.Split([|token|], System.StringSplitOptions.None)
            if mts = null then 0 else mts.Length
        member x.CountOccurencesRegex (regexMatchString:string) =
            let mts = System.Text.RegularExpressions.Regex.Matches(x, regexMatchString)
            if mts = null then 0 else mts.Count
        member this.GetRight (iLen:int) =
            try
                this.Substring(this.Length - iLen, iLen)
            with |_ -> ""
        member this.GetLeft (iLen:int) =
            try
                this.Substring(0, iLen)
            with |_ -> ""
        member this.TrimLeft (iCount:int) =
            this.Substring(iCount, this.Length - iCount)
        member this.TrimRight (iCount:int) =
            this.Substring(0, this.Length - iCount)    
        member this.TrimBoth (iLeft:int) (iRight:int) =
            if iLeft + iRight > this.Length
                then
                    ""
                else
                    (this.TrimLeft iLeft) |> (fun x-> x.TrimRight iRight)
        member this.TrimTo (desiredLength:int) =
            if this.Length <= desiredLength
                then
                    this
                else
                    this.GetLeft desiredLength
        /// adds the number of spaces to the beginning of the string
        member this.AddSpaces (numSpaces) =
            let prefix = new System.String(' ', numSpaces)
            prefix+this
        /// Centers text using spaces given a certain line length
        member this.PadBoth (len:int) =
            let leftPadCount = len/2 + this.Length/2
            this.PadLeft(leftPadCount).PadRight(len)
        member this.ToSafeFileName() =
            let temp=this.ToLower().ToCharArray() |> Array.map(fun x->
                let badChar=System.IO.Path.GetInvalidFileNameChars()|>Seq.exists(fun y->y=x)
                if badChar || x=' ' then '-' else x
            )
             new System.String(temp)
    type System.Text.StringBuilder with
        /// Write a line ending with the current OS newline character
        member x.wl (stringToWrite:string) =
            x.Append(stringToWrite + System.Environment.NewLine) |> ignore
        /// Write a line at a certain tab level ending with the current OS newline character
        member x.wt (level:int) (content:string) =
            let prefix = new System.String(' ', level*2)
            x.Append(prefix+content + System.Environment.NewLine) |> ignore
        /// Centers text across line using spaces on both sides. Default 80-character line can be overridden
        member x.wc (content:string) (?lineLength:int) =
            if lineLength.IsSome
                then
                    x.Append((content.PadBoth lineLength.Value) + System.Environment.NewLine) |> ignore
                else
                    x.Append((content.PadBoth 80) + System.Environment.NewLine) |> ignore
    type System.IO.TextWriter with
        /// Shorter version of WriteLine
        member x.wl (stringToWrite:string) =
            x.WriteLine(stringToWrite)
        /// WriteLine at a certain tab level
        member x.wt (level:int) (content:string) =
            let prefix = new System.String(' ', level*2)
            x.WriteLine(prefix+content)
        /// Centers text across line using spaces on both sides. Default 80-character line can be overridden
        member x.wc (content:string) (?lineLength:int) =
            if lineLength.IsSome
                then
                    x.WriteLine(content.PadBoth lineLength.Value)
                else
                    x.WriteLine(content.PadBoth 80) 

    type System.Collections.Generic.Dictionary<'A, 'B> with
        member x.stringValueOrEmptyForKey n = 
            if x.ContainsKey n then x.Item(n).ToString() else ""
        member x.TryFind n = 
            let x,(y:'B) = x.TryGetValue n
            if x then Some y else None
    //type Microsoft.FSharp.Collections.List<'T when 'T : equality> with
    //    member this.IntersectionWithOtherList (b:List<'T> when 'T : equality) = this |> List.filter (fun x -> not (List.contains x b))
    type System.Text.RegularExpressions.MatchCollection with
        member this.toSeq =
            seq {for i = 0 to this.Count - 1 do yield this.[i]}
        member this.toArray =
            [|for i = 0 to this.Count - 1 do yield this.[i] |]
    type System.Text.RegularExpressions.Match with
        member this.lastGroup =
            this.Groups.[this.Groups.Count-1]
        member this.lastIndex =
            this.lastGroup.Index + this.lastGroup.Length
    //
    // Common Exception Types
    //
    exception UserNeedsHelp of string
    exception ExpectedResponseFail of string
    type Verbosity =
        | Silent            = 1
        | BatchMinimum      = 2
        | Minimum           = 3
        | BatchNormal       = 4
        | Normal            = 5
        | BatchVerbose      = 6
        | Verbose           = 7
    //
    // Program Command Line Config Settings
    //
    let getMatchingParameters (args:string []) (symbol:string) = 
        args |> Array.filter(fun x->
                    let argParms = x.Split([|':'|],2)
                    let parmName = (argParms.[0]).Substring(1).ToUpper()
                    if argParms.Length > 0 then parmName=symbol.ToUpper() else false
                    )
    let getValuePartOfMostRelevantCommandLineMatch (args:string []) (symbol:string) =
        let matchingParms = getMatchingParameters args symbol
        if matchingParms.Length > 0
            then
                // if there are multiple entries, last one overrides the rest
                let commandLineParm = matchingParms.[matchingParms.Length-1]
                let parmSections=commandLineParm.Split([|':'|], 2)
                if parmSections.Length<2 then Some "" else Some parmSections.[1]
            else
                None
    type FileParm = string*System.IO.FileInfo option
    type DirectoryParm = string*System.IO.DirectoryInfo option
    type SortOrder = Ascending | Descending
                        static member ToList()=[Ascending;Descending]
                        override this.ToString()=
                            match this with
                                | Ascending->"Ascending"
                                | Descending->"Descending"
                        static member TryParse(stringToParse:string) =
                            match stringToParse with
                                |"a"|"asc"|"ascending"|"A"|"ASC"|"Ascending"|"Asc"|"ASCENDING"->true,SortOrder.Ascending
                                |"d"|"desc"|"descending"|"D"|"DESC"|"Descending"|"Desc"|"DESCENDING"->true,SortOrder.Descending
                                |_->false, SortOrder.Ascending
                        static member Parse(stringToParse:string) =
                            match stringToParse with
                                |"a"|"asc"|"ascending"|"A"|"ASC"|"Ascending"|"Asc"|"ASCENDING"->SortOrder.Ascending
                                |"d"|"desc"|"descending"|"D"|"DESC"|"Descending"|"Desc"|"DESCENDING"->SortOrder.Descending
                                |_->raise(new System.ArgumentOutOfRangeException("Sort Order","The string value provided for Sort Order is not in the Sort Order enum"))
    type TagOrAtt = Tag | Att
                        static member ToList()=[Tag;Att]
                        override this.ToString()=
                            match this with
                                | Tag->"Tag"
                                | Att->"Att"
                        static member TryParse(stringToParse:string) =
                            match stringToParse with
                                |"T"|"t"|"tag"|"Tag"|"TAG"->true,TagOrAtt.Tag
                                |"A"|"a"|"att"|"Att"|"ATT"->true,TagOrAtt.Att
                                |_->false, TagOrAtt.Tag
                        static member Parse(stringToParse:string) =
                            match stringToParse with
                                |"T"|"t"|"tag"|"Tag"|"TAG"->TagOrAtt.Tag
                                |"A"|"a"|"att"|"Att"|"ATT"->TagOrAtt.Att
                                |_->raise(new System.ArgumentOutOfRangeException("Tag Or Attribute","The string value provided for TagOrAtt is not in the TagOrAtt enum"))
    type ConvertTo = DontConvert | Int | Float | Money | DateTime | TimeSpan
                        static member ToList()=[DontConvert;Int;Float;Money;DateTime;TimeSpan]
                        override this.ToString()=
                            match this with
                                | DontConvert->"Don't Convert"
                                | Int->"Int"
                                | Float->"Float"
                                | Money->"Money"
                                | DateTime->"DateTime"
                                | TimeSpan->"TimeSpan"
                        static member TryParse(stringToParse:string) =
                            match stringToParse with
                                |"d"|"date"|"datetime"|"D"|"DATE"|"Datetime"|"DateTime"|"DATETIME"->true,ConvertTo.DateTime
                                |"f"|"float"|"F"|"Float"|"FLOAT"->true,ConvertTo.Float
                                |"i"|"int"|"I"|"Int"->true,ConvertTo.Int
                                |"m"|"money"|"M"|"Money"|"dec"|"Dec"|"decimal"|"Decimal"->true,ConvertTo.Money
                                |"t"|"timespan"|"T"|"Timespan"|"TimeSpan"->true,ConvertTo.TimeSpan
                                |_->false,ConvertTo.DontConvert
                        static member Parse(stringToParse:string) =
                            match stringToParse with
                                |"d"|"date"|"datetime"|"D"|"DATE"|"Datetime"|"DateTime"|"DATETIME"->ConvertTo.DateTime
                                |"f"|"float"|"F"|"Float"|"FLOAT"->ConvertTo.Float
                                |"i"|"int"|"I"|"Int"->ConvertTo.Int
                                |"m"|"money"|"M"|"Money"|"dec"|"Dec"|"decimal"|"Decimal"->ConvertTo.Money
                                |"t"|"timespan"|"T"|"Timespan"|"TimeSpan"->ConvertTo.TimeSpan
                                |_->raise(new System.ArgumentOutOfRangeException("Sort Convert To","The string value provided for ConvertTo is not in the ConvertTo enum"))
    type ModelOutputType = 
        | AMOUT 
        | HTML 
        | TEXT 
        | CSV
        | GHERKIN
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
         static member TryParse(stringToParse:string) =
            match stringToParse.ToUpper() with
                |"BEHAVIOR"|"BEHAVIOUR"->(true,Buckets.Behavior)
                |"STRUCTURE"->(true,Buckets.Structure)
                |"SUPPLEMENTAL"->(true,Buckets.Supplemental)
                |_->(false, Buckets.Unknown)
         static member Parse(stringToParse:string) =
            match stringToParse.ToUpper() with
                |"BEHAVIOR"|"BEHAVIOUR"->Buckets.Behavior
                |"STRUCTURE"->Buckets.Structure
                |"SUPPLEMENTAL"->Buckets.Supplemental
                |_->raise(new System.ArgumentOutOfRangeException("Buckets","The string value provided for Buckets is not in the Buckets enum"))
        member self.ToModelOutputSectionHeading(outputType:ModelOutputType) =
            match outputType with 
                | AMOUT | TEXT ->
                    self.ToString().ToUpper() + ":"
                | HTML->"<span class='bucketDescription'>" + self.ToString() + "</span> <!-- bucketDescription -->"
                | CSV->
                    self.ToString().ToUpper()
                | GHERKIN->""

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
         static member TryParse(stringToParse:string) =
            match stringToParse.ToUpper() with
                |"BUSINESS"|"BIZ"|"BUS"->true,Genres.Business
                |"SYSTEM"|"SYS"->true,Genres.System
                |"META"->true,Genres.Meta
                |_->(false, Genres.Unknown)
         static member Parse(stringToParse:string) =
            match stringToParse.ToUpper() with
                |"BUSINESS"|"BIZ"|"BUS"->Genres.Business
                |"SYSTEM"|"SYS"->Genres.System
                |"META"->Genres.Meta
                |_->raise(new System.ArgumentOutOfRangeException("Genres","The string value provided for Genres is not in the Genres enum"))
        member self.ToModelOutputSectionHeading(outputType:ModelOutputType) =
            match outputType with 
                | AMOUT | TEXT ->
                    self.ToString().ToUpper() + ":"
                | HTML->"<span class='genreDescription'>" + self.ToString() + "</span> <!-- genreDescription -->"
                | CSV->
                    self.ToString().ToUpper()
                | GHERKIN->""
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
         static member TryParse(stringToParse:string) =
            match stringToParse.ToUpper() with
                |"ABS"|"ABSTRACT"->true,AbstractionLevels.Abstract
                |"REAL"|"REALIZED"|"Actual"->true,AbstractionLevels.Realized
                |_->(false, AbstractionLevels.Unknown)
         static member Parse(stringToParse:string) =
            match stringToParse.ToUpper() with
                |"ABS"|"ABSTRACT"->AbstractionLevels.Abstract
                |"REAL"|"REALIZED"|"Actual"->AbstractionLevels.Realized
                |_->raise(new System.ArgumentOutOfRangeException("AbstractionLevels","The string value provided for AbstractionLevels is not in the AbstractionLevels enum"))
        member self.ToModelOutputSectionHeading(outputType:ModelOutputType) =
            match outputType with 
                | AMOUT | TEXT ->
                    self.ToString().ToUpper() + ":"
                | HTML->"<span class='abstractionLevelDescription'>" + self.ToString() + "</span> <!-- abstractionLevelDescription -->"
                | CSV->
                    self.ToString().ToUpper()
                | GHERKIN->""
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
         static member TryParse(stringToParse:string) =
            match stringToParse.ToUpper() with
                |"WAS"->true,TemporalIndicators.Was
                |"ASIS"|"AS-IS"|"IS"->true,TemporalIndicators.AsIs
                |"TOBE"|"TO-BE"|"TO BE"->true,TemporalIndicators.ToBe
                |_->(false, TemporalIndicators.Unknown)
         static member Parse(stringToParse:string) =
            match stringToParse.ToUpper() with
                |"WAS"->TemporalIndicators.Was
                |"ASIS"|"AS-IS"|"IS"->TemporalIndicators.AsIs
                |"TOBE"|"TO-BE"|"TO BE"->TemporalIndicators.ToBe
                |_->raise(new System.ArgumentOutOfRangeException("TemporalIndicators","The string value provided for TemporalIndicators is not in the TemporalIndicators enum"))

    /// Parameterized type to allow command-line argument processing without a lot of extra coder work
    /// Instantiate the type with the type of value you want. Make a default entry in case nothing is found
    /// Then call the populate method. Will pull from args and return a val and args with the found value (if any consumed)
    type ConfigEntry<'A> =
        {
            commandLineParameterSymbol:string
            commandLineParameterName:string
            parameterHelpText:string[]
            parameterValue:'A
        } with
            member this.printVal =
                printfn "%s: %s" this.commandLineParameterName (this.parameterValue.ToString())
            member this.printHelp =
                printfn "%s" this.commandLineParameterName
                this.parameterHelpText |> Seq.iter(System.Console.WriteLine)
            member this.swapInNewValue x =
                {this with parameterValue=x}
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<Verbosity>), (args:string[])):ConfigEntry<Verbosity>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        let parsedNumValue = System.Int32.Parse("0" + parmValue.Value)
                        let parsedVerbosityValue = enum<Verbosity>(parsedNumValue)
                        defaultConfig.swapInNewValue parsedVerbosityValue
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<string>), (args:string[])):ConfigEntry<string>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        defaultConfig.swapInNewValue parmValue.Value
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<DirectoryParm>), (args:string[])):ConfigEntry<DirectoryParm>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        if System.IO.Directory.Exists(parmValue.Value)
                            then 
                                let tempDirectoryInfo = Some(System.IO.DirectoryInfo(parmValue.Value))
                                defaultConfig.swapInNewValue (parmValue.Value, tempDirectoryInfo)
                            else defaultConfig.swapInNewValue (parmValue.Value, Option.None)
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<FileParm>), (args:string[])):ConfigEntry<FileParm>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        if System.IO.File.Exists(parmValue.Value)
                            then
                                let tempFileInfo = Some(System.IO.FileInfo(parmValue.Value))
                                defaultConfig.swapInNewValue (parmValue.Value, tempFileInfo)
                            else
                                defaultConfig.swapInNewValue (parmValue.Value, Option.None)
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<bool>), (args:string[])):ConfigEntry<bool> =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        if parmValue.Value.ToUpper() = "FALSE" || parmValue.Value = "0" || parmValue.Value.ToUpper() = "F" || parmValue.Value.ToUpper() = "NO"
                            then
                                defaultConfig.swapInNewValue false
                            else
                                defaultConfig.swapInNewValue true
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<int>), (args:string[])):ConfigEntry<int>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        let parmInt = System.Int32.Parse("0" + parmValue.Value)
                        defaultConfig.swapInNewValue parmInt
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<System.Uri>), (args:string[])):ConfigEntry<System.Uri>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        defaultConfig.swapInNewValue (new System.Uri(parmValue.Value))
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<System.DateTime>), (args:string[])):ConfigEntry<System.DateTime>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                if parmValue.IsSome
                    then
                        defaultConfig.swapInNewValue (System.DateTime.Parse(parmValue.Value))
                    else
                        defaultConfig
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<SortOrder>), (args:string[])):ConfigEntry<SortOrder>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                let newVal=if parmValue.IsNone then defaultConfig.parameterValue else
                            let tp=SortOrder.TryParse parmValue.Value
                            if fst tp=true then snd tp else defaultConfig.parameterValue
                defaultConfig.swapInNewValue newVal
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<TagOrAtt>), (args:string[])):ConfigEntry<TagOrAtt>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                let newVal=if parmValue.IsNone then defaultConfig.parameterValue else
                            let tp=TagOrAtt.TryParse parmValue.Value
                            if fst tp=true then snd tp else defaultConfig.parameterValue
                defaultConfig.swapInNewValue newVal
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<ConvertTo>), (args:string[])):ConfigEntry<ConvertTo>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                let newVal=if parmValue.IsNone then defaultConfig.parameterValue else
                            let tp=ConvertTo.TryParse parmValue.Value
                            if fst tp=true then snd tp else defaultConfig.parameterValue
                defaultConfig.swapInNewValue newVal
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<Genres>), (args:string[])):ConfigEntry<Genres>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                let newVal=if parmValue.IsNone then defaultConfig.parameterValue else
                            let tp=Genres.TryParse parmValue.Value
                            if fst tp=true then snd tp else defaultConfig.parameterValue
                defaultConfig.swapInNewValue newVal
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<Buckets>), (args:string[])):ConfigEntry<Buckets>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                let newVal=if parmValue.IsNone then defaultConfig.parameterValue else
                            let tp=Buckets.TryParse parmValue.Value
                            if fst tp=true then snd tp else defaultConfig.parameterValue
                defaultConfig.swapInNewValue newVal
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<AbstractionLevels>), (args:string[])):ConfigEntry<AbstractionLevels>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                let newVal=if parmValue.IsNone then defaultConfig.parameterValue else
                            let tp=AbstractionLevels.TryParse parmValue.Value
                            if fst tp=true then snd tp else defaultConfig.parameterValue
                defaultConfig.swapInNewValue newVal
            static member populateValueFromCommandLine ((defaultConfig:ConfigEntry<TemporalIndicators>), (args:string[])):ConfigEntry<TemporalIndicators>  =
                let parmValue = getValuePartOfMostRelevantCommandLineMatch args defaultConfig.commandLineParameterSymbol
                let newVal=if parmValue.IsNone then defaultConfig.parameterValue else
                            let tp=TemporalIndicators.TryParse parmValue.Value
                            if fst tp=true then snd tp else defaultConfig.parameterValue
                defaultConfig.swapInNewValue newVal
    /// A type so that programs can report what they're doing as they do it
    // This was the programmer can decide what to do with it instead of the OS
    [<NoComparison>]
    type InterimProgress =
        {
            items:System.Collections.Generic.Dictionary<string, System.Text.StringBuilder>
        } with
        member this.addItem key (vl:string) =
            let lookup = 
                if this.items.ContainsKey key then this.items.Item(key)
                    else
                        let newItem = new System.Text.StringBuilder(65535)
                        this.items.Add(key,newItem)
                        newItem
            lookup.Append("\r\n" + vl) |> ignore
        member this.getItem key  =
            if this.items.ContainsKey key
                then
                    this.items.Item(key).ToString()
                else
                    ""
    // All programs have at least this configuration on the command line
    [<NoComparison>]
    type ConfigBase =
        {
            programName:string
            programTagLine:string
            programHelpText:string[]
            verbose:ConfigEntry<Verbosity>
            interimProgress:InterimProgress
        }
        member this.printProgramDescription =
            this.programHelpText |> Seq.iter(System.Console.WriteLine)
        member this.printThis =
            printfn "%s" this.programName
            this.programHelpText |> Seq.iter(System.Console.WriteLine)

    /// Command-line parameters for this particular (easyam) program
    [<NoComparison>]
    type EasyAMProgramConfig =
        {
            configBase:ConfigBase
            sourceDirectory:ConfigEntry<DirectoryParm>
            destinationDirectory:ConfigEntry<DirectoryParm>
            nameSpace:ConfigEntry<string>
            sortTagOrAtt:ConfigEntry<TagOrAtt>
            sortThing:ConfigEntry<string>
            sortConvertTo:ConfigEntry<ConvertTo>
            sortOrder:ConfigEntry<SortOrder>
            filterGenre:ConfigEntry<Genres>
            filterBucket:ConfigEntry<Buckets>
            filterAbstractionLevel:ConfigEntry<AbstractionLevels>
            filterTemporalIndicator:ConfigEntry<TemporalIndicators>
            filterTagOrAtt:ConfigEntry<TagOrAtt>
            filterThing:ConfigEntry<string>
            filterConvertTo:ConfigEntry<ConvertTo>
            filterOrder:ConfigEntry<SortOrder>
            filterFromVal:ConfigEntry<string>
            filterToVal:ConfigEntry<string>
            outputFormat:ConfigEntry<string>
        }
        member this.printThis() =
            printfn "EasyAMConfig Parameters Provided"
            this.configBase.verbose.printVal
            this.sourceDirectory.printVal
            printfn "sourceDirectoryExists: %b" (snd this.sourceDirectory.parameterValue).IsSome
            this.destinationDirectory.printVal
            printfn "destinationDirectoryExists: %b" (snd this.destinationDirectory.parameterValue).IsSome
            this.nameSpace.printVal
            printfn "Namespace: %s" this.nameSpace.parameterValue
            printfn "Sort Tag or Att: %s" (string this.sortTagOrAtt.parameterValue)
            printfn "Thing to sort: %s" (string this.sortThing.parameterValue)
            printfn "Convert the thing into: %s" (string this.sortConvertTo.parameterValue)
            printfn "Sort order: %s" (string this.sortOrder.parameterValue)
            printfn "Filter Genre: %s" (string this.filterGenre.parameterValue)
            printfn "Filter Bucket: %s" (string this.filterBucket.parameterValue)
            printfn "Filter Abstraction Level: %s" (string this.filterAbstractionLevel.parameterValue)
            printfn "Filter Temporal Indicator: %s" (string this.filterTemporalIndicator.parameterValue)
            printfn "Filter Tag Or Att: %s" (string this.filterTagOrAtt.parameterValue)
            printfn "Filter Thing: %s" (string this.filterThing.parameterValue)
            printfn "Filter Convert To: %s" (string this.filterConvertTo.parameterValue)
            printfn "Filter order: %s" (string this.filterOrder.parameterValue)
            printfn "Filter From Val: %s" (string this.filterFromVal.parameterValue)
            printfn "Filter To Val: %s" (string this.filterToVal.parameterValue)
            printfn "Output format: %s" (string this.outputFormat.parameterValue)

    let directoryExists (dir:ConfigEntry<DirectoryParm>) = (snd (dir.parameterValue)).IsSome
    let fileExists (dir:ConfigEntry<FileParm>) = (snd (dir.parameterValue)).IsSome
    [<NoComparison>]
    type ProgramDirectories =
        {
            SourceDirectoryInfo:System.IO.DirectoryInfo
            DestinationDirectoryInfo:System.IO.DirectoryInfo
            //BehaviorDirectoryInfo:System.IO.DirectoryInfo
            //StructureDirectoryInfo:System.IO.DirectoryInfo
            //SupplementalDirectoryInfo:System.IO.DirectoryInfo
            FeaturesDirectoryInfo:System.IO.DirectoryInfo
        }
    [<NoComparison>]
    type ProgramInputFiles = 
        {
            Files:System.IO.FileInfo list
        }

    let replaceArrayItemInPlace (arr:'A []) (itemToReplace:'A) (funEquality:'A->'A->bool):'A [] =
        let itemIndex=
            let tempFind = arr |> Array.tryFindIndex(funEquality itemToReplace)
            if tempFind.IsSome then tempFind.Value else raise(new System.ArgumentOutOfRangeException())
        Array.set arr itemIndex itemToReplace
        arr

 /// Homegrown/copied pure functional stack implementation
 /// from https://viralfsharp.com/2012/02/11/implementing-a-stack-in-f/
    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type Stack<'a> = 
        | StackNode of 'a list
        with
            member private t.StructuredFormatDisplay = 
                if t.length = 0 then "()"
                else
                    let str = t |> Seq.fold (fun st e -> st + e.ToString() + "; ") "("
                    str.Substring(0, str.Length - 2) + ")"
 
            member t.length =
                t |> Seq.length
            member internal t.asList = 
                match t with StackNode(x) -> x
 
            member t.isEmpty = t.length = 0
            static member empty=StackNode(FSharp.Collections.List<'a>.Empty)
            interface IEnumerable<'a> with
                member x.GetEnumerator() = (x.asList |> List.toSeq).GetEnumerator()
 
            interface IEnumerable with
                member x.GetEnumerator() =  (x.asList |> List.toSeq).GetEnumerator() :> IEnumerator
    let peek = function
        | StackNode([]) -> Unchecked.defaultof<'a>
        | StackNode(hd::tl) -> hd
 
    let pushStack hd tl = 
        match tl with
        |StackNode(x) -> StackNode(hd::x)
    let concatStack firstStack secondStack  =
        match firstStack, secondStack with
        |StackNode(stack1),StackNode(stack2)->
        let ret = (List.concat [stack1;stack2])
        StackNode(ret)
    let pushStackNTimes (stack:Stack<'a>) (itemToAdd:'a) (n:int) =
        let stackToAdd=StackNode(List.init n (fun y->itemToAdd))
        stack |> concatStack stackToAdd
    let pop = function
        | StackNode([]) -> Unchecked.defaultof<'a>, StackNode([])
        | StackNode(hd::tl) -> hd, StackNode(tl)
    let popMany n (stack : Stack<'a>) =
         let noopReturn = [], stack
         if stack.length = 0 then noopReturn
         else
             match n with
             | x when x <= 0 || stack.length < n -> noopReturn
             | x -> 
                 let rec popManyTail n st acc =
                     match n with
                     | 0 -> acc   // exit recursion
                     | _ -> 
                         let hd, tl = List.head st, List.tail st
                         popManyTail (n - 1) tl (hd::fst acc, StackNode(tl)) //keep track of intermediate results
                 popManyTail n stack.asList ([],StackNode(FSharp.Collections.List<'a>.Empty)) // call the actual worker function