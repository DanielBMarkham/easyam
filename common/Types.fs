module Types
    open System.Text.RegularExpressions

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
        member x.CovertIntoOSSafeFileName =
            let getRidOfSystemInvalidCharacters =  x.ReplaceAny (System.IO.Path.GetInvalidFileNameChars()) '_'
            let addHyphensAndToLower = x.ToLower().Replace(' ', '-')
            addHyphensAndToLower
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

    type System.Collections.Generic.Dictionary<'A, 'B> with
        member x.stringValueOrEmptyForKey n = 
            if x.ContainsKey n then x.Item(n).ToString() else ""
        member x.TryFind n = 
            let x,(y:'B) = x.TryGetValue n
            if x then Some y else None
            
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
                            else defaultConfig.swapInNewValue (parmValue.Value, None)
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
                                defaultConfig.swapInNewValue (parmValue.Value, None)
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

    type EasyAMProgramConfig =
        {
            configBase:ConfigBase
            sourceDirectory:ConfigEntry<DirectoryParm>
            destinationDirectory:ConfigEntry<DirectoryParm>
        }
        member this.printThis() =
            printfn "EasyAMConfig Parameters Provided"
            this.configBase.verbose.printVal
            this.sourceDirectory.printVal
            printfn "sourceDirectoryExists: %b" (snd this.sourceDirectory.parameterValue).IsSome
            this.destinationDirectory.printVal
            printfn "destinationDirectoryExists: %b" (snd this.destinationDirectory.parameterValue).IsSome


    let directoryExists (dir:ConfigEntry<DirectoryParm>) = (snd (dir.parameterValue)).IsSome
    let fileExists (dir:ConfigEntry<FileParm>) = (snd (dir.parameterValue)).IsSome
    type SVGSetup =
        {
            FontSize:int
            FontName:string
            TextMargin:int
            EntityBorderColor:string
            EntityBorderWidth:string
            EntityFillColor:string
            EntityFillOpacity:string
        }
    let defaultSVGSetup =
        {
            FontSize=12
            FontName="Verdana"
            TextMargin=6
            EntityBorderColor="#ff0000"
            EntityBorderWidth="1"
            EntityFillColor="#dddddd"
            EntityFillOpacity="80%"
        }
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