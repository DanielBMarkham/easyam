module IntModelCreationTests
    open NUnit.Framework
    open FsUnit
    open EasyamParsingEngine
    open Types
    open SAModel

    // Funky code. I need some way to pretend I have a real OS incoming file I'm processing
    let getFakeFileInfo() = 
        let tempColl = (new System.CodeDom.Compiler.TempFileCollection(System.AppDomain.CurrentDomain.BaseDirectory, false))
        tempColl.AddExtension("bsx") |> ignore
        let rndPrefix = System.IO.Path.GetRandomFileName()
        let tempFileName = System.IO.Path.Combine(System.AppDomain.CurrentDomain.BaseDirectory, (rndPrefix + "_tester.bsx"))
        tempColl.AddFile(tempFileName,false)
        let fs1=System.IO.File.OpenWrite(tempFileName)
        let sw1=new System.IO.StreamWriter(fs1)
        sw1.WriteLine("test")
        sw1.Close()
        fs1.Close()
        let ret=new System.IO.FileInfo(tempFileName)
        tempColl.Delete()
        ret
    // memoize one to reuse
    let dummyFileInfo = getFakeFileInfo()

    let setupCompilationScenario fileNumber incomingRawLineCount incomingLineCountWithEmptyLinesDeletedCount rawLineArray =
        initialProcessingOfIncomingFileLines fileNumber dummyFileInfo incomingRawLineCount incomingLineCountWithEmptyLinesDeletedCount rawLineArray


    [<Test>]
    let ``INT MODEL CREATION: Mentioning same item later in same file does not add new item``() =
        let testText = [|"BUSINESS STRUCTURE ABSTRACT TO-BE"; "  Customer"; "      //People who give us money"; "  Account"; "      //Where we keep track of things"; "META BEHAVIOR REALIZED TO-BE"; "    Go do things"; "        Do some more things"; ""; "BUSINESS STRUCTURE ABSTRACT TO-BE"; "  Customer"|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 5
    [<Test>]
    let ``INT MODEL CREATION: Mentioning same item later in same file allows annotations``() =
        let testText = [|"BUSINESS STRUCTURE ABSTRACT TO-BE"; "  Customer"; "      //People who give us money"; "  Account"; "      //Where we keep track of things"; "META BEHAVIOR REALIZED TO-BE"; "    Go do things"; "        Do some more things"; ""; "BUSINESS STRUCTURE ABSTRACT TO-BE"; "  Customer"; "        //Customers are nice. I like them."|]
        let compilationScenario = setupCompilationScenario 0 0 0 testText
        let newCompilerStatus=makeRawModel compilationScenario beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 5
        newCompilerStatus.ModelItems.[1].Annotations |> should haveLength 2
    [<Test>]
    let ``INT MODEL CREATION: Mentioning same item later in a different file allows annotations``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|"Here's some text"; "BUSINESS BEHAVIOR ABSTRACT TO-BE"; "  Reconcile account"; "      Pull the plug"|]
        let testText2 = [|"A new beginning"; "SYSTEM SUPPLEMENTAL REALIZED WAS"; "  Everybody gets a sticker"; "      The stickers blow up"; "BUSINESS BEHAVIOR ABSTRACT TO-BE"; "    Reconcile account"; "        Q:What's a sprog?"|]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 5
        let tok, valu = newCompilerStatus.ModelItems.[1].Annotations.[0]
        valu="What's a sprog?" |> should equal true 

    [<Test>]
    let ``INT MODEL CREATION: Simple supplemental can affect a simple behavior``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|"BEHAVIOR"; "    Reconcile Account"|]
        let testText2 = [|"SUPPLEMENTAL"; "    Users don't like to do math AFFECTS Reconcile Account"|]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 3
        newCompilerStatus.ModelItems.[1].Relations |> should haveLength 1
        newCompilerStatus.ModelItems.[2].Relations |> should haveLength 1
        newCompilerStatus.ModelItems.[1].Relations.[0].ModelJoinType |> should equal ModelJoin.AffectedBy
        newCompilerStatus.ModelItems.[2].Relations.[0].ModelJoinType |> should equal ModelJoin.Affects
        
    [<Test>]
    let ``INT MODEL CREATION: multi-item join on same line works``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|"STRUCTURE"; "    Bicycle"; "    Auto"|]
        let testText2 = [|"BEHAVIOR"; "    Visit grandpa USES Bicycle, Auto"|]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 4
        newCompilerStatus.ModelItems.[1].Relations |> should haveLength 1
        newCompilerStatus.ModelItems.[2].Relations |> should haveLength 1
        newCompilerStatus.ModelItems.[3].Relations |> should haveLength 2
        newCompilerStatus.ModelItems.[1].Relations.[0].ModelJoinType |> should equal ModelJoin.UsedBy
        newCompilerStatus.ModelItems.[2].Relations.[0].ModelJoinType |> should equal ModelJoin.UsedBy
        newCompilerStatus.ModelItems.[3].Relations.[0].ModelJoinType |> should equal ModelJoin.Uses
    [<Test>]
    let ``INT MODEL CREATION: multi-item join same line with structure``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let fileInfo3 = getFakeFileInfo()
        let testText1 = [|"STRUCTURE"; "    Bicycle"; "    Auto"; "    Train"; "    Wheels"; "    Seats"; "    Roof"|]
        let testText2 = [|"BEHAVIOR"; "    Fly through the jungle USES Auto, Train"|]
        let testText3 = [|"STRUCTURE"; "    Bicycle HASA Wheels, Seats"; "    Auto HASA Wheels, Seats, Roof"; "    Train HASA Wheels, Seats, Roof"|]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2);(fileInfo3,testText3)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 8
        newCompilerStatus.ModelItems.[1].Relations |> should haveLength 2
        newCompilerStatus.ModelItems.[2].Relations |> should haveLength 4
        newCompilerStatus.ModelItems.[3].Relations |> should haveLength 4
        newCompilerStatus.ModelItems.[4].Relations |> should haveLength 3
        newCompilerStatus.ModelItems.[5].Relations |> should haveLength 3
        newCompilerStatus.ModelItems.[6].Relations |> should haveLength 2
        newCompilerStatus.ModelItems.[7].Relations |> should haveLength 2

    [<Test>]
    let ``INT MODEL CREATION: open commas on joiners are ignored``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|"STRUCTURE"; "    Bicycle"; "    Auto"|]
        let testText2 = [|"BEHAVIOR"; "    Visit grandpa USES ,Bicycle, , Auto"|]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 4
        newCompilerStatus.ModelItems.[1].Relations |> should haveLength 1
        newCompilerStatus.ModelItems.[2].Relations |> should haveLength 1
        newCompilerStatus.ModelItems.[3].Relations |> should haveLength 2
        newCompilerStatus.ModelItems.[1].Relations.[0].ModelJoinType |> should equal ModelJoin.UsedBy
        newCompilerStatus.ModelItems.[2].Relations.[0].ModelJoinType |> should equal ModelJoin.UsedBy
        newCompilerStatus.ModelItems.[3].Relations.[0].ModelJoinType |> should equal ModelJoin.Uses
    [<Test>]
    let ``INT MODEL CREATION: join using reverse works``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|"SUPPLEMENTAL"; "    Be good"; "    Be fast"; "    Be cheap"|]
        let testText2 = [|"BEHAVIOR"; "    Solve world hunger AFFECTEDBY Be good, Be cheap"|]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 5
        newCompilerStatus.ModelItems.[1].Relations |> should haveLength 1
        newCompilerStatus.ModelItems.[2].Relations |> should haveLength 0
        newCompilerStatus.ModelItems.[3].Relations |> should haveLength 1
        newCompilerStatus.ModelItems.[4].Relations.[0].ModelJoinType |> should equal ModelJoin.AffectedBy
        newCompilerStatus.ModelItems.[4].Relations.[1].ModelJoinType |> should equal ModelJoin.AffectedBy
        newCompilerStatus.ModelItems.[3].Relations.[0].ModelJoinType |> should equal ModelJoin.Affects
    [<Test>]
    let ``INT MODEL CREATION: join over multiple lines``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|"SUPPLEMENTAL"; "    Be good"; "    Be fast"; "    Be cheap"|]
        let testText2 = [|"BEHAVIOR"; "    Solve world hunger AFFECTEDBY "; "        Be fast"; "        Be cheap"|]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 5
        newCompilerStatus.ModelItems.[1].Relations |> should haveLength 0
        newCompilerStatus.ModelItems.[2].Relations |> should haveLength 1
        newCompilerStatus.ModelItems.[3].Relations |> should haveLength 1
        newCompilerStatus.ModelItems.[4].Relations |> should haveLength 2
        newCompilerStatus.ModelItems.[4].Relations.[0].ModelJoinType |> should equal ModelJoin.AffectedBy
        newCompilerStatus.ModelItems.[4].Relations.[1].ModelJoinType |> should equal ModelJoin.AffectedBy
        newCompilerStatus.ModelItems.[3].Relations.[0].ModelJoinType |> should equal ModelJoin.Affects
    [<Test>]
    let ``INT MODEL CREATION: multiple item and line joins followed by comments work``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|"SUPPLEMENTAL"; "    Be good"; "    Be fast"; "    Be cheap"|]
        let testText2 = [|"BEHAVIOR"; "    Solve world hunger AFFECTEDBY "; "        Be fast"; "        Be cheap"; "            (By cheap we don't mean shabby)"|]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 5
        newCompilerStatus.ModelItems.[1].Relations |> should haveLength 0
        newCompilerStatus.ModelItems.[2].Relations |> should haveLength 1
        newCompilerStatus.ModelItems.[3].Relations |> should haveLength 1
        newCompilerStatus.ModelItems.[4].Relations |> should haveLength 2
        newCompilerStatus.ModelItems.[4].Relations.[0].ModelJoinType |> should equal ModelJoin.AffectedBy
        newCompilerStatus.ModelItems.[4].Relations.[1].ModelJoinType |> should equal ModelJoin.AffectedBy
        newCompilerStatus.ModelItems.[3].Relations.[0].ModelJoinType |> should equal ModelJoin.Affects
        newCompilerStatus.ModelItems.[3].Annotations |> should haveLength 1

    [<Test>]
    let ``INT MODEL CREATION: Basic Model Attributes``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|"BEHAVIOR"; "    Receive Shipment"; "    Reconcile BOL"; "    Conduct spot inventory"|]
        let testText2 = [|"BEHAVIOR"; "    Receive Shipment"; "        WHEN The truck arrives at the gate"; "        ASA Shipping Clerk"; "        SOTHAT We know how many shipments have arrived"|]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 4
        newCompilerStatus.ModelItems.[1].Attributes |> should haveLength 3
    [<Test>]
    let ``INT MODEL CREATION: Basic mult same line attributes``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|"BEHAVIOR"; "    Receive Shipment"; "    Reconcile BOL"; "    Conduct spot inventory"|]
        let testText2 = [|"BEHAVIOR"; "    Receive Shipment"; "        WHEN The truck arrives at the gate"; "        ASA Shipping Clerk, Guard, Warehouse worker"; "        INEEDTO Receive the incoming new shipment"; "        SOTHAT We know how many shipments have arrived"|]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 4
        newCompilerStatus.ModelItems |> should haveLength 4
        newCompilerStatus.ModelItems.[1].Attributes |> should haveLength 6
        newCompilerStatus.ModelItems.[1].Attributes.[2].Description |> should equal "Guard"
    [<Test>]
    let ``INT MODEL CREATION: mult att split up over different lines``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|"BEHAVIOR"; "    Receive Shipment"; "    Reconcile BOL"; "    Conduct spot inventory"|]
        let testText2 = [|"BEHAVIOR"
                        ; "    Conduct spot inventory"
                        ; "        WHEN "
                        ; "            The truck arrives at the gate"
                        ; "            An accountant calls on the phone"
                        ; "            There's a break-in at the warehouse"
                        ; "        ASA "
                        ; "            Warehouse worker"
                        ; "            Warehouse supervisor"
                        ; "            Nightshift guard supervisor"
                        ; "        INEEDTO "
                        ; "            Conduct a formal spot inventory by hand using the older books"
                        ; "        SOTHAT"
                        ; "            The insurance company is notified in case of loss"
                        ; "            Incoming shipments will adequately update the running inventory"
                        |]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 4
        newCompilerStatus.ModelItems |> should haveLength 4
        newCompilerStatus.ModelItems.[3].Attributes |> should haveLength 9
        newCompilerStatus.ModelItems.[3].Attributes.[3].Description |> should equal "Warehouse worker"
        newCompilerStatus.ModelItems.[3].Attributes.[3].AttributeType |> should equal ModelAttributeTypes.Actor

    [<Test>]
    let ``INT MODEL CREATION: sameAs-line comment on attribute token goes back to parent``() =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|"BEHAVIOR"; "    Receive Shipment"; "    Reconcile BOL"; "    Conduct spot inventory"|]
        let testText2 = [|"BEHAVIOR"
                        ; "    Conduct spot inventory //I love inventory"
                        ; "        WHEN "
                        ; "            The truck arrives at the gate // could be any kind of truck"
                        ; "            An accountant calls on the phone"
                        ; "            There's a break-in at the warehouse"
                        ; "        ASA // the actor list isn't complete yet"
                        ; "            Warehouse worker"
                        ; "            Warehouse supervisor"
                        ; "            Nightshift guard supervisor"
                        ; "        INEEDTO "
                        ; "            Conduct a formal spot inventory by hand using the older books"
                        ; "        SOTHAT"
                        ; "            The insurance company is notified in case of loss"
                        ; "            Incoming shipments will adequately update the running inventory"
                        |]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 4
        newCompilerStatus.ModelItems.[3].Annotations |> should haveLength 2
        newCompilerStatus.ModelItems.[3].Attributes |> should haveLength 9
        newCompilerStatus.ModelItems.[3].Attributes.[0].Annotations |> should haveLength 1


