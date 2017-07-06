module IntModelCreationTests
    open NUnit.Framework
    open FsUnit
    open EasyamParsingEngine
    open Types
    open Utils
    open SAModel
    open BasicModel1

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
        let testText = [|
                          "BUSINESS STRUCTURE ABSTRACT TO-BE"
                        ; "  Customer"
                        ; "      //People who give us money"
                        ; "  Account"
                        ; "      //Where we keep track of things"
                        ; "META BEHAVIOR REALIZED TO-BE"
                        ; "    Go do things"
                        ; "    Do some more things"
                        ; ""
                        ; "BUSINESS STRUCTURE ABSTRACT TO-BE"
                        ; "  Customer"|]
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

    [<Test>]
    let ``INT MODEL CREATION: notes at top of one file don't attach to previous file``()=
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|
                          "Here's the initial master use case list based on our conversation last Monday"
                        ; "BEHAVIOR"
                        ; "    Pick Shipment"
                        ; "    Ship Goods"
                        ; "    Review Daily Warehouse Activity"
                        ; "    MISC"
                        ; "    ALL"
                        |]
        let testText2 = [|
                          ""
                        ; "Here's the intial master domain model last also based on last Monday's conversation"
                        ; ""
                        ; ""
                        ; "STRUCTURE"
                        ; "    Shipment"
                        ; "    Order"
                        |]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 8
        newCompilerStatus.ModelItems.[5].Annotations |> should haveLength 0
    [<Test>]
    let ``INT MODEL CREATION: note starting with paren on new file isnt considered a new behavior item``()=
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "Here's the intial master domain model last also based on last Monday's conversation"
                        ; ""
                        ; ""
                        ; "STRUCTURE"
                        ; "    Shipment"
                        ; "    Order"
                        ; "    Bill Of Lading"
                        ; "    Inventory Item"
                        ; "    Employee"
                        ; "    Employee Type"
                        ; "    SKU"
                        ; "    Customer"
                        ; "    Invoice"
                        ; "    Invoice Line"
                        ; "    Invoice Line Item"
                        ; "    Vendor"
                        ; ""
                        ; ""
                        ; "    Shipment HASA Bill Of Lading"
                        ; "    Order HASA Shipment"
                        ; "    Shipment HASA Bill Of Lading"
                        ; "    Vendor HASA Shipment"
                        ; "    Vendor HASA Invoice"
                        ; "    Customer HASA Order"
                        ; "    Vendor HASA Order"
                        ; "    Customer HASA Invoice"
                        ; "    Invoice HASA Invoice Line"
                        ; "    Invoice HASA Invoice Line Item"
                        ; "    SKU HASA Inventory Item"
                        ; "    Invoice Line Item HASA SKU"
                        |]
        let testText2 = [|
                          ""
                        ; "From the initial talk, the master domain model items used by the master user stories"
                        ; "Note the only requirement at the _BIZ_ABST_ level is to include or exclude items based on how often they show up in conversation"
                        ; "(It's not a data model)"
                        ; ""
                        ; ""
                        ; "BEHAVIOR"
                        ; "    Order Goods USES Order, Vendor, Invoice, Shipment"
                        ; "    Receive Order USES Order, Customer, Invoice, Shipment"
                        ; "    Receive Shipment USES Shipment, Order, Bill of Lading, Inventory Item, Employee, SKU, Vendor, Invoice, Invoice Line, Invoice Line Item"
                        ; "    Reject Shipment USES"
                        ; "        Order"
                        ; "        Shipment"
                        |]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        getMasterUserStories newCompilerStatus.ModelItems |> should haveLength 4

    [<Test>]
    let ``INT MODEL CREATION: multiples on new line after a join are counted right``()=
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "Here's the initial master use case list based on our conversation last Monday"
                        ; ""
                        ; ""
                        ; "BEHAVIOR"
                        ; "    Order Goods"
                        ; "    Receive Order"
                        ; "    Receive Shipment"
                        ; "    Reject Shipment"
                        ; "    Reject Part Of Shipment"
                        ; "    Reconcile BOL"
                        ; "    Put Away New Shipment"
                        ; "    Conduct Spot Inventory"
                        ; "    Conduct Regular Inventory"
                        ; "    Recieve Order"
                        ; "    Create Shipment"
                        ; "    Pick Shipment"
                        ; "    Ship Goods"
                        ; "    Review Daily Warehouse Activity"
                        ; "    MISC"
                        ; "    ALL"
                        |]
        let testText2 = [|
                          ""
                        ;  ""
                        ;  ""
                        ; "From our first talk, here's how the supps map to the MUS    "
                        ;  ""
                        ;  ""
                        ; "SUPPLEMENTALS"
                        ; "    Always respond in a way that's easy for the user to understand"
                        ; "        AFFECTS"
                        ; "            ALL"
                        ; "    Never make the user wait"
                        ; "        AFFECTS"
                        ; "            ALL"
                        ; "    Never confuse the user"
                        ; "        AFFECTS"
                        ; "            ALL"
                        ; "    Always keep track of the path users take to arrive on-site"
                        ; "        AFFECTS"
                        ; "            Order Goods, MISC"
                        ; "    Continuously inform the team about everything the users do"
                        ; "        AFFECTS"
                        ; "            ALL"
                        ; "    Make it as easy as possible for the user to provide negative feedback"
                        ; "        AFFECTS"
                        ; "            Reject Shipment, Reject Part Of Shipment"
                        ; "    Always do the best we can to cheer the user up during any interaction"
                        ; "        AFFECTS"
                        ; "            ALL"
                        ; "    "
                        ; "    "
                        ; "    "
                        |]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        getNotes (getMasterSupplementals newCompilerStatus.ModelItems).[3] |> should haveLength 0
        getNotes (getMasterSupplementals newCompilerStatus.ModelItems).[5] |> should haveLength 0

    [<Test>]
    let ``INT MODEL CREATION: dupe attributes are ignored``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "Here's the intial master domain model last also based on last Monday's conversation"
                        ; ""
                        ; ""
                        ; "STRUCTURE"
                        ; "    Shipment"
                        ; "    Order"
                        ; "    Bill Of Lading"
                        ; ""
                        ; ""
                        ; "    Shipment CONTAINS Trucking Company"
                        ; "    Shipment CONTAINS Driver"
                        ; "    Shipment CONTAINS Trailer"
                        ; "    Shipment CONTAINS Weight"
                        ; "    Shipment CONTAINS Trucking Company"
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 4
        newCompilerStatus.ModelItems.[1].Attributes |> should haveLength 4

    [<Test>]
    let ``INT MODEL CREATION: dupe relations are ignored``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "Here's the intial master domain model last also based on last Monday's conversation"
                        ; ""
                        ; ""
                        ; "STRUCTURE"
                        ; "    Shipment"
                        ; "    Order"
                        ; "    Bill Of Lading"
                        ; "    Inventory Item"
                        ; "    Employee"
                        ; "    Employee Type"
                        ; "    SKU"
                        ; "    Customer"
                        ; "    Invoice"
                        ; "    Invoice Line"
                        ; "    Invoice Line Item"
                        ; "    Vendor"
                        ; ""
                        ; ""
                        ; "    Shipment HASA Bill Of Lading"
                        ; "    Order HASA Shipment"
                        ; "    Shipment HASA Bill Of Lading"
                        ; "    Vendor HASA Shipment"
                        ; "    Vendor HASA Invoice"
                        ; "    Customer HASA Order"
                        ; "    Vendor HASA Order"
                        ; "    Customer HASA Invoice"
                        ; "    Invoice HASA Invoice Line"
                        ; "    Invoice HASA Invoice Line Item"
                        ; "    SKU HASA Inventory Item"
                        ; "    Invoice Line Item HASA SKU"
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 13
        newCompilerStatus.ModelItems.[1].Relations |> should haveLength 3

    [<Test>]
    let ``INT MODEL CREATION: Question that moves indent outward re-adjusts the target for follow-on items``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; ""
                        ; "BEHAVIOR"
                        ; "    Order Goods"
                        ; "    // Order Goods was specifically requested by the guy in the plaid sweater"
                        ; "    // It seems to be very important for some reason"
                        ; "        WHEN The inventory count for an item falls below its re-order point"
                        ; "        ASA Warehouse Worker"
                        ; "        INEEDTO Place an order with our suppliers to replenish needed items"
                        ; "        SOTHAT we don't run out of things and are able to fulfill our orders from our customers"
                        ; "    Receive Order"
                        ; "        WHEN I'm out of something I realize I need"
                        ; "        ASA Customer"
                        ; "        INEEDTO Place an order for new stuff"
                        ; "        SOTHAT I can be happy owning things I think I need"
                        ; "    Conduct Spot Inventory //I love inventory"
                        ; "        WHEN "
                        ; "            A truck arrives at the gate // could be any kind of truck"
                        ; "            An accountant calls on the phone"
                        ; "            There's a break-in at the warehouse"
                        ; "        ASA // the actor list isn't complete yet"
                        ; "            Warehouse Worker"
                        ; "            Warehouse Supervisor"
                        ; "            Nightshift Guard Supervisor"
                        ; "        INEEDTO "
                        ; "            Conduct a formal spot inventory by hand using the older books"
                        ; "        SOTHAT"
                        ; "            The insurance company is notified in case of loss"
                        ; "            Incoming shipments will adequately update the running inventory"
                        ; "                Q: Is there such a thing as a 'walking inventory'"
                        ; "        Q: Do we differentiate between goods and services?"
                        ; "    Conduct Regular Inventory"
                        ; "        WHEN "
                        ; "            The first of the month rolls around"
                        ; "        ASA"
                        ; "            Warehouse Supervisor"
                        ; "        INEEDTO "
                        ; "            Conduct a formal inventory using an audited procedure"
                        ; "        SOTHAT"
                        ; "            Our accounting system stays coherent"
                        ; "            We know what to order"
                        ; "    Receive Order "
                        ; "        WHEN "
                        ; "            One of our customers decides they want something from us"
                        ; "        ASA"
                        ; "            Warehouse Worker"
                        ; "        INEEDTO "
                        ; "            Receive an order over the telephone"
                        ; "        SOTHAT"
                        ; "            Customers can get what they want"
                        ; "            Customers will like us and want to call us again"
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 5
        newCompilerStatus.ModelItems.[3].Attributes|> should haveLength 9

    [<Test>]
    let ``INT MODEL CREATION: multiple indented annotations under a new item``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "Here's the intial master domain model last also based on last Monday's conversation"
                        ; ""
                        ; ""
                        ; "STRUCTURE"
                        ; "    Shipment"
                        ; "        QUESTIONS: "
                        ; "            Does this involve real ships?"
                        ; "            Do we get free mints?"
                        ; "            Are the ships full of mints?"
                        ; "    Order"
                        ; "    Bill Of Lading"
                        ; "    Inventory Item"
                        ; "    Employee"
                        ; "    Employee Type"
                        ; "    SKU"
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines beginningCompilerStatus
        newCompilerStatus.ModelItems |> should haveLength 8
        newCompilerStatus.ModelItems.[1].Annotations|> should haveLength 3
    [<Test>]
    let ``INT MODEL CREATION: joins after multiple annotations pops up to right level``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "Here's the intial master domain model last also based on last Monday's conversation"
                        ; ""
                        ; ""
                        ; "STRUCTURE"
                        ; "    Shipment"
                        ; "    Order"
                        ; "    Bill Of Lading"
                        ; "    Inventory Item"
                        ; "    Employee"
                        ; "    Employee Type"
                        ; "    SKU"
                        ; "    Customer"
                        ; "    Invoice"
                        ; "    Invoice Line"
                        ; "    Invoice Line Item"
                        ; "    Vendor"
                        ; "        TODO:"
                        ; "            Meet one of the vendors"
                        ; "            Sit in on a vendor call"
                        ; ""
                        ; ""
                        ; "    Shipment HASA Bill Of Lading"
                        ; "    Order HASA Shipment"
                        ; "    Shipment HASA Bill Of Lading"
                        ; "    Vendor HASA Shipment"
                        ; "    Vendor HASA Invoice"
                        ; "    Customer HASA Order"
                        ; "    Vendor HASA Order"
                        ; "    Customer HASA Invoice"
                        ; "    Invoice HASA Invoice Line"
                        ; "    Invoice HASA Invoice Line Item"
                        ; "    SKU HASA Inventory Item"
                        ; "    Invoice Line Item HASA SKU"
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 13
        newCompilerStatus.ModelItems.[12].Annotations|> should haveLength 2
        ( fst newCompilerStatus.ModelItems.[12].Annotations.[0]) |> should equal ANNOTATION_TOKEN_TYPE.ToDo
        ( fst newCompilerStatus.ModelItems.[12].Annotations.[1]) |> should equal ANNOTATION_TOKEN_TYPE.ToDo

    [<Test>]
    let ``INT MODEL CREATION: Simple shortcut works``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "This is a file"
                        ; ""
                        ; "PROGRAM BACKLOG"
                        ; "  Make peace with the universe"
                        ; "  Be one with nature"
                        ; ""
                        ; ""
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 3
        newCompilerStatus.ModelItems.[1].Location.Bucket |> should equal Buckets.Behavior
        newCompilerStatus.ModelItems.[1].Location.AbstractionLevel |> should equal AbstractionLevels.Realized
        newCompilerStatus.ModelItems.[1].Location.Genre |> should equal Genres.Business
        newCompilerStatus.ModelItems.[1].Location.TemporalIndicator |> should equal TemporalIndicators.ToBe

    [<Test>]
    let ``INT MODEL CREATION: Shortcut in the middle of a file works``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "This is a file"
                        ; ""
                        ; "MASTER DOMAIN MODEL"
                        ; "  universe"
                        ; "  nature"
                        ; ""
                        ; ""
                        ; ""
                        ; "PROJECT SUPPLEMENTAL MODEL"
                        ; "  Spread happiness"
                        ; "  Admire things"
                        ; ""
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 5
        newCompilerStatus.ModelItems.[1].Location.Bucket |> should equal Buckets.Structure
        newCompilerStatus.ModelItems.[1].Location.AbstractionLevel |> should equal AbstractionLevels.Abstract
        newCompilerStatus.ModelItems.[1].Location.Genre |> should equal Genres.Business
        newCompilerStatus.ModelItems.[1].Location.TemporalIndicator |> should equal TemporalIndicators.ToBe
        newCompilerStatus.ModelItems.[2].Location.Bucket |> should equal Buckets.Structure
        newCompilerStatus.ModelItems.[2].Location.AbstractionLevel |> should equal AbstractionLevels.Abstract
        newCompilerStatus.ModelItems.[2].Location.Genre |> should equal Genres.Business
        newCompilerStatus.ModelItems.[2].Location.TemporalIndicator |> should equal TemporalIndicators.ToBe
        newCompilerStatus.ModelItems.[3].Location.Bucket |> should equal Buckets.Supplemental
        newCompilerStatus.ModelItems.[3].Location.AbstractionLevel |> should equal AbstractionLevels.Realized
        newCompilerStatus.ModelItems.[3].Location.Genre |> should equal Genres.Business
        newCompilerStatus.ModelItems.[3].Location.TemporalIndicator |> should equal TemporalIndicators.ToBe
    [<Test>]
    let ``INT MODEL CREATION: Shortcut with items in same line works``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "MASTER DOMAIN MODEL: Universe, Nature"
                        ; ""
                        ; "PROJECT SUPPLEMENTAL MODEL: Spread happiness, Admire things"
                        ; ""
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 5
        newCompilerStatus.ModelItems.[1].Location.Bucket |> should equal Buckets.Structure
        newCompilerStatus.ModelItems.[1].Location.AbstractionLevel |> should equal AbstractionLevels.Abstract
        newCompilerStatus.ModelItems.[1].Location.Genre |> should equal Genres.Business
        newCompilerStatus.ModelItems.[1].Location.TemporalIndicator |> should equal TemporalIndicators.ToBe
        newCompilerStatus.ModelItems.[2].Location.Bucket |> should equal Buckets.Structure
        newCompilerStatus.ModelItems.[2].Location.AbstractionLevel |> should equal AbstractionLevels.Abstract
        newCompilerStatus.ModelItems.[2].Location.Genre |> should equal Genres.Business
        newCompilerStatus.ModelItems.[2].Location.TemporalIndicator |> should equal TemporalIndicators.ToBe
        newCompilerStatus.ModelItems.[3].Location.Bucket |> should equal Buckets.Supplemental
        newCompilerStatus.ModelItems.[3].Location.AbstractionLevel |> should equal AbstractionLevels.Realized
        newCompilerStatus.ModelItems.[3].Location.Genre |> should equal Genres.Business
        newCompilerStatus.ModelItems.[3].Location.TemporalIndicator |> should equal TemporalIndicators.ToBe
    [<Test>]
    let ``INT MODEL CREATION: Two shortcuts with item list all on same line``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "MASTER DOMAIN MODEL: Universe, Nature PROJECT SUPPLEMENTAL MODEL: Spread happiness, Admire things"
                        ; ""
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 5
        newCompilerStatus.ModelItems.[1].Location.Bucket |> should equal Buckets.Structure
        newCompilerStatus.ModelItems.[1].Location.AbstractionLevel |> should equal AbstractionLevels.Abstract
        newCompilerStatus.ModelItems.[1].Location.Genre |> should equal Genres.Business
        newCompilerStatus.ModelItems.[1].Location.TemporalIndicator |> should equal TemporalIndicators.ToBe
        newCompilerStatus.ModelItems.[2].Location.Bucket |> should equal Buckets.Structure
        newCompilerStatus.ModelItems.[2].Location.AbstractionLevel |> should equal AbstractionLevels.Abstract
        newCompilerStatus.ModelItems.[2].Location.Genre |> should equal Genres.Business
        newCompilerStatus.ModelItems.[2].Location.TemporalIndicator |> should equal TemporalIndicators.ToBe
        newCompilerStatus.ModelItems.[3].Location.Bucket |> should equal Buckets.Supplemental
        newCompilerStatus.ModelItems.[3].Location.AbstractionLevel |> should equal AbstractionLevels.Realized
        newCompilerStatus.ModelItems.[3].Location.Genre |> should equal Genres.Business
        newCompilerStatus.ModelItems.[3].Location.TemporalIndicator |> should equal TemporalIndicators.ToBe

    // NOTE: The following kind of compound statement not supported (haven't figured syntax out yet)
    // "MASTER DOMAIN MODEL: Universe CONTAINS Stars, Nature CONTAINS Animals"

    [<Test>]
    let ``INT MODEL CREATION: Namespace simple``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "STRUCTURE"
                        ; "  Dog"
                        ; "  Cat"
                        ; "NAMESPACE Zoo"
                        ; "  Elephant"
                        ; "  Giraffe"
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 5
        newCompilerStatus.ModelItems.[2].Location.Namespace |> should equal ""
        newCompilerStatus.ModelItems.[3].Location.Namespace |> should equal "Zoo"
        newCompilerStatus.ModelItems.[3].Location.Bucket |> should equal Buckets.Structure
        newCompilerStatus.ModelItems.[3].Location.Genre |> should equal Genres.Business
        newCompilerStatus.ModelItems.[3].Location.AbstractionLevel |> should equal AbstractionLevels.Abstract
        newCompilerStatus.ModelItems.[3].Location.TemporalIndicator |> should equal TemporalIndicators.ToBe
    [<Test>]
    let ``INT MODEL CREATION: Namespace two in same file work``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          "NAMESPACE Tiger Team"
                        ; "STRUCTURE"
                        ; "  Dog"
                        ; "  Cat"
                        ; "NAMESPACE Zookeepers"
                        ; "  Elephant"
                        ; "  Giraffe"
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 5
        newCompilerStatus.ModelItems.[2].Location.Namespace |> should equal "Tiger Team"
        newCompilerStatus.ModelItems.[3].Location.Namespace |> should equal "Zookeepers"
    [<Test>]
    let ``INT MODEL CREATION: Namespace embedded in complex line works``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "MASTER BACKLOG Check Balance, Reconcile Account, Get Cash NAMESPACE Tiger Team PROJECT BACKLOG Do Stuff, Do Other Things"
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 6
        newCompilerStatus.ModelItems.[0].Location.Namespace |> should equal ""
        newCompilerStatus.ModelItems.[4].Location.Namespace |> should equal "Tiger Team"
    [<Test>]
    let ``INT MODEL CREATION: Team backlog basic``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "MASTER BACKLOG Check Balance, Reconcile Account, Get Cash"
                        ; "NAMESPACE Tiger Team"
                        ; "PROJECT BACKLOG Check Balance using ATM PARENT Check Balance"
                        ; "  Reconcilce Account On Paper PARENT Reconcile Account"
                        ; "  Get Cash From ATM PARENT Get Cash"
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 7
        newCompilerStatus.ModelItems.[0].Location.Namespace |> should equal ""
        newCompilerStatus.ModelItems.[6].Location.Namespace |> should equal "Tiger Team"

    [<Test>]
    let ``INT MODEL CREATION: simplest useful model``() =
        let listToProcess = basicModel1
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        saveMasterQuestionList (System.AppDomain.CurrentDomain.BaseDirectory) "mql.html" newCompilerStatus
        saveModelGuide (System.AppDomain.CurrentDomain.BaseDirectory + "master-cards.html") newCompilerStatus
        saveCanonicalModel System.AppDomain.CurrentDomain.BaseDirectory newCompilerStatus
        true |> should equal true 



