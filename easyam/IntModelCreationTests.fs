﻿module IntModelCreationTests
    open NUnit.Framework
    open FsUnit
    open EasyamParsingEngine
    open Types
    open Utils
    open Persist
    open SAModel
    open Lenses
    open BasicModel1


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
        let tok, valu = newCompilerStatus.ModelItems.[1].Annotations.[0].AnnotationType,newCompilerStatus.ModelItems.[1].Annotations.[0].AnnotationText
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
        let testText2 = [|"BEHAVIOR"; "    Solve world hunger AFFECTEDBY "; "        Be fast"; "        Be cheap"; "            //(By cheap we don't mean shabby)"|]
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
        let mus=getMasterUserStories newCompilerStatus.ModelItems 
        mus |> should haveLength 4

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
        newCompilerStatus.ModelItems.[12].Annotations.[0].AnnotationType |> should equal AnnotationTokenType.ToDo
        newCompilerStatus.ModelItems.[12].Annotations.[1].AnnotationType |> should equal AnnotationTokenType.ToDo

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

    //
    // Begin testing model round-tripping, last bit of int model work to test
    [<Test>]
    let ``ROUNDTRIP: Mult forward lookups work``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""

                        ; "//                        MASTER DOMAIN MODEL - ENTITY                         "
                        ; "//                                  CUSTOMER                                   "
                        ; "//                   Model Generation: 07/12/2017 07:27:16                     "
                        ; "//                                                                             "
                        ; ""
                        ; "BUSINESS STRUCTURE ABSTRACT TO-BE: Customer"
                        ; "    CONTAINS: "
                        ; "      Customer Number"
                        ; "      Address"
                        ; ""
                        ; ""
                        ; "    HASA: "
                        ; "      Order"
                        ; "      Invoice"                        ; ""
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 4
        newCompilerStatus.ModelItems.[1].Description |> should equal "Customer"
        newCompilerStatus.ModelItems.[1].Attributes |> should haveLength 2
        newCompilerStatus.ModelItems.[2].Description |> should equal "Order"
        newCompilerStatus.ModelItems.[3].Description |> should equal "Invoice"

    [<Test>]
    let ``ROUNDTRIP: Sample complete dumped MUS``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "//                             MASTER USER STORY                               "
                        ; "//                           CONDUCT SPOT INVENTORY                            "
                        ; "//                   Model Generation: 07/12/2017 13:03:13                     "
                        ; "//                                                                             "
                        ; ""
                        ; "MASTER USER STORY: Conduct Spot Inventory"
                        ; "    WHEN: "
                        ; "      A truck arrives at the gate"
                        ; "        NOTES: "
                        ; "           could be any kind of truck"
                        ; "      An accountant calls on the phone"
                        ; "      There's a break-in at the warehouse"
                        ; "    ASA: "
                        ; "      Warehouse Worker"
                        ; "      Warehouse Supervisor"
                        ; "      Nightshift Guard Supervisor"
                        ; "    INEEDTO: "
                        ; "      Conduct a formal spot inventory by hand using the older books"
                        ; "    SOTHAT: "
                        ; "      The insurance company is notified in case of loss"
                        ; "      Incoming shipments will adequately update the running inventory"
                        ; "        QUESTIONS: "
                        ; "          Is there such a thing as a 'walking inventory'"
                        ; ""
                        ; "    NOTES: "
                        ; "      I love inventory"
                        ; "      the actor list isn't complete yet"
                        ; "    QUESTIONS: "
                        ; "      Do we differentiate between goods and services?"
                        ; ""
                        ; "    AFFECTEDBY: "
                        ; "      Always respond in a way that's easy for the user to understand"
                        ; "      Never make the user wait"
                        ; "        QUESTIONS: Do they really mean never?"
                        ; "        NOTES: Never is a long time"
                        ; "      Never confuse the user"
                        ; "      Continuously inform the team about everything the users do"
                        ; "      Always do the best we can to cheer the user up during any interaction"
                        ; ""
                        ; "    USES: "
                        ; "      Inventory Item"
                        ; "      SKU"
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 9
        newCompilerStatus.ModelItems.[1].Description |> should equal "Conduct Spot Inventory"
        newCompilerStatus.ModelItems.[1].Attributes |> should haveLength 9
        newCompilerStatus.ModelItems.[1].Attributes.[8].Annotations |> should haveLength 1
        newCompilerStatus.ModelItems.[1].Attributes.[8].Annotations.[0].AnnotationType |> should equal AnnotationTokenType.Question
        newCompilerStatus.ModelItems.[1].Attributes.[8].Annotations.[0].AnnotationText |> should equal "Is there such a thing as a 'walking inventory'"



    [<Test>]
    let ``ROUNDTRIP: annotate in middle of def``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "BUSINESS STRUCTURE ABSTRACT TO-BE: Vendor"
                        ; "    CONTAINS: "
                        ; "      Vendor Number"
                        ; "      Address"
                        ; "      Last Order Date"
                        ; ""
                        ; "    TODO: "
                        ; "      Meet some vendors"
                        ; "      Listen to stories about vendors"
                        ; ""
                        ; "    HASA: "
                        ; "      Shipment"
                        ; "      Invoice"
                        ; "      Order"
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 5


     // Forgot to test some abbreviations
    [<Test>]
    let ``ABBREV: User Story Abbreviation 1``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "MASTER BACKLOG: Do things, Do other things"
                        ; ""
                        ; "BUSINESS STRUCTURE REALIZED AS-IS"
                        ; "  Car"
                        ; "  Wheel"
                        ; "  Window"
                        ; ""
                        ; "  US: Do more things, Do more things better"
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 8
        newCompilerStatus.ModelItems.[1].Location.AbstractionLevel |> should equal AbstractionLevels.Abstract
        newCompilerStatus.ModelItems.[1].Location.Bucket |> should equal Buckets.Behavior
        newCompilerStatus.ModelItems.[3].Location.Bucket |> should equal Buckets.Structure
        newCompilerStatus.ModelItems.[3].Location.TemporalIndicator |> should equal TemporalIndicators.AsIs
        newCompilerStatus.ModelItems.[6].Location.Bucket |> should equal Buckets.Behavior
        newCompilerStatus.ModelItems.[6].Location.TemporalIndicator |> should equal TemporalIndicators.AsIs
        newCompilerStatus.ModelItems.[6].Location.AbstractionLevel |> should equal AbstractionLevels.Realized

    [<Test>]
    let ``INT MODEL: Quick list works``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "MASTER DOMAIN MODEL"
                        ; ""
                        ; "  Car"
                        ; "  Wheel"
                        ; "  Invoice Line Item"
                        ; "  Window"
                        ; "  Invoice"
                        ; ""
                        ; "  US: Do more things, Do more things better"
                        |]
        let fileInfo2 = getFakeFileInfo()
        let testText2 = [|
                          ""
                        ; "BUSINESS STRUCTURE ABSTRACT TO-BE: Invoice"
                        ; "    CONTAINS: "
                        ; "      Invoice Number"
                        ; "      Invoice Date"
                        ; "      Invoice Total Amount"
                        ; ""
                        ; ""
                        ; "    HASA: "
                        ; "      Invoice Line"
                        ; "      Invoice Line Item"
                        ; ""
                        |]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2, testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 9
        newCompilerStatus.ModelItems.[5].Description |> should equal "Invoice"
        newCompilerStatus.ModelItems.[5].Relations |> should haveLength 2
        let firstRelation=newCompilerStatus.ModelItems|>Array.find(fun x->x.Id=newCompilerStatus.ModelItems.[5].Relations.[0].TargetId)
        let secondRelation=newCompilerStatus.ModelItems|>Array.find(fun x->x.Id=newCompilerStatus.ModelItems.[5].Relations.[1].TargetId)
        firstRelation.Description |> should equal "Invoice Line"
        secondRelation.Description |> should equal "Invoice Line Item"


    [<Test>]
    let ``ROUNDTRIP: don't make dupe annotations``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; ""
                        ; "//                              MASTER USE CASES                               "
                        ; "//                   Model Generation: 07/19/2017 05:55:37                     "
                        ; "//                                                                             "
                        ; ""
                        ; "BUSINESS BEHAVIOR ABSTRACT TO-BE"
                        ; "  ALL"
                        ; ""
                        ; ""
                        ; "  Conduct Regular Inventory"
                        ; "    WHEN: "
                        ; "      The first of the month rolls around"
                        ; "    ASA: "
                        ; "      Warehouse Supervisor"
                        ; "    INEEDTO: "
                        ; "      Conduct a formal inventory using an audited procedure"
                        ; "    SOTHAT: "
                        ; "      Our accounting system stays coherent"
                        ; "      We know what to order"
                        ; ""
                        ; ""
                        ; "  Conduct Spot Inventory"
                        ; "    WHEN: "
                        ; "      A truck arrives at the gate"
                        ; "      An accountant calls on the phone"
                        ; "      There's a break-in at the warehouse"
                        ; "    ASA: "
                        ; "      Warehouse Worker"
                        ; "      Warehouse Supervisor"
                        ; "      Nightshift Guard Supervisor"
                        ; "    INEEDTO: "
                        ; "      Conduct a formal spot inventory by hand using the older books"
                        ; "    SOTHAT: "
                        ; "      The insurance company is notified in case of loss"
                        ; "      Incoming shipments will adequately update the running inventory"
                        ; "    NOTES: "
                        ; "      I love inventory"
                        ; "      the actor list isn't complete yet"
                        ; "      I love inventory"
                        ; "      the actor list isn't complete yet"
                        ; "    QUESTIONS: "
                        ; "      Do we differentiate between goods and services?"
                        ; "      Do we differentiate between goods and services?"
                        ; ""
                        ; ""
                        ; "  Create Shipment"
                        ; "    WHEN: "
                        ; "      An order has been picked and put in the staging area"
                        ; "    ASA: "
                        ; "      Warehouse Worker"
                        ; "    INEEDTO: "
                        ; "      Create a shipment to the customer"
                        ; "    SOTHAT: "
                        ; "      The items customers want will arrive on-time for them"
                        ; "      The staging area is freed up to process shipments for customers"
                        ; ""
                        ; ""
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 5

    [<Test>]
    let ``ROUNDTRIP: simple backlog list with annotations``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          "MASTER BACKLOG:"
                        ; "  02.Start Quote // 20 points"
                        ; "  01.Identify Potential Customer // 12 points"
                        ; "  03.Specify Quote Type // 12 points"
                        ; "  04.Identify Policy Type // 12 points"
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 5

    [<Test>]
    let ``ROUNDTRIP: master supp page works``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "BUSINESS SUPPLEMENTAL ABSTRACT TO-BE: Has to use existing back-end"
                        |]
        let fileInfo2 = getFakeFileInfo()
        let testText2 = [|
                          ""
                        ; "BUSINESS SUPPLEMENTAL ABSTRACT TO-BE: Left nav must synchronize with main screen"
                        |]
        let fileInfo3 = getFakeFileInfo()
        let testText3 = [|
                          ""
                        ; "BUSINESS SUPPLEMENTAL ABSTRACT TO-BE"
                        ; "  Has to use existing back-end"
                        ; ""
                        ; ""
                        ; "  Left nav must synchronize with main screen"
                        ; ""
                        ; ""
                        ; "  Must be ADA compliant"
                        ; "    NOTES: "
                        ; "      <code goes here>"
                        ; ""
                        ; ""
                        ; "  Must be compatible with major recent browsers"
                        ; ""
                        ; ""
                        ; "  Must be responsive to different screen sizes"
                        ; ""
                        ; ""
                        ; "  Must confirm to Service Level Agreements"
                        ; ""
                        ; ""
                        ; "  Must support multiple user types"
                        ; ""
                        ; ""
                        ; "  Must use existing middleware"
                        ; ""
                        ; ""
                        ; "  Top summary must synchronize with main screen"
                        ; ""
                        ; ""
                        |]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2);(fileInfo3,testText3)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 10
        newCompilerStatus.ModelItems.[1].Annotations |> should haveLength 0

    [<Test>]
    let ``ROUNDTRIP: attribute annotations dont fall back to parent``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          ""
                        ; "MASTER BACKLOG:"
                        ; "  Start Quote"
                        ; "  Identify Potential Customer"
                        |]
        let fileInfo2 = getFakeFileInfo()
        let testText2 = [|
                          ""
                        ; ""
                        |]
        let fileInfo3 = getFakeFileInfo()
        let testText3 = [|
                          ""
                        ; "MASTER BACKLOG"
                        ; "  Start Quote"
                        ; "    QUESTIONS:"
                        ; "      What's a quote?"
                        ; "      Why are we here?"
                        ; "    NOTES:"
                        ; "      This is a great meeting"
                        ; "      I really like ice cream"
                        ; "    TODO:"
                        ; "      Meet the agents"
                        ; "      Have lunch with the agents"
                        ; ""
                        ; "  WHEN: A potential customer purchases new property"
                        ; "  ASA: Agent"
                        ; "    Agent Supervisor"
                        ; "  INEEDTO: Start a new quote"
                        ; "    QUESTION: Is this all done online or can it be done in person?"
                        ; "  SOTHAT: The potential customer becomes a real customer"
                        ; ""
                        ; "MASTER SUPPLEMENTALS"
                        ; "  Must be ADA compliant"
                        ; "    NOTES:"
                        ; "      //"
                        ; "      // <code goes here>"
                        ; "      //"
                        ; ""
                        ; "PROJECT BACKLOG"
                        ; "  Identify Customer Blank Screen PARENT Identify Potential Customer"
                        ; "  ASA: Agent"
                        ; "    NOTE: Agents are associated by contract to the company"
                        ; "  INEEDTO: See the Customer Identification Screen"
                        ; "  SOTHAT: I can interact with it"
                        ; ""
                        ; ""
                        |]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2);(fileInfo3,testText3)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 6
        newCompilerStatus.ModelItems.[5].Annotations |> should haveLength 0
        newCompilerStatus.ModelItems.[5].Attributes |> should haveLength 3
        newCompilerStatus.ModelItems.[5].Attributes.[0].Annotations |> should haveLength 1
        newCompilerStatus.ModelItems.[5].Attributes.[0].Annotations.[0].AnnotationType |> should equal AnnotationTokenType.Note
        newCompilerStatus.ModelItems.[5].Attributes.[0].Annotations.[0].AnnotationText |> should equal "Agents are associated by contract to the company"


    [<Test>]
    let ``ROUNDTRIP: indentation bug``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          "BUSINESS BEHAVIOR ABSTRACT TO-BE"
                        ; "  Conduct Spot Inventory"
                        ; "    USES"
                        ; "      Inventory Item"
                        ; "      SKU"
                        ; "    NOTE:"
                        ; "      I love inventory"
                        ; "      the actor list isn't complete yet"
                        ; "    QUESTION:"
                        ; "      Do we differentiate between goods and services?"
                        ; "    WHEN:"
                        ; "      A truck arrives at the gate"
                        ; "        NOTE:"
                        ; "          could be any kind of truck"
                        ; "      An accountant calls on the phone"
                        ; "      There's a break-in at the warehouse"
                        ; "    ASA:"
                        ; "      Warehouse Worker"
                        ; "      Warehouse Supervisor"
                        ; "      Nightshift Guard Supervisor"
                        ; "    INEEDTO:"
                        ; "      Conduct a formal spot inventory by hand using the older books"
                        ; "    SOTHAT:"
                        ; "      The insurance company is notified in case of loss"
                        ; "      Incoming shipments will adequately update the running inventory"
                        ; "        QUESTION:"
                        ; "          Is there such a thing as a 'walking inventory'"
                        ; "  Conduct Regular Inventory"
                        ; "    USES"
                        ; "      Inventory Item"
                        ; "      SKU"
                        ; ""
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 5
        newCompilerStatus.ModelItems.[1].Attributes |> should haveLength 9
        newCompilerStatus.ModelItems.[1].Attributes.[0].AttributeType |> should equal ModelAttributeTypes.Trigger
        newCompilerStatus.ModelItems.[1].Attributes.[0].Annotations |> should haveLength 1




    [<Test>]
    let ``ROUNDTRIP: notation bug``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                          "BUSINESS STRUCTURE ABSTRACT TO-BE"
                        ; ""
                        ; "  Shipment"
                        ; "    USEDBY"
                        ; "      Order Goods"
                        ; "      Receive Order"
                        ; "      Receive Shipment"
                        ; "      Reject Shipment"
                        ; "      Reject Part Of Shipment"
                        ; "      Reconcile BOL"
                        ; "      Create Shipment"
                        ; "      Pick Shipment"
                        ; "      Ship Goods"
                        ; "    HASA"
                        ; "      Bill Of Lading"
                        ; "    ISOWNEDBYA"
                        ; "      Order"
                        ; "      Vendor"
                        ; "    QUESTION:"
                        ; "      Does this involve real ships?"
                        ; "      Do we get free mints?"
                        ; "      Are the ships full of mints?"
                        ; "    CONTAINS:"
                        ; "      Shipment Number"
                        ; "      Shipment Date"
                        ; "      Expected Arrival Date"
                        ; "  Order"
                        ; "    USEDBY"
                        ; "      Order Goods"
                        ; "      Receive Order"
                        ; "      Receive Shipment"
                        ; "      Reject Shipment"
                        ; "      Reject Part Of Shipment"
                        ; "      Reconcile BOL"
                        ; "      Create Shipment"
                        ; "      Pick Shipment"
                        ; "      Ship Goods"
                        ; "    HASA"
                        ; "      Shipment"
                        ; "    ISOWNEDBYA"
                        ; "      Customer"
                        ; "      Vendor"
                        ; "    CONTAINS:"
                        ; "      Order Number"
                        ; "      Order Date"
                        ; "      Customer Number"
                        ; "      Total Amount"
                        ; "  Bill Of Lading"
                        ; "    USEDBY"
                        ; "      Receive Order"
                        ; "      Receive Shipment"
                        ; "      Reconcile BOL"
                        ; "    ISOWNEDBYA"
                        ; "      Shipment"
                        ; "    CONTAINS:"
                        ; "      Pallet List"
                        ; "  Inventory Item"
                        ; "    USEDBY"
                        ; "      Receive Shipment"
                        ; "      Put Away New Shipment"
                        ; "      Conduct Spot Inventory"
                        ; "      Conduct Regular Inventory"
                        ; "      Review Daily Warehouse Activity"
                        ; "    ISOWNEDBYA"
                        ; "      SKU"
                        ; "    CONTAINS:"
                        ; "      Inventory Item Description"
                        ; "      Inventory Short Code"
                        ; "      Inventory Long Code"
                        ; "  Employee"
                        ; "    USEDBY"
                        ; "      Receive Order"
                        ; "      Receive Shipment"
                        ; "      Create Shipment"
                        ; "      Pick Shipment"
                        ; "    CONTAINS:"
                        ; "      First Name"
                        ; "      Last Name"
                        ; "      Social Security Number"
                        ; "      Hire Date"
                        ; "  Employee Type"
                        ; "    USEDBY"
                        ; "      Receive Order"
                        ; "      Create Shipment"
                        ; "    CONTAINS:"
                        ; "      Role Name"
                        ; "      Salary Level"
                        ; "  SKU"
                        ; "    USEDBY"
                        ; "      Receive Shipment"
                        ; "      Put Away New Shipment"
                        ; "      Conduct Spot Inventory"
                        ; "      Conduct Regular Inventory"
                        ; "    HASA"
                        ; "      Inventory Item"
                        ; "    ISOWNEDBYA"
                        ; "      Invoice Line Item"
                        ; "    CONTAINS:"
                        ; "      Item Number"
                        ; "      Quantity"
                        ; "  Customer"
                        ; "    USEDBY"
                        ; "      Create Shipment"
                        ; "      Ship Goods"
                        ; "    HASA"
                        ; "      Order"
                        ; "      Invoice"
                        ; "    CONTAINS:"
                        ; "      Customer Number"
                        ; "      Address"
                        ; "  Invoice"
                        ; "    USEDBY"
                        ; "      Order Goods"
                        ; "      Receive Shipment"
                        ; "      Create Shipment"
                        ; "      Pick Shipment"
                        ; "    HASA"
                        ; "      Invoice Line"
                        ; "      Invoice Line Item"
                        ; "    ISOWNEDBYA"
                        ; "      Vendor"
                        ; "      Customer"
                        ; "    CONTAINS:"
                        ; "      Invoice Number"
                        ; "      Invoice Date"
                        ; "      Invoice Total Amount"
                        ; "  Invoice Line"
                        ; "    USEDBY"
                        ; "      Receive Shipment"
                        ; "    ISOWNEDBYA"
                        ; "      Invoice"
                        ; "    CONTAINS:"
                        ; "      Line Number"
                        ; "      Line Total"
                        ; "  Invoice Line Item"
                        ; "    USEDBY"
                        ; "      Receive Shipment"
                        ; "      Create Shipment"
                        ; "    HASA"
                        ; "      SKU"
                        ; "    ISOWNEDBYA"
                        ; "      Invoice"
                        ; "    CONTAINS:"
                        ; "      Long Description"
                        ; "  Vendor"
                        ; "    USEDBY"
                        ; "      Order Goods"
                        ; "      Receive Order"
                        ; "      Receive Shipment"
                        ; "      Reject Shipment"
                        ; "    HASA"
                        ; "      Shipment"
                        ; "      Invoice"
                        ; "      Order"
                        ; "    TODO:"
                        ; "      Meet some vendors"
                        ; "      Listen to stories about vendors"
                        ; "    CONTAINS:"
                        ; "      Vendor Number"
                        ; "      Address"
                        ; "      Last Order Date"
                        ; ""
                        |]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 26
        newCompilerStatus.ModelItems.[13].Description |> should equal "Vendor"
        newCompilerStatus.ModelItems.[13].Relations  |> should haveLength 7
        newCompilerStatus.ModelItems.[13].Annotations |> should haveLength 2

    [<Test>]
    let ``ROUNDTRIP: backlog parent bug``()=
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|
                         ""
                        ; "MASTER BACKLOG:"
                        ; "  Start Quote"
                        ; "  Identify Potential Customer"
                        ; "  Specify Quote Type"
                        ; "  Identify Policy Type"
                        ; "  Identify Coverables"
                        ; "  ALL"
                        ; "  MISC"
                        ; ""
                        ; "MASTER SUPPLEMENTALS:"
                        ; "  Has to use existing back-end"
                        ; "  Must be compatible with major recent browsers"
                        ; ""
                        ; "MASTER DOMAIN MODEL:"
                        ; "  Customer"
                        ; "  Agent"
                        ; "  Account"
                        |]
        let fileInfo2 = getFakeFileInfo()
        let testText2 = [|
                         ""
                        ; "// These are the notes we took during that meeting with the client"
                        ; ""
                        ; ""
                        ; "MASTER BACKLOG"
                        ; "  Start Quote"
                        ; "    QUESTIONS:"
                        ; "      What's a quote?"
                        ; "      Why are we here?"
                        ; "    NOTES:"
                        ; "      This is a great meeting"
                        ; "      I really like ice cream"
                        ; "    TODO:"
                        ; "      Meet the agents"
                        ; "      Have lunch with the agents"
                        ; ""
                        ; "  WHEN: A potential customer purchases new property"
                        ; "  ASA: Agent"
                        ; "    Agent Supervisor"
                        ; "  INEEDTO: Start a new quote"
                        ; "    QUESTION: Is this all done online or can it be done in person?"
                        ; "  SOTHAT: The potential customer becomes a real customer"
                        ; ""
                        ; "MASTER SUPPLEMENTALS"
                        ; "  Must be ADA compliant"
                        ; "    NOTES:"
                        ; "      //"
                        ; "      // <code goes here>"
                        ; "      //"
                        ; ""
                        ; "PROJECT BACKLOG"
                        ; "  Identify Customer Blank Screen PARENT Identify Potential Customer"
                        ; "  ASA: Agent"
                        ; "    NOTE: Agents are associated by contract to the company"
                        ; "  INEEDTO: See the Customer Identification Screen"
                        ; "  SOTHAT: I can interact with it"
                        ; ""
                        ; "  Enter Initial Customer Information PARENT Identify Potential Customer"
                        ; "  WHEN: I enter Customer Identification information"
                        ; "  ASA: Agent"
                        ; "  INEEDTO: Store the information entered"
                        ; "  SOTHAT: I can work with the customer"
                        ; "  "
                        ; "  Retrieve Existing Customer Information PARENT Identify Potential Customer"
                        ; "  ASA: Agent"
                        ; "  WHEN: I enter Customer Information"
                        ; "  INEEDTO: See relevant customer information"
                        ; "  SOTHAT: I can make sure I am working with the right customer"
                        ; ""
                        |]
        let listToProcess = [|(fileInfo1,testText1);(fileInfo2,testText2)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let newCompilerStatus=makeRawModel processedIncomingLines compilerReturn
        newCompilerStatus.ModelItems |> should haveLength 18
        newCompilerStatus.ModelItems.[0].Annotations |> should haveLength 1
        newCompilerStatus.ModelItems.[17].Relations |> should haveLength 1
        newCompilerStatus.ModelItems.[17].Relations.[0].ModelJoinType |> should equal ModelJoin.Parent




    [<Test>]
    /// Take whatever is in the directory, process it, then process that again, and see if it matches
    let ``ROUNDTRIP: SANITY CHECK``()=
        let allFiles = System.IO.Directory.EnumerateFiles(System.Environment.CurrentDirectory, "*.amin", System.IO.SearchOption.AllDirectories)
        let fileList = allFiles |> Seq.toArray |> Array.map(fun x->System.IO.FileInfo(x)) |> Array.sortBy(fun x->x.FullName)
        let listToProcess = loadInAllIncomingLines fileList

        //processing first time
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let firstCompilerResult = makeRawModel processedIncomingLines compilerReturn        
        let originalConsoleOut = System.Console.Out
        let writer=new System.IO.StringWriter()
        System.Console.SetOut(writer)
        let dummyOutputDirectory=new System.IO.DirectoryInfo(System.IO.Directory.GetCurrentDirectory())
        writeOutModel firstCompilerResult.ModelItems firstCompilerResult.ModelItems ModelOutputType.AMOUT dummyOutputDirectory  true ""
        let firstModelOutput = writer.GetStringBuilder().ToString()

        //processing second time
        let fileInfo1 = getFakeFileInfo()
        let testText1 = [|firstModelOutput|]
        let listToProcess = [|(fileInfo1,testText1)|]
        let processedIncomingLines, compilerReturn = bulkFileLineProcessing listToProcess
        let secondCompilerResult=makeRawModel processedIncomingLines compilerReturn
        let secondWriter=new System.IO.StringWriter()
        System.Console.SetOut(secondWriter)
        let dummyOutputDirectory=new System.IO.DirectoryInfo(System.IO.Directory.GetCurrentDirectory())
        writeOutModel secondCompilerResult.ModelItems secondCompilerResult.ModelItems ModelOutputType.AMOUT dummyOutputDirectory  true ""
        let secondModelOutput = writer.GetStringBuilder().ToString()

        System.Console.SetOut(originalConsoleOut)

        firstModelOutput |> should equal secondModelOutput
        firstCompilerResult.ModelItems |> should haveLength secondCompilerResult.ModelItems.Length
        firstCompilerResult.ModelItems |> Array.iteri(fun i x->
                x.Description|>should equal secondCompilerResult.ModelItems.[i].Description
                x.Attributes.Length |> should equal secondCompilerResult.ModelItems.[i].Attributes.Length
                x.Attributes|> Array.iteri(fun j y->
                    y.AttributeType |> should equal secondCompilerResult.ModelItems.[i].Attributes.[j].AttributeType
                    y.Description |> should equal secondCompilerResult.ModelItems.[i].Attributes.[j].Description
                    y.Annotations.Length |> should equal secondCompilerResult.ModelItems.[i].Attributes.[j].Annotations.Length
                    y.Annotations |> Array.iteri(fun k z->
                        z.AnnotationType |> should equal secondCompilerResult.ModelItems.[i].Attributes.[j].Annotations.[k].AnnotationType
                        z.AnnotationText |> should equal secondCompilerResult.ModelItems.[i].Attributes.[j].Annotations.[k].AnnotationText
                        )
                    )
                // don't compare the default root item. Lots of noise up there, including model generation stuff
                // if rest of model checks out, it doesn't matter
                if x.Id=(-1) then () else
                x.Annotations.Length |> should equal secondCompilerResult.ModelItems.[i].Annotations.Length
                x.Annotations |> Array.iteri(fun j y->
                    y.AnnotationType |> should equal secondCompilerResult.ModelItems.[i].Annotations.[j].AnnotationType
                    y.AnnotationText |> should equal secondCompilerResult.ModelItems.[i].Annotations.[j].AnnotationText
                    )
                x.Relations.Length |> should equal secondCompilerResult.ModelItems.[i].Relations.Length
                x.Relations|>Array.iteri(fun k z->
                    z.ModelJoinType |> should equal secondCompilerResult.ModelItems.[i].Relations.[k].ModelJoinType
                    z.TargetId |> should equal secondCompilerResult.ModelItems.[i].Relations.[k].TargetId
                    )
            )