module BasicModel1
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

    let basicModel1 =
        let fileInfo1 = getFakeFileInfo()
        let fileInfo2 = getFakeFileInfo()
        let fileInfo3 = getFakeFileInfo()
        let fileInfo4 = getFakeFileInfo()
        let fileInfo5 = getFakeFileInfo()
        let fileInfo6 = getFakeFileInfo()
        let fileInfo7 = getFakeFileInfo()
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
        let testText3 = [|
                          ""
                        ; "From the initial talk, the master domain model items used by the master user stories"
                        ; "Note the only requirement at the _BIZ_ABST_ level is to include or exclude items based on how often they show up in conversation"
                        ; "(It's not a data model)"
                        ; "(Also note that we had to purposely mispell keywords in order to prevent the compiler from compiling the note wrong)"
                        ; ""
                        ; ""
                        ; "BEHAVIOR"
                        ; "    Order Goods USES Order, Vendor, Invoice, Shipment"
                        ; "    Receive Order USES Order, Customer, Invoice, Shipment"
                        ; "    Receive Shipment USES Shipment, Order, Bill of Lading, Inventory Item, Employee, SKU, Vendor, Invoice, Invoice Line, Invoice Line Item"
                        ; "    Reject Shipment USES"
                        ; "        Order"
                        ; "        Shipment"
                        ; "    Reject Part Of Shipment USES Order, Shipment"
                        ; "    Reconcile BOL USES Order, Shipment, Bill of Lading"
                        ; "    Put Away New Shipment USES Inventory Item, SKU"
                        ; "    Conduct Spot Inventory USES Inventory Item, SKU"
                        ; "    Conduct Regular Inventory USES Inventory Item, SKU"
                        ; "    Recieve Order USES Vendor, Order, Shipment, Bill of Lading, Employee, Employee Type"
                        ; "    Create Shipment"
                        ; "        USES"
                        ; "            Employee"
                        ; "            Employee Type"
                        ; "            Customer"
                        ; "            Order"
                        ; "            Invoice"
                        ; "            Shipment"
                        ; "            Invoice Line Item"
                        ; "    Pick Shipment USES Employee, Order, Invoice, Shipment"
                        ; "    Ship Goods USES Customer, Order, Shipment"
                        ; "    Review Daily Warehouse Activity USES Inventory Item"
                        ; "    "
                        ; "    "
                        |]
        let testText4 = [|
                          ""
                        ; "Here's our initial master supplemental list"
                        ; ""
                        ; ""
                        ; "SUPPLEMENTALS"
                        ; "    Always respond in a way that's easy for the user to understand"
                        ; "    Never make the user wait"
                        ; "    Never confuse the user"
                        ; "    Always keep track of the path users take to arrive on-site"
                        ; "    Continuously inform the team about everything the users do"
                        ; "    Make it as easy as possible for the user to provide negative feedback"
                        ; "    Always do the best we can to cheer the user up during any interaction"
                        |]
        let testText5 = [|
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
        let testText6 = [|
                          ""
                        ; ""
                        ; ""
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
                        ; "        ASA Cusomter"
                        ; "        INEEDTO Place an order for new stuff"
                        ; "        SOTHAT I can heppy owning things I think I need"
                        ; "    Conduct spot inventory //I love inventory"
                        ; "        WHEN "
                        ; "            The truck arrives at the gate // could be any kind of truck"
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
                        |]
        let testText7 = [|
                            ""
                        ; "    "
                        ; "    "
                        ; "    "
                        |]
        [|(fileInfo1,testText1);(fileInfo2,testText2);(fileInfo3,testText3);(fileInfo4,testText4);(fileInfo5,testText5);(fileInfo6,testText6);(fileInfo7,testText7)|]

