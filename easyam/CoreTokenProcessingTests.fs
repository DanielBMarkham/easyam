module findInitialTextKeywordAndRemainingTextOnALineTests
    open NUnit.Framework
    open FsUnit
    open EasyamParsingEngine
    open Types
    open SAModel


    [<Test>]
    let ``CORE TOKEN PROCESSING: Nothing returns nothing``() =
        let testTokens = [""]
        let testText = ""
        let commandList = splitOutIncomingLineIntoCommandList testTokens testText
        commandList.Length |> should equal 0

    [<Test>]
    let ``CORE TOKEN PROCESSING: String by itself is fully consumed``() =
        let testTokens = ["//";"Q:"]
        let testText = "The lazy brown fox jumped over the dirty dog"
        let commandList = splitOutIncomingLineIntoCommandList testTokens testText
        commandList.Length |> should equal 1
        commandList.[0].Value |> should equal testText
        commandList.[0].Token.Length |> should equal 0

    [<Test>]
    let ``CORE TOKEN PROCESSING: Token at beginning of line works``() =
        let testTokens = ["//";"Q:"]
        let testText = "Q: Why are we here?"
        let commandList = splitOutIncomingLineIntoCommandList testTokens testText
        commandList.Length |> should equal 1
        commandList.[0].Value |> should equal "Why are we here?"
        commandList.[0].Token.Length |> should equal 2
        commandList.[0].Token |> should equal "Q:"

    [<Test>]
    let ``CORE TOKEN PROCESSING: Token in the middle of the line works``() =
        let testTokens = ["//";"Q:";"TODO:"; "NOTE:"]
        let testText = "I saw this guy on TV Q: Why are we here?"
        let commandList = splitOutIncomingLineIntoCommandList testTokens testText
        commandList.Length |> should equal 2
        commandList.[0].Value |> should equal "I saw this guy on TV"
        commandList.[0].Token.Length |> should equal 0
        commandList.[1].Value |> should equal "Why are we here?"
        commandList.[1].Token.Length |> should equal 2
        commandList.[1].Token |> should equal "Q:"

    [<Test>]
    let ``CORE TOKEN PROCESSING: Token at end of the line works``() =
        let testTokens = ["//";"Q:";"TODO:"; "NOTE:"]
        let testText = "Dreams that may never come Q:"
        let commandList = splitOutIncomingLineIntoCommandList testTokens testText
        commandList.Length |> should equal 2
        commandList.[0].Value |> should equal "Dreams that may never come"
        commandList.[0].Token.Length |> should equal 0
        commandList.[1].Value |> should equal ""
        commandList.[1].Token.Length |> should equal 2
        commandList.[1].Token |> should equal "Q:"
        
    [<Test>]
    let ``CORE TOKEN PROCESSING: Empty tokens stacked in a line works``() =
        let testTokens = ["//";"Q:";"TODO:"; "NOTE:"]
        let testText = "//Q:TODO:NOTE:"
        let commandList = splitOutIncomingLineIntoCommandList testTokens testText
        commandList.Length |> should equal 4
        commandList.[0].Value |> should equal ""
        commandList.[0].Token |> should equal "//"
        commandList.[1].Value |> should equal ""
        commandList.[1].Token |> should equal "Q:"
        commandList.[2].Value |> should equal ""
        commandList.[2].Token |> should equal "TODO:"
        commandList.[3].Value |> should equal ""
        commandList.[3].Token |> should equal "NOTE:"

    ///
    /// Splitting a line up into commands and values
    ///
    let miscCommandTokens = [
        "NOTE:";
        "NOTE ";
        "NOTES ";
        "//";
        "Q:";
        "QUESTION:";
        "QUESTION ";
        "QUESTIONS ";
        "TODO:";
        "TODO ";
        "TODOS ";
        "WORK:";
        "WORK ";
        "WORKS "
        ]

    let systemTokens = miscCommandTokens |> List.append []

    [<Test>]
    let ``CORE TOKEN PROCESSING: Comment line creates one Command Item``() =
        let testTokens = systemTokens
        let testText = "Wow this is my freeform text"
        let retResultingCommands = splitOutIncomingLineIntoCommandList testTokens testText
        retResultingCommands.Length |> should equal 1
        retResultingCommands.[0].Token |> should equal ""
        retResultingCommands.[0].Value |> should equal "Wow this is my freeform text"

    [<Test>]
    let ``CORE TOKEN PROCESSING: Freeform text then question makes two appropriate command lines``() =
        let testTokens = systemTokens
        let testText = "I like being able to type in text Q:Why is that?"
        let retResultingCommands = splitOutIncomingLineIntoCommandList testTokens testText
        retResultingCommands.Length |> should equal 2
        retResultingCommands.[0].Token |> should equal ""
        retResultingCommands.[0].Value |> should equal "I like being able to type in text"
        retResultingCommands.[1].Token |> should equal "Q:"
        retResultingCommands.[1].Value |> should equal "Why is that?"

    [<Test>]
    let ``CORE TOKEN PROCESSING: Compound/Complex line``() =
        let testTokens = systemTokens
        let testText = "I wish we would have lasagna Q:Why?NOTE:I really don't like lasagna TODO: Learn to like lasagna    QUESTION:LasagnaTODO:"
        let retResultingCommands = splitOutIncomingLineIntoCommandList testTokens testText
        retResultingCommands.Length |> should equal 6
        retResultingCommands.[0].Token |> should equal ""
        retResultingCommands.[0].Value |> should equal "I wish we would have lasagna"
        retResultingCommands.[1].Token |> should equal "Q:"
        retResultingCommands.[1].Value |> should equal "Why?"
        retResultingCommands.[2].Token |> should equal "NOTE:"
        retResultingCommands.[2].Value |> should equal "I really don't like lasagna"
        retResultingCommands.[3].Token |> should equal "TODO:"
        retResultingCommands.[3].Value |> should equal "Learn to like lasagna"
        retResultingCommands.[4].Token |> should equal "QUESTION:"
        retResultingCommands.[4].Value |> should equal "Lasagna"
        retResultingCommands.[5].Token |> should equal "TODO:"
        retResultingCommands.[5].Value |> should equal ""
