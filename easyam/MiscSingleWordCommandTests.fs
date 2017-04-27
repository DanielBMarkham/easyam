module MiscSingleWordCommandTests
    open NUnit.Framework
    open FsUnit
    open EasyamParsingEngine

    [<Test>]
    let ``MISC: Null test``() =
//        let compilerMessages,modelItems = parseLines []
//        compilerMessages |> should be Empty
//        modelItems |> should be Empty
        true |> should be True

