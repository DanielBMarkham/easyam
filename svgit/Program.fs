let fl="//															Example 1\n\
//															Initial Phone Notes\n\
\
Joe's Dental Project\n\
\
WORK: Called Joe to talk about upcoming project &time=45\n\
\
MASTER DOMAIN MODEL\n\
	Patient\n\
	Dentist\n\
	Procedure\n\
	Q: Is there more than one dentist?\n\
	Q: How many different kinds of procedures are they? Is there a list/codes?\n\
    \n\
MASTER BACKLOG\n\
	Perform Procedure\n\
	Make Apointment // This seems to be a hot button with Joe\n\
	Plan Daily Work\n\
		NOTE: used by the office manager and others (?) to optimize day\n\
        \n\
MASTER SUPPLEMENTAL MODEL\n\
	The customer should never wait\n\
		Telephone calls should be no more than 5 minutes total\n\
		Anything done on the phone should also be availale online\n\
	We always need to learn how we're doing\n\
		Follow-up contacts should occur with any customer interaction\n\
			Follow-up contacts should be fun\n\
		We have big, visible, public displays of how we're doing\n\
	\n\
	Q: Are we going to graph a lot of things?\n\
	Q: Is there a big printer available?\n\
	Q: What about web dashboards?\n\
    \n\
TODO: Get contract template together and filled out\n\
TODO: Have the initial consulting, work-for-hire conversation\n\
\n\
// Hmm\n\
##It'll be good to remind Joe of our key offerings\n\
- Nice people\n\
- We smile a lot\n\
- We like pets\n\
\n\
WORK: set up folders, todos, and initial easyam compile pipeline &time=15\n\
\n\
"

type SearchDirection =
    | Forward
    | Backward
// 'A is the language token state
// 'B is the context
type Token<'A,'B> =
    {
        SearchDirection:SearchDirection
        RegexMatch:string
        FunMatchAndEat:(string*'B->'A*'B*string)
    }
let myTokens:Token<string, string>[] = 
    [|
        {
            SearchDirection=Backward
            RegexMatch="//"
            FunMatchAndEat=(fun(a,b)->
                                    "\\",b,a
                                )
        }
    |]


[<EntryPoint>]
let main argv = 
    let fll = fl.Split([|"\n"|],System.StringSplitOptions.None)

    printfn "%A" argv
    0 // return an integer exit code
