﻿module SAModel
    open Types

//// REFACTOR
//// Everything entered in has three things: an origin, a tagged context, and free text (markup)
//// There are four types of models: Structure, Behavior, Supplemental, and Meta
//// Each model is structured so: 
////      Model Business Abstract
////          Item(s) [Diagrams: Item/Relationship list]
////              Model Business Realized
////                  Item(s) [Diagrams: Item/Relationship list]
////                      Model System Abstract
////                          Items(s) [Diagrams: Item/Relationship list]
////                              Model System Realized
////                                  Item(s) [Diagrams: Item/Relationship list]
//// HYPOTHESIS->Item (title format) -> Desired change list of behavior, structure, and suppl (at any level)
////
//// Any one of these can have notes (freetext tagged to something), questions, TODOs, and name-value tags
//// Items can be unique to that layer -- or simply refer to an ancestor item up the tree
//// Items have a short name, a long name, and a detailed name (super long)? Ids are INTERNAL-ONLY,
//// we force people organize their shared mental models around the English Language, not around codes and numbers
//// Nesting is implied by whitespace or colons, eg BUSINESS BEHAVIOR: Do Dishes
////
//// compilation rules:
////      no more than 40 items per layer (error)
////      no behavior nouns not mentioned in structure (and vice versa) (warning)
////      

//    let IntegerFactory = 
//        let counter = ref 0
//        fun () -> 
//            counter.Value <- !counter + 1
//            !counter
//    let getNextItemNumber()=IntegerFactory()
//    // Structured Analysis Model Super Types
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


//    // HYPOTHESIS
//    [<NoComparison>]
//    type Hypothesis =
//        {
//            Genre:Genres
//            ShortName:string
//            Observations:string list
//            ExpectedActorsImpacted:Actor list
//            ExpectedImpactMetricGiven:string list
//            ExpectedSuccessfulResult:string list
//            ProposedChangesToTestHypothesis:ModelItem list
//            ExpectedExperimentSuspenseTime:string
//        }
//    let defaultHypothesis = 
//        {
//            Genre=Genres.Business
//            ShortName=""
//            Observations=[]
//            ExpectedActorsImpacted=[]
//            ExpectedImpactMetricGiven=[]
//            ExpectedSuccessfulResult=[]
//            ProposedChangesToTestHypothesis=[]
//            ExpectedExperimentSuspenseTime=""
//        }

    [<NoComparison>]
    type TOKEN_TYPE =
        |RELATIVE_LOCATOR
        |ABSOLUTE_LOCATOR
        |JOINER
         static member ToList() =
            [RELATIVE_LOCATOR;ABSOLUTE_LOCATOR;JOINER]
         override self.ToString() =
          match self with
            | RELATIVE_LOCATOR->"Relative Locator"
            | ABSOLUTE_LOCATOR->"Absolute Locator"
            | JOINER->"Joiner"
    [<NoComparison>]
    type TOKEN_TARGET_TYPE =
        |SINGLE_TARGET
        |MULTIPLE_TARGETS
         static member ToList() =
            [SINGLE_TARGET;MULTIPLE_TARGETS]
         override self.ToString() =
          match self with
            | SINGLE_TARGET->"Single Target"
            | MULTIPLE_TARGETS->"Multiple Targets"
    [<NoComparison>]
    type TOKEN_CATEGORY = 
        |BUCKETS
        |GENRE
        |TEMPORAL
        |ABSTRACTION_LEVEL
        |TASKS
        |MISC
        |HDD
        |SCOPING
        |SHORTCUT
        |CONNECTIVE
        |ATTRIBUTE
        |NAMESPACE
        |TAG
    [<NoComparison>]
    type EASYAM_TOKEN =
        {
            Type:TOKEN_TYPE
            TargetType:TOKEN_TARGET_TYPE
            Category:TOKEN_CATEGORY
            Token:string
        }
    type ANNOTATION_TOKEN_TYPE =
        | None
        | Note
        | Question
        | ToDo
        | Work
        | Diagram
         static member ToList() =
            [None;Note;Question;ToDo;Work;Diagram]
         override self.ToString() =
          match self with
            | None->"None"
            | Note->"Note"
            | Question->"Question"
            | ToDo->"ToDo"
            | Work->"Work"
            | Diagram->"Diagram"
    let EasyAMTokens = 
        [
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="NOTES:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="NOTES"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="NOTE:"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="//"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="Q:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="QUESTIONS:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="QUESTIONS"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="QUESTION:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="TODOS:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="TODOS"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="TODO:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="TO-DOS:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="TO-DOS"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="TO-DO:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="WORKS:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="WORKS"}
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=MISC;                 Token="WORK:"}
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="DIAGRAMS:"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="DIAGRAMS"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="DIAGRAM"};

            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="PROGRAM BACKLOG:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="PROGRAM BACKLOG"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="PRODUCT BACKLOG:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="PRODUCT BACKLOG"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="PROJECT BACKLOG:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="PROJECT BACKLOG"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="SPRINT BACKLOG:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="SPRINT BACKLOG"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="MASTER BACKLOG:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="MASTER BACKLOG"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="MASTER USER STORY:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="MASTER USER STORY"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="MASTER USER STORIES:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="MASTER USER STORIES"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="MASTER DOMAIN MODEL:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="MASTER DOMAIN MODEL"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="PROJECT DOMAIN MODEL:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="PROJECT DOMAIN MODEL"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="PRODUCT DOMAIN MODEL:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="PRODUCT DOMAIN MODEL"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="MASTER SUPPLEMENTAL MODEL:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="MASTER SUPPLEMENTAL MODEL"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="PROJECT SUPPLEMENTAL MODEL:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="PROJECT SUPPLEMENTAL MODEL"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="PRODUCT SUPPLEMENTAL MODEL:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="PRODUCT SUPPLEMENTAL MODEL"};

            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="US:"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="USER STORY:"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="USER STORIES:"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="ENT:"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="ENTITY:"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="ENTITIES:"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=SHORTCUT;             Token="SUPPL:"};

            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=BUCKETS;              Token="BEHAVIOR:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=BUCKETS;              Token="BEHAVIOR"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=BUCKETS;              Token="STRUCTURE:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=BUCKETS;              Token="STRUCTURE"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=BUCKETS;              Token="SUPPLEMENTALS:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=BUCKETS;              Token="SUPPLEMENTALS"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=BUCKETS;              Token="SUPPLEMENTAL:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=BUCKETS;              Token="SUPPLEMENTAL"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=GENRE;                Token="BUSINESS:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=GENRE;                Token="BUSINESS"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=GENRE;                Token="SYSTEM:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=GENRE;                Token="SYSTEM"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=GENRE;                Token="META:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=GENRE;                Token="META"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=TEMPORAL;             Token="WAS:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=TEMPORAL;             Token="WAS"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=TEMPORAL;             Token="AS-IS:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=TEMPORAL;             Token="AS-IS"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=TEMPORAL;             Token="TO-BE:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=TEMPORAL;             Token="TO-BE"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ABSTRACTION_LEVEL;    Token="ABSTRACT:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ABSTRACTION_LEVEL;    Token="ABSTRACT"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ABSTRACTION_LEVEL;    Token="REALIZED:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ABSTRACTION_LEVEL;    Token="REALIZED"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=HDD;                  Token="HDD:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=HDD;                  Token="HDD"};

            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=NAMESPACE;            Token="NAMESPACE:"};
            {Type=RELATIVE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=NAMESPACE;            Token="NAMESPACE"};

//            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=HDD;                  Token="HYPOTHESES:"};
//            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=HDD;                  Token="HYPOTHESES"};
//            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=HDD;                  Token="HYPOTHESIS:"};
//            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=HDD;                  Token="HYPOTHESIS"};
//            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=HDD;                  Token="HYP:"};
//            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=HDD;                  Token="OBSERVATIONS:"};
//            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=HDD;                  Token="OBSERVATIONS"};
//            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=HDD;                  Token="OBSERVATION"};
//            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=HDD;                  Token="OBSERVATION:"};

            {Type=JOINER;               TargetType=SINGLE_TARGET;        Category=CONNECTIVE;           Token="PARENT:"};
            {Type=JOINER;               TargetType=SINGLE_TARGET;        Category=CONNECTIVE;           Token="PARENT"};
            {Type=JOINER;               TargetType=SINGLE_TARGET;        Category=CONNECTIVE;           Token="CHILD:"};
            {Type=JOINER;               TargetType=SINGLE_TARGET;        Category=CONNECTIVE;           Token="CHILD"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="CHILDREN:"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="CHILDREN"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="AFFECTS:"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="AFFECTS"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="AFFECTEDBY:"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="AFFECTEDBY"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="USES:"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="USES"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="USEDBY:"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="USEDBY"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="HASA:"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="HASA"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="ISOWNEDBYA:"};
            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="ISOWNEDBYA"};
//            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="ABDUCTSTO:"};
//            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="ABDUCTSTO"};
//            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="ABDUCTEDFROM:"};
//            {Type=JOINER;               TargetType=MULTIPLE_TARGETS;     Category=CONNECTIVE;           Token="ABDUCTEDFROM"};

            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="WHENEVER:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="WHENEVER"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="WHEN:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="WHEN"};
            // note the initial space below. ASA cannot appear in first column
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="ASA:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="ASA"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="INEEDTO:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="INEEDTO"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="SOTHAT:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="SOTHAT"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="SCENARIO:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="SCENARIO"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="CONTAINS:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="CONTAINS"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="INEEDTO:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="INEEDTO"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="BECAUSE:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="BECAUSE"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="ITHASTOBETHAT:"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=ATTRIBUTE;           Token="ITHASTOBETHAT"};

            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=TAG;           Token="@.+"};
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=TAG;           Token="&.+"};

        ]
    let CommandTokens = EasyAMTokens |> List.map(fun x->x.Token)

    type Command =
        {
            CommandIndentLevel:int
            Token:string
            Value:string
        }
         override self.ToString() =
            "Indent: " + self.CommandIndentLevel.ToString() + " Token: " + self.Token + " Value: " + self.Value
    [<NoComparison>]
    type IncomingLine =
        {
            FileCompilationNumber:int
            File:System.IO.FileInfo
            FileRawLineNumber:int
            FileEmptyLinesStrippedLineNumber:int
            SourceRawLineNumber:int
            SourceEmptyLinesStrippedLineNumber:int
            LineText:string
            LineWithoutLeadingSpaces:string
            IndentLevel:int
            Commands:Command []
        }
    let defaultIncomingLine =
        {
            FileCompilationNumber=0
            File=null
            FileRawLineNumber=0
            FileEmptyLinesStrippedLineNumber=0
            SourceRawLineNumber=0
            SourceEmptyLinesStrippedLineNumber=0
            LineText=""
            LineWithoutLeadingSpaces=""
            IndentLevel=0
            Commands=Array.empty<Command>
        }



    type CompilerMessageType =
        | Info
        | Warning
        | Error
         static member ToList() =
            [Info;Warning;Error]
         override self.ToString() =
          match self with
            | Info->"Info"
            | Warning->"Warning"
            | Error->"Error"
    [<NoComparison>]
    type CompilerMessage =
            {
                MessageType:CompilerMessageType
                Message:string
                SourceFileShort:string
                SourceFileLong:string
                SourceLineBegin:int option
                SourceLineEnd:int option
                SourceLineColumnBegin:int option
                SourceLineColumnEnd:int option
            }
    let prependToDelimitedList (prependString:string) (currentString:string) (newStringItem:string) =
        let prepend = if currentString.Length=0 || (currentString.GetRight 1) = prependString
                        then ""
                        else prependString.ToString()
        if newStringItem.Length=0 then currentString else
            (currentString + prepend + newStringItem)
    let printCompilerMessages (compilerMessages:CompilerMessage []) =
        compilerMessages |> Array.iteri(fun i x->
            let messagePrefix=match x.MessageType with
                                    |CompilerMessageType.Info->"INFO"
                                    |CompilerMessageType.Warning->"WARN"
                                    |CompilerMessageType.Error->"ERROR"
            let formattedMessage=match x.SourceLineBegin,x.SourceLineEnd,x.SourceLineColumnBegin,x.SourceLineColumnEnd with 
                                    |option.None, option.None, option.None, option.None->
                                        let part1 = (prependToDelimitedList ": " messagePrefix x.SourceFileShort)
                                        let part3 = (prependToDelimitedList ": " part1 x.Message )
                                        part3
                                    |Some beginningLine, option.None, option.None, option.None->
                                        x.SourceFileShort + ":" + string beginningLine + ": " + messagePrefix + ":" + x.Message
                                    |Some beginningLine, endingLine, option.None, option.None->
                                        x.SourceFileShort + ":" + string beginningLine + "-" + string endingLine + ": " + messagePrefix + ":" + x.Message
                                    |_,_,_,_->
                                        let part1=prependToDelimitedList ": " x.SourceFileShort messagePrefix
                                        part1 + x.Message
            System.Console.WriteLine(formattedMessage)
            )
    [<NoComparison>]
    type LastCompilerOperations =
        | PointerReset
        | NewModelItem
        | ReferenceExistingItem
        | LocationChange
        | NewJoin
        | NewAttribute
        | ReferenceExistingAttribute
        | NewAnnotation
        | NewAttributeAnnotation
         static member ToList() =
            [PointerReset;NewModelItem;ReferenceExistingItem;LocationChange;NewJoin;NewAttribute;ReferenceExistingAttribute;NewAnnotation;NewAttributeAnnotation]
         override self.ToString() =
          match self with
            | PointerReset->"PointerReset"
            | NewModelItem->"NewModelItem"
            | ReferenceExistingItem->"ReferenceExistingItem"
            | LocationChange->"LocationChange"
            | NewJoin->"NewJoin"
            | NewAttribute->"NewAttribute"
            | ReferenceExistingAttribute->"ReferenceExistingAttribute"
            | NewAnnotation->"NewAnnotation"
            | NewAttributeAnnotation->"NewAttributeAnnotation"
    [<NoComparison>]
    type ModelAttributeTypes =
        | Trigger
        | Actor
        | Goal
        | BusinessContext
        | Scenario
        | Contains
        | Because
        | Whenever
        | ItHasToBeThat
         static member ToList() =
            [Trigger;Actor;Goal;BusinessContext;Scenario;Contains;Because;Whenever;ItHasToBeThat]
         override self.ToString() =
          match self with
            | Trigger->"PointerReset"
            | Actor->"NewModelItem"
            | Goal->"ReferenceExistingItem"
            | BusinessContext->"LocationChange"
            | Scenario->"" // why are these things not the enum names?
            | Contains->"NewJoin"
            | Because->"NewAttribute"
            | Whenever->"ReferenceExistingAttribute"
            | ItHasToBeThat->"NewAnnotation"
    [<NoComparison>]
    type ModelItem2Attribute =
        {
            id:int
            ModelItemParentId:int
            AttributeType:ModelAttributeTypes
            Description:string
            Annotations:(ANNOTATION_TOKEN_TYPE*string) []
            SourceReferences:IncomingLine []
        }
    let defaultModelItem2Attribute =
        {
            id=(-1)
            ModelItemParentId=(-1)
            AttributeType=ModelAttributeTypes.Goal
            Description=""
            Annotations=[||]
            SourceReferences=[||]
        }
    [<NoComparison>]
    type ModelLocationPointer =
        {
            Namespace:string
            ParentId:int
            AttributeType:ModelAttributeTypes option
            AttributeId:int option
            LastJoinTargetId:int option
            InHDDMode:bool
            Bucket:Buckets
            Genre:Genres
            AbstractionLevel:AbstractionLevels
            TemporalIndicator:TemporalIndicators
            AnnotationIndicator:ANNOTATION_TOKEN_TYPE
            Tags:System.Collections.Generic.KeyValuePair<string,string>[]
        }
    let defaultModelLocationPointer =
        {
            Namespace = ""
            ParentId = -1
            AttributeType=option.None
            AttributeId = option.None
            LastJoinTargetId= option.None
            InHDDMode=false
            Bucket=Buckets.None
            Genre=Genres.None
            AbstractionLevel=AbstractionLevels.None
            TemporalIndicator=TemporalIndicators.None
            AnnotationIndicator=ANNOTATION_TOKEN_TYPE.None
            Tags=[||]
        }
    [<NoComparison>]
    type ModelJoin =
        |Parent
        |Child
        |Affects
        |AffectedBy
        |Uses
        |UsedBy
        |HasA
        |IsOwnedByA
         static member ToList() =
            [Parent;Child;Affects;AffectedBy;Uses;UsedBy;HasA;IsOwnedByA]
         override self.ToString() =
          match self with
            | Parent->"Parent"
            | Child->"Child "
            | Affects->"Affects"
            | AffectedBy->"AffectedBy"
            | Uses->"Uses"
            | UsedBy->"IsUsedBy"
            | HasA->"HasA"
            | IsOwnedByA->"IsOwnedByA"
    let getReverseJoin (sourceJoin:ModelJoin) =
        match sourceJoin with 
            | Parent->Child
            | Child->Parent
            | Affects->AffectedBy
            | AffectedBy->Affects
            | Uses->UsedBy
            | UsedBy->Uses
            | HasA->IsOwnedByA
            | IsOwnedByA->HasA
    [<NoComparison>]
    type ModelRelation =
        {
            id:int
            ModelJoinType:ModelJoin
            TargetId:int
            SourceReference:IncomingLine
        }
    [<NoComparison>]
    type ModelItem2 =
        {
            Id:int
            Location:ModelLocationPointer
            Description:string
            Attributes:ModelItem2Attribute []
            Annotations:(ANNOTATION_TOKEN_TYPE*string) []
            SourceReferences:IncomingLine []
            Relations:ModelRelation []
            Tags:System.Collections.Generic.KeyValuePair<string,string>[]
        }
    let defaultModelItem2:ModelItem2 =
        {
            Id=(-1)
            Location=defaultModelLocationPointer
            Description=""
            Attributes=[||]
            Annotations=[||]
            SourceReferences=[||]
            Relations=[||]
            Tags=[||]
        }
    [<NoComparison>]
    type CompilerWaitingFor =
        |Nothing
        |SingleTarget
        |MultipleTargets
        |MultipleAttributeTargets
        |MultipleAnnotationTargets
        |MultipleJoinTargets
         static member ToList() =
            [Nothing;SingleTarget;MultipleTargets;MultipleAttributeTargets;MultipleAnnotationTargets;MultipleJoinTargets]
         override self.ToString() =
          match self with
            | Nothing->"Nothing"
            | SingleTarget->"SingleTarget"
            | MultipleTargets->"MultipleTargets"
            | MultipleAttributeTargets->"MultipleAttributeTargets"
            | MultipleAnnotationTargets->"MultipleAnnotationTargets"
            | MultipleJoinTargets->"MultipleJoinTargets"
    [<NoComparison>]
    type IndentLevelComparisons =
        | IndentIsSameAsPreviousIndent
        | IndentIsLessThanPreviousIndent
        | IndentIsMoreThanPreviousIndent
         static member ToList() =
            [IndentIsSameAsPreviousIndent; IndentIsLessThanPreviousIndent; IndentIsMoreThanPreviousIndent]
         override self.ToString() =
          match self with
            | IndentIsSameAsPreviousIndent->"IndentIsSameAsPreviousIndent"
            | IndentIsLessThanPreviousIndent->"IndentIsLessThanPreviousIndent"
            | IndentIsMoreThanPreviousIndent->"IndentIsMoreThanPreviousIndent"
    [<NoComparison>]
    type CompilerState =
        {
            WaitingFor:CompilerWaitingFor
            LastFileNameProcessed:string
            TagValueList:(string*string) list
            LastCompilerOperation:LastCompilerOperations
            LastJoinType:ModelJoin option
            CurrentIndentLevel:int
            IndentLevelChange:IndentLevelComparisons
        }
    let defaultCompilerState=
        {
            WaitingFor=CompilerWaitingFor.Nothing
            LastFileNameProcessed=""
            TagValueList=[]
            LastCompilerOperation=LastCompilerOperations.PointerReset
            LastJoinType=option.None
            CurrentIndentLevel=0
            IndentLevelChange=IndentLevelComparisons.IndentIsSameAsPreviousIndent
        }
    [<NoComparison>]
    type CompilerReturn = 
        {
            CompilerState:CompilerState
            CurrentLocation:ModelLocationPointer
            //CompilerWaitingForState:CompilerWaitingFor
            CompilerMessages:CompilerMessage []
            ModelItems:ModelItem2 []
        }
    let beginningCompilerStatus =
        {
            CompilerState=defaultCompilerState
            CurrentLocation=defaultModelLocationPointer
            //CompilerWaitingForState=CompilerWaitingFor.Nothing
            CompilerMessages=[||]
            ModelItems= [|defaultModelItem2|]
        }
    [<NoComparison>]
    type IncomingFileProcessingStatus =
        {
            FileNumber:int
            IncomingRawLineCount:int
            IncomingLineCountWithEmptyLinesStripped:int
            IncomingLinesConcatenated:IncomingLine []
            CompilerReturn:CompilerReturn
        }

//// MIXED TYPES
//    type SVGEntityBox =
//        {
//            xPos:int
//            yPos:int
//            width:int
//            height:int
//            Entity:Entity
//        }

//    type VertexData<'V> =
//        int (* identifier *) *
//        'V (* vertex data *)

//    type EdgeData<'E> =
//        int (* identifier *) *
//        int (* priority *) *
//        int (* vertex target *) *
//        'E (* edge data *)

//    (* The graph uses adjacency list notation *)
//    type Adjacency<'E> = EdgeData<'E> list

//    (* The Vertex type represents the internal structure
//        of the graph *)
//    type Vertex<'V, 'E> = VertexData<'V> * Adjacency<'E>

//    (* A Graph is a Vertex list.  The nextNode allows for
//        consistent addressing of nodes *)
//    type Graph<'V, 'E> =
//        int (* nextNode identifier *) *
//        Vertex<'V, 'E> list

//    (* Empty graph construction *)
//    let empty: Graph<_,_> = (0, [])

//    (* Helper methods for getting the data from a Vertex *)
//    let vertexId (v:Vertex<_,_>) = v |> fst |> fst
//    let vertexData (v:Vertex<_,_>) = v |> fst |> snd
//    (* Helper methods for getting the data from an Edge *)
//    let edgeId ((x,_,_,_):EdgeData<_>) = x
//    let edgePriority ((_,x,_,_):EdgeData<_>) = x
//    let edgeTarget ((_,_,x,_):EdgeData<_>) = x
//    let edgeData ((_,_,_,x):EdgeData<_>) = x

//    (* Getting a vertex from a graph by id *)
//    let getVertex v (g:Graph<_, _>) : Vertex<_,_> =
//        snd g |> List.find (fun V -> vertexId V = v)
//    (* Getting all edges from a graph by a vertex id *)
//    let getEdges v (g:Graph<_, _>) =
//        g |> getVertex v |> snd

//    (* Add a new vertex *)
//    let addVertex (v:'V) (g:Graph<'V, _>)
//        : (int*Graph<'V,_>) =
//            let id = fst g
//            let s = snd g
//            let newVD : VertexData<_> = (id, v)
//            let newA : Adjacency<_> = []
//            let newV = (newVD, newA)
//            (id, (id + 1, newV::s))

//    (* Add a new edge.  Edges include a priority value *)
//    let addEdge priority
//        (v:int) (v':int) (e:'E) (g:Graph<'V, 'E>)
//        : (int*Graph<'V,'E>) =
//            let id = fst g
//            let s = snd g
//            let newE : EdgeData<_> = (id, priority, v', e)
//            (id,
//                (id + 1,
//                    s |> List.map (fun V ->
//                    if (vertexId V) = v then
//                        (fst V, newE::(snd V))
//                    else V)))

//    (* The edges aren't sorted by default so this function
//        sorts them by priority *)
//    let sortEdges (a:Adjacency<_>) =
//        a |> List.sortBy edgePriority

//    (* Removes an edge from a graph by id *)
//    let removeEdge (id:int) (g:Graph<_,_>)
//        : Graph<_,_> =
//            let next = fst g
//            let s = snd g
//            (next, s |> List.map ( fun (v, a) ->
//            (v, a |> 
//                List.filter (fun x -> (edgeId x) <> id)))) 

//    (* Removes a vertex from a graph by id and removes
//        any related edges *)
//    let removeVertex (id:int) (g:Graph<_,_>) 
//        : Graph<_,_> =
//            let next = fst g
//            let s = snd g
//            (next, s |> ([] |> List.fold (fun s' (v, a) ->
//            if (fst v) = id then s'
//            else
//                let f = fun x -> ((edgeTarget x) <> id)
//                let newA = a |> List.filter f
//                let newV = (v, newA)
//                newV::s')))        