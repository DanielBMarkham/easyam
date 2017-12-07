// types (and type helpers) for the easyam program
module SAModel
    open Types

    let prependToDelimitedList (prependString:string) (currentString:string) (newStringItem:string) =
        let prepend = if currentString.Length=0 || (currentString.GetRight 1) = prependString
                        then ""
                        else prependString.ToString()
        if newStringItem.Length=0 then currentString else
            (currentString + prepend + newStringItem)


    type TokenType =
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
    type TokenTargetType =
        |SINGLE_TARGET
        |MULTIPLE_TARGETS
         static member ToList() =
            [SINGLE_TARGET;MULTIPLE_TARGETS]
         override self.ToString() =
          match self with
            | SINGLE_TARGET->"Single Target"
            | MULTIPLE_TARGETS->"Multiple Targets"
    type TokenCategory = 
        |MISC
        |SHORTCUT
        |BUCKETS
        |GENRE
        |TEMPORAL
        |ABSTRACTION_LEVEL
        |HDD
        |NAMESPACE
        |CONNECTIVE
        |ATTRIBUTE
        |TAG
    [<NoComparison>]
    type EASYAM_Token =
        {
            Type:TokenType
            TargetType:TokenTargetType
            Category:TokenCategory
            Token:string
        }
    type AnnotationTokenType =
        | None
        | Note
        | Question
        | ToDo
        | Work
        | Diagram
        | Code
        | Defect
         static member ToList() =
            [None;Note;Question;ToDo;Work;Diagram;Code]
         override self.ToString() =
          match self with
            | None->"None"
            | Note->"Note"
            | Question->"Question"
            | ToDo->"ToDo"
            | Work->"Work"
            | Diagram->"Diagram"
            | Code->"Code"
            | Defect->"Defect"
        member self.ToModelOutputSectionHeading(outputType:ModelOutputType) =
            match outputType with 
                | AMOUT | TEXT ->
                    self.ToString().ToUpper() + ":"
                | HTML->"<div class='annotationTitle'>" + self.ToString() + "</div> <!-- annotationTitle -->"
                | CSV->
                    self.ToString().ToUpper()
                | GHERKIN->""

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
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="CODE:"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="DEFECTS:"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="DEFECTS"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="DEFECT:"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="DEFECT"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="BUGS:"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="BUGS"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="BUG:"};
            {Type=RELATIVE_LOCATOR;     TargetType=SINGLE_TARGET;        Category=MISC;                 Token="BUG"};

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

            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=TAG;           Token="@[^@|^&]+"}; //[^\\s\"']+|\"([^\"]*)\"|'([^']*)'[^\s]
            {Type=ABSOLUTE_LOCATOR;     TargetType=MULTIPLE_TARGETS;     Category=TAG;           Token="&[^@|^&]+"};

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
    let printCompilerMessages (compilerMessages:CompilerMessage []) (printAsEASYAMComments:bool) =
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
            if printAsEASYAMComments=false
                then System.Console.WriteLine(formattedMessage)
                else System.Console.WriteLine("// " + formattedMessage)
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
            | Trigger->"WHEN"
            | Actor->"ASA"
            | Goal->"INEEDTO"
            | BusinessContext->"SOTHAT"
            | Scenario->"SCENARIO"
            | Contains->"CONTAINS"
            | Because->"BECAUSE"
            | Whenever->"WHENEVER"
            | ItHasToBeThat->"ITHASTOBETHAT"
        member self.ToModelOutputSectionHeading(outputType:ModelOutputType) =
            match outputType with 
                | AMOUT | TEXT ->
                    self.ToString().ToUpper() + ":"
                | HTML->"<span class='modelAttributeDescription'>" + self.ToString() + "</span> <!-- modelAttributeDescription -->"
                | CSV->
                    self.ToString().ToUpper()
                | GHERKIN->""
    [<CustomEquality;CustomComparison>]
    type ModelItemAnnotation =
        {
            AnnotationType:AnnotationTokenType
            AnnotationText:string
        }
        override x.GetHashCode()=hash x
        override x.Equals(yobj)=
            match yobj with 
                | :? ModelItemAnnotation as y->(x=y)
                |_->false
        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with 
                    | :? ModelItemAnnotation as y->compare (x.AnnotationType, x.AnnotationText) (y.AnnotationType, y.AnnotationText)
                    |_-> invalidArg "yobj" "cannot compare value of different types"

    [<CustomEquality;CustomComparison>]
    type ModelItemAttribute =
        {
            id:int
            ModelItemParentId:int
            AttributeType:ModelAttributeTypes
            Description:string
            Annotations:ModelItemAnnotation[]
            //Annotations:(ANNOTATION_TOKEN_TYPE*string) []
            SourceReferences:IncomingLine []
        }
        override x.GetHashCode()=hash (x.id,x.ModelItemParentId,x.AttributeType,x.Description)
        override x.Equals(yobj)=
            match yobj with 
                | :? ModelItemAttribute as y->(x.id=y.id && x.ModelItemParentId=y.ModelItemParentId && x.AttributeType=y.AttributeType && x.Description = y.Description)
                |_->false
        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with 
                    | :? ModelItemAttribute as y->compare (x.AttributeType, x.Description, x.Annotations) (y.AttributeType, y.Description, y.Annotations)
                    |_-> invalidArg "yobj" "cannot compare value of different types"
    let defaultModelItemAttribute =
        {
            id=(-1)
            ModelItemParentId=(-1)
            AttributeType=ModelAttributeTypes.Goal
            Description=""
            Annotations=[||]
            SourceReferences=[||]
        }

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
            | UsedBy->"UsedBy"
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
    [<CustomEquality;CustomComparison>]
    type ModelRelation =
        {
            id:int
            ModelJoinType:ModelJoin
            TargetId:int
            SourceReference:IncomingLine
        }
        override x.GetHashCode()=hash x
        override x.Equals(yobj)=
            match yobj with 
                | :? ModelRelation as y->
                    (
                           x.id=y.id
                        && x.ModelJoinType=y.ModelJoinType
                        && x.TargetId=y.TargetId
                    )
                |_->false
        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with 
                    | :? ModelRelation as y->
                        compare
                            (x.id, x.ModelJoinType, x.TargetId)
                            (y.id, y.ModelJoinType, y.TargetId)
                    |_-> invalidArg "yobj" "cannot compare value of different types"
    [<CustomEquality;CustomComparison>]
    type ModelLocationPointer =
        {
            Namespace:string

            CurrentId:int
            ParentId:int
            AttributeId:int option
            AttributeType:ModelAttributeTypes option

            RelationId:int option
            RelationType:ModelJoin option
            RelationSourceId:int option
            RelationTargetId:int option

            InHDDMode:bool
            Bucket:Buckets
            Genre:Genres
            AbstractionLevel:AbstractionLevels
            TemporalIndicator:TemporalIndicators
            AnnotationIndicator:AnnotationTokenType
            Tags:System.Collections.Generic.KeyValuePair<string,string>[]
        }
        override x.GetHashCode()=hash x
        override x.Equals(yobj)=
            match yobj with 
                | :? ModelLocationPointer as y->
                    (
                           x.Namespace=y.Namespace
                        && x.ParentId=y.ParentId
                        && x.AttributeType=y.AttributeType
                        && x.AttributeId=y.AttributeId
                        && x.RelationTargetId=y.RelationTargetId
                        && x.InHDDMode=y.InHDDMode
                        && x.Bucket=y.Bucket
                        && x.Genre=y.Genre
                        && x.AbstractionLevel=y.AbstractionLevel
                        && x.TemporalIndicator=y.TemporalIndicator
                        && x.AnnotationIndicator=y.AnnotationIndicator
                        && (Array.fold (&&) true (Array.zip x.Tags y.Tags |> Array.map(fun (aa,bb)->aa=bb)))
                    )
                |_->false
        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with 
                    | :? ModelLocationPointer as y->
                        compare
                            //(x.Namespace, x.Genre, x.Bucket, x.AbstractionLevel, x.TemporalIndicator, x.AttributeType, x.ParentId, x.AttributeId, x.RelationTargetId, x.InHDDMode, x.AnnotationIndicator)
                            //(y.Namespace, y.Genre, y.Bucket, y.AbstractionLevel, y.TemporalIndicator, y.AttributeType, y.ParentId, y.AttributeId, y.RelationTargetId, y.InHDDMode, y.AnnotationIndicator)
                            (x.Namespace, x.Genre, x.Bucket, x.AbstractionLevel, x.TemporalIndicator)
                            (y.Namespace, y.Genre, y.Bucket, y.AbstractionLevel, y.TemporalIndicator)
                    |_-> invalidArg "yobj" "cannot compare value of different types"
    let defaultModelLocationPointer =
        {
            Namespace=""
            CurrentId = -1
            ParentId = -1
            AttributeId=option.None
            AttributeType=option.None
            RelationId=option.None
            RelationType=option.None
            RelationSourceId=option.None
            RelationTargetId=option.None
            InHDDMode=false
            Bucket=Buckets.None
            Genre=Genres.None
            AbstractionLevel=AbstractionLevels.None
            TemporalIndicator=TemporalIndicators.None
            AnnotationIndicator=AnnotationTokenType.None
            Tags=[||]
        }
    let areTwoLocationsTheSame (loc1:ModelLocationPointer) (loc2:ModelLocationPointer) =
        loc1.Genre=loc2.Genre
        && loc1.Bucket=loc2.Bucket
        && loc1.AbstractionLevel=loc2.AbstractionLevel
        && loc1.TemporalIndicator=loc2.TemporalIndicator
    let areTwoAttributeListsEqual (list1:ModelItemAttribute[]) (list2:ModelItemAttribute[]) =
        if list1.Length<>list2.Length then false else
        let sorted1 = list1 |> Array.sortBy(fun x->x.Description)
        let sorted2 = list2 |> Array.sortBy(fun x->x.Description)
        (Array.fold (&&) true (Array.zip sorted1 sorted2 |> Array.map(fun (aa,bb)->aa.AttributeType=bb.AttributeType && aa.Description=bb.Description)))
    let areTwoAnnotationListsEqual (list1:ModelItemAnnotation[]) (list2:ModelItemAnnotation[]) =
        if list1.Length<>list2.Length then false else
        let sorted1 = list1 |> Array.sortBy(fun x->x.AnnotationType.ToString() + x.AnnotationText)
        let sorted2 = list2 |> Array.sortBy(fun x->x.AnnotationType.ToString() + x.AnnotationText)
        (Array.fold (&&) true (Array.zip sorted1 sorted2 |> Array.map(fun (aa,bb)->aa.AnnotationType=bb.AnnotationType && aa.AnnotationText=bb.AnnotationText)))
    let areTwoTagListsEqual (list1:System.Collections.Generic.KeyValuePair<string,string>[]) (list2:System.Collections.Generic.KeyValuePair<string,string>[]) =
        if list1.Length<>list2.Length then false else
        let sorted1 = list1 |> Array.sortBy(fun x->x.Key + x.Value)
        let sorted2 = list2 |> Array.sortBy(fun x->x.Key + x.Value)
        (Array.fold (&&) true (Array.zip sorted1 sorted2 |> Array.map(fun (aa,bb)->(aa.Key+aa.Value=bb.Key+bb.Value))))

    [<CustomEquality;CustomComparison>]
    [<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
    type ModelItem =
        {
            Id:int
            Location:ModelLocationPointer
            Description:string
            Attributes:ModelItemAttribute []
            Annotations:ModelItemAnnotation []
            SourceReferences:IncomingLine []
            Relations:ModelRelation []
            Tags:System.Collections.Generic.KeyValuePair<string,string>[]
        }
    with
        member private t.StructuredFormatDisplay = 
            if t.Id=(-1) then "ROOT"
            else
                t.Id.ToString() + ":" + t.Description + ":" + t.Location.Bucket.ToString() + "/" + t.Location.Genre.ToString() + "/" + t.Location.AbstractionLevel.ToString() + "/" + t.Location.TemporalIndicator.ToString()
        override x.GetHashCode()=hash x
        override x.Equals(yobj)=
            match yobj with 
                | :? ModelItem as y->
                    //(
                    //       x.Id=y.Id
                    //    && x.Location=y.Location
                    //    && x.Description=y.Description
                    //    && x.Attributes=y.Attributes
                    //    && x.Annotations=y.Annotations
                    //    && x.Relations=y.Relations
                    //    && x.Tags=y.Tags
                    //)
                    let areTheLocationsTheSame=areTwoLocationsTheSame x.Location y.Location
                    let areTheAttributeListsEqual=areTwoAttributeListsEqual x.Attributes y.Attributes
                    let areTheAnnotationListsEqual=areTwoAnnotationListsEqual x.Annotations y.Annotations
                    let areTheTwoTagListsEqual=areTwoTagListsEqual x.Tags y.Tags
                    (
                        areTheLocationsTheSame
                        && x.Description=y.Description
                        && areTheAttributeListsEqual
                        && areTheAnnotationListsEqual
                        //&& x.Relations=y.Relations // since we use ids, no way to check this
                        && areTheTwoTagListsEqual
                    )
                |_->false
        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with 
                    | :? ModelItem as y->
                        compare
                            (x.Location, x.Description, x.Attributes, x.Annotations, x.Relations)
                            (y.Location, y.Description, y.Attributes, y.Annotations, y.Relations)
                    |_-> invalidArg "yobj" "cannot compare value of different types"
    let defaultModelItem2:ModelItem =
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
    /// Flattened model -- used to indicate which items to use for reporting
    type ModelDetailItemType =
        |Everything
        |Item
        |AttributesALL
        |AttributesTrigger
        |AttributesActor
        |AttributesGoal
        |AttributesBusinessContext
        |AttributesScenario
        |AttributesContains
        |AttributesBecause
        |AttributesWhenever
        |AttributesItHasToBeThat
        |AttributeAnnotationsALL
        |AttributeAnnotationsNote
        |AttributeAnnotationsQuestion
        |AttributeAnnotationsToDo
        |AttributeAnnotationsWork
        |AttributeAnnotationsDiagram
        |AttributeAnnotationsCode
        |AttributeAnnotationsDefect
        |AnnotationsALL
        |AnnotationsNote
        |AnnotationsQuestion
        |AnnotationsToDo
        |AnnotationsWork
        |AnnotationsDiagram
        |AnnotationsCode
        |AnnotationsDefect
        |JoinsALL
        |JoinsParent
        |JoinsChild
        |JoinsAffects
        |JoinsAffectedBy
        |JoinsUses
        |JoinsUsedBy
        |JoinsHasA
        |JoinsIsOwnedByA
        |JoinsSourceReferencesALL
        |JoinsTagsALL
    type ModelDetailLevelFlags = ModelDetailItemType[]
    [<NoComparison>]
    type CompilerWaitingFor =
        |Nothing
        |MultipleModelItems
        |MultipleAttributes
        |MultipleAnnotations
        |MultipleAttributeAnnotations
        |MultipleRelations
         static member ToList() =
            [Nothing;MultipleModelItems;MultipleAttributes;MultipleAnnotations;MultipleAttributeAnnotations;MultipleRelations]
         override self.ToString() =
          match self with
            | Nothing->"Nothing"
            | MultipleModelItems->"MultipleModelItems"
            | MultipleAttributes->"MultipleAttributes"
            | MultipleAnnotations->"MultipleAnnotations"
            | MultipleAttributeAnnotations->"MultipleAttributeAnnotations"
            | MultipleRelations->"MultipleRelations"
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
            LocationStack:Types.Stack<ModelLocationPointer>
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
            //LocationStack=Types.Stack<ModelLocationPointer>.empty
            LocationStack = Types.Stack<ModelLocationPointer>.empty |>pushStack defaultModelLocationPointer
        }
    [<NoComparison>]
    type CompilerReturn = 
        {
            CompilerState:CompilerState
            CurrentLocation:ModelLocationPointer
            CompilerMessages:CompilerMessage []
            ModelItems:ModelItem []
        }
    let beginningCompilerStatus =
        {
            CompilerState=defaultCompilerState
            CurrentLocation=defaultModelLocationPointer
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


    type sortParameterType =
        {
           TagOrAtt:TagOrAtt
           Thing:string
           ConvertTo:ConvertTo
           Order:SortOrder
        }
    [<NoComparison>]
    type FilterParmeterType =
        {
            Genre:Genres
            Bucket:Buckets
            AbstractionLevel:AbstractionLevels
            TemporalIndicator:TemporalIndicators
            CheckValue:sortParameterType
            FromVal:string
            ToVal:string
        }
