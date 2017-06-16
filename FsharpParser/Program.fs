open System
open System.IO

type Result<'a> =
    | Success of 'a
    | Failure of string

type Parser<'a> = Parser of (char list -> Result<'a * char list>)

let run parser input = 
    let (Parser innerFn) = parser
    innerFn input

let stringToCharList str =
    List.ofSeq str

let pChar charToMatch = 
    let innerFn inputChars = 
        match inputChars with 
        | c :: remainingChars -> 
            if c = charToMatch then Success (c, remainingChars)
            else Failure (sprintf "Expected an %c. Got %c" charToMatch c)
        | [] ->
            Failure "Expected a string"

    Parser innerFn

let orElse parser1 parser2 = 
    let innerFn input =
        match run parser1 input with
        | Success result -> Success result
        | Failure _ -> run parser2 input
    Parser innerFn

let andThen parser1 parser2 = 
    let innerFn input = 
        match run parser1 input with 
        | Failure err -> Failure err
        | Success (value1, remaining1) -> 
            match run parser2 remaining1 with 
            | Failure err -> Failure err
            | Success (value2, remaining2) -> 
                let newValue = (value1, value2)
                Success (newValue, remaining2)
    Parser innerFn

let choice parserList = 
    parserList
    |> List.reduce orElse 

let anyOf listOfChar =
    listOfChar
    |> List.map pChar
    |> choice


let mapP f parser = 
    let innerFn input =
        match run parser input with
        | Success (value, remaining) ->
            let newValue = f value
            Success (newValue, remaining)

        | Failure err -> 
            Failure err
    Parser innerFn

let returnP x = 
    let innerFn input =
        Success (x, input)
    Parser innerFn




let ( .>>. ) = andThen
let ( <|> ) = orElse
let ( <!> ) = mapP
let ( |>> ) x f = mapP f x

let applyP fP xP = 
    (fP .>>. xP)
    |> mapP (fun (f,x) -> f x)


let ( <*> ) = applyP

   


let lift2 f xP yP =
    returnP f <*> xP <*> yP

let addP =
    lift2 (+)

let startsWith (str:string) prefix =
    str.StartsWith(prefix)

let startsWithP = 
    lift2 startsWith

let rec sequence parserList = 
    let cons head tail = head::tail

    let consP = lift2 cons


    match parserList with
    | [] ->
        returnP []
    | head::tail ->
        consP head (sequence tail)


let parsers = [pChar 'A'; pChar 'B'; pChar 'C'];
let combined = sequence parsers;

stringToCharList "ABCABC"
|> run combined
|> printfn "%A" 

//helper to create a string from a list fo chars
let charListToStr charList = 
    String(List.toArray charList)

//match as specific string
let pstring str = 
    str
    //Convert str to charlist
    |> List.ofSeq
    //Map each char to pChar
    |> List.map pChar
    //Convert to Parser<char List>
    |> sequence
    //Convert Parser<char List> to Parser<String>
    |> mapP charListToStr
  


let parseDaan = pstring "Daan"

stringToCharList "Daan"
|> run parseDaan
|> printfn "%A"

let rec parseZeroOrMore parser input =
    let firstResult = run parser input

    match firstResult with 
    | Failure err -> 
        ([], input)
    | Success (firstValue, inputAfterFirstParse) ->
        //if then parse succeeds, call recursively
        let (subsequentValues, remainingInput) = 
            parseZeroOrMore parser inputAfterFirstParse

        let values = firstValue::subsequentValues
        (values, remainingInput)
 
let many parser = 
    let rec innerFn input = 
        Success(parseZeroOrMore parser input)

    Parser innerFn

let manyA = many (pChar 'a')

stringToCharList "aaan" 
|> run manyA
|> printfn "%A"

let parseDigit = 
    anyOf ['0'..'9']
   
let parseChar = 
    anyOf ['a'..'z']



let parseFourDigitAsStr = 
    (parseDigit .>>. parseDigit .>>. parseDigit .>>. parseDigit)
    |>> fun (((c1, c2), c3), c4) -> String [| c1; c2; c3; c4 |]

let parseThreeDigitAsInt = 
    mapP int parseFourDigitAsStr 



 
stringToCharList "lol"
|> run (parseChar)
|> printfn "%A"

stringToCharList "1234asd"
|> run parseThreeDigitAsInt
|> printfn "%A"

stringToCharList "1234asd"
|> run parseFourDigitAsStr
|> printfn "%A"


stringToCharList "test"
|> run (pChar 't')
|> printfn "%A"

