type Expr =
    | Num of int
    | Plus of Expr * Expr
    | Mult of Expr * Expr
    | Parens of Expr
    | Negative of Expr

let isDigit c = c >= '0' && c <= '9'
let digitToInt c = int c - int '0'

exception SyntaxError

let parseInt (s: string) =
    let digits = Seq.takeWhile isDigit s
    let mutable value = 0

    for d in digits do
        value <- value * 10 + digitToInt d

    Num value, s.Substring(Seq.length digits)


let rec parseExpr (s: string) =
    let v, rest = parseSingle s

    if rest = "" then
        v, rest
    else
        let o = Seq.head rest

        match o with
        | '+' ->
            let v2, rest = parseExpr <| rest.Substring 1
            Plus(v, v2), rest
        | '*' ->
            let v2, rest = parseExpr <| rest.Substring 1
            Mult(v, v2), rest
        | _ -> v, rest

and parseParens (s: string) =
    let body, rest = parseExpr <| s.Substring 1

    if rest.StartsWith ")" then
        Parens body, rest.Substring 1
    else
        raise SyntaxError

and parseSingle (s: string) =
    match Seq.head s with
    | '0'
    | '1'
    | '2'
    | '3'
    | '4'
    | '5'
    | '6'
    | '7'
    | '8'
    | '9' -> parseInt s
    | '(' -> parseParens s
    | '-' -> parseNegative s
    | _ -> raise SyntaxError

and parseNegative s =
    let v, rest = parseSingle <| s.Substring 1
    Negative v, rest

//printfn "%A" (parseExpr "((123+456)*789)+10000")

let r = parseExpr "1+(2*3)+-4"
let x = fst r

let rec evaluate e =
    match e with
    | Num i -> i
    | Plus (a, b) -> evaluate a + evaluate b
    | Mult (a, b) -> evaluate a * evaluate b
    | Parens body -> evaluate body
    | Negative value -> 0 - evaluate value

//printfn "result is %d" <| evaluate x


let calc = parseExpr >> fst >> evaluate

let prettyCalc s = calc s |> printfn "%s = %d" s

// prettyCalc "0

try
    Seq.iter prettyCalc
    <| Seq.skip 1 (fsi.CommandLineArgs)
with SyntaxError -> printfn "syntax error"
