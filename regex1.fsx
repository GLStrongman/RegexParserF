let args = fsi.CommandLineArgs

let checkArgs =
    if args.Length < 3 then
        failwith "ERROR: Too few arguments"
    else if args.Length > 3 then
        failwith "ERROR: Too many arguments"

checkArgs
exception SyntaxError

let exprFile = System.IO.File.ReadLines(args.[1])
let targFile = System.IO.File.ReadLines(args.[2])

type Expr =
    | Lit of string //num
    | Star of Expr //mult
    | Pipe of Expr * Expr //neg
    | Parens of Expr

let printRes e t f =
    match f with
    | true -> printfn "YES: %s with %s" e t
    | false -> printfn "NO:  %s with %s" e t

let printSyntax e t = printfn "SYNTAX ERROR: %s with %s" e t

let printError e t = printfn "ERROR: %s with %s" e t

// let rec evaluate e =
//     match e with
//     | Lit i -> i
//     | Dot (a, b) -> evaluate a + evaluate b
//     | Mult (a, b) -> evaluate a * evaluate b
//     | Parens body -> evaluate body
//     | Negative value -> 0 - evaluate value

let isLit c =
    match c with
    | ')'
    | '('
    | '*'
    | '|' -> false
    | _ -> true


let parseLit (s: string) =
    let chars = Seq.takeWhile isLit s
    let value = (s.Substring(0, Seq.length chars))
    Lit value, s.Substring(Seq.length chars)

let rec parseExpr (expr: string) =
    if expr = "" then
        Lit expr, expr
    else
        let v, rest = parseSingle expr

        if rest = "" then
            v, rest
        else
            let o = Seq.head rest

            match o with
            | '|' ->
                let v2, rest = parseExpr <| rest.Substring 1
                Pipe(v, v2), rest
            | '*' ->
                let v2, rest = parseExpr <| rest.Substring 1
                Star(v), rest
            | _ -> v, rest

and parseParens (s: string) =
    let body, rest = parseExpr <| s.Substring 1

    if rest.StartsWith ")" then
        Parens body, rest.Substring 1
    else
        raise SyntaxError

and parseSingle (expr: string) =
    match Seq.head expr with
    | '(' -> parseParens expr
    | '*' -> raise SyntaxError
    | _ -> parseLit expr

let compareLit (s: string) (targ: string) =
    if s.Length <> targ.Length then
        false
    else
        let mutable flag = true

        for i in 0 .. s.Length - 1 do
            if (s.[i] <> '.') && (s.[i] <> targ.[i]) then
                flag <- false

        flag



let rec evaluate parsed expr targ =
    match parsed with
    | Lit s -> compareLit s targ
    | Star e -> evaluate e expr targ
    | Pipe (a, b) -> (evaluate a expr targ || evaluate b expr targ)
    | Parens body -> evaluate body expr targ

let regexParser (e: string) targ =
    let mutable expr = e
    let count x = Seq.filter ((=) x) >> Seq.length

    if (expr |> count '(') <> (expr |> count ')') then
        raise SyntaxError

    if (expr |> count '|' <= 0)
       && (expr |> count '*' <= 0) then
        let mutable s = ""

        for c in expr do
            if (c <> '(' && c <> ')') then
                s <- s + string c 
            else
                s <- s
        expr <- s

    let parsed = fst (parseExpr expr)
    let result = evaluate parsed expr targ
    printRes expr targ result


let main expr targ =
    try
        regexParser expr targ
    with
    | SyntaxError -> printSyntax expr targ
    | error -> printfn "@ %s with %s, error:%s" expr targ (error.ToString())
// Call regex parser method, iterating over the expressions and
// target file simultaneously
Seq.iter2 main exprFile targFile
