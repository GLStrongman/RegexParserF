// Read arguments in and open files
let args = fsi.CommandLineArgs

if args.Length < 3 then
    failwith "ERROR: Too few arguments"
else if args.Length > 3 then
    failwith "ERROR: Too many arguments"

let exprFile = System.IO.File.ReadLines(args.[1])
let targFile = System.IO.File.ReadLines(args.[2])

// Exception to handle syntax errors
exception SyntaxError

// Defining regex sybmol types and their format
type Expr =
    | Lit of string //num
    | Star of Expr //mult
    | Pipe of Expr * Expr //neg
    | Parens of Expr

// Print results to the console in the correct format
let printRes e t f =
    match f with
    | true -> printfn "YES: %s with %s" e t
    | false -> printfn "NO:  %s with %s" e t

let printSyntax e t = printfn "SYNTAX ERROR: %s with %s" e t

let printError e t = printfn "ERROR: %s with %s" e t

// Check whether character is a literal or regex symbol
let isLit c =
    match c with
    | ')'
    | '('
    | '*'
    | '|' -> false
    | _ -> true

// Pull a block of literals out of a given string, returns an expression
let parseLit (s: string) =
    let chars = Seq.takeWhile isLit s
    let value = (s.Substring(0, Seq.length chars))
    Lit value, s.Substring(Seq.length chars)

// Recursively parse a given string to return expressions
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

// Recursively parses string into parenthesis expression
and parseParens (s: string) =
    let body, rest = parseExpr <| s.Substring 1
    Parens body, rest

// Controller method, recursively calls other parser methods
and parseSingle (expr: string) =
    match Seq.head expr with
    | '(' -> parseParens expr
    | '*' -> raise SyntaxError //invalid * placement
    | _ -> parseLit expr

// Compare parsed string to target, evaluates whether they match
let compareLit (s: string) (targ: string) =
    if s.Length <> targ.Length then
        false
    else
        let mutable flag = true
        // Comparison loop that handles dots and literals
        for i in 0 .. s.Length - 1 do
            if (s.[i] <> '.') && (s.[i] <> targ.[i]) then
                flag <- false
        flag

// Recursively evaluates parsed string against target, controller method for evaluation
let rec evaluate parsed expr targ =
    match parsed with
    | Lit s -> compareLit s targ
    | Star e -> evaluate e expr targ
    | Pipe (a, b) -> (evaluate a expr targ || evaluate b expr targ)
    | Parens body -> evaluate body expr targ

// Main program method, pre-processes strings and calls parser and evaluate methods
let regexParser (expr_str: string) (targ:string) =
    let mutable expr = expr_str
    let mutable result = false
    let count x = Seq.filter ((=) x) >> Seq.length

    // Check that all brackets have a match
    if (expr |> count '(') <> (expr |> count ')') then
        raise SyntaxError
    
    // Get rid of unnecessary brackets where there is no other regex symbol 
    if (expr |> count '|' <= 0)
       && (expr |> count '*' <= 0) then
        let mutable str = ""

        for char in expr do
            if (char <> '(' && char <> ')') then
                str <- str + string char 
            else
                str <- str
        expr <- str
    
    // Call parser and evaluation methods
    let parsed = fst (parseExpr expr)
    result <- evaluate parsed expr targ
    printRes expr targ result

// Wrap main method for error handling
let main expr targ =
    try
        regexParser expr targ
    with
    | SyntaxError as se -> printSyntax expr targ
    | error -> printfn "@ %s with %s, error:%s" expr targ (error.ToString())
    
// Call main method, iterating over the expressions and target file simultaneously
Seq.iter2 main exprFile targFile
