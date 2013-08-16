open FParsec

type AstNode =
    | Identifier of string
    | Function of string * AstNode
    | Application of AstNode * AstNode

let parser =
    let mainParser, mainParserRef = createParserForwardedToRef<AstNode, unit> ()
    let pstringspaces str = pstring str .>> spaces
    let identifier = many1Satisfy2L isLetter (fun c -> isLetter c || isDigit c) "identifier" .>> spaces
    let id = identifier |>> (fun i -> Identifier(i))
    let func = pipe2 (pstringspaces "\\" >>. identifier) (pstringspaces "." >>. mainParser) (fun l r -> Function(l, r))
    let paren = between (pstringspaces "(") (pstringspaces ")") mainParser
    mainParserRef := many (choice [paren; id; func]) |>> (List.reduce (fun l r -> Application(l, r)))
    spaces >>. mainParser .>> eof

let rec print = function
    | Identifier id -> id
    | Function (id, node) -> "\\" + id + ".(" + print node + ")"
    | Application (left, right) -> "(" + print left + ") (" + print right + ")"

let rec rename variable newname = function
    | Identifier id -> if id = variable then Identifier newname else Identifier id
    | Function (id, node) ->
        let newid = if id = variable then newname else id
        Function (newid, rename variable newname node)
    | Application (left, right) -> Application (rename variable newname left, rename variable newname right)

let rec namesin = function
    | Identifier id -> [ id ]
    | Function (id, node) -> id :: namesin node
    | Application (left, right) -> namesin left @ namesin right

let rec renameIfContains variable list =
    if List.exists (fun item -> item = variable) list then renameIfContains (variable + "_") list else variable

let rec replace variable value = function
    | Identifier id ->
        let newId = renameIfContains id (namesin value)
        if newId = variable then value else Identifier newId
    | Function (id, node) ->
        let newnode = if id = variable then rename id (id + "_") node else node
        Function (id, replace variable value newnode)
    | Application (left, right) ->
        Application (replace variable value left, replace variable value right)

let rec beta = function
    | Identifier id -> Identifier id
    | Function (id, node) -> Function (id, beta node)
    | Application (left, right) ->
        match left with
        | Identifier id -> Application (left, beta right)
        | Application (l, r) -> Application (beta left, beta right)
        | Function (id, node) -> replace id right node

let rec solve tree =
    printfn "%s" (print tree)
    let newtree = beta tree
    if newtree <> tree then solve newtree else newtree

while true do
    System.Console.Write "> "
    let parsed = run parser (System.Console.ReadLine())
    match parsed with
    | Success(result, _, _) -> printfn "%s" (print (solve result))
    | Failure(errorMsg, _, _) -> printfn "Syntax error: %s" errorMsg
