type t = {
    input : string;
    pos : int;
    ch : char option;
}
[@@deriving show]

let init input =
    if String.length (input) == 0 
    then { input = input; pos = 0; ch = None }
    else { input = input; pos = 0; ch = Some (String.get input 0) }

let is_alpha ch = match ch with
    | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false

let is_digit ch = match ch with
    | '0' .. '9' -> true
    | _ -> false

let is_whitespace ch = match ch with
    | ' ' | '\t' -> true
    | _ -> false

let is_identifier ch = is_alpha ch || ch == '_'

let scan_ended lexer = lexer.pos >= String.length lexer.input - 1

let advance lexer =
    let next_pos = lexer.pos + 1 in
    if
        scan_ended lexer
    then
        { lexer with pos = next_pos; ch = None }
    else
        let next_ch = String.get lexer.input next_pos in
        { lexer with pos = next_pos; ch = Some(next_ch) }

let peek lexer =
    if
        scan_ended lexer
    then
        None
    else
        let peeked_ch = String.get lexer.input (lexer.pos + 1) in
        Some(peeked_ch)

let seek lexer predicate =
    let rec aux lexer =
        if predicate (lexer.ch)
        then aux (advance lexer)
        else lexer, lexer.pos
    in aux lexer

let skip_whitespace lexer =
    let lexer, _ = seek lexer (fun ch ->
        match ch with
        | Some ch -> is_whitespace ch
        | None -> false)
    in lexer

let read_while lexer predicate =
    let pos_start = lexer.pos in
    let lexer, pos_end = seek lexer (fun ch ->
        match ch with
        | Some ch -> predicate ch
        | None -> false)
    in lexer, String.sub lexer.input pos_start (pos_end - pos_start)

let read_immediate lexer =
    let lexer, immediate = read_while lexer is_digit in
    lexer, Token.Immediate (immediate)

let read_identifier lexer =
    let lexer, identifier = read_while lexer is_identifier in
    match peek lexer with
    | Some ':' -> advance lexer, Token.Label (identifier)
    | _ -> lexer, Token.Instruction (identifier)

let read_register lexer =
    let lexer, register = read_while lexer (fun ch -> ch == 'r') in
    let lexer, number = read_while lexer is_digit in
    lexer, Token.Register (register ^ number)

let next lexer =
    let open Token in
    let lexer = skip_whitespace lexer in
    match lexer.ch with
    | None -> lexer, None
    | Some ch ->
        let lexer, token = match ch with
        | ',' -> advance lexer, Comma
        | '\n' -> advance lexer, Newline
        | '(' -> advance lexer, LeftParen
        | ')' -> advance lexer, RightParen
        | '$' -> read_register (advance lexer)
        | ch when is_digit ch -> read_immediate lexer
        | ch when is_identifier ch -> read_identifier lexer
        | _ -> advance lexer, Illegal
        in lexer, Some(token)
