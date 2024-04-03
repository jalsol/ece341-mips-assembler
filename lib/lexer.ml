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

let is_hex_digit ch = is_digit ch || (match ch with
    | 'A' .. 'F' | 'a' .. 'f' -> true
    | _ -> false)

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
        { lexer with pos = next_pos; ch = Some next_ch }

(* let peek lexer = *)
(*     if *)
(*         scan_ended lexer *)
(*     then *)
(*         None *)
(*     else *)
(*         let peeked_ch = String.get lexer.input (lexer.pos + 1) in *)
(*         Some peeked_ch *)

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

let read_number lexer =
    let lexer, number = read_while lexer is_digit in
    lexer, Token.Immediate (int_of_string number)

let read_number_base lexer =
    let lexer = advance lexer in
    match lexer.ch with
    | None -> lexer, Token.Immediate (0)
    | Some 'x' ->
        let base = "x" in
        let lexer = advance lexer in
        let lexer, number = read_while lexer is_hex_digit in
        lexer, Token.Immediate ("0" ^ base ^ number |> int_of_string)
    | Some ch ->
        let base = ch |> String.make 1 in
        let lexer = advance lexer in
        let lexer, number = read_while lexer is_digit in
        lexer, Token.Immediate ("0" ^ base ^ number |> int_of_string)

let read_identifier lexer =
    let lexer, identifier = read_while lexer is_identifier in
    match lexer.ch with
    | Some ':' -> advance lexer, Token.Label identifier
    | _ -> lexer, Token.Instruction identifier

let read_register lexer =
    let lexer, number = read_while lexer is_digit in
    lexer, Token.Register (int_of_string number)

let next lexer =
    let open Token in
    let lexer = skip_whitespace lexer in
    (* let _ = Printf.printf "pos: %d\n" lexer.pos in *)
    match lexer.ch with
    | None -> lexer, None
    | Some ch ->
        let lexer, token = match ch with
        | ',' -> advance lexer, Comma
        | '\n' -> advance lexer, Newline
        | '(' -> advance lexer, LeftParen
        | ')' -> advance lexer, RightParen
        | 'r' -> read_register (advance lexer)
        | '0' -> read_number_base lexer
        | ch when is_digit ch -> read_number lexer
        | ch when is_identifier ch -> read_identifier lexer
        | _ -> advance lexer, Illegal
        in lexer, Some(token)
