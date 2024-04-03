(* Directives are ignored for now *)

type t =
    | Illegal
    | Instruction of string
    | Register of int
    | Immediate of int
    | Label of string
    | Comma
    | Newline
    | LeftParen
    | RightParen

let to_string = function
    | Illegal -> "Illegal"
    | Instruction s -> "Instruction " ^ s
    | Register i -> "Register " ^ Printf.sprintf "%d" i
    | Immediate i -> "Immediate " ^ "0x" ^ Printf.sprintf "%x" i
    | Label s -> "Label " ^ s
    | Comma -> "Comma"
    | Newline -> "Newline"
    | LeftParen -> "LeftParen"
    | RightParen -> "RightParen"
