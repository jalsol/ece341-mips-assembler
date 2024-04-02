(* Directives are ignored for now *)

type t =
    | Illegal
    | Instruction of string
    | Register of string
    | Immediate of (int * string)
    | Label of string
    | Comma
    | Newline
    | LeftParen
    | RightParen

let to_string = function
    | Illegal -> "Illegal"
    | Instruction s -> "Instruction " ^ s
    | Register s -> "Register " ^ s
    | Immediate (base, s) -> "Immediate " ^ (string_of_int base) ^ " " ^ s
    | Label s -> "Label " ^ s
    | Comma -> "Comma"
    | Newline -> "Newline"
    | LeftParen -> "LeftParen"
    | RightParen -> "RightParen"
