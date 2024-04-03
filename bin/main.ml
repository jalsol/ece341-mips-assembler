open Jalmas

let code =
    let rec read_lines () =
        try
            let line = read_line () in
            line ^ "\n" ^ read_lines ()
        with
        | End_of_file -> ""
    in read_lines ()

(* let () = *)
(*     let lexer = Lexer.init code in *)
(*     let rec loop lexer = *)
(*         let next_lexer, token = Lexer.next lexer in *)
(*         match token with *)
(*         | None -> ( *)
(*             Printf.printf "End of file\n"; *)
(*             ()) *)
(*         | Some Illegal -> ( *)
(*             Printf.printf "Illegal token\n"; *)
(*             ()) *)
(*         | Some token -> ( *)
(*             Printf.printf "%s\n" (Token.to_string token); *)
(*             loop next_lexer) *)
(*     in loop lexer *)

let () =
    let parser = Parser.init code in
    let _parser = Parser.parse_instruction parser in
    ()
