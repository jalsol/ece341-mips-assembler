open MIPS_asm

let () =
    let code = "a:
ldr $r0, 2
" in
    let lexer = Lexer.init code in
    let rec loop lexer =
        let next_lexer, token = Lexer.next lexer in
        match token with
        | None -> (
            Printf.printf "End of file\n";
            ())
        | Some Illegal -> (
            Printf.printf "Illegal token\n";
            ())
        | Some token -> (
            Printf.printf "%s\n" (Token.to_string token);
            loop next_lexer)
    in loop lexer
