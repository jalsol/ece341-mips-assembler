open MIPS_asm

let code =
"li $r20, 0x1234
li $r2, 3
add $r3, $r20, $r2
add $r4, $r3, $r3
add $r5, $r4, $r4
addiu $r2, $r2, 33
j 0x008"

let () =
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
