type t = {
    lexer : Lexer.t;
    addr : int;
    label_table : (string, int) Hashtbl.t;
}

let init input =
    let lexer = Lexer.init input in
    { lexer; addr = 0x000; label_table = Hashtbl.create 0x10}

let parse_label parser =
    let old_lexer = parser.lexer in
    let rec loop parser =
        let lexer, token = Lexer.next parser.lexer in
        match token with
        | Some Token.Label label ->
            let _ = Hashtbl.add parser.label_table label parser.addr in
            (* let _ = Printf.printf "%s -> 0x%03x\n" label parser.addr in *)
            loop { parser with lexer; }
        | Some Instruction _ ->
            loop { parser with lexer; addr = parser.addr + 0x4; }
        | Some _ ->
            loop { parser with lexer; }
        | None ->
            { parser with lexer = old_lexer; addr = 0; }
    in loop parser

let get_label_addr parser label =
    match Hashtbl.find_opt parser.label_table label with
    | Some addr -> addr
    | None -> -1

let print_reg_opcode address ~f ~s ~t ~a ~d =
    let op0 = (s lsr 3) land 0xFF in
    let op1 = ((s lsl 5) lor t) land 0xFF in
    let op2 = ((d lsl 3) lor (a lsr 2)) land 0xFF in
    let op3 = ((a lsl 6) lor f) land 0xFF in
    Printf.printf "%03x: %02x %02x %02x %02x" address op0 op1 op2 op3

let print_imm_opcode address ~o ~s ~t ~i =
    let op0 = ((o lsl 2) lor (s lsr 3)) land 0xFF in
    let op1 = ((s lsl 5) lor t) land 0xFF in
    let op2 = (i lsr 8) land 0xFF in
    let op3 = i land 0xFF in
    Printf.printf "%03x: %02x %02x %02x %02x" address op0 op1 op2 op3

let print_jmp_opcode address ~o ~i =
    let op0 = ((o lsl 2) lor (i lsr 26)) land 0xFF in
    let op1 = (i lsr 16) land 0xFF in
    let op2 = (i lsr 8) land 0xFF in
    let op3 = i land 0xFF in
    Printf.printf "%03x: %02x %02x %02x %02x" address op0 op1 op2 op3

let add_instr parser =
    let error_msg = "Invalid syntax: add" in
    let lexer = parser.lexer in
    let lexer, d_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match d_token, s_token, t_token with
        | Some Token.Register d, Some Token.Register s, Some Token.Register t ->
            let _ = print_reg_opcode parser.addr ~f:0b100000 ~s ~t ~d ~a:0 in
            let _ = Printf.printf " // r%d = r%d + r%d (signed)\n" d s t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let addu_instr parser =
    let error_msg = "Invalid syntax: addu" in
    let lexer = parser.lexer in
    let lexer, d_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match d_token, s_token, t_token with
        | Some Token.Register d, Some Token.Register s, Some Token.Register t ->
            let _ = print_reg_opcode parser.addr ~f:0b100001 ~d ~s ~t ~a:0 in
            let _ = Printf.printf " // r%d = r%d + r%d (unsigned)\n" d s t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let addi_instr parser =
    let error_msg = "Invalid syntax: addi" in
    let lexer = parser.lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, i_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match t_token, s_token, i_token with
        | Some Token.Register t, Some Token.Register s, Some Token.Immediate i ->
            let _ = print_imm_opcode parser.addr ~o:0b001000 ~s ~t ~i in
            let _ = Printf.printf " // r%d = r%d + #%d (signed)\n" t s i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let addiu_instr parser =
    let error_msg = "Invalid syntax: addiu" in
    let lexer = parser.lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, i_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match t_token, s_token, i_token with
        | Some Token.Register t, Some Token.Register s, Some Token.Immediate i ->
            let _ = print_imm_opcode parser.addr ~o:0b001001 ~s ~t ~i in
            let _ = Printf.printf " // r%d = r%d + #%d (unsigned)\n" t s i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let and_instr parser =
    let error_msg = "Invalid syntax: and" in
    let lexer = parser.lexer in
    let lexer, d_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match d_token, s_token, t_token with
        | Some Token.Register d, Some Token.Register s, Some Token.Register t ->
            let _ = print_reg_opcode parser.addr ~f:0b100100 ~s ~t ~d ~a:0 in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let andi_instr parser =
    let error_msg = "Invalid syntax: andi" in
    let lexer = parser.lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, i_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match t_token, s_token, i_token with
        | Some Token.Register t, Some Token.Register s, Some Token.Immediate i ->
            let _ = print_imm_opcode parser.addr ~o:0b001100 ~s ~t ~i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let div_instr parser =
    let error_msg = "Invalid syntax: div" in
    let lexer = parser.lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    match comma with
    | Some Token.Comma -> (
        match s_token, t_token with
        | Some Token.Register s, Some Token.Register t ->
            let _ = Printf.printf "%03x: lo, hi = r%d / r%d\n" parser.addr s t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let divu_instr parser =
    let error_msg = "Invalid syntax: divu" in
    let lexer = parser.lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    match comma with
    | Some Token.Comma -> (
        match s_token, t_token with
        | Some Token.Register s, Some Token.Register t ->
            let _ = Printf.printf "%03x: lo, hi = r%d / r%d\n" parser.addr s t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let mult_instr parser =
    let error_msg = "Invalid syntax: mult" in
    let lexer = parser.lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    match comma with
    | Some Token.Comma -> (
        match s_token, t_token with
        | Some Token.Register s, Some Token.Register t ->
            let _ = Printf.printf "%03x: lo, hi = r%d * r%d\n" parser.addr s t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let multu_instr parser =
    let error_msg = "Invalid syntax: multu" in
    let lexer = parser.lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    match comma with
    | Some Token.Comma -> (
        match s_token, t_token with
        | Some Token.Register s, Some Token.Register t ->
            let _ = Printf.printf "%03x: lo, hi = r%d * r%d\n" parser.addr s t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let nor_instr parser =
    let error_msg = "Invalid syntax: nor" in
    let lexer = parser.lexer in
    let lexer, d_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match d_token, s_token, t_token with
        | Some Token.Register d, Some Token.Register s, Some Token.Register t ->
            let _ = Printf.printf "%03x: r%d = ~(r%d | r%d)\n" parser.addr d s t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let or_instr parser =
    let error_msg = "Invalid syntax: or" in
    let lexer = parser.lexer in
    let lexer, d_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match d_token, s_token, t_token with
        | Some Token.Register d, Some Token.Register s, Some Token.Register t ->
            let _ = Printf.printf "%03x: r%d = r%d | r%d\n" parser.addr d s t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let ori_instr parser =
    let error_msg = "Invalid syntax: ori" in
    let lexer = parser.lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, i_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match t_token, s_token, i_token with
        | Some Token.Register t, Some Token.Register s, Some Token.Immediate i ->
            let _ = Printf.printf "%03x: r%d = r%d | #%d\n" parser.addr t s i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let sll_instr parser =
    let error_msg = "Invalid syntax: sll" in
    let lexer = parser.lexer in
    let lexer, d_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, i_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match d_token, t_token, i_token with
        | Some Token.Register d, Some Token.Register t, Some Token.Immediate i ->
            let _ = Printf.printf "%03x: r%d = r%d << #%d\n" parser.addr d t i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let sllv_instr parser =
    let error_msg = "Invalid syntax: sllv" in
    let lexer = parser.lexer in
    let lexer, d_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match d_token, t_token, s_token with
        | Some Token.Register d, Some Token.Register t, Some Token.Register s ->
            let _ = Printf.printf "%03x: r%d = r%d << r%d\n" parser.addr d t s in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let sra_instr parser =
    let error_msg = "Invalid syntax: sra" in
    let lexer = parser.lexer in
    let lexer, d_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, i_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match d_token, t_token, i_token with
        | Some Token.Register d, Some Token.Register t, Some Token.Immediate i ->
            let _ = Printf.printf "%03x: r%d = r%d >> #%d\n" parser.addr d t i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let srav_instr parser =
    let error_msg = "Invalid syntax: srav" in
    let lexer = parser.lexer in
    let lexer, d_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match d_token, t_token, s_token with
        | Some Token.Register d, Some Token.Register t, Some Token.Register s ->
            let _ = Printf.printf "%03x: r%d = r%d >> r%d\n" parser.addr d t s in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let srl_instr parser =
    let error_msg = "Invalid syntax: srl" in
    let lexer = parser.lexer in
    let lexer, d_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, i_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match d_token, t_token, i_token with
        | Some Token.Register d, Some Token.Register t, Some Token.Immediate i ->
            let _ = Printf.printf "%03x: r%d = r%d >> #%d\n" parser.addr d t i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let srlv_instr parser =
    let error_msg = "Invalid syntax: srlv" in
    let lexer = parser.lexer in
    let lexer, d_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match d_token, t_token, s_token with
        | Some Token.Register d, Some Token.Register t, Some Token.Register s ->
            let _ = Printf.printf "%03x: r%d = r%d >> r%d\n" parser.addr d t s in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let sub_instr parser =
    let error_msg = "Invalid syntax: sub" in
    let lexer = parser.lexer in
    let lexer, d_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match d_token, s_token, t_token with
        | Some Token.Register d, Some Token.Register s, Some Token.Register t ->
            let _ = print_reg_opcode parser.addr ~f:0b100010 ~s ~t ~d ~a:0 in
            let _ = Printf.printf " // r%d = r%d - r%d (signed)\n" d s t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let subu_instr parser =
    let error_msg = "Invalid syntax: subu" in
    let lexer = parser.lexer in
    let lexer, d_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match d_token, s_token, t_token with
        | Some Token.Register d, Some Token.Register s, Some Token.Register t ->
            let _ = Printf.printf "%03x: r%d = r%d - r%d (unsigned)\n" parser.addr d s t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let xor_instr parser =
    let error_msg = "Invalid syntax: xor" in
    let lexer = parser.lexer in
    let lexer, d_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match d_token, s_token, t_token with
        | Some Token.Register d, Some Token.Register s, Some Token.Register t ->
            let _ = Printf.printf "%03x: r%d = r%d ^ r%d\n" parser.addr d s t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let xori_instr parser =
    let error_msg = "Invalid syntax: xori" in
    let lexer = parser.lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, i_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match t_token, s_token, i_token with
        | Some Token.Register t, Some Token.Register s, Some Token.Immediate i ->
            let _ = Printf.printf "%03x: r%d = r%d ^ #%d\n" parser.addr t s i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let li_instr parser =
    let error_msg = "Invalid syntax: li" in
    let lexer = parser.lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma = Lexer.next lexer in
    let lexer, i_token = Lexer.next lexer in
    match comma with
    | Some Token.Comma -> (
        match t_token, i_token with
        | Some Token.Register t, Some Token.Immediate i ->
            let _ = print_imm_opcode parser.addr ~o:0b001001 ~s:0 ~t ~i in
            let _ = Printf.printf " // r%d = #%d\n" t i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let lhi_instr parser =
    let error_msg = "Invalid syntax: lhi" in
    let lexer = parser.lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma = Lexer.next lexer in
    let lexer, i_token = Lexer.next lexer in
    match comma with
    | Some Token.Comma -> (
        match t_token, i_token with
        | Some Token.Register t, Some Token.Immediate i ->
            let _ = Printf.printf "%03x: hi(r%d) = #0x%x\n" parser.addr t i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let llo_instr parser =
    let error_msg = "Invalid syntax: llo" in
    let lexer = parser.lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma = Lexer.next lexer in
    let lexer, i_token = Lexer.next lexer in
    match comma with
    | Some Token.Comma -> (
        match t_token, i_token with
        | Some Token.Register t, Some Token.Immediate i ->
            let _ = Printf.printf "%03x: lo(r%d) = #0x%x\n" parser.addr t i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let lb_instr parser =
    let error_msg = "Invalid syntax: lb" in
    let lexer = parser.lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma = Lexer.next lexer in
    let lexer, i_token = Lexer.next lexer in
    let lexer, lparen = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, rparen = Lexer.next lexer in
    match comma, lparen, rparen with
    | Some Token.Comma, Some Token.LeftParen, Some Token.RightParen -> (
        match t_token, i_token, s_token with
        | Some Token.Register t, Some Token.Immediate i, Some Token.Register s ->
            let _ = Printf.printf "%03x: r%d = mem[r%d + #%d]:1\n" parser.addr t s i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let lh_instr parser =
    let error_msg = "Invalid syntax: lh" in
    let lexer = parser.lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma = Lexer.next lexer in
    let lexer, i_token = Lexer.next lexer in
    let lexer, lparen = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, rparen = Lexer.next lexer in
    match comma, lparen, rparen with
    | Some Token.Comma, Some Token.LeftParen, Some Token.RightParen -> (
        match t_token, i_token, s_token with
        | Some Token.Register t, Some Token.Immediate i, Some Token.Register s ->
            let _ = Printf.printf "%03x: r%d = mem[r%d + #%d]:2\n" parser.addr t s i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let lw_instr parser =
    let error_msg = "Invalid syntax: lw" in
    let lexer = parser.lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma = Lexer.next lexer in
    let lexer, i_token = Lexer.next lexer in
    let lexer, lparen = Lexer.next lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, rparen = Lexer.next lexer in
    match comma, lparen, rparen with
    | Some Token.Comma, Some Token.LeftParen, Some Token.RightParen -> (
        match t_token, i_token, s_token with
        | Some Token.Register t, Some Token.Immediate i, Some Token.Register s ->
            let _ = Printf.printf "%03x: r%d = mem[r%d + #%d]:4\n" parser.addr t s i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let beq_instr parser =
    let error_msg = "Invalid syntax: beq" in
    let lexer = parser.lexer in
    let lexer, s_token = Lexer.next lexer in
    let lexer, comma_1 = Lexer.next lexer in
    let lexer, t_token = Lexer.next lexer in
    let lexer, comma_2 = Lexer.next lexer in
    let lexer, i_token = Lexer.next lexer in
    match comma_1, comma_2 with
    | Some Token.Comma, Some Token.Comma -> (
        match s_token, t_token, i_token with
        | Some Token.Register s, Some Token.Register t, Some Token.Immediate i ->
            let _ = print_imm_opcode parser.addr ~o:0b000100 ~s ~t ~i in
            let _ = Printf.printf " // if r%d == r%d then pc += #%d\n" s t i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let j_instr parser =
    let error_msg = "Invalid syntax: j" in
    let lexer = parser.lexer in
    let lexer, addr_token = Lexer.next lexer in
    match addr_token with
    | Some Token.Instruction label -> (
        let label_addr = get_label_addr parser label in
        let i = label_addr lsr 2 in
        let _ = print_jmp_opcode parser.addr ~o:0b000010 ~i in
        let _ = Printf.printf " // j 0x%03x\n" label_addr in
        { parser with lexer; addr = parser.addr + 0x4; })
    | Some Token.Immediate i -> (
        let i = i lsr 2 in
        let _ = print_jmp_opcode parser.addr ~o:0b000010 ~i in
        let _ = Printf.printf " // j 0x%03x\n" i in
        { parser with lexer; addr = parser.addr + 0x4; })
    | _ -> failwith error_msg

let parse_instruction parser =
    let _ = print_endline "v3.0 hex words addressed" in
    let parser = parse_label parser in
    let rec loop parser =
        let lexer, token = Lexer.next parser.lexer in
        match token with
        | Some Instruction inst -> (
            let parser = { parser with lexer; } in
            match inst with
            (* Arithmetic and Logic *)
            | "add" -> parser |> add_instr |> loop
            | "addu" -> parser |> addu_instr |> loop
            | "addi" -> parser |> addi_instr |> loop
            | "addiu" -> parser |> addiu_instr |> loop
            | "and" -> parser |> and_instr |> loop
            | "andi" -> parser |> andi_instr |> loop
            | "div" -> parser |> div_instr |> loop
            | "divu" -> parser |> divu_instr |> loop
            | "mult" -> parser |> mult_instr |> loop
            | "multu" -> parser |> multu_instr |> loop
            | "nor" -> parser |> nor_instr |> loop
            | "or" -> parser |> or_instr |> loop
            | "ori" -> parser |> ori_instr |> loop
            | "sll" -> parser |> sll_instr |> loop
            | "sllv" -> parser |> sllv_instr |> loop
            | "sra" -> parser |> sra_instr |> loop
            | "srav" -> parser |> srav_instr |> loop
            | "srl" -> parser |> srl_instr |> loop
            | "srlv" -> parser |> srlv_instr |> loop
            | "sub" -> parser |> sub_instr |> loop
            | "subu" -> parser |> subu_instr |> loop
            | "xor" -> parser |> xor_instr |> loop
            | "xori" -> parser |> xori_instr |> loop
            (* Load *)
            | "li" -> parser |> li_instr |> loop
            | "lhi" -> parser |> lhi_instr |> loop
            | "llo" -> parser |> llo_instr |> loop
            | "lb" -> parser |> lb_instr |> loop
            | "lh" -> parser |> lh_instr |> loop
            | "lw" -> parser |> lw_instr |> loop
            (* Branch *)
            | "beq" -> parser |> beq_instr |> loop
            | "j" -> parser |> j_instr |> loop
            | _ -> failwith inst)
            (* let _ = Printf.printf "0x%03x: %s\n" parser.addr inst in *)
            (* loop { parser with lexer; addr = parser.addr + 0x4; } *)
        | Some Label _
        | Some Newline ->
            loop { parser with lexer; }
        | Some _ ->
            failwith "No tokens other than Label and Instruction are at the start"
        | None ->
            parser
    in loop parser
