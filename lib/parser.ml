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

let add_inst parser =
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
            let _ = Printf.printf " # r%d = r%d + r%d (signed)\n" d s t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let addu_inst parser =
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
            let _ = Printf.printf " # r%d = r%d + r%d (unsigned)\n" d s t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let addi_inst parser =
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
            let _ = Printf.printf " # r%d = r%d + #%d (signed)\n" t s i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let addiu_inst parser =
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
            let _ = Printf.printf " # r%d = r%d + #%d (unsigned)\n" t s i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let and_inst parser =
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

let andi_inst parser =
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

let div_inst parser =
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

let divu_inst parser =
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

let mult_inst parser =
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

let multu_inst parser =
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

let nor_inst parser =
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

let or_inst parser =
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

let ori_inst parser =
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

let sll_inst parser =
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

let sllv_inst parser =
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

let sra_inst parser =
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

let srav_inst parser =
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

let srl_inst parser =
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

let srlv_inst parser =
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

let sub_inst parser =
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
            let _ = Printf.printf " # r%d = r%d - r%d (signed)\n" d s t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let subu_inst parser =
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

let xor_inst parser =
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

let xori_inst parser =
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

let li_inst parser =
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
            let _ = Printf.printf " # r%d = #0x%x\n" t i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let lhi_inst parser =
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

let llo_inst parser =
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

let lb_inst parser =
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
            let _ = print_imm_opcode parser.addr ~o:0b100000 ~s ~t ~i in
            let _ = Printf.printf " # r%d = mem[r%d + #%d]:1\n" t s i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let lh_inst parser =
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

let lw_inst parser =
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
            let _ = print_imm_opcode parser.addr ~o:0b100011 ~s ~t ~i in
            let _ = Printf.printf " # r%d = mem[r%d + #%d]:4\n" t s i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let sb_inst parser =
    let error_msg = "Invalid syntax: sb" in
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
            let _ = print_imm_opcode parser.addr ~o:0b101000 ~s ~t ~i in
            let _ = Printf.printf " # mem[r%d + #%d]:1 = r%d\n" s i t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let sh_inst parser =
    let error_msg = "Invalid syntax: sh" in
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
            let _ = print_imm_opcode parser.addr ~o:0b101001 ~s ~t ~i in
            let _ = Printf.printf " # mem[r%d + #%d]:2 = r%d\n" s i t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let sw_inst parser =
    let error_msg = "Invalid syntax: sw" in
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
            let _ = print_imm_opcode parser.addr ~o:0b101011 ~s ~t ~i in
            let _ = Printf.printf " # mem[r%d + #%d]:4 = r%d\n" s i t in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let beq_inst parser =
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
            let _ = Printf.printf " # if r%d == r%d then pc += #%d\n" s t i in
            { parser with lexer; addr = parser.addr + 0x4; }
        | _ -> failwith error_msg)
    | _ -> failwith error_msg

let j_inst parser =
    let error_msg = "Invalid syntax: j" in
    let lexer = parser.lexer in
    let lexer, addr_token = Lexer.next lexer in
    match addr_token with
    | Some Token.Instruction label -> (
        let label_addr = get_label_addr parser label in
        let i = label_addr lsr 2 in
        let _ = print_jmp_opcode parser.addr ~o:0b000010 ~i in
        let _ = Printf.printf " # j 0x%03x\n" label_addr in
        { parser with lexer; addr = parser.addr + 0x4; })
    | Some Token.Immediate i -> (
        let i = i lsr 2 in
        let _ = print_jmp_opcode parser.addr ~o:0b000010 ~i in
        let _ = Printf.printf " # j 0x%03x\n" i in
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
            | "add" -> parser |> add_inst |> loop
            | "addu" -> parser |> addu_inst |> loop
            | "addi" -> parser |> addi_inst |> loop
            | "addiu" -> parser |> addiu_inst |> loop
            | "and" -> parser |> and_inst |> loop
            | "andi" -> parser |> andi_inst |> loop
            | "div" -> parser |> div_inst |> loop
            | "divu" -> parser |> divu_inst |> loop
            | "mult" -> parser |> mult_inst |> loop
            | "multu" -> parser |> multu_inst |> loop
            | "nor" -> parser |> nor_inst |> loop
            | "or" -> parser |> or_inst |> loop
            | "ori" -> parser |> ori_inst |> loop
            | "sll" -> parser |> sll_inst |> loop
            | "sllv" -> parser |> sllv_inst |> loop
            | "sra" -> parser |> sra_inst |> loop
            | "srav" -> parser |> srav_inst |> loop
            | "srl" -> parser |> srl_inst |> loop
            | "srlv" -> parser |> srlv_inst |> loop
            | "sub" -> parser |> sub_inst |> loop
            | "subu" -> parser |> subu_inst |> loop
            | "xor" -> parser |> xor_inst |> loop
            | "xori" -> parser |> xori_inst |> loop
            (* Load *)
            | "li" -> parser |> li_inst |> loop
            | "lhi" -> parser |> lhi_inst |> loop
            | "llo" -> parser |> llo_inst |> loop
            | "lb" -> parser |> lb_inst |> loop
            | "lh" -> parser |> lh_inst |> loop
            | "lw" -> parser |> lw_inst |> loop
            (* Store *)
            | "sb" -> parser |> sb_inst |> loop
            | "sh" -> parser |> sh_inst |> loop
            | "sw" -> parser |> sw_inst |> loop
            (* Branch *)
            | "beq" -> parser |> beq_inst |> loop
            | "j" -> parser |> j_inst |> loop
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
