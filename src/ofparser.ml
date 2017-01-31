
exception Error

let _eRR =
  Error

type token = 
  | WHILE
  | VAR
  | TRUE
  | STRING_LITERAL of (string)
  | SEMI
  | RPAREN
  | RETURN
  | RBRACKET
  | RBRACE
  | PLUS
  | OR
  | NUMERIC_LITERAL of (float)
  | NE
  | MULT
  | MINUS
  | LT
  | LPAREN
  | LE
  | LBRACKET
  | LBRACE
  | IF
  | IDENTIFIER of (string)
  | GT
  | GE
  | FOR
  | FN
  | FALSE
  | EQ
  | EOF
  | ELSE
  | DIV
  | COMMA
  | ASSIGN
  | AND

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState117
  | MenhirState109
  | MenhirState107
  | MenhirState105
  | MenhirState103
  | MenhirState101
  | MenhirState98
  | MenhirState96
  | MenhirState93
  | MenhirState90
  | MenhirState87
  | MenhirState85
  | MenhirState83
  | MenhirState80
  | MenhirState78
  | MenhirState74
  | MenhirState72
  | MenhirState70
  | MenhirState69
  | MenhirState65
  | MenhirState63
  | MenhirState54
  | MenhirState51
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState24
  | MenhirState22
  | MenhirState20
  | MenhirState18
  | MenhirState12
  | MenhirState10
  | MenhirState9
  | MenhirState8
  | MenhirState6
  | MenhirState3
  | MenhirState0
  
  open Ofsyntax

let rec _menhir_goto_list_statement_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.statement list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ofsyntax.Ast.statement))), _, (xs : (Ofsyntax.Ast.statement list))) = _menhir_stack in
        let _v : (Ofsyntax.Ast.statement list) =     ( x :: xs ) in
        _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (stmts : (Ofsyntax.Ast.statement list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ofsyntax.Ast.statement list) =     (
      stmts
    ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState74 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (cond : (Ofsyntax.Ast.expression))), _, (stmts : (Ofsyntax.Ast.statement list))) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.While_statement (cond, stmts)
    ) in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | MenhirState85 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | LBRACE ->
                        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
                | FOR | IDENTIFIER _ | IF | RBRACE | RETURN | RPAREN | SEMI | VAR | WHILE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _, (cond : (Ofsyntax.Ast.expression))), _, (tstmts : (Ofsyntax.Ast.statement list))) = _menhir_stack in
                    let _4 = () in
                    let _2 = () in
                    let _1 = () in
                    let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.If_statement (cond, tstmts, None)
    ) in
                    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState87 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (cond : (Ofsyntax.Ast.expression))), _, (tstmts : (Ofsyntax.Ast.statement list))), _, (fstmts : (Ofsyntax.Ast.statement list))) = _menhir_stack in
                let _6 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.If_statement (cond, tstmts, Some fstmts)
    ) in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | MenhirState107 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((((_menhir_stack, _menhir_s), _, (init_stmt : (Ofsyntax.Ast.statement))), _, (cond : (Ofsyntax.Ast.expression))), _, (prop_stmt : (Ofsyntax.Ast.statement))), _, (stmts : (Ofsyntax.Ast.statement list))) = _menhir_stack in
                let _8 = () in
                let _6 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.For_statement (init_stmt, cond, prop_stmt, stmts)
    ) in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | MenhirState69 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), (funcname : (string))), _, (xs0 : (Ofsyntax.Ast.identifier list))), _, (stmts : (Ofsyntax.Ast.statement list))) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (Ofsyntax.Ast.external_definition) = let params =
                  let xs = xs0 in
                      ( xs )
                in
                    (
      Ofsyntax.Ast.Function_external_definition (funcname, params, stmts)
    ) in
                _menhir_goto_external_definition _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (funcname : (string))), _, (xs0 : (Ofsyntax.Ast.expression list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ofsyntax.Ast.expression) = let args =
              let xs = xs0 in
                  ( xs )
            in
                (
      Ofsyntax.Ast.Function_call_expression (funcname, args)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (funcname : (string))), _, (xs0 : (Ofsyntax.Ast.expression list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ofsyntax.Ast.statement) = let args =
              let xs = xs0 in
                  ( xs )
            in
                (
      Ofsyntax.Ast.Function_call_statement (funcname, args)
    ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.statement) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | IDENTIFIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
            | LBRACKET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | LPAREN ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | MINUS ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | NUMERIC_LITERAL _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
            | PLUS ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | STRING_LITERAL _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState107
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState109 | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FOR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | IDENTIFIER _v ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
        | IF ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | RETURN ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | VAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | WHILE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | RBRACE ->
            _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_field_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) list)) = _v in
        let _v : ((Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_field__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ofsyntax.Ast.expression * Ofsyntax.Ast.expression))) = _menhir_stack in
        let _2 = () in
        let _v : ((Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_field_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState90 | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ofsyntax.Ast.expression list)) = _v in
        let _v : (Ofsyntax.Ast.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ofsyntax.Ast.expression list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ofsyntax.Ast.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Ofsyntax.Ast.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_reduce31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ofsyntax.Ast.statement list) =     ( [] ) in
    _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | IDENTIFIER _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | LBRACKET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | MINUS ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NUMERIC_LITERAL _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | PLUS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | STRING_LITERAL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | IDENTIFIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | LBRACKET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LPAREN ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | MINUS ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | NUMERIC_LITERAL _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | PLUS ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | STRING_LITERAL _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
        | FOR | IDENTIFIER _ | IF | RBRACE | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (varname : (string))) = _menhir_stack in
            let _1 = () in
            let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.Variable_declaration_statement (varname, None)
    ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | FOR | IF | RBRACE | RETURN | RPAREN | SEMI | VAR | WHILE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.Return_statement None
    ) in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | IDENTIFIER _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | LBRACKET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MINUS ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | NUMERIC_LITERAL _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | PLUS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | STRING_LITERAL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run89 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | IDENTIFIER _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | LBRACKET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MINUS ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | NUMERIC_LITERAL _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | PLUS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | STRING_LITERAL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | LBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | IDENTIFIER _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | LBRACKET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MINUS ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | NUMERIC_LITERAL _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | PLUS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | STRING_LITERAL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | IDENTIFIER _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | LBRACKET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MINUS ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | NUMERIC_LITERAL _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | PLUS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | STRING_LITERAL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | RPAREN ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run100 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FOR ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | IDENTIFIER _v ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | IF ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | RETURN ->
            _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | VAR ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | WHILE ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_loption_separated_nonempty_list_COMMA_field__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (xs0 : ((Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Ofsyntax.Ast.expression) = let fields =
          let xs = xs0 in
              ( xs )
        in
            (
      Ofsyntax.Ast.Table_constructor_expression (fields)
    ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ofsyntax.Ast.expression list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState90 | MenhirState42 | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | IDENTIFIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | LBRACKET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | LPAREN ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | MINUS ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | NUMERIC_LITERAL _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | PLUS ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | STRING_LITERAL _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _v : (Ofsyntax.Ast.expression list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | MINUS | NE | OR | PLUS | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (Ofsyntax.Ast.expression))), _, (rhs : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Plus, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (lhs : (Ofsyntax.Ast.expression))), _, (rhs : (Ofsyntax.Ast.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Mult, rhs)
    ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (lhs : (Ofsyntax.Ast.expression))), _, (rhs : (Ofsyntax.Ast.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Div, rhs)
    ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | EOF | FN | FOR | IDENTIFIER _ | IF | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (Ofsyntax.Ast.expression))), _, (rhs : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Or, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | NE | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (Ofsyntax.Ast.expression))), _, (rhs : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Ne, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | MINUS | NE | OR | PLUS | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (Ofsyntax.Ast.expression))), _, (rhs : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Minus, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | NE | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (Ofsyntax.Ast.expression))), _, (rhs : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Lt, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | NE | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (Ofsyntax.Ast.expression))), _, (rhs : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Le, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | NE | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (Ofsyntax.Ast.expression))), _, (rhs : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Gt, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | NE | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (Ofsyntax.Ast.expression))), _, (rhs : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Ge, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | NE | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (Ofsyntax.Ast.expression))), _, (rhs : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Eq, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | FN | FOR | IDENTIFIER _ | IF | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (lhs : (Ofsyntax.Ast.expression))), _, (rhs : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.And, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (varname : (string))), _, (exp : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Table_value_expression (varname, exp)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState10 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | IDENTIFIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | LBRACKET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | LPAREN ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | MINUS ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | NUMERIC_LITERAL _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | PLUS ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | STRING_LITERAL _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (key : (Ofsyntax.Ast.expression))), _, (value : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) =     (
      (key, value)
    ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FALSE ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | IDENTIFIER _v ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | LBRACKET ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | LPAREN ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | MINUS ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | NUMERIC_LITERAL _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | PLUS ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | STRING_LITERAL _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
            | RBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (Ofsyntax.Ast.expression * Ofsyntax.Ast.expression))) = _menhir_stack in
                let _v : ((Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) list) =     ( [ x ] ) in
                _menhir_goto_separated_nonempty_list_COMMA_field_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (exp : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Paren_expression (exp)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (exp : (Ofsyntax.Ast.expression))) = _menhir_stack in
        let _1 = () in
        let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Unary_operation_expression (Ofsyntax.Ast.Uminus, exp)
    ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (exp : (Ofsyntax.Ast.expression))) = _menhir_stack in
        let _1 = () in
        let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Unary_operation_expression (Ofsyntax.Ast.Uplus, exp)
    ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | EOF | FN | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (varname : (string))), _, (exp : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ofsyntax.Ast.external_definition) =     (
      Ofsyntax.Ast.Variable_external_definition (varname, Some exp)
    ) in
            _menhir_goto_external_definition _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | FOR | IDENTIFIER _ | IF | RBRACE | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (varname : (string))), _, (exp : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.Variable_declaration_statement (varname, Some exp)
    ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | FOR | IDENTIFIER _ | IF | RBRACE | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (exp : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.Return_statement (Some exp)
    ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACE ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSIGN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FALSE ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | IDENTIFIER _v ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
                | LBRACKET ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | LPAREN ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | MINUS ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | NUMERIC_LITERAL _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
                | PLUS ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | STRING_LITERAL _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | FOR | IDENTIFIER _ | IF | RBRACE | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (varname : (string))), _, (key : (Ofsyntax.Ast.expression))), _, (value : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.Table_value_assign_statement (varname, key, value)
    ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | FOR | IDENTIFIER _ | IF | RBRACE | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (varname : (string))), _, (exp : (Ofsyntax.Ast.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.Variable_assign_statement (varname, exp)
    ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FOR ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | IDENTIFIER _v ->
                _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | IF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | RETURN ->
                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | VAR ->
                _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | WHILE ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FOR ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | IDENTIFIER _v ->
        _menhir_run89 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IF ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | RETURN ->
        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | VAR ->
        _menhir_run76 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | WHILE ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | RBRACE ->
        _menhir_reduce31 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.identifier list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ofsyntax.Ast.identifier list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (Ofsyntax.Ast.identifier list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ofsyntax.Ast.identifier list)) = _v in
        let _v : (Ofsyntax.Ast.identifier list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_external_definition_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (defs : (Ofsyntax.Ast.program))) = _menhir_stack in
            let _2 = () in
            let _v : (Ofsyntax.Ast.program) =     ( defs ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ofsyntax.Ast.program)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ofsyntax.Ast.external_definition))), _, (xs : (Ofsyntax.Ast.program))) = _menhir_stack in
        let _v : (Ofsyntax.Ast.program) =     ( x :: xs ) in
        _menhir_goto_list_external_definition_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_external_definition : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.external_definition) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FN ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | VAR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | EOF ->
        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Boolean_literal_expression (true)
    ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (str : (string)) = _v in
    let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.String_literal_expression (str)
    ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (num : (float)) = _v in
    let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Numeric_literal_expression (num)
    ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | IDENTIFIER _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | LBRACKET ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | LPAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState10 in
        let _v : ((Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_field__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | IDENTIFIER _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | LBRACKET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | MINUS ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | NUMERIC_LITERAL _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | PLUS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | STRING_LITERAL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | IDENTIFIER _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | LBRACKET ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | MINUS ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NUMERIC_LITERAL _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | PLUS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | STRING_LITERAL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | RPAREN ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | AND | ASSIGN | COMMA | DIV | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | MINUS | MULT | NE | OR | PLUS | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (varname : (string))) = _menhir_stack in
        let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Variable_expression (varname)
    ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Boolean_literal_expression (false)
    ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.identifier list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (string))) = _menhir_stack in
        let _v : (Ofsyntax.Ast.identifier list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ofsyntax.Ast.program) =     ( [] ) in
    _menhir_goto_list_external_definition_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | IDENTIFIER _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | LBRACKET ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | LPAREN ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | MINUS ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | NUMERIC_LITERAL _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | PLUS ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | STRING_LITERAL _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
        | EOF | FN | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), (varname : (string))) = _menhir_stack in
            let _1 = () in
            let _v : (Ofsyntax.Ast.external_definition) =     (
      Ofsyntax.Ast.Variable_external_definition (varname, None)
    ) in
            _menhir_goto_external_definition _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENTIFIER _v ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState63 in
                let _v : (Ofsyntax.Ast.identifier list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ofsyntax.Ast.program) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FN ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VAR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

