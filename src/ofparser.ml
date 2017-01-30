exception Error

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

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState114
  | MenhirState106
  | MenhirState104
  | MenhirState102
  | MenhirState100
  | MenhirState98
  | MenhirState95
  | MenhirState93
  | MenhirState90
  | MenhirState87
  | MenhirState84
  | MenhirState82
  | MenhirState80
  | MenhirState77
  | MenhirState75
  | MenhirState71
  | MenhirState69
  | MenhirState67
  | MenhirState66
  | MenhirState62
  | MenhirState60
  | MenhirState53
  | MenhirState50
  | MenhirState43
  | MenhirState41
  | MenhirState39
  | MenhirState37
  | MenhirState35
  | MenhirState33
  | MenhirState31
  | MenhirState29
  | MenhirState27
  | MenhirState25
  | MenhirState23
  | MenhirState21
  | MenhirState19
  | MenhirState17
  | MenhirState11
  | MenhirState9
  | MenhirState8
  | MenhirState6
  | MenhirState3
  | MenhirState0

  
  open Ofsyntax
let _eRR =
  Error

let rec _menhir_goto_list_statement_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.statement list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ofsyntax.Ast.statement list) =     ( x :: xs ) in
        _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, stmts) = _menhir_stack in
            let _v : (Ofsyntax.Ast.statement list) =     (
      stmts
    ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState71 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, cond), _, stmts) = _menhir_stack in
                let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.While_statement (cond, stmts)
    ) in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | MenhirState82 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | LBRACE ->
                        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState84
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
                | FOR | IDENTIFIER _ | IF | RBRACE | RETURN | RPAREN | SEMI | VAR | WHILE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _, cond), _, tstmts) = _menhir_stack in
                    let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.If_statement (cond, tstmts, None) 
    ) in
                    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState84 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, cond), _, tstmts), _, fstmts) = _menhir_stack in
                let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.If_statement (cond, tstmts, Some fstmts)
    ) in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | MenhirState104 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((((_menhir_stack, _menhir_s), _, init_stmt), _, cond), _, prop_stmt), _, stmts) = _menhir_stack in
                let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.For_statement (init_stmt, cond, prop_stmt, stmts)
    ) in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | MenhirState66 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), funcname), _, xs0), _, stmts) = _menhir_stack in
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
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, funcname), _, xs0) = _menhir_stack in
            let _v : (Ofsyntax.Ast.expression) = let args =
              let xs = xs0 in
                  ( xs )
            in
                (
      Ofsyntax.Ast.Function_call_expression (funcname, args)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, funcname), _, xs0) = _menhir_stack in
            let _v : (Ofsyntax.Ast.statement) = let args =
              let xs = xs0 in
                  ( xs )
            in
                (
      Ofsyntax.Ast.Function_call_statement (funcname, args)
    ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.statement) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | IDENTIFIER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | LBRACKET ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | MINUS ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | NUMERIC_LITERAL _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | PLUS ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | STRING_LITERAL _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | LBRACE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState106 | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FOR ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | IDENTIFIER _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
        | IF ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | RETURN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | VAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | WHILE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | RBRACE ->
            _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState106
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_field_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : ((Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_field__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : ((Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_field_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState87 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : (Ofsyntax.Ast.expression list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ofsyntax.Ast.expression list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run17 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run19 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run21 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_reduce30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ofsyntax.Ast.statement list) =     ( [] ) in
    _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | IDENTIFIER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | LBRACKET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | MINUS ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | NUMERIC_LITERAL _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | PLUS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | STRING_LITERAL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | IDENTIFIER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | LBRACKET ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | MINUS ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | NUMERIC_LITERAL _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | PLUS ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | STRING_LITERAL _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | FOR | IDENTIFIER _ | IF | RBRACE | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), varname) = _menhir_stack in
            let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.Variable_declaration_statement (varname, None)
    ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LBRACKET ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | MINUS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NUMERIC_LITERAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | PLUS ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | STRING_LITERAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | FOR | IF | RBRACE | RETURN | RPAREN | SEMI | VAR | WHILE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.Return_statement None
    ) in
        _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run79 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | IDENTIFIER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | LBRACKET ->
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
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run86 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | IDENTIFIER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | LBRACKET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MINUS ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | NUMERIC_LITERAL _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | PLUS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | STRING_LITERAL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | LBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | IDENTIFIER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | LBRACKET ->
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
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | IDENTIFIER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | LBRACKET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | MINUS ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | NUMERIC_LITERAL _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | PLUS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | STRING_LITERAL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | RPAREN ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FOR ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | IDENTIFIER _v ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
        | IF ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | RETURN ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | VAR ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | WHILE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
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
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, xs0) = _menhir_stack in
        let _v : (Ofsyntax.Ast.expression) = let fields =
          let xs = xs0 in
              ( xs )
        in
            (
      Ofsyntax.Ast.Table_constructor_expression (fields)
    ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ofsyntax.Ast.expression list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState87 | MenhirState41 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | IDENTIFIER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | LBRACKET ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | MINUS ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | NUMERIC_LITERAL _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | PLUS ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | STRING_LITERAL _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, x) = _menhir_stack in
            let _v : (Ofsyntax.Ast.expression list) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | MINUS | NE | OR | PLUS | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Plus, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
        let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Mult, rhs)
    ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
        let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Div, rhs)
    ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | EOF | FN | FOR | IDENTIFIER _ | IF | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Or, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | NE | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Ne, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | MINUS | NE | OR | PLUS | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Minus, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | NE | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Lt, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | NE | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Le, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | NE | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Gt, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | NE | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Ge, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | NE | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.Eq, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | EOF | FN | FOR | IDENTIFIER _ | IF | OR | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, lhs), _, rhs) = _menhir_stack in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Binary_operation_expression (lhs, Ofsyntax.Ast.And, rhs)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, varname), _, exp) = _menhir_stack in
            let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Table_value_expression (varname, exp)
    ) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | IDENTIFIER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | LBRACKET ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | MINUS ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | NUMERIC_LITERAL _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | PLUS ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | STRING_LITERAL _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, key), _, value) = _menhir_stack in
            let _v : (Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) =     (
      (key, value)
    ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FALSE ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                | IDENTIFIER _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
                | LBRACKET ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                | MINUS ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                | NUMERIC_LITERAL _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
                | PLUS ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                | STRING_LITERAL _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
            | RBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                let _v : ((Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) list) =     ( [ x ] ) in
                _menhir_goto_separated_nonempty_list_COMMA_field_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, exp) = _menhir_stack in
        let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Unary_operation_expression (Ofsyntax.Ast.Uminus, exp)
    ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, exp) = _menhir_stack in
        let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Unary_operation_expression (Ofsyntax.Ast.Uplus, exp)
    ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | EOF | FN | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), varname), _, exp) = _menhir_stack in
            let _v : (Ofsyntax.Ast.external_definition) =     (
      Ofsyntax.Ast.Variable_external_definition (varname, Some exp)
    ) in
            _menhir_goto_external_definition _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | LBRACE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState71
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | FOR | IDENTIFIER _ | IF | RBRACE | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), varname), _, exp) = _menhir_stack in
            let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.Variable_declaration_statement (varname, Some exp)
    ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | FOR | IDENTIFIER _ | IF | RBRACE | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, exp) = _menhir_stack in
            let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.Return_statement (Some exp)
    ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | LBRACE ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState82
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ASSIGN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | FALSE ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | IDENTIFIER _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
                | LBRACKET ->
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
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | FOR | IDENTIFIER _ | IF | RBRACE | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, varname), _, key), _, value) = _menhir_stack in
            let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.Table_value_assign_statement (varname, key, value)
    ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | FOR | IDENTIFIER _ | IF | RBRACE | RETURN | RPAREN | SEMI | VAR | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, varname), _, exp) = _menhir_stack in
            let _v : (Ofsyntax.Ast.statement) =     (
      Ofsyntax.Ast.Variable_assign_statement (varname, exp)
    ) in
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FOR ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | IDENTIFIER _v ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
            | IF ->
                _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | RETURN ->
                _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | VAR ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | WHILE ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState102
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FOR ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | IDENTIFIER _v ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | IF ->
        _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | RETURN ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | VAR ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | WHILE ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | RBRACE ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.identifier list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ofsyntax.Ast.identifier list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
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
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, defs) = _menhir_stack in
            let _v : (Ofsyntax.Ast.program) =     ( defs ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x), _, xs) = _menhir_stack in
        let _v : (Ofsyntax.Ast.program) =     ( x :: xs ) in
        _menhir_goto_list_external_definition_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_external_definition : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.external_definition) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FN ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | VAR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | EOF ->
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Boolean_literal_expression (true)
    ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let str = _v in
    let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.String_literal_expression (str)
    ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | LBRACKET ->
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
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let num = _v in
    let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Numeric_literal_expression (num)
    ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | LBRACKET ->
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
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | IDENTIFIER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | LBRACKET ->
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
    | RBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState9 in
        let _v : ((Ofsyntax.Ast.expression * Ofsyntax.Ast.expression) list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_field__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | LBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | IDENTIFIER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | LBRACKET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | MINUS ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | NUMERIC_LITERAL _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | PLUS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | STRING_LITERAL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | IDENTIFIER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | LBRACKET ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | MINUS ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | NUMERIC_LITERAL _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | PLUS ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | STRING_LITERAL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | RPAREN ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
    | AND | ASSIGN | COMMA | DIV | EOF | EQ | FN | FOR | GE | GT | IDENTIFIER _ | IF | LE | LT | MINUS | MULT | NE | OR | PLUS | RBRACE | RBRACKET | RETURN | RPAREN | SEMI | VAR | WHILE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, varname) = _menhir_stack in
        let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Variable_expression (varname)
    ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ofsyntax.Ast.expression) =     (
      Ofsyntax.Ast.Boolean_literal_expression (false)
    ) in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ofsyntax.Ast.identifier list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | LBRACE ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : (Ofsyntax.Ast.identifier list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_IDENTIFIER_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
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
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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

and _menhir_reduce28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ofsyntax.Ast.program) =     ( [] ) in
    _menhir_goto_list_external_definition_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | IDENTIFIER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | LBRACKET ->
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
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
        | EOF | FN | VAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), varname) = _menhir_stack in
            let _v : (Ofsyntax.Ast.external_definition) =     (
      Ofsyntax.Ast.Variable_external_definition (varname, None)
    ) in
            _menhir_goto_external_definition _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENTIFIER _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDENTIFIER _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState60 in
                let _v : (Ofsyntax.Ast.identifier list) =     ( [] ) in
                _menhir_goto_loption_separated_nonempty_list_COMMA_IDENTIFIER__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ofsyntax.Ast.program) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = max_int;
      } in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FN ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VAR ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



