(* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ *)

type code =
  | FGSTORE
  | FGLOAD
  | TEST
  | JUMP
  | STORE
  | LOAD
  | STORETBL
  | LOADTBL
  | CALL
  | RET
  | CONST
  | NEWTBL
  | MINUS
  | ADD
  | SUB
  | MUL
  | DIV
  | LT
  | LE
  | GT
  | GE
  | NE
  | EQ
  | AND
  | OR
  | HALT

let of_string = function
  | "FGSTORE"  -> FGSTORE
  | "FGLOAD"   -> FGLOAD
  | "TEST"     -> TEST
  | "JUMP"     -> JUMP
  | "STORE"    -> STORE
  | "LOAD"     -> LOAD
  | "STORETBL" -> STORETBL
  | "LOADTBL"  -> LOADTBL
  | "CALL"     -> CALL
  | "RET"      -> RET
  | "CONST"    -> CONST
  | "NEWTBL"   -> NEWTBL
  | "MINUS"    -> MINUS
  | "ADD"      -> ADD
  | "SUB"      -> SUB
  | "MUL"      -> MUL
  | "DIV"      -> DIV
  | "LT"       -> LT
  | "LE"       -> LE
  | "GT"       -> GT
  | "GE"       -> GE
  | "NE"       -> NE
  | "EQ"       -> EQ
  | "AND"      -> AND
  | "OR"       -> OR
  | "HALT"     -> HALT
  | _ -> raise Not_found

let to_string = function
  | FGSTORE  -> "FGSTORE"
  | FGLOAD   -> "FGLOAD"
  | TEST     -> "TEST"
  | JUMP     -> "JUMP"
  | STORE    -> "STORE"
  | LOAD     -> "LOAD"
  | STORETBL -> "STORETBL"
  | LOADTBL  -> "LOADTBL"
  | CALL     -> "CALL"
  | RET      -> "RET"
  | CONST    -> "CONST"
  | NEWTBL   -> "NEWTBL"
  | MINUS    -> "MINUS"
  | ADD      -> "ADD"
  | SUB      -> "SUB"
  | MUL      -> "MUL"
  | DIV      -> "DIV"
  | LT       -> "LT"
  | LE       -> "LE"
  | GT       -> "GT"
  | GE       -> "GE"
  | NE       -> "NE"
  | EQ       -> "EQ"
  | AND      -> "AND"
  | OR       -> "OR"
  | HALT     -> "HALT"

let int_code_assoc =
  [ (0x0000, FGSTORE);
    (0x0001, FGLOAD);
    (0x0002, TEST);
    (0x0003, JUMP);
    (0x0004, STORE);
    (0x0005, LOAD);
    (0x0006, STORETBL);
    (0x0007, LOADTBL);
    (0x0008, CALL);
    (0x0009, RET);
    (0x000a, CONST);
    (0x000b, NEWTBL);
    (0x000c, MINUS);
    (0x000d, ADD);
    (0x000e, SUB);
    (0x000f, MUL);
    (0x0010, DIV);
    (0x0011, LT);
    (0x0012, LE);
    (0x0013, GT);
    (0x0014, GE);
    (0x0015, NE);
    (0x0016, EQ);
    (0x0017, AND);
    (0x0018, OR);
    (0x0019, HALT);
  ]
                  
let of_int n =
  let tmp =
    List.find (fun p ->
        (fst p) = n
      ) int_code_assoc in
  snd tmp

let to_int c =
  let tmp =
    List.find (fun p ->
        (snd p) = c
      ) int_code_assoc in
  fst tmp
