(* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ *)

type value =
  | String of string
  | Number of float
  | Function of string * int * int * int
  | Table of (string * value) list
  | Bool of bool
                  
       
