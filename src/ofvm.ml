(* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ *)

class vm = object(self)
  val mutable ip_register = 0
  val mutable fp_register = 0
  val mutable sp_register = 0
  val mutable code = Array.make 0 0
  val mutable globals = Array.make 0 0
  val mutable consts = Array.make 0 0
  val mutable call_stack = Array.make 0 0
  val mutable operands = Array.make 0 0

  method add_function funcname impl =
    
end
