(* ============================================================
 * Copyright (c) 2017 tacigar. All rights reserved.
 * https://github.com/tacigar/oldfashion
 * ============================================================ *)

class vm = object(self)
  val mutable ip_register = 0
  val mutable fp_register = 0
  val mutable sp_register = 0
  val mutable code_memory = Array.make 0 0
end
