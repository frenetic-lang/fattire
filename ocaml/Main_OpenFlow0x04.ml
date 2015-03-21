open Async.Std
open Core.Std

module Test = RegexFTTest

(* configuration state *)
let controller = ref "learn"

(* command-line arguments *)
let arg_specs = 
  [ ("-c", 
     Arg.Set_string controller, 
     "<controller> run a specific controller")
  ]
 
let arg_rest rest = ()

let usage = 
  "desmoines [options]"

let () = Arg.parse arg_specs arg_rest usage

let _ =
  let main () =
    Async_NetKAT_Controller.start Test.app ~update:`BestEffort ()
  in
  ignore (main ());
  Core.Std.never_returns (Async.Std.Scheduler.go ())
  (* never_returns (Scheduler.go_main ~max_num_open_file_descrs:4096 ~main ()) *)
