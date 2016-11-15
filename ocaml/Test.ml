open Printf
open Unix
open Pathetic.Regex
open Pathetic.RegexUtils

let dltype n = Frenetic_NetKAT.Test( Frenetic_NetKAT.EthType n )
let test = RegUnion(RegPol(dltype 0, Star, 0),
		    RegUnion(RegPol(dltype 1, Star, 1),
			     RegInter(RegPol(dltype 2, Star, 2),
				      RegPol(dltype 3, Star, 3))))

let main () = 
  Sys.catch_break true;
  printf "[test] %s\n%!" (regexPol_to_string test);
  printf "[test] [%s]\n%!" (regexPol_to_string (to_dnf test));
  printf "[test] [%s]\n%!" (regexPol_to_string (blast_inter (to_dnf test)));
  printf "[test] [%s]\n%!" (String.concat ";" (List.map regexPol_to_string (normalize test)));
  exit 1
      
let _ = main ()
