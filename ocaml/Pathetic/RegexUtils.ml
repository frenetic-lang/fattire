open Frenetic_NetKAT
open Regex
module N = Node
module T = Net.Topology
module R = Regex

let oldPol (a,b) = Seq (Filter a, b)
let oldInPort p =
  Test (Location p)

  (* Naive compilation: does not guarantee loop-free semantics
     Possible issues:
     1) reg contains an explicit loop
     2) We compile star paths to contain the same node

     Possible solutions:
     1) Second compilation phase that detects repeated nodes and tags packets inbetween such repeats
  *)

module Q = Queue

let rec seen_link_before sw1 sw2 path =
  let open Pervasives in
  match path with
    | [] -> false
    | [_] -> false
    | a :: b :: path -> if a = sw1 && b = sw2 then true else
	seen_link_before sw1 sw2 (b :: path)

let get_nbrs g node =
  let ports = T.vertex_to_ports g node in
  T.PortSet.fold ports ~f:(fun s pt -> match (T.next_hop g node pt) with
      | None -> s
      | Some e -> T.EdgeSet.add s e) ~init:T.EdgeSet.empty

(* Need to add constraints to avoid routing through hosts *)
let rec bfs' graph queue =
  let (sw, re, path) = (Q.take queue) in
  Printf.printf "bfs' %s %s %s \n%!" (N.to_string sw) (regex_to_string re)
    (String.concat ";" (List.map N.to_string path));
  match (match_path re [Const sw]) with
    | true -> path
    | false -> 
      let nbrs = (T.neighbors graph (T.vertex_of_label graph sw)) in
      T.VertexSet.iter nbrs (fun x ->
          let x = T.vertex_to_label graph x in
	if seen_link_before sw x path then () else
	  let re' = deriv (Const x) re in
	  match is_empty re' with
	    | false ->
	      Q.add (x, re', x :: path) queue
	    | true -> ());
      bfs' graph queue


exception NoPath of string*string

let bfs graph re = 
  let q = Queue.create () in
  T.VertexSet.iter (T.vertexes graph)
    (fun src' ->
      let src = T.vertex_to_label graph src' in
      let re' = deriv (Const src) re in
      match is_empty re' with
      | false -> Q.add (src, re', [src]) q
      | true -> ());
  (try (bfs' graph q)
   with Queue.Empty -> raise (NoPath("unknown", "unknown")))


let expand_re re topo = 
  try let return = List.rev (bfs topo re) in
      (* Printf.printf "expand_re returned %s\n" (String.concat ";" (List.map N.to_string return)); *)
      return
  with 
    | NoPath(s1,s2) -> Printf.printf "Couldn't find path for %s in graph\n\t%s\n" (regex_to_string re) (Net.Pretty.to_string topo);
      raise (NoPath("unknown","unknown"))

let shortest_path_re re src topo = 
  Printf.printf "shortest_path_re %s %s %s\n" (regex_to_string re) (N.to_string src) (Net.Pretty.to_string topo);
  let q = Queue.create () in
  let re' = deriv (Const src) re in
  (match is_empty re' with
    | false -> Q.add (src, re', [src]) q
    | true -> ());
  (try List.rev (bfs' topo q)
   with Queue.Empty -> raise (NoPath("unknown", "unknown")))
 
let rec compile_path1 pred path topo port =
  match path with
  | Switch s1 :: Switch s2 :: path ->
    let e = T.find_edge topo (T.vertex_of_label topo (Switch s1)) (T.vertex_of_label topo (Switch s2)) in
    let _,p1 = T.edge_src e in
    let _,p2 = T.edge_dst e in
    Frenetic_NetKAT.Union (Seq (Filter (And (pred, (And (oldInPort port, Test (Switch s1))))), Mod (Location (Physical p1))), ((compile_path1 pred ((Switch s2) :: path) topo (Physical p2))))
  | Switch s1 :: [Host (mac,ip)] ->
    let _,p1 = T.edge_src (T.find_edge topo (T.vertex_of_label topo (Switch s1)) (T.vertex_of_label topo (Host (mac,ip)))) in
    Seq (Filter (And (pred, (And (oldInPort port, Test (Switch s1))))),
	 Seq( Mod(Vlan 0xFFFF),
              Mod(Location (Physical p1))))
  | _ -> Filter False

let print_list printer lst = 
  Printf.sprintf "[%s]" (String.concat ";" (List.map printer lst))

let get_ports topo v1 v2 =
  let e = T.find_edge topo (T.vertex_of_label topo v1) (T.vertex_of_label topo v2) in
  snd (T.edge_src e), snd (T.edge_dst e)

let compile_path pred path topo vid =
  match path with
  | Host (mac1,ip1) :: Switch s :: [Host (mac2,ip2)] ->
    let (_,p1) = get_ports topo (Host (mac1, ip1)) (Switch s) in
    let (p2,_) = get_ports topo (Switch s) (Host (mac2, ip2)) in
    Seq (Filter (And (pred, (And (oldInPort (Physical p1), Test (Switch s))))),
         Mod (Location (Physical p2)))
  | Host (h1,h2) :: Switch s1 :: Switch s2 :: path ->
    let _,inport = get_ports topo (Host (h1,h2)) (Switch s1) in
    let p1,p2 = get_ports topo (Switch s1) (Switch s2) in
    let pol = oldPol (And (pred, (And (oldInPort (Physical inport), Test (Switch s1)))),
		      Seq( Mod(Vlan vid),
                           Mod(Location (Physical p1)))) in
    Frenetic_NetKAT.Union (pol, compile_path1 (And (And (Test (Vlan vid), Test (VlanPcp 0)), pred)) (Switch s2 :: path) topo (Physical p2))
  | [] -> Filter False
  | _ -> failwith (Printf.sprintf "Trying to compile path %s which does not start with a host followed by a switch" (print_list N.to_string path))



module Gensym =
struct
  let count = ref 0
  let next () = incr count; !count
end

let rec dnf_form pol = match pol with
  | RegPol _ -> true
  | RegInter(RegPol _, RegPol _) -> true
  | RegInter(RegPol _, RegInter _) -> false
  | RegInter(RegInter _, RegPol _) -> false
  | RegInter(RegUnion _, _) -> false
  | RegInter(_, RegUnion _) -> false
  | RegInter(a,b) -> dnf_form a & dnf_form b
  | RegUnion(RegPol _, RegPol _) -> true
  | RegUnion(RegPol _, RegInter (a,b)) -> dnf_form (RegInter (a,b))
  | RegUnion(RegInter (a,b), RegPol _) -> dnf_form (RegInter (a,b))
  | RegUnion(RegUnion _, RegPol _) -> false
  | RegUnion(RegPol _, RegUnion _) -> false
  | RegUnion(a,b) -> dnf_form a & dnf_form b

let rec to_dnf' pol = match pol with
  | RegInter (RegPol _, RegPol _) -> pol
  | RegInter (RegUnion(a,b), c) -> RegUnion(to_dnf' (RegInter(a,c)), to_dnf' (RegInter(b,c)))
  | RegInter (c,RegUnion(a,b)) -> RegUnion(to_dnf' (RegInter(a,c)), to_dnf' (RegInter(b,c)))
  | RegInter (RegInter (pol1,pol2), RegPol(pr,re,k) ) -> 
    let pol3 = RegPol(pr,re,k) in
    RegInter (to_dnf' (RegInter(pol1, pol3)), 
	      to_dnf' (RegInter(pol2, pol3)))
  | RegInter (RegPol(pr,re,k), RegInter (pol1,pol2)) -> 
    let pol3 = RegPol(pr,re,k) in
    RegInter (to_dnf' (RegInter(pol1, pol3)), 
	      to_dnf' (RegInter(pol2, pol3)))
  | RegInter (a,b) -> RegInter(to_dnf' a, to_dnf' b)
  | RegUnion (RegPol _, RegPol _) -> pol
  | RegUnion (RegUnion(a,b), RegPol (pr,re,k)) -> RegUnion( to_dnf' (RegUnion (a, (RegPol (pr, re, k)))),
							    to_dnf' (RegUnion (b, (RegPol (pr, re, k)))))
  | RegUnion (RegPol (pr,re,k), RegUnion(a,b)) -> RegUnion( to_dnf' (RegUnion (a, (RegPol (pr, re, k)))),
							    to_dnf' (RegUnion (b, (RegPol (pr, re, k)))))
  | RegUnion (a, b) -> RegUnion( to_dnf' a, to_dnf' b)
  | RegPol _ -> pol

let rec to_dnf pol = 
  let pol' = to_dnf' pol in
  if dnf_form pol' then pol'
  else to_dnf pol'

let rec to_dnf_list pol = match pol with
  | RegUnion(a,b) -> to_dnf_list a @ to_dnf_list b
  | RegInter(a,b) -> [List.concat (to_dnf_list a @ to_dnf_list b) ]
  | RegPol(pr,re,k) -> [[(pr,re,k)]]

let rec dnf_form_pred pred = match pred with
  | And(Or _, _) -> false
  | And(_, Or _) -> false
  | Neg (Neg _) -> false
  | Neg (And _) -> false
  | Neg (Or _) -> false
  | And(a,b) -> dnf_form_pred a & dnf_form_pred b
  | Or(a,b) -> dnf_form_pred a & dnf_form_pred b
  | _ -> true

let rec to_dnf_pred' pred = match pred with
  | And (Or(a,b), c) -> Or(to_dnf_pred' (And(a,c)), to_dnf_pred' (And(b,c)))
  | And (c,Or(a,b)) -> Or(to_dnf_pred' (And(a,c)), to_dnf_pred' (And(b,c)))
  | And (a,b) -> And(to_dnf_pred' a, to_dnf_pred' b)
  | Or (a, b) -> Or( to_dnf_pred' a, to_dnf_pred' b)
  | _ -> pred

let rec demorganize pred = match pred with
  | Neg (And(a,b)) -> Or (demorganize (Neg a), demorganize (Neg b))
  | Neg (Or(a,b)) -> And (demorganize (Neg a), demorganize (Neg b))
  | Neg (Neg a) -> demorganize a
  | And(a,b) -> And (demorganize a, demorganize b)
  | Or(a,b) -> Or (demorganize a, demorganize b)
  | _ -> pred

let rec to_dnf_pred'' pred = 
  let pred' = to_dnf_pred' pred in
  if dnf_form_pred pred' then pred'
  else to_dnf_pred'' pred'

let rec to_dnf_pred pred = 
  to_dnf_pred'' (demorganize pred)

(* takes a DNF pred *)
let rec to_dnf_pred_list pred : pred list list = match pred with
  | Or(a,b) -> to_dnf_pred_list a @ to_dnf_pred_list b
  | And(a,b) -> [List.concat (to_dnf_pred_list a @ to_dnf_pred_list b)]
  | _ -> [[pred]]

let trivial_pol = RegPol(False, Star, 0)

let rec product lst1 lst2 = match lst1 with
  | [] -> []
  | a :: lst1 -> (List.map (fun x -> (a,x)) lst2) @ product lst1 lst2

(* returns true if atomic pred pr2 is subsumed by atomic pred pr1 *)
let atom_matches pr1 pr2 = match pr1,pr2 with
  | True,_ -> true
  | _,False -> true
  | _ -> pr1 = pr2
  
(* pr2 is an atomic pred *)
let rec pred_matches pr1 pr2 =
  match pr1 with
    | True -> true
    | False -> false
    | And(a,b) -> pred_matches a pr2 & pred_matches b pr2
    | Or(a,b) -> pred_matches a pr2 or pred_matches b pr2
    | Neg a -> not (pred_matches a pr2)
    | _ -> pr1 = pr2

(* thm: for phi,psi DNF, phi overlaps with psi iff one conjunction of phi overlaps with one conjunction of psi *)
(* DNF normal form: list of list of atoms *)
(* let rec preds_overlap pr1 pr2 = match pr1 with *)
(*   | All -> not (is_empty_pred pr2) *)
(*   | NoPackets -> false *)
(*   | Or(a,b) -> preds_overlap a pr2 or preds_overlap b pr2 *)
(*   | Not a -> not (preds_overlap a (Not pr2)) *)
(*   | And(a,b) -> preds_overlap a pr2 & preds_overlap b pr2 *)
(*   (\* atomic case *\) *)
(*   | _ -> pred_matches pr2 pr1 *)
(* and *)
(*     is_empty_pred pr = match pr with *)
(*       | NoPackets -> true *)
(*       | All -> false *)
(*       | DlType _ -> false *)
(*       | Or(a,b) -> is_empty_pred a & is_empty_pred b *)
(*       | Not a -> not (is_empty_pred a) *)
(*       | And(a,b) -> not (preds_overlap a b) *)

(* preds in DNF list form *)
let rec conj_overlap' pr1 pr2 =
  List.for_all (fun pr -> List.exists (atom_matches pr) pr2) pr1

(* preds in DNF list form *)
let rec conj_overlap pr1 pr2 = conj_overlap' pr1 pr2 or conj_overlap' pr2 pr1

(* preds in DNF list form *)
let rec preds_overlap pr1 pr2 = 
  (List.for_all (fun pr -> List.exists (conj_overlap pr) pr2) pr1) or
    (List.for_all (fun pr -> List.exists (conj_overlap pr) pr1) pr2)

let contradiction cnj = List.exists (fun pr -> not (conj_overlap [pr] cnj)) cnj

let is_empty dnf = List.for_all contradiction dnf

(* pr2 approx: may say no when equiv *)
let rec preds_equiv pr1 pr2 =
  pr1 = pr2

let rec blast_inter_list pol = 
  List.map (fun (a,b,c) -> RegPol(a,b,c)) 
    (List.map
       (fun pol -> (List.fold_left (fun (pr_a, re_a, k_a) (pr,re,k) ->
	 let pr1' = to_dnf_pred_list pr_a in
	 let pr2' = to_dnf_pred_list pr in
	 if preds_overlap pr1' pr2' 
	 then
	   (to_dnf_pred (And(pr_a, pr)), re_a && re, max k_a k)
	 else (False, Star, 0)) (True, Star, 0) pol)) pol)

(* Takes in a DNF policy and eliminates all intersections, returning a union of atomic policies *)
let rec blast_inter pol = match pol with
  | RegInter(RegPol(pr1,re1,k1), RegPol(pr2,re2,k2)) ->
    let pr1' = to_dnf_pred_list pr1 in
    let pr2' = to_dnf_pred_list pr2 in
    if preds_overlap pr1' pr2' 
    then
      RegPol(And(pr1,pr2), re1 && re2, max k1 k2)
    else trivial_pol
  | RegInter(a, b) -> 
    (* We know this will return an atomic pol b/c pol is DNF *)
    let RegPol(pr1, re1, k1) = blast_inter a in
    let RegPol(pr2, re2, k2) = blast_inter b in
    let pr1' = to_dnf_pred_list pr1 in
    let pr2' = to_dnf_pred_list pr2 in
    if preds_overlap pr1' pr2' 
    then
      RegPol(And(pr1,pr2), re1 && re2, max k1 k2)
    else trivial_pol
  | RegUnion(a, b) -> RegUnion(blast_inter a, blast_inter b)
  | RegPol _ -> pol

(* Takes in two atomic policies and returns a list of equivalent (when
   unioned) disjoint policies *)
let rec blast_union1 pol1 pol2 = match pol1,pol2 with
  | RegPol(pr1,re1,k1), RegPol(pr2,re2,k2) ->
    let pr1' = to_dnf_pred_list pr1 in
    let pr2' = to_dnf_pred_list pr2 in
    (match is_empty pr1', is_empty pr2' with
      | true,true -> []
      | true, false -> [pol2]
      | false, true -> [pol1]
      | _ ->
	if preds_equiv pr1 pr2 then
	  [RegPol(pr1, re1 && re2, max k1 k2)]
	else
	  if preds_overlap pr1' pr2' then
	    [RegPol(And (pr1, Neg pr2), re1, k1);
	     RegPol(And (pr2, Neg pr1), re2, k2);
	     RegPol(And (pr1, pr2), re1 && re2, max k1 k2)]
	  else
	    [RegPol(pr1,re1,k1); 
	     RegPol(pr2,re2,k2)])
  | _ -> failwith "blast_union1 takes atomic policies only"

(* Takes a list of atomic policies and an atomic policy 'pol' and
   removes all policies equivalent to 'pol' *)
let rec remove_dups' lst pol = match lst with
  | [] -> ([], pol)
  | RegPol(pr',re',k') :: lst -> 
    let RegPol(pr,re,k) = pol in
    if preds_equiv pr' pr then
      remove_dups' lst (RegPol(pr, re && re', max k k'))
    else
      let (rst, pol') = remove_dups' lst pol in
      (RegPol(pr',re',k') :: rst, pol')

let rec remove_dups pols = match pols with
  | [] -> []
  | p :: pols -> let rst,p' = remove_dups' pols p in
		 p' :: remove_dups rst
		   
let rec blast_union pols = match pols with
  | [] -> []
  | pol :: pols -> List.concat (List.map (blast_union1 pol) pols) @ blast_union pols

(* let rec normalize pol =  *)
(*   blast_union (blast_inter (to_dnf pol)) *)

let rec simpl_pred pred = match pred with
  | Or(True, _) -> True
  | Or(_, True) -> True
  | Or(False, a) -> a
  | Or(a, False) -> a
  | Or (a,b) -> let a' = simpl_pred a in
		let b' = simpl_pred b in
		if preds_equiv a' b' then a' else Or (a', b')
  | And(True, a) -> a
  | And(a, True) -> a
  | And(False, _) -> False
  | And(_, False) -> False
  | And (a,b) -> let a' = simpl_pred a in
		 let b' = simpl_pred b in
		 if preds_equiv a' b' then a' else And (a', b')
  | Neg (Neg a) -> a
  | Neg True -> False
  | Neg False -> True
  | _ -> pred

let rec simpl_re re = match re with
  | R.Union(Star, _) -> Star
  | R.Union(_, Star) -> Star
  | R.Union(a, EmptySet) -> simpl_re a
  | R.Union(EmptySet, a) -> simpl_re a
  | R.Union(a,b) -> let a' = simpl_re a in
		  let b' = simpl_re b in
		  if a' = b' then a' else R.Union(a',b')
  | Intersection(Star, a) -> simpl_re a
  | Intersection(a, Star) -> simpl_re a
  | Intersection(EmptySet, a) -> EmptySet
  | Intersection(a, EmptySet) -> EmptySet
  | Intersection(a,b) -> let a' = simpl_re a in
			 let b' = simpl_re b in
			 if a' = b' then a' else Intersection(a',b')
  | Sequence(Empty,a) -> simpl_re a
  | Sequence(a,Empty) -> simpl_re a
  | Sequence(EmptySet, a) -> EmptySet
  | Sequence(a, EmptySet) -> EmptySet
  | Sequence(a,b) -> Sequence(simpl_re a, simpl_re b)
  | Comp Star -> EmptySet
  | Comp EmptySet -> Star
  | Comp (Comp a) -> simpl_re a
  | Comp a -> Comp (simpl_re a)
  | _ -> re

(* Simplifies each policy in a list of atomic policies *)
let simpl_pol = 
  List.map (fun pol ->
    let RegPol(pr,re,k) = pol in
    RegPol(simpl_pred pr, simpl_re re, k))

(* Removes trivial policies (empty pred/re) from a list of atomic policies *)
let remove_trivial = 
  List.fold_left (fun acc p -> 
    let RegPol(pr,re,k) = p in
    if pr = False or re = EmptySet then acc else p :: acc) []

(* Takes in a list of atomic policies *)
let simplify pols =
  remove_dups (remove_trivial (simpl_pol pols))

(* atomic pols *)
let pols_disjoint pol1 pol2 = 
  let RegPol(pr1,_,_) = pol1 in
  let RegPol(pr2,_,_) = pol2 in
  let pr1' = to_dnf_pred_list (to_dnf_pred pr1) in
  let pr2' = to_dnf_pred_list (to_dnf_pred pr2) in
  not (preds_overlap pr1' pr2')

let rec are_disjoint pols = match pols with
  | [] -> true
  | pol :: pols -> List.for_all (pols_disjoint pol) pols & are_disjoint pols

(* iterate until the set of policies are disjoint. At each step,
   subtract off all the policies that do not overlap with any other
   policies, iterate over the remainder *)

let rec normalize' pols = 
  let pols' = simplify (blast_union (simplify pols))
  in
  if are_disjoint pols' then pols' else normalize' pols'

let normalize pol = 
  normalize' (blast_inter_list (to_dnf_list (to_dnf pol)))

let rec compile_regex pol topo = match pol with
  | RegPol (pred, reg, _) -> compile_path pred (expand_re reg topo) topo (Gensym.next ())
  | RegUnion (pol1, pol2) -> Frenetic_NetKAT.Union (compile_regex pol1 topo, compile_regex pol2 topo)
