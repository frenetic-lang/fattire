open NetKAT_Types
open Pathetic.Regex
open Pathetic.RegexUtils
open Async_NetKAT
(* open NetCoreFT *)

module G = Net.Topology

let fake_choice (a,b) = NetKAT_Types.Union (a,b)

(* open NetCoreEval0x04 *)

(** Constructing k-resilient trees (k-trees) from regular expression paths **)

let trivial_pol = Filter False

let rec action_choice_to_choice lst = match lst with
  | [a] -> a
  | a :: lst -> fake_choice (a, action_choice_to_choice lst)

let lpar (a,b) = Seq (Filter a, action_choice_to_choice b)

(* Tree w/ ordered children. Leafs are hosts, internal nodes are switches *)
type k_tree = 
  | KLeaf of node
  | KTree of node * (k_tree list)
  | KRoot of node * k_tree

exception NoTree of string

let print_list prntr lst = String.concat "; " (List.map prntr lst)
let print_tuple prntr1 prntr2 tpl = Printf.sprintf "(%s,%s)" (prntr1 (fst tpl)) (prntr2 (snd tpl))
let print_tuple1 prntr tpl = Printf.sprintf "(%s,%s)" (prntr (fst tpl)) (prntr (snd tpl))

let node_to_string n = match n with
  | Switch sw -> Printf.sprintf "Switch %Ld" sw
  | Host (mac, ip) -> Printf.sprintf "Host (%Ld, %ld)" mac ip

let rec k_tree_to_string tree = match tree with
  | KLeaf n -> 
    Printf.sprintf "KLeaf (%s)" (node_to_string n)
  | KTree(n, children) -> 
    Printf.sprintf "KTree(%s, [ %s ])" (node_to_string n) (print_list k_tree_to_string children)
  | KRoot(root, child) -> Printf.sprintf "KRoot (%s, %s)" (node_to_string root) (k_tree_to_string child)

let del_links topo links = List.fold_left (fun topo l -> G.remove_edge topo l) topo links

let shortest_path_fail_set re sw topo fail_set =
  (* Printf.printf "[FaultTolerance.ml] shortest_path_fail_set %s %s %s\n" (regex_to_string re) (G.node_to_string sw) *)
  (*   (print_list (print_tuple1 G.node_to_string) fail_set); *)
  let topo' = del_links topo fail_set in
  List.tl (shortest_path_re re sw topo')

(* Initial version: no backtracking *)
(* Build an (n - k) fault tolerant tree along 'path', avoiding links in 'fail_set' *)
let rec build_k_tree_from_path path regex n k fail_set topo = 
  (* Printf.printf "[FaultTolerance.ml] build_k_tree_from_path %s %s %d %d [%s]\n%!" (print_list G.node_to_string path) (regex_to_string regex) n k (print_list (print_tuple1 G.node_to_string) fail_set); *)
  match path with
    | sw :: [ h ] -> Some (KTree(sw, [KLeaf h]))
    | Host (mac,ip) :: path ->
      (match build_k_tree_from_path path (deriv (Const (Host (mac,ip))) regex) n k fail_set topo with
	(* We haven't made any choices at this point, so we backtrack
	   up to our parent if we fail *)
	| None -> None
	| Some tree -> Some (KRoot (Host (mac, ip), tree)))
    | sw :: path -> 
      (match build_k_children sw (deriv (Const sw) regex) n k fail_set topo Star with
	(* We haven't made any choices at this point, so we backtrack
	   up to our parent if we fail *)
	| None -> None
	| Some children -> Some (KTree(sw, children)))
    | [] -> failwith "Can not build ktree from empty path"
and
    (* Build (n - k) backup paths at 'sw' according to 'regex',
       avoiding links in 'fail_set'. Order regex restricts the nodes
       we're allowed to use for the primary path and is used for
       backtracking *)
    build_k_children sw regex n k fail_set topo order_regex =
  (* Printf.printf "[FaultTolerance.ml] build_k_children %s %d %d [%s]\n%!" (regex_to_string regex) n k (print_list (print_tuple1 G.node_to_string) fail_set); *)
  if k > n then Some [] 
  else
    let path = shortest_path_fail_set (Intersection(regex, order_regex)) sw topo fail_set in
    match (List.hd path) with
      | Host (mac,ip) -> Some [KLeaf (Host (mac,ip))]
      | new_sw' -> let bad_choice = Comp (Sequence(Const new_sw', Star)) in
		   (match build_k_tree_from_path path regex n k fail_set topo with
		     (* If we fail then we need to pick a new path *)
		     | None -> let bad_path = Intersection(regex, bad_choice) in
			       build_k_children sw bad_path n k fail_set topo order_regex
       | Some tree -> let e = G.find_edge topo (G.vertex_of_label topo sw) (G.vertex_of_label topo new_sw') in
         (match build_k_children sw regex n (k + 1) (e :: fail_set) topo Star with
	      (* If we fail here, either because we chose a bad
		 ordering, or because we chose a bad path earlier. We try
		 all orderings, then backtrack to find a new path *)
			 | None -> build_k_children sw regex n k fail_set topo (Intersection(order_regex, bad_choice))
			 | Some children -> Some (tree :: children)))


let build_k_tree n regex topo = 
  let path = expand_re regex topo in
  match List.hd path with
    | Host (mac,ip) ->
      (match build_k_tree_from_path path regex n 0 [] topo with
	| None -> raise (NoTree "failed to build k-tree")
	| Some tree -> tree)
    | _ -> failwith (Printf.sprintf "Path %s returned for %s doesn't begin with a host" (print_list node_to_string path) (regex_to_string regex))


(** Compiling k-trees into NetCore policies **)

let strip_tag  = Mod (Vlan 0xFFFF)

let stamp_path_tag pathTag tag = 
  (* If we set the VLAN, the controller will push a new tag on. Should probably fix that *)
  Seq (Mod (Vlan pathTag),
       Mod (VlanPcp tag))


let stamp_tag tag = 
  (* If we set the VLAN, the controller will push a new tag on. Should probably fix that *)
  Mod (VlanPcp tag)

let match_tag pathTag tag = 
  (And (Test (Vlan pathTag),
        Test (VlanPcp tag)))

module GenSym =
struct
  let create () = ref 0
  let next_val g =  incr g; !g
end

type tagged_k_tree = 
  | KLeaf_t of node
  | KTree_t of node * ((int * tagged_k_tree) list)
  | KRoot_t of node * tagged_k_tree

let rec tagged_k_tree_to_string tree = match tree with
  | KLeaf_t h -> Printf.sprintf "KLeaf_t %s" (node_to_string h)
  | KTree_t(sw, children) -> Printf.sprintf "KTree(%s, [ %s ])" (node_to_string sw)
    (print_list (print_tuple (Printf.sprintf "%d") tagged_k_tree_to_string) children)
  | KRoot_t(h, tree) -> Printf.sprintf "KRoot_t %s (%s)" (node_to_string h) (tagged_k_tree_to_string tree)

let rec tag_k_tree tree tag gensym = match tree with
  | KLeaf h -> KLeaf_t h
  | KTree (sw, (first :: rest)) -> 
    (* First child gets the parent's tag *)
    let first_child = (tag, tag_k_tree first tag gensym) in
    (* Other children get unique genSym'ed tag *)
    let backup_children = List.map (fun child -> let new_tag = (GenSym.next_val gensym) in
						 (new_tag, tag_k_tree child new_tag gensym)) rest in
    KTree_t (sw, first_child :: backup_children)
  | KRoot (h, tree) -> KRoot_t(h, tag_k_tree tree tag gensym)
  | KTree(sw, []) -> failwith (Printf.sprintf "tag_k_tree: ktree at node %s contained no children" (node_to_string sw))

let get_ports topo v1 v2 =
  let e = G.find_edge topo (G.vertex_of_label topo v1) (G.vertex_of_label topo v2) in
  snd (G.edge_src e), snd (G.edge_dst e)

let next_port_from_k_tree sw topo pathTag tree = 
  (* Printf.printf "[FaulTolerance.ml] next_hop_from_k_tree %s\n%!" (k_tree_to_string tree); *)
  match tree with
  | (tag, KLeaf_t host) -> 
    let (_,p1) = get_ports topo host sw in
    Seq(strip_tag, Mod(Location (Physical p1)))
  | (tag, KTree_t (sw', _)) -> 
    let p1,p2 = get_ports topo sw sw' in
    Seq(stamp_tag tag, Mod(Location (Physical p1)))
  | (tag, KRoot_t (h,sw)) ->
    failwith "next_port_from_k_tree: off-by-one error. Should not be called on KRoot_t node"

let next_hop_from_k_tree sw topo tree = 
  (* Printf.printf "[FaulTolerance.ml] next_hop_from_k_tree %s\n%!" (k_tree_to_string tree); *)
  match tree with
  | (_, KLeaf_t host) -> 
    let (p1,p2) = get_ports topo host sw in
    (sw, p1, tree)
  | (_, KTree_t (sw', _)) -> 
    let p1,p2 = get_ports topo sw sw' in
    (sw', p2, tree)
  | (tag, KRoot_t (h,sw)) ->
    failwith "next_hop_from_k_tree: off-by-one error. Should not be called on KRoot_t node"

(* Converts a k fault tolerant tree into a NetCore policy *)
let rec policy_from_k_tree' inport tree topo path_tag tag = 
  (* Printf.printf "[FaulTolerance.ml] policy_from_k_tree' %ld %s\n%!" inport (tagged_k_tree_to_string tree); *)
  match tree with
    | KLeaf_t h -> 
      trivial_pol
    | KTree_t(sw', children) -> 
      let Switch sw = sw' in
      let children_actions = List.map (next_port_from_k_tree sw' topo path_tag) children in
      let backup = lpar(And( Test(Switch sw), And (match_tag path_tag tag, Test (Location (Physical inport)))),
			children_actions) in
      let next_hops = List.map (next_hop_from_k_tree sw' topo) children in
      let children_pols = List.fold_left 
	(fun a (sw'', inport,tree) -> 
	  NetKAT_Types.Union(a, policy_from_k_tree' inport (snd tree) topo path_tag (fst tree))) trivial_pol next_hops in
      NetKAT_Types.Union(backup, children_pols)

let next_port_from_k_tree_root sw topo pathTag tree = 
  match tree with
  | (tag, KLeaf_t host) -> 
    let (_,p1) = get_ports topo host sw in
    Seq(strip_tag, Mod (Location (Physical p1)))
  | (tag, KTree_t (sw', _)) ->
    let p1,p2 = get_ports topo sw sw' in
    Seq(stamp_path_tag pathTag tag, Mod(Location (Physical p1)))

let policy_from_k_tree pr tree topo path_tag tag =  
  (* Printf.printf "[FaulTolerance.ml] policy_from_k_tree %s\n%!" (tagged_k_tree_to_string tree); *)
  match tree with
    | KRoot_t(Host (mac, ip), KTree_t(Switch sw, children)) ->
      let sw' = Switch sw in
      let _,inport = get_ports topo (Host (mac,ip)) sw' in
      let children_ports = List.map (next_port_from_k_tree_root sw' topo path_tag) children in
      let backup = lpar(And( Test (Switch sw), And (pr, Test (Location (Physical inport)))),
			children_ports) in
      let next_hops = List.map (next_hop_from_k_tree (Switch sw) topo) children in
      let children_pols = List.fold_left 
	(fun a (sw'', inport,treeTag) -> 
	  NetKAT_Types.Union(a, policy_from_k_tree' inport (snd treeTag) topo path_tag (fst treeTag))) trivial_pol next_hops in
      NetKAT_Types.Union(backup, children_pols)


(* let rec pred_to_netkat_pred pr = match pr with *)
(*   | NetCore_Types.Hdr _ -> failwith "NYI: convert header to NetKAT" *)
(*   | NetCore_Types.OnSwitch sw -> Test (Switch, VInt.Int64 sw) *)
(*   | NetCore_Types.Or (a,b) -> Types.Or (pred_to_netkat_pred a, pred_to_netkat_pred b) *)
(*   | NetCore_Types.And (a,b) -> Types.And (pred_to_netkat_pred a, pred_to_netkat_pred b) *)
(*   | NetCore_Types.Not a -> Neg (pred_to_netkat_pred a) *)
(*   | NetCore_Types.Everything -> True *)
(*   | NetCore_Types.Nothing -> False *)

let rec compile_ft_regex pol vid topo = 
  let RegPol(pred, regex, k) = pol in
  let ktree = build_k_tree k regex topo in
  let genSym = GenSym.create() in
  let tag = GenSym.next_val genSym in
  let tagged_ktree = tag_k_tree ktree tag genSym in
  policy_from_k_tree pred tagged_ktree topo vid tag

let rec compile_ft_to_kat regpol topo =
  let genSym = GenSym.create() in
  let pols = normalize regpol in
  List.fold_left (fun acc pol -> let vid = (GenSym.next_val genSym) in 
				 NetKAT_Types.Union(compile_ft_regex pol vid topo, acc))
    trivial_pol pols
