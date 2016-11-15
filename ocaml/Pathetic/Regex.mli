(* type graph = (switchId * switchId * int) list *)
open Frenetic_NetKAT
open Frenetic_Packet

(** [node] is an entity in the network, currently either a switch with a
    datapath id, or a host with a MAC and IPv4 address. *)
type node =
  | Switch of switchId
  | Host of dlAddr * nwAddr

module Node : Frenetic_Network.VERTEX
  with type t = node
module Link : Frenetic_Network.EDGE
  with type t = unit

(** A representation of the network, with [node] as a label for vertices, and
    [unit] as labels for edges. *)
module Net : Frenetic_Network.NETWORK
  with module Topology.Vertex = Node
   and module Topology.Edge = Link

type regex =
  | Const of Node.t
  | Star
  | Sequence of regex * regex
  | Union of regex * regex
  | Intersection of regex * regex
  | Comp of regex
  | Empty
  | EmptySet

type regex_policy =
    RegPol of pred * regex * int
  | RegUnion of regex_policy * regex_policy
  | RegInter of regex_policy * regex_policy

val ( <+> ) : regex_policy -> regex_policy -> regex_policy
val ( <*> ) : regex_policy -> regex_policy -> regex_policy
val ( && ) : regex -> regex -> regex
val ( || ) : regex -> regex -> regex
val ( <.> ) : regex -> regex -> regex

val regex_to_string : regex -> string
val regexPol_to_string : regex_policy -> string
val is_empty : regex -> bool
val match_path : regex -> regex list -> bool
val deriv : regex -> regex -> regex
