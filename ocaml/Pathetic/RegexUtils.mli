open Regex

val compile_regex : regex_policy -> Net.Topology.t -> Frenetic_NetKAT.policy
val shortest_path_re : regex -> Node.t -> Net.Topology.t -> Node.t list
val expand_re : regex -> Net.Topology.t -> Node.t list
val to_dnf : regex_policy -> regex_policy
val blast_inter : regex_policy -> regex_policy
val normalize : regex_policy -> regex_policy list
