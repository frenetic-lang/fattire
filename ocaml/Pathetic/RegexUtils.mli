open Regex

val compile_regex : regex_policy -> Async_NetKAT.Net.Topology.t -> NetKAT_Types.policy
val shortest_path_re : regex -> Async_NetKAT.Node.t -> Async_NetKAT.Net.Topology.t -> Async_NetKAT.Node.t list
val expand_re : regex -> Async_NetKAT.Net.Topology.t -> Async_NetKAT.Node.t list
val to_dnf : regex_policy -> regex_policy
val blast_inter : regex_policy -> regex_policy
val normalize : regex_policy -> regex_policy list
