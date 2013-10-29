open Regex

val compile_regex : regex_policy -> NetCore_Graph.Graph.graph -> NetCore_Types.pol
val shortest_path_re : regex -> NetCore_Graph.Graph.node -> NetCore_Graph.Graph.graph -> NetCore_Graph.Graph.node list
val expand_re : regex -> NetCore_Graph.Graph.graph -> NetCore_Graph.Graph.node list
val to_dnf : regex_policy -> regex_policy
val blast_inter : regex_policy -> regex_policy
val normalize : regex_policy -> regex_policy list
