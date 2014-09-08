open Async_NetKAT

module G = Net.Topology

let s1 = Switch 1L
let s2 = Switch 2L
let s3 = Switch 3L
let s4 = Switch 4L

let h1 = Host (1L, 1l)
let h2 = Host (2L, 2l)

let add_edges topo edges = List.fold_left (fun topo (h1,p1,h2,p2) -> fst (G.add_edge topo (G.vertex_of_label topo h1) p1 () (G.vertex_of_label topo h2) p2)) topo edges

let add_vertexes topo vertexes = List.fold_left (fun topo v -> fst (G.add_vertex topo v)) topo vertexes

let make_topo () =
  let topo = add_vertexes (G.empty ())
      [s1; s2; s3; s4; h1; h2]
  in
    add_edges topo [(h1, (Int32.of_int 1), s1, (Int32.of_int 1));
                    (h2, (Int32.of_int 1), s4, (Int32.of_int 1));
                    (s1, (Int32.of_int 2), s2, (Int32.of_int 1));
                    (s2, (Int32.of_int 1), s1, (Int32.of_int 2));
                    (s1, (Int32.of_int 3), s3, (Int32.of_int 1));
                    (s3, (Int32.of_int 1), s1, (Int32.of_int 3));
                    (s2, (Int32.of_int 2), s3, (Int32.of_int 2));
                    (s3, (Int32.of_int 2), s2, (Int32.of_int 2));
                    (s2, (Int32.of_int 3), s4, (Int32.of_int 2));
                    (s4, (Int32.of_int 2), s2, (Int32.of_int 3));
                    (s3, (Int32.of_int 3), s4, (Int32.of_int 3));
                    (s4, (Int32.of_int 3), s3, (Int32.of_int 3))]
