open Graph

module G = Imperative.Digraph.Abstract(struct type t = string end)
module SCC = Components.Make(G)

let _ =
  let g = G.create() in
  let a = G.V.create "a" in
  let b = G.V.create "b" in
  let c = G.V.create "c" in
  G.add_edge g a b;
  G.add_edge g b a;
  G.add_edge g c a;
  let vss = SCC.scc_list g in
  List.iter begin fun vs ->
    Printf.printf "[%s]\n" (String.concat "," (List.map G.V.label vs))
  end vss
