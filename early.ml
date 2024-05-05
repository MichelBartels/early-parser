type token =
  | NonTerminal of string
  | Terminal of string
[@@deriving show]

type rule = {
  lhs: string;
  rhs: token list;
}

type edge = {
  start: int;
  end_: int;
  lhs: string;
  rhs_parsed: token list;
  rhs_unparsed: token list;
  history: edge list;
}
[@@deriving show]

let string_of_token = function
  | NonTerminal s -> "NonTerminal " ^ s
  | Terminal s -> "Terminal " ^ s

let rec show_edge edge =
  Printf.sprintf "{Edge:\nstart=%d\nend=%d\nlhs=%s\nrhs_parsed=[%s]\nrhs_unparsed=[%s]\nhistory=[%s]}\n"
    edge.start
    edge.end_
    edge.lhs
    (List.map string_of_token edge.rhs_parsed |> String.concat ",\n")
    (List.map string_of_token edge.rhs_unparsed |> String.concat ",\n")
    (List.map show_edge edge.history |> String.concat ",\n")

module EdgeSet = Set.Make(struct
    type t = edge
    let compare e1 e2 = compare e1 e2
  end)

let show_edges edges =
  Printf.printf "Edges:\n";
  EdgeSet.iter (fun edge ->
      Printf.printf "%s\n" (show_edge edge)
    ) edges

let find_rules lhs rules =
  List.filter (fun (rule: rule) -> rule.lhs = lhs) rules

let predict edge rules =
  match edge.rhs_unparsed with
  | (NonTerminal lhs) :: _ ->
    let rules = find_rules lhs rules in
    List.map (fun rule -> {
        start = edge.end_;
        end_ = edge.end_;
        lhs = lhs;
        rhs_parsed = [];
        rhs_unparsed = rule.rhs;
        history = [];
      }) rules |> EdgeSet.of_list
  | _ -> EdgeSet.empty


let scan token i edge =
  match (edge.end_, edge.rhs_unparsed) with
  | (i', (Terminal t) :: rest) when t = token && i' = i ->
    {
      start = edge.start;
      end_ = edge.end_ + 1;
      lhs = edge.lhs;
      rhs_parsed = edge.rhs_parsed @ [Terminal t];
      rhs_unparsed = rest;
      history = [];
    }
  | _ -> edge

let complete edge edges =
  match edge.rhs_unparsed with
  | (NonTerminal lhs) :: rest ->
    let completed_edges = EdgeSet.filter (fun e -> e.lhs = lhs && e.rhs_unparsed = [] && e.start = edge.end_) edges in
    EdgeSet.map (fun completed_edge -> {
        start = edge.start;
        end_ = completed_edge.end_;
        lhs = edge.lhs;
        rhs_parsed = edge.rhs_parsed @ [NonTerminal lhs];
        rhs_unparsed = rest;
        history = edge.history @ [completed_edge];
      }) completed_edges
  | _ -> EdgeSet.empty

let start_edges rules = find_rules "S" rules |> List.map (fun rule -> {
      start = 0;
      end_ = 0;
      lhs = "S";
      rhs_parsed = [];
      rhs_unparsed = rule.rhs;
      history = [];
    }) |> EdgeSet.of_list

let closure f x =
  let rec loop x =
    let x' = f x in
    if EdgeSet.equal x x' then x else loop x'
  in
  loop x

let step rules edges (i, token) =
  let edges = closure (fun edges ->
      EdgeSet.fold (fun edge edges ->
          EdgeSet.union edges (predict edge rules)
        ) edges edges
    ) edges in
  let edges = EdgeSet.map (scan token i) edges in
  let edges = closure (fun edges ->
      EdgeSet.fold (fun edge edges ->
          EdgeSet.union edges (complete edge edges)
        ) edges edges
    ) edges in
  edges

let parse tokens rules =
  let edges = start_edges rules in
  let tokens = List.mapi (fun i token -> (i, token)) tokens in
  let edges = List.fold_left (step rules) edges tokens in
  EdgeSet.filter (fun edge -> edge.start = 0 && edge.end_ = List.length tokens && edge.lhs = "S") edges

let toy_rules = [
  { lhs = "S"; rhs = [NonTerminal "NP"; NonTerminal "VP"] };
  { lhs = "NP"; rhs = [NonTerminal "N"; NonTerminal "PP"] };
  { lhs = "NP"; rhs = [NonTerminal "N"] };
  { lhs = "PP"; rhs = [NonTerminal "P"; NonTerminal "NP"] };
  { lhs = "VP"; rhs = [NonTerminal "VP"; NonTerminal "PP"] };
  { lhs = "VP"; rhs = [NonTerminal "V"; NonTerminal "VP"] };
  { lhs = "VP"; rhs = [NonTerminal "V"; NonTerminal "NP"] };
  { lhs = "VP"; rhs = [NonTerminal "V"] };
  { lhs = "N"; rhs = [Terminal "they"] };
  { lhs = "N"; rhs = [Terminal "can"] };
  { lhs = "N"; rhs = [Terminal "fish"] };
  { lhs = "N"; rhs = [Terminal "rivers"] };
  { lhs = "N"; rhs = [Terminal "December"] };
  { lhs = "P"; rhs = [Terminal "in"] };
  { lhs = "V"; rhs = [Terminal "can"] };
  { lhs = "V"; rhs = [Terminal "fish"] };
]

let example1 = [
  "they";
  "can";
  "fish";
  "in";
  "rivers";
]

let example2 = [
  "they";
  "can";
  "fish";
  "in";
  "rivers";
  "in";
  "December";
]

let () =
  let edges = parse example1 toy_rules in
  Printf.printf "Example 1:\n";
  show_edges edges;
  let edges = parse example2 toy_rules in
  Printf.printf "Example 2:\n";
  show_edges edges
