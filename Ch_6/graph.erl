-module(graph).
-compile(export_all).
-include("code/Ch_6/records.hrl").


draw(Filename) ->
  {ok, Genotype} = file:consult(Filename),
  Paths = find_paths(Genotype),
  Nodes = find_nodes(Genotype),
  write_file("paths.dot", Paths, Nodes).


find_paths(Genotype) ->
  find_paths(Genotype, []).

find_paths([], Acc) ->
  lists:flatten(Acc);
find_paths([Record | Genotype], Acc) ->
  Links = parse_record(Record),
  find_paths(Genotype, [draw_links(Links) | Acc]).

find_nodes(Genotype) ->
  find_nodes(Genotype, []).

find_nodes([], Acc) ->
  Acc;
find_nodes([Record | Genotype], Acc) ->
  Links = parse_record(Record),
  Id = element(1, Links),
  Type = element(1, Id),
  find_nodes(Genotype, [{Id, Type} | Acc]).

write_file(OutputFile, Paths, Nodes) ->
  {ok, File} = file:open(OutputFile, write),
  io:format(File, "digraph NN {~n", []),
  write_node(File, Nodes),
  write_path(File, Paths),
  io:format(File, "}", []),
  file:close(File).

write_node(_OutputFile, []) ->
  ok;
write_node(OutputFile, [{Id, Type} | Paths]) ->
  Line = io_lib:format("  ~s [label=\"~s\", color=~s];", [printable_string(Id), Type, node_color(Type)]),
  io:format(OutputFile, "~s~n", [Line]),
  write_node(OutputFile, Paths).

node_color(sensor) ->
  yellow;
node_color(neuron) ->
  green;
node_color(cortex) ->
  black;
node_color(actuator) ->
  red.

write_path(_OutputFile, []) ->
  ok;
write_path(OutputFile, [{Source, Dest} | Paths]) ->
  Line = io_lib:format("  ~s -> ~s [arrowsize=0.5, color=\"#00000020\"];", [printable_string(Source), printable_string(Dest)]),
  io:format(OutputFile, "~s~n", [Line]),
  write_path(OutputFile, Paths).

printable_string(Var) ->
  Str = io_lib:format("~p", [Var]),
  io:format("line: ~s", [Str]),
  re:replace(Str, "[^a-zA-Z0-9_]", "_", [global]).


parse_record(#cortex{id = Id}) ->
  {Id, []};
parse_record(#neuron{id = Id, output_ids = Out_Ids}) ->
  {Id, Out_Ids};
parse_record(#sensor{id = Id, fanout_ids = FO_IDS}) ->
  {Id, FO_IDS};
parse_record(#actuator{id = Id}) ->
  {Id, []}.

draw_links({Source, Dests}) ->
  draw_links(Source, Dests, []).

draw_links(_Source, [], Acc) ->
  Acc;
draw_links(Source, [Dest|Dests], Acc) ->
  draw_links(Source, Dests, [{Source, Dest} | Acc]).
