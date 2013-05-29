-module(graph).
-compile(export_all).
-include("records.hrl").


draw(Genotype, Counter, Fitness, Iter) ->
  Paths = find_paths(ets:tab2list(Genotype)),
  Nodes = find_nodes(ets:tab2list(Genotype)),
  OutputFilename = io_lib:format("paths-~4.10.0B.dot", [Counter]),
  write_file(OutputFilename, Paths, Nodes, Fitness, Iter).

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

write_file(OutputFile, Paths, Nodes, Fitness, Iter) ->
  {ok, File} = file:open(OutputFile, write),
  io:format(File, "digraph NN {~n", []),
  io:format(File, "  label=<<table border=\"0\" cellpadding=\"2\" cellborder=\"0\" cellspacing=\"2\" align=\"left\">
  <tr>
    <td align=\"left\">Iteration:</td>
    <td align=\"left\">~B</td>
  </tr>
  <tr>
    <td align=\"left\">Fitness:</td>
    <td width=\"200\" align=\"left\">~.3f</td>
  </tr>
</table>>", [Iter, Fitness]),
  write_node(File, Nodes),
  write_path(File, Paths),
  io:format(File, "}", []),
  file:close(File).

write_node(_OutputFile, []) ->
  ok;
write_node(OutputFile, [{_, cortex} | Paths]) ->
  write_node(OutputFile, Paths);
write_node(OutputFile, [{Id, Type} | Paths]) ->
  Line = io_lib:format("  ~s [label=\"~s\", shape=~s];", [printable_string(Id), Type, node_shape(Type)]),
  io:format(OutputFile, "~s~n", [Line]),
  write_node(OutputFile, Paths).

node_shape(sensor) ->
  triangle;
node_shape(neuron) ->
  hexagon;
node_shape(cortex) ->
  ellipse;
node_shape(actuator) ->
  invtriangle.

write_path(_OutputFile, []) ->
  ok;
write_path(OutputFile, [{Source, Dest, Weight} | Paths]) ->
  Line = io_lib:format("  ~s -> ~s [arrowsize=0.5, color=\"~s\"];", [printable_string(Source), printable_string(Dest), path_color(Weight)]),
  io:format(OutputFile, "~s~n", [Line]),
  write_path(OutputFile, Paths).

path_color(Weights) ->
  path_color(Weights, []).

path_color([], Acc) ->
  string:join(Acc, ":");
path_color([undef|Weights], Acc) ->
  path_color(Weights, ["#000000" | Acc]);
path_color([Weight | Weights], Acc) ->
  Alpha = erlang:round((erlang:abs(Weight) / (math:pi() * 2)) * 255),
  if
    Weight > 0 ->
      path_color(Weights, [io_lib:format("#FF0000~.16B", [Alpha]) | Acc]);
    Weight < 0 ->
      path_color(Weights, [io_lib:format("#0000FF~.16B", [Alpha]) | Acc]);
    true ->
      path_color(Weights, [io_lib:format("#00FF00~.16B", [Alpha]) | Acc])
  end.



printable_string(Var) ->
  Str = io_lib:format("~p", [Var]),
  re:replace(Str, "[^a-zA-Z0-9_]", "_", [global]).


parse_record(#cortex{}) ->
  {{cortex,cortex}, []};
parse_record(R = #neuron{id = Id, input_idps = In}) ->
  {Id, In};
parse_record(#sensor{id = Id}) ->
  {Id, []};
parse_record(#actuator{id = Id, fanin_ids = In}) ->
  {Id, In}.

draw_links({Dest, Sources}) ->
  draw_links(Dest, Sources, []).

draw_links(_Dest, [], Acc) ->
  Acc;
draw_links(Dest, [{bias,_Weight}], Acc) ->
  draw_links(Dest, [], Acc);
draw_links(Dest = {actuator, _IdN}, [Source|Sources], Acc) ->
  draw_links(Dest, Sources, [{Source, Dest, [undef]} | Acc]);
draw_links(Dest, [Source_Weight|Sources], Acc) ->
  Source = element(1, Source_Weight),
  Weight = element(2, Source_Weight),
  draw_links(Dest, Sources, [{Source, Dest, Weight} | Acc]).
