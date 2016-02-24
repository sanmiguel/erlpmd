-module(erlpmd_ets).

-behaviour(erlpmd_store).
% Callback module to store data in ETS

-export([
         init/1,
         register_node/5,
         node_port/2,
         names/1,
         dump/2,
         node_stopped/2,
         remove_node/2
        ]).

-record(node, {name, port_no, type, protocol, highest_version, lowest_version,
               extra, fd, creation}).

init([]) ->
    erlpmd = ets:new(erlpmd, [public, named_table, {keypos, #node.name}]),
    {ok, erlpmd}.

register_node(NodeName, {PortNo, NodeType, Protocol, HighestVersion, LowestVersion, Extra}, Fd, Creation, erlpmd) ->
    case ets:lookup(erlpmd, NodeName) of
        [] ->
            Node = #node{name=NodeName, port_no=PortNo, type=NodeType,
                         protocol=Protocol, highest_version=HighestVersion,
                         lowest_version=LowestVersion, extra=Extra, fd=Fd, creation=Creation},
            ets:insert_new(erlpmd, Node),
            ok;
        _ -> {error, registered}
    end.

node_port(NodeName, erlpmd) ->
	case ets:lookup(erlpmd, NodeName) of
		[] ->
            {error, not_found};
        %% TODO Should probably really validate the returned structure here
		[{NodeName, _}=NodeInfo] ->
            {ok, NodeInfo}
    end.

names(erlpmd) ->
    {ok, [{Name, Port}
          || [Name, Port] <- ets:match(erlpmd, #node{name='$1', port_no='$2', type=77, _='_'})]}.

dump(all, erlpmd) -> dump('_', erlpmd);
dump(NodeType, erlpmd) ->
    {ok, [{Name, Port, Fd}
          || [Name, Port, Fd] <- ets:match(erlpmd,
                                           #node{name='$1', port_no='$2', type=NodeType, fd='$3', _='_'})]}.

node_stopped(Fd, erlpmd) ->
	case ets:match(erlpmd, #node{name='$1', fd=Fd, _='_'}) of
		[[NodeName]] -> remove_node(NodeName, erlpmd);
		_ -> ok
	end.

remove_node(NodeName, erlpmd) ->
    true = ets:delete(erlpmd, NodeName),
    {ok, erlpmd}.
