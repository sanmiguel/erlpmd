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

init([]) ->
	erlpmd = ets:new(erlpmd, [public, named_table]),
	{ok, erlpmd}.

register_node(NodeName, {PortNo, NodeType, Protocol, HighestVersion, LowestVersion, Extra}, Fd, Creation, erlpmd) ->
	case ets:lookup(erlpmd, NodeName) of
		[] ->
			ets:insert_new(erlpmd, {NodeName, {PortNo, NodeType, Protocol, HighestVersion, LowestVersion, Extra, Fd, Creation}}),
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
		  || [Name, Port] <- ets:match(erlpmd, {'$1', {'$2', 77, '_', '_', '_', '_', '_', '_'}})]}.

dump(all, erlpmd) -> dump('_', erlpmd);
dump(NodeType, erlpmd) ->
	{ok, [{Name, Port, Fd}
		  || [Name, Port, Fd] <- ets:match(erlpmd,
										   {'$1', {'$2', NodeType, '_', '_', '_', '_', '$3', '_'}})]}.

node_stopped(Fd, erlpmd) ->
	case ets:match(erlpmd, {'$1', {'_', '_', '_', '_', '_', '_', '_', Fd}}) of
		[[NodeName]] -> remove_node(NodeName, erlpmd);
		_ -> ok
	end.

remove_node(NodeName, erlpmd) ->
	ets:delete(NodeName).
