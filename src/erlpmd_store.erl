-module(erlpmd_store).

-include("erlpmd.hrl").

-callback init(Args :: list(any()) ) -> {ok, State :: any()}
					| {error, Reason :: term()}.

-callback register_node(
			NodeName :: binary(),
			{
			 PortNo :: inet:port_number(),
			 NodeType :: ?NORMAL_NODE | ?HIDDEN_NODE,
			 Protocol :: non_neg_integer(),
			 HighestVersion :: non_neg_integer(),
			 LowestVersion :: non_neg_integer(),
			 Extra :: binary()
			},
			Fd :: inet:socket(),
			Creation :: non_neg_integer(),
			State :: any()) ->
	ok
	| {ok, State1 :: any()}
	| {error, registered}
	| {error, Reason :: term()}.

-callback node_port(NodeName :: binary(), State :: any()) ->
	{ok, PortNo :: inet:port_number()}
	| {error, Reason :: term()}.

-callback names(Type :: ?NORMAL_NODE | ?HIDDEN_NODE, State :: any()) ->
	{ok, Names :: list(binary())}
	| {error, Reason :: term()}.

-callback dump(NodeType :: ?NORMAL_NODE | ?HIDDEN_NODE | all, State :: any()) ->
	{ok, list({Name :: binary(), Port :: inet:port_number(), Fd :: inet:socket()})}
	| {error, Reason :: term()}.

-callback node_stopped(Fd :: inet:socket(), State :: any()) ->
	ok
	| {ok, State1 :: any()}
	| {error, Reason :: term()}.

-callback remove_node(NodeName :: binary(), State :: any()) ->
	ok
	| {ok, State1 :: any()}
	| {error, Reason :: term()}.

%% TODO While it doesn't require any work for erlpmd_ets, maybe we need a callback for KILL_REQ?
