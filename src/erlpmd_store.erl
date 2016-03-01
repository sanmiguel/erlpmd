-module(erlpmd_store).

-callback init(Args :: list(any()) ) -> {ok, State :: any()}
					| {error, Reason :: term()}.

-callback register_node(
			NodeName :: binary(),
			{
			 PortNo :: non_neg_integer(), % TODO I'm sure there's an inet type() to use
			 NodeType :: 72 | 77,
			 Protocol :: non_neg_integer(),
			 HighestVersion :: non_neg_integer(),
			 LowestVersion :: non_neg_integer(),
			 Extra :: binary()
			},
			Fd :: inet:socket(), %% TODO Check this type I can never remember the correct thing
			Creation :: non_neg_integer(),
			State :: any()) ->
	ok
	| {ok, State1 :: any()}
	| {error, registered}
	| {error, Reason :: term()}.

-callback node_port(NodeName :: binary(), State :: any()) ->
	{ok, PortNo :: non_neg_integer()}
	| {error, Reason :: term()}.

-callback names(State :: any()) ->
	{ok, Names :: list(binary())}
	| {error, Reason :: term()}.

-callback dump(NodeType :: 72 | 77 | all, State :: any()) ->
	{ok, list({Name :: binary(), Port :: non_neg_integer(), Fd :: inet:socket()})}
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
