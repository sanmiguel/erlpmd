%% Copyright (c) 2012 Peter Lemenkov.
%%
%% The MIT License
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%

-module(erlpmd).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {
         store :: {atom(), any()},
         relaxed_cmd :: boolean()
        }).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	Store = erlpmd_ets:init(),
	error_logger:info_msg("ErlPMD: started.~n"),
	self() ! notify_init,
	{ok, RelaxedCommandCheck} = application:get_env(erlpmd, relaxed_command_check),
    State = #state{
       store = {erlpmd_ets, Store},
       relaxed_cmd = RelaxedCommandCheck
      },
	{ok, State}.

handle_call(Request, From, State) ->
	error_logger:warning_msg("ErlPMD: strange call: ~p from: ~p.~n", [Request, From]),
	{reply, ok, State}.

handle_cast({{msg,From},<<$x, PortNo:16, NodeType:8, Proto:8, HiVer:16, LoVer:16, NLen:16, Rest/binary>>, Fd, Ip, Port}, State) ->
	<<NodeName:NLen/binary, _ELen:16, Extra/binary>> = Rest,
    #state{store = {Store, S0}} = State,
	Creation = random:uniform(3),
	error_logger:info_msg(
		"ErlPMD: alive request from ~s:~b PortNo: ~b, NodeType: ~b, Proto: ~b, HiVer: ~b, LoVer: ~b, NodeName: '~s', Extra: ~p, Creation: ~b.~n",
		[inet_parse:ntoa(Ip), Port, PortNo, NodeType, Proto, HiVer, LoVer, NodeName, Extra, Creation]),
    case Store:register_node(NodeName, {PortNo, NodeType, Proto, HiVer, LoVer, Extra}, Fd, Creation, S0) of
		ok ->
			gen_server:cast(From, {msg, <<$y, 0:8, Creation:16>>, Ip, Port});
        {error, registered} ->
			% Already registered - reply with error
			error_logger:error_msg("ErlPMD: ~s 'name' is already registered.~n", [NodeName]),
			gen_server:cast(From, {msg, <<$y, 1:8, 99:16>>, Ip, Port})
	end,
	{noreply, State};


handle_cast({{msg, From},<<$z, NodeName/binary>>, _Fd, Ip, Port}, State) ->
    #state{store = {Store, S0}} = State,
	error_logger:info_msg("ErlPMD: port ~s request from ~s:~p.~n", [NodeName, inet_parse:ntoa(Ip), Port]),
    case Store:node_port(NodeName, S0) of
        {error, not_found} ->
			gen_server:cast(From, {msg, <<$w, 1:8>>, Ip, Port});
		{NodeName, {PortNo, NodeType, Proto, HiVer, LoVer, Extra, _, _}} ->
			NLen = size(NodeName),
			ELen = size(Extra),
			gen_server:cast(From, {msg, <<$w, 0:8, PortNo:16, NodeType:8, Proto:8, HiVer:16, LoVer:16, NLen:16, NodeName:NLen/binary, ELen:16, Extra:ELen/binary>>, Ip, Port})
	end,
	gen_server:cast(From, {close, Ip, Port}),
	{noreply, State};

handle_cast({{msg, From},<<$n>>, _Fd, Ip, Port}, State) ->
    #state{store = {Store, S0}} = State,
	error_logger:info_msg("ErlPMD: name(s) request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
    NodeInfos = Store:names(S0),
	Nodes = list_to_binary(lists:flatten([ io_lib:format("name ~s at port ~p~n", [X, Y]) || [X, Y] <- NodeInfos])),
    %% TODO This looks like a hard-coded port number...
	gen_server:cast(From, {msg, <<4369:32, Nodes/binary>>, Ip, Port}),
	gen_server:cast(From, {close, Ip, Port}),
	{noreply, State};

handle_cast({{msg, From},<<$d>>, _Fd, Ip, Port}, State) ->
    #state{store = {Store, S0}} = State,
	error_logger:info_msg("ErlPMD: dump request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
    NodeDump = Store:dump(S0),
	Nodes = list_to_binary(lists:flatten([ io_lib:format("active name     ~s at port ~p, fd = ~p ~n", [X, Y, F]) || {X, Y, F} <- NodeDump])),
    %% TODO Again this looks suspiciously like a hard-coded port no
	gen_server:cast(From, {msg, <<4369:32, Nodes/binary>>, Ip, Port}),
	gen_server:cast(From, {close, Ip, Port}),
	{noreply, State};

handle_cast({{msg, From},<<$k>>, _Fd, Ip, Port}, #state{relaxed_cmd=true}=State) ->
	% Allow stop command in case we're running with -relaxed_command_check
	% w/o checking for actually available nodes
	error_logger:info_msg("ErlPMD: kill request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
	gen_server:cast(From, {msg, <<"OK">>, Ip, Port}),
	gen_server:cast(From, stop),
	{stop, normal, State};
handle_cast({{msg, From},<<$k>>, _Fd, Ip, Port}, #state{relaxed_cmd=false}=State) ->
    #state{store = {Store, S0}} = State,
	error_logger:info_msg("ErlPMD: kill request from ~s:~p.~n", [inet_parse:ntoa(Ip), Port]),
	gen_server:cast(From, {msg, <<"OK">>, Ip, Port}),
    %% TODO This is ETS specific knowledge '_'
    case Store:dump(all, S0) of
        {ok, []} ->
			% No live nodes - we may exit now
			gen_server:cast(From, stop),
			{stop, normal, State};
        {ok, _} ->
			% Disallow killing witl live nodes
			{noreply, State}
	end;

handle_cast({{msg, From},<<$s, NodeName/binary>>, _Fd, Ip, Port}, #state{relaxed_cmd=false}=State) ->
	% Ignore stop command in case we're running w/o -relaxed_command_check
	error_logger:info_msg("ErlPMD: '~s' stop request from ~s:~p. (IGNORED)~n", [NodeName, inet_parse:ntoa(Ip), Port]),
	gen_server:cast(From, {msg, <<"STOPPED">>, Ip, Port}),
	{noreply, State};
handle_cast({{msg, From},<<$s, NodeName/binary>>, _Fd, Ip, Port}, #state{relaxed_cmd=true}=State) ->
    #state{store = {Store, S0}} = State,
	error_logger:info_msg("ErlPMD: '~s' stop request from ~s:~p.~n", [NodeName, inet_parse:ntoa(Ip), Port]),
    case Store:dump('_', S0) of
        {ok, []} ->
			gen_server:cast(From, {msg, <<"NOEXIST">>, Ip, Port});
        {ok, _} ->
            Store:remove_node(NodeName, S0),
			gen_server:cast(From, {msg, <<"STOPPED">>, Ip, Port})
	end,
	gen_server:cast(From, {close, Ip, Port}),
	{noreply, State};

handle_cast({{close, _From}, Fd}, State) ->
    #state{store = {Store, S0}} = State,
	error_logger:info_msg("ErlPMD: closed connection: ~p.~n", [Fd]),
    Store:node_stopped(Fd, S0),
	{noreply, State};

handle_cast(Msg, State) ->
	error_logger:warning_msg("ErlPMD: strange cast: ~p.~n", [Msg]),
	{noreply, State}.

handle_info(notify_init, State) ->
	error_logger:warning_msg("ErlPMD: info: ~p while ~p.~n", [notify_init, State]),
	{module, sd_notify} == code:load_file(sd_notify) andalso sd_notify:sd_notifyf(0, "READY=1~nSTATUS=~s", ["Hello from ErlPMD"]),
	{noreply, State};

handle_info(Info, State) ->
	error_logger:warning_msg("ErlPMD: strange info: ~p.~n", [Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	error_logger:info_msg("ErlPMD: stopped.~n"),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
