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

-module(erlpmd_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Ips,Port) when is_list(Ips) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Ips,Port]).

start_link(Ips, Port, StoreMod, StoreOpts) when is_list(Ips), is_atom(StoreMod), is_list(StoreOpts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Ips, Port, StoreMod, StoreOpts]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Ips,Port | Opts ]) when is_list (Ips) ->
    [StoreMod, StoreOpts] =
        case Opts of
            [] ->
                % TODO This is really unclean. No opts if you set it via appenv?
                SMod = application:get_env(erlpmd, storage_module, erlpmd_ets),
                [SMod, []];
            [SMod, SOpts] when is_atom(SMod), is_list(SOpts) ->
                Opts
        end,
    ErlPMD = {erlpmd,
              {erlpmd, start_link, [[StoreMod, StoreOpts]]},
              transient, 5000, worker, [erlpmd]},
	Listeners = [{{ip, Ip},
                  {erlpmd_tcp_listener, start_link, [[Ip,Port]]},
                  transient, 5000, worker, [erlpmd_tcp_listener]}
                 || Ip <- Ips],
	{ok, {{one_for_one, 5, 10}, [ErlPMD | Listeners]}}.

