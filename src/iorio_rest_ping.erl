-module(iorio_rest_ping).

-export([init/3, terminate/3]).

-ignore_xref([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2
        ]).

-ignore_xref([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2
        ]).

-record(state, {mod, mod_state}).

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest};
init({ssl, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
    {mod, Mod} = proplists:lookup(mod, Opts),
    {mod_state, ModState} = proplists:lookup(mod_state, Opts),
    {ok, Req, #state{mod=Mod, mod_state=ModState}}.

allowed_methods(Req, State) -> {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, State=#state{mod=Mod, mod_state=ModState}) ->
    lager:info("handling ping"),
    {pong, Partition} = Mod:ping(ModState),
    {iorio_json:encode([{pong, integer_to_binary(Partition)}]), Req, State}.

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.
