-module(iorio_listen_handler).

-export([init/4, stream/3, info/3, terminate/2]).

-ignore_xref([init/4, stream/3, info/3, terminate/2]).

-include_lib("sblob/include/sblob.hrl").
-include("include/iorio.hrl").

-record(state, {channels=[], iorio_mod, iorio_state, token, info, access, active}).

% TODO: handle CORS in COMET on bullet?
init(_Transport, Req, Opts, Active) ->
    {access, Access} = proplists:lookup(access, Opts),
    {iorio_mod, Iorio} = proplists:lookup(iorio_mod, Opts),
    {iorio_state, IorioState} = proplists:lookup(iorio_state, Opts),
    {Method, Req1} = cowboy_req:method(Req),
    case Method of
        <<"GET">> -> do_init(Req1, Access, Iorio, IorioState, Active);
        Other ->
            lager:warning("got ~p request on listen, closing connection", [Other]),
            {shutdown, Req1, #state{}}
    end.

stream(Data, Req, State) ->
    lager:debug("msg received ~p~n", [Data]),
    Msg = iorio_json:decode_plist(Data),
    Id = proplists:get_value(<<"id">>, Msg, 0),
    case proplists:get_value(<<"cmd">>, Msg) of
        <<"subscribe">> -> handle_subscribe(Msg, Id, Req, State);
        <<"unsubscribe">> -> handle_unsubscribe(Msg, Id, Req, State);
        <<"ping">> -> handle_ping(Msg, Id, Req, State);
        _ -> {reply, encode_error(<<"unknown command">>, Id), Req, State}
    end.

info({smc, {heartbeat, Props}}, Req, State=#state{channels=Channels}) ->
    Subs = proplists:get_value(subs, Props, 0),
    if Subs =< 0 ->
           lager:warning("channel subs <= 0 (~p) for ~p", [Subs, Channels]);
       true -> ok
    end,
    {ok, Req, State};
info({smc, Info}, Req, State=#state{channels=Channels}) ->
    lager:info("channel update for ~p: ~p (~p)", [self(), Info, Channels]),
    {ok, Req, State};
info({entry, BucketName, Stream, _Entry}=FullEntry, Req, State) ->
    lager:debug("entry received ~s/~s~n", [BucketName, Stream]),
    reply_entries_json([FullEntry], Req, State);

info({replay, Entries}, Req, State) when is_list(Entries) ->
    lager:debug("replay entries received~n"),
    reply_entries_json(Entries, Req, State).

% in case someone sends some weird request and we do a shutdown
terminate(_Req, #state{active=undefined}) -> ok;
terminate(_Req, #state{channels=Channels, iorio_mod=Iorio, iorio_state=IorioState, active=Active}) ->
    Pid = self(),
    lists:foreach(fun ({Bucket, Stream}) ->
                          lager:debug("unsubscribing from ~s/~s~n", [Bucket, Stream]),
                          Iorio:unsubscribe(IorioState, Bucket, Stream, Pid)
                  end, Channels),
    iorio_stats:listen_disconnect(Active),
    ok.

% private

reply_entries_json([], Req, State) ->
    {ok, Req, State};

reply_entries_json(Entries, Req, State) ->
    Json = entries_to_json(Entries),
    JsonBin = iorio_json:encode(Json),
    {reply, JsonBin, Req, State}.

entries_to_json(Entries) ->
    lists:map(fun entry_to_json/1, Entries).

entry_to_json({entry, Bucket, Stream, #sblob_entry{seqnum=SeqNum, timestamp=Timestamp, data=Data}}) ->
    [{meta, [{id, SeqNum}, {t, Timestamp}, {bucket, Bucket}, {stream, Stream}]},
     {data, iorio_json:decode_plist(Data)}].

get_channel_args(Msg) ->
    {proplists:get_value(<<"bucket">>, Msg),
     proplists:get_value(<<"stream">>, Msg)}.

encode_ok(Id, Type) ->
    iorio_json:encode([{ok, true}, {id, Id}, {type, Type}]).

handle_ping(_Msg, Id, Req, State) ->
    {reply, encode_ok(Id, <<"ping">>), Req, State}.

fail(Msg, Id, State) ->
    {encode_error(Msg, Id), State}.

with_stream(Fn, Msg, Id, Req, State) ->
    {Body, NewState} = case get_channel_args(Msg) of
               {undefined, undefined} ->
                               fail(<<"missing bucket and stream">>, Id, State);
               {undefined, _} ->
                               fail(<<"missing bucket">>, Id, State);
               {_, undefined} ->
                               fail(<<"missing stream">>, Id, State);
               {Bucket, Stream} ->
                               Fn(Bucket, Stream)
           end,

    {reply, Body, Req, NewState}.

if_has_permission(Bucket, Stream, Action, Id, Fn, State=#state{access=Access, info=Info}) ->
    {ok, Info1} = ioriol_access:update_req(Info,
                                           [{bucket, Bucket}, {stream, Stream}]),
    case ioriol_access:is_authorized_for_stream(Access, Info1, Action) of
        {ok, Info2} ->
            {FBody, State1} = Fn(Bucket, Stream),
            State2 = State1#state{info=Info2},
            {FBody, State2};
        _Other -> fail(<<"unauthorized">>, Id, State)
    end.

contains(_, []) -> false;
contains(H, [H|_]) -> true;
contains(V, [_|T]) -> contains(V, T).

remove(V, L) -> remove(V, L, []).

remove(_V, [], Accum) -> lists:reverse(Accum);
remove(V, [V|T], Accum) -> remove(V, T, Accum);
remove(V, [H|T], Accum) -> remove(V, T, [H|Accum]).

subscribe_all(Subs, State) ->
    Action = ?PERM_STREAM_GET,
    Fun = fun ({Bucket, Stream}, State0) ->
                  Fn = fun (B, S) ->
                               State1 = subscribe(B, S, nil, State0),
                               {nil, State1}
                       end,
                  {_, FState} = if_has_permission(Bucket, Stream, Action, 0,
                                                  Fn, State),
                  FState;
              ({Bucket, Stream, FromSeqNum}, State0) ->
                  Fn = fun (B, S) ->
                               State1 = subscribe(B, S, FromSeqNum, State0),
                               {nil, State1}
                       end,
                  {_, FState} = if_has_permission(Bucket, Stream, Action, 0,
                                                  Fn, State),
                  FState;
              (Sub, State0) ->
                  lager:warning("malformed sub? ~p", [Sub]),
                  State0
          end,
    lists:foldl(Fun, State, Subs).

subscribe(Bucket, Stream, FromSeqNum, State=#state{channels=Channels,
                                                   iorio_mod=Iorio,
                                                   iorio_state=IorioState}) ->
    Key = {Bucket, Stream},
    lager:debug("subscribing: ~s/~s~n", [Bucket, Stream]),
    Iorio:subscribe(IorioState, Bucket, Stream, FromSeqNum, self()),
    NewChannels = [Key|Channels],
    State#state{channels=NewChannels}.

% NOTE handle subscribing here from a seqnum?
handle_subscribe(Msg, Id, Req, State=#state{channels=Channels}) ->
    with_stream(fun (Bucket, Stream) ->
                        Key = {Bucket, Stream},
                        IsSubscribed = contains(Key, Channels),
                        if
                            IsSubscribed ->
                                lager:warning("already subscribed ~s/~s~n", [Bucket, Stream]),
                                {encode_error(<<"already subscribed">>, Id), State};

                            true ->
                                {encode_ok(Id, <<"subscribe">>),
                                 subscribe(Bucket, Stream, nil, State)}
                        end
                end, Msg, Id, Req, State).


handle_unsubscribe(Msg, Id, Req, State=#state{channels=Channels,
                                              iorio_mod=Iorio,
                                              iorio_state=IorioState})
->
    with_stream(fun (Bucket, Stream) ->
                        Key = {Bucket, Stream},
                        IsSubscribed = contains(Key, Channels),
                        if
                            IsSubscribed ->
                                lager:debug("unsubscribing ~s/~s~n", [Bucket, Stream]),
                                Iorio:unsubscribe(IorioState, Bucket, Stream, self()),
                                NewChannels = remove(Key, Channels),
                                State1 = State#state{channels=NewChannels},
                                {encode_ok(Id, <<"unsubscribe">>), State1};

                            true ->
                                lager:warning("not subscribed ~s/~s~n", [Bucket, Stream]),
                                {encode_error(<<"not subscribed">>, Id), State}

                        end
                end, Msg, Id, Req, State).

encode_error(Reason, Id) ->
    lager:warning("error ~s ~p~n", [Reason, Id]),
    IdBin = integer_to_binary(Id),
    <<"{\"ok\":false,\"id\":", IdBin/binary, ",\"reason\":\"", Reason/binary, "\"}">>.

set_json_response(Req) ->
    cowboy_req:set_resp_header(<<"Content-Type">>, <<"application/json">>, Req).

do_init(Req, Access, Iorio, IorioState, Active) ->
    {Token, Req1} = cowboy_req:qs_val(<<"jwt">>, Req, nil),
    {Params, Req2} = cowboy_req:qs_vals(Req1),
    Req3 = cowboy_req:compact(Req2),
    RawSubs = proplists:get_all_values(<<"s">>, Params),
    Subs = iorio_parse:subscriptions(RawSubs),
    {ok, Info} = ioriol_access:new_req([]),
    case iorio_session:fill_session_from_token(Access, Info, Token) of
        {ok, Info1} ->
            iorio_stats:listen_connect(Active),
            State = #state{channels=[], iorio_mod=Iorio, iorio_state=IorioState,
                           access=Access, token=Token, info=Info1, active=Active},

            State1 = subscribe_all(Subs, State),
            Req4 = if Active == once -> set_json_response(Req3);
                      true -> Req3
                   end,
            {ok, Req4, State1};
        {error, Reason} ->
            lager:warning("shutdown listen ~p", [Reason]),
            {shutdown, Req3, #state{}}
    end.
