-module(iorio_rest).
-export([setup/1]).
-ignore_xref([setup/1]).

setup(AccessLogic) ->
    % TODO: check here that secret is binary and algorigthm is a valid one
    {ok, ApiAlgorithm} = env(auth_algorithm),
    N = envd(req_n, 3),
    W = envd(req_w, 3),
    Timeout = envd(req_timeout, 5000),
    SessionDurationSecs = envd(session_duration_secs, 3600),

    BaseDispatchRoutes = [
               {"/listen", bullet_handler, [{handler, iorio_listen_handler},
                                            {access, AccessLogic}]},
               {"/streams/:bucket", iorio_rest_list, [{access, AccessLogic}]},
               {"/streams/:bucket/:stream", iorio_rest_stream,
                [{access, AccessLogic}, {n, N}, {w, W}, {timeout, Timeout}]},
               {"/buckets/", iorio_rest_list, [{access, AccessLogic}]},
               {"/access/:bucket/", iorio_rest_access, [{access, AccessLogic}]},
               {"/access/:bucket/:stream", iorio_rest_access, [{access, AccessLogic}]},

               {"/sessions", iorio_rest_session,
                [{access, AccessLogic}, {algorithm, ApiAlgorithm},
                 {session_duration_secs, SessionDurationSecs}]},
               {"/users/", iorio_rest_user, [{access, AccessLogic}]},
               {"/ping", iorio_rest_ping, []},

               {"/x/:handler/[...]", iorio_rest_custom, [{access, AccessLogic}]}
    ],

    UserDispatchRoutes = envd(api_handlers,
                             [{"/ui/[...]", iorio_cowboy_static,
                               {priv_dir, iorio, "assets",
                                [{mimetypes, cow_mimetypes, all}]}}]),
    lager:info("configuring routes with following user provided routes: ~p",
               UserDispatchRoutes),
    DispatchRoutes = BaseDispatchRoutes ++ UserDispatchRoutes,

    Dispatch = cowboy_router:compile([{'_', DispatchRoutes}]),

    HttpEnabled = envd(http_enabled, true),
    ApiPort = envd(http_port, 8080),
    ApiAcceptors = envd(http_acceptors, 100),
    if HttpEnabled ->
            lager:info("http api enabled, starting"),
           {ok, _} = cowboy:start_http(http, ApiAcceptors, [{port, ApiPort}],
                                       [{env, [{dispatch, Dispatch}]}]);
       true ->
           lager:info("http api disabled"),
           ok
    end,

    SecureEnabled = envd(https_enabled, false),
    SecureApiPort = envd(https_port, 8443),

    if
        SecureEnabled ->
            lager:info("https api enabled, starting"),
            SSLCACertPath = envd(https_cacert, notset),
            {ok, SSLCertPath} = env(https_cert),
            {ok, SSLKeyPath} = env(https_key),

            BaseSSLOpts = [{port, SecureApiPort}, {certfile, SSLCertPath},
                           {keyfile, SSLKeyPath}],

            SSLOpts = if SSLCACertPath == notset -> BaseSSLOpts;
                         true -> [{cacertfile, SSLCACertPath}|BaseSSLOpts]
                      end,

            {ok, _} = cowboy:start_https(https, ApiAcceptors, SSLOpts,
                                         [{env, [{dispatch, Dispatch}]}]);
        true ->
            lager:info("https api disabled"),
            ok
    end,

    ExtensionsLoadOk = load_extensions_configs(),
    if ExtensionsLoadOk -> ok;
       true ->
           lager:warning("Some extension's Config failed to load. This may"
                         " cause unexpected behaviour on those extensions")
    end,

    ok.

env(Par) -> env(iorio, Par).
envd(Par, Def) -> env(iorio, Par, Def).

env(App, Par) ->
    application:get_env(App, Par).

env(App, Par, Def) ->
    application:get_env(App, Par, Def).

load_extensions_configs() ->
    ExtensionsLoadResult = iorio_x:load_configs(envd(extension, [])),
    FailedExntentionLoad = lists:filter(fun (ok) -> false; (_) -> true end,
                                        ExtensionsLoadResult),
    length(FailedExntentionLoad) == 0.

