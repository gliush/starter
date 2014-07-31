-module(starter_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    process_flag(trap_exit, true),

    {ok, #state{}}.

% BC starts
handle_call({evaluate, Lang, Code}, _F, State) ->
    handle_call({evaluate, Lang, Code, []}, _F, State);
% BC ends 

handle_call({evaluate, Lang, Code, InputFiles}, _, State) ->

    {ok, DockerImage} = application:get_env(starter, docker_image),
    {ok, StorageDir} = application:get_env(starter, storage_dir),

    UUID = uuid:to_string(uuid:uuid1()),

    % make tmp dir '/var/tmp/starter/UUID' with all the files for the current run
    % mount this dir as '/var/tmp/starter' and change workdir during docker run
    % starter point is script with name=UUID

    TmpDir = filename:join(StorageDir, UUID),
    ok = file:make_dir(TmpDir),

    write_files(TmpDir, [{UUID, Code} | InputFiles]),

    LangCommand = langs:command(Lang, UUID),
    Runner = io_lib:format("docker run -w ~s -v ~s:~s:ro", [StorageDir, TmpDir, StorageDir]),
    DockerCommand = io_lib:format("~s ~s ~s", [Runner, DockerImage, LangCommand]),

    Msg = exec:run(lists:flatten(DockerCommand), [stdout, stderr, sync]),
    {reply, Msg, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    %% lager:debug("uncactched msg: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

write_files(TmpDir, InputFiles) ->
    [ ok = file:write_file(filename:join(TmpDir,P), C) 
        || {P,  C} <- InputFiles ].
