-module(qmsg_server).
-behaviour(gen_server).

%% API
-export([start_link/0, server/0, stop/0, accept/1, sendmsg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sock}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: server() -> ok
%% Description: Starts listening to port 5678
%%--------------------------------------------------------------------
server() ->
    gen_server:cast(?MODULE, server).

%%--------------------------------------------------------------------
%% Function: stop() -> ok
%% Description: Closes listening port and stops myserver
%%--------------------------------------------------------------------
stop() ->
    gen_server:call(?MODULE, stop).

sendmsg(Uid, Msg) when is_integer(Uid), is_list(Msg) ->
    MsgBin = list_to_binary(Msg),
    ReqBody = <<"**##", Uid:32/integer, MsgBin/binary, "##**">>,
    BodyLen = byte_size(ReqBody),
    ReqBin = <<BodyLen:32/little, ReqBody/binary>>,
    do_send_msg(ReqBin).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, LSock} = gen_tcp:listen(5678, [binary, {packet, 0},
                                        {active, true}]),
    {ok, #state{sock = LSock}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(server, #state{sock = LSock} = State) ->
    Reply = case gen_tcp:accept(LSock, 1000) of
                {ok, Sock} ->
                    error_logger:info_msg("Server accepted socket~n"),
                    Pid = proc_lib:spawn(?MODULE, accept, [Sock]),
                    ok = gen_tcp:controlling_process(Sock, Pid),
                    ?MODULE:server(),
                    {noreply, State};
                {error, timeout} ->
                    ?MODULE:server(),
                    {noreply, State};
                {error, closed} ->
                    {stop, normal, State}
            end,
    Reply;
handle_cast(_Msg, State) ->
    {noreply, State}.

process_message(Sock, <<Len:32/little, LeftBin:Len/binary>>) ->
    TxtBinLen = Len - 12,
    <<"**##", Uid:32/integer, _TxtBinary:TxtBinLen/binary, "##**">> = LeftBin,
    error_logger:info_msg("Server Got Msg : ~p~n", [LeftBin]),

    RandomCode = get_random_num(1000000),
    %% generate the response
    RespBody = <<16:32/little, "**##", Uid:32/integer, RandomCode:32/integer, "##**">>,
    gen_tcp:send(Sock, RespBody);
process_message(Sock, Data) ->
    error_logger:info_msg("Server Got Unknown Msg : ~p~n", [Data]),
    RespBody = <<16:32/little, "**##", 0:32/integer, 0/integer, "##**">>,
    gen_tcp:send(Sock, RespBody).

accept(Sock) ->
    receive
        {tcp, Sock, Msg} ->
            process_message(Sock, Msg),
            ok = gen_tcp:close(Sock);
        E ->
            error_logger:info_msg("Server Got unknown msg ~p~n", [E]),
            ok = gen_tcp:close(Sock)
    after 30000 ->
              error_logger:info_msg("Server socket timeout~n"),
              ok = gen_tcp:close(Sock)
    end.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{sock = Sock}) ->
    gen_tcp:close(Sock),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec do_send_msg(iodata()) -> any().
do_send_msg(Msg) ->
    {ok, Sock} =
    gen_tcp:connect("localhost",
                    5678,
                    [binary,
                     {packet, 0},
                     {active, true}]),
    ok = gen_tcp:send(Sock, Msg),
    receive
        {tcp, Sock, Msg0} ->
            error_logger:info_msg("client got msgs ~p~n", [Msg0])
    after 30000 ->
              error_logger:info_msg("client timeout~n")
    end,
    ok = gen_tcp:close(Sock).

-spec get_random_num(integer()) -> integer().
get_random_num(Max) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    random:uniform(Max).
