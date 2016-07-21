%%%-------------------------------------------------------------------
%%% @author nieyong@staff.weibo.com
%%% @copyright 2016 nieyong@staff.weibo.com
%%% @doc
%%%     The Qmsg Protocl Server
%%% @end
%%%-------------------------------------------------------------------
-module(qmsg_server).

-behaviour(gen_server).

%% API functions
-export([start/0, accept_loop/1, sendmsg/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(LISTEN_PORT, 5678).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    Port = ?LISTEN_PORT,
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

%%--------------------------------------------------------------------
%% @doc
%% accept the listen socket
%%
%% @spec accept_loop({Server, LSocket}) -> any()
%% @end
%%--------------------------------------------------------------------
accept_loop({Server, LSocket}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(Server, {accepted, LSocket}),
    recv_loop(Socket).

%%--------------------------------------------------------------------
%% @doc
%% Connect the Server, send msg and get response
%%
%% @spec sendmsg(integer(), list()) -> any()
%% @end
%%--------------------------------------------------------------------
sendmsg(Uid, Msg) when is_integer(Uid), is_list(Msg) ->
    MsgBin = list_to_binary(Msg),
    ReqBody = <<"**##", Uid:32/integer, MsgBin/binary, "##**">>,
    BodyLen = byte_size(ReqBody),
    ReqBin = <<BodyLen:32/little, ReqBody/binary>>,
    do_send_msg(?LISTEN_PORT, ReqBin).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Port]) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, LSocket} ->
            {ok, accept(LSocket)};
        {error, Reason} ->
            {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({accepted, LSocket}, _State) ->
    {noreply, accept(LSocket)};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

accept(LSocket) ->
    proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket}]),
    ok.

recv_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            process_message(Socket, Data),
            recv_loop(Socket);
        {error, closed} ->
            ok
    end.

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

-spec do_send_msg(integer(), iodata()) -> any().
do_send_msg(Port, Msg) ->
    {ok, Sock} =
    gen_tcp:connect("localhost",
                    Port,
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
