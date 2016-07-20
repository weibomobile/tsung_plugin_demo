-module(ts_qmsg).
-author('neiyong@staff.weibo.com').

-behavior(ts_plugin).

-include("ts_profile.hrl").
-include("ts_qmsg.hrl").

-include("ts_macros.hrl").

-export([add_dynparams/4,
         get_message/2,
         session_defaults/0,
         subst/2,
         parse/2,
         parse_bidi/2,
         dump/2,
         parse_config/2,
         decode_buffer/2,
         new_session/0]).

%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session (ack_type and persistent)
%% Returns: {ok, true|false}
%%----------------------------------------------------------------------
session_defaults() ->
    {ok, true}.

%% @spec decode_buffer(Buffer::binary(),Session::record(raw)) ->  NewBuffer::binary()
%% @doc We need to decode buffer (remove chunks, decompress ...) for
%%      matching or dyn_variables
%% @end
decode_buffer(Buffer, #qmsg_request{}) ->
    Buffer.

%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
    #qmsg_request{}.
%%----------------------------------------------------------------------
%% Function: get_message/1
%% Purpose: Build a message/request
%% Args:    #jabber
%% Returns: binary
%%----------------------------------------------------------------------
get_message(#qmsg_request{uid = Uid, data = Data}, #state_rcv{session = S})->
    MsgBin = list_to_binary(Data),
    ReqBody = <<"**##", Uid:32/integer, MsgBin/binary, "##**">>,
    BodyLen = byte_size(ReqBody),
    ReqBin = <<BodyLen:32/little, ReqBody/binary>>,

    {ReqBin, S}.

%%----------------------------------------------------------------------
%% Function: parse/3
%% Purpose: Parse the given data and return a new state
%% Args:    Data (binary)
%%            State (record)
%% Returns: NewState (record)
%%----------------------------------------------------------------------
%% no parsing . use only ack

parse(closed, State) ->
    {State#state_rcv{ack_done = true, datasize = 0}, [], true};
parse(Data, State=#state_rcv{acc = [], datasize = 0}) ->
    parse(Data, State#state_rcv{datasize= size(Data)});
parse(<<Len:32/little, LeftBin:Len/binary>> = Data, State = #state_rcv{datasize = DataSize}) ->
    <<"**##", Uid:32/integer, Random:32/integer, "##**">> = LeftBin,

    ?LOGF("qmsg_request encode result : ~p", [{"**##", Uid, Random, "##**"}], ?DEB),

    AckResult =
    case Random of
        Num when Num > 0 ->
            true;
        0 ->
            false
    end,

    NewDataSize = DataSize + Len + 4,
    parse(Data, State#state_rcv{ack_done = AckResult, acc = [], datasize = NewDataSize});
parse(_Data, State) ->
    {State, [], false}.

parse_bidi(Data, State) ->
    ts_plugin:parse_bidi(Data,State).

dump(A,B) ->
    ts_plugin:dump(A,B).

parse_config(Element, Conf) ->
    ts_config_qmsg:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: add dynamic parameters to build the message
%%----------------------------------------------------------------------
add_dynparams(_,[], Param, _Host) ->
    Param;
add_dynparams(true, {DynVars, _Session}, OldReq, _Host) ->
    subst(OldReq, DynVars);
add_dynparams(_Subst, _DynData, Param, _Host) ->
    Param.

string_to_term(String) ->
    {ok, T, _} = erl_scan:string(String ++ "."),
    case erl_parse:parse_term(T) of
        {ok, Term} ->
            {ok, Term};
        {error, Error} ->
            Error
    end.

%%----------------------------------------------------------------------
%% Function: subst/1
%%----------------------------------------------------------------------
subst(Req = #qmsg_request{uid = Uid, data = Data}, DynVars) ->
    NewData = ts_search:subst(Data, DynVars),
    NewUid = ts_search:subst(Uid, DynVars),
    ?LOGF("subst data result : ~p", [{NewUid, NewData}], ?DEB),
    Req#qmsg_request{uid = NewUid, data = NewData}.
