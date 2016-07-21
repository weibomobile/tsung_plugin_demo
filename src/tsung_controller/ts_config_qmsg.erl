-module(ts_config_qmsg).
-author('nieyong@staff.weibo.com').

-export([parse_config/2]).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_qmsg.hrl").

-include("xmerl.hrl").

%%----------------------------------------------------------------------
%% Func: parse_config/2
%% Args: Element, Config
%% Returns: List
%% Purpose: parse a request defined in the XML config file
%%----------------------------------------------------------------------
%% Parsing other elements
parse_config(Element = #xmlElement{name = dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
parse_config(Element = #xmlElement{name = qmsg, attributes = Attrs},
             Config=#config{curid = Id, session_tab = Tab,
                            sessions = [CurS | _], dynvar = DynVar,
                            subst    = SubstFlag, match = MatchRegExp}) ->

    %% support dynparams subst
    Uid = ts_config:getAttr(string, Attrs, uid, "0"),
    Ack  = ts_config:getAttr(atom, Attrs, ack, parse),
    Data = ts_config:getText(Element#xmlElement.content),

    Req = #qmsg_request{uid = Uid, data = Data},
    ts_config:mark_prev_req(Id - 1, Tab, CurS),
    Msg=#ts_request{ack     = Ack,
                    subst   = SubstFlag,
                    match   = MatchRegExp,
                    param   = Req},
    ets:insert(Tab,{{CurS#session.id, Id}, Msg#ts_request{endpage = true,
                                                         dynvar_specs = DynVar}}),
    lists:foldl( fun(A,B)->ts_config:parse(A, B) end,
                 Config#config{dynvar = []},
                 Element#xmlElement.content);
%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.
