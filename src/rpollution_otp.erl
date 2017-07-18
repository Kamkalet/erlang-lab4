%%%-------------------------------------------------------------------
%%% @author AD
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2017 12:12
%%%-------------------------------------------------------------------
-module(rpollution_otp).
-author("AD").
-behavior(gen_server).

%% API
-export([init/1, handle_call/3, terminate/2, start_link/0, handle_info/2, code_change/3, handle_cast/2, addStation2/2, addValue2/4, removeValue2/3, getOneValue2/3, getMinimumPollutionStation2/1]).

start_link() ->
  gen_server:start_link(
    {local, ?MODULE},
    ?MODULE,
    []
    , []).

init([]) ->
  {ok,[]}.

addStation2(Name, Coords) ->
  gen_server:call(rpollution_otp, {addStation, Name, Coords}).

addValue2(GivenName, GivenDate, GivenType, GivenValue) ->
  gen_server:call(rpollution_otp, {add_value, GivenName, GivenDate, GivenType, GivenValue}).

removeValue2(GivenName, GivenDate, GivenType) ->
  gen_server:call(rpollution_otp, {remove_value, GivenName, GivenDate, GivenType}).

getOneValue2(GivenName, GivenDate, GivenType) ->
  gen_server:call(rpollution_otp, {get_one_value, GivenName, GivenDate, GivenType}).

getMinimumPollutionStation2(GivenType) ->
  gen_server:call(rpollution_otp, {get_minimum, GivenType}).


handle_call({addStation, Name, Coords}, _From, Monitor) ->
  check(Monitor, pollution:addStation( Name, Coords, Monitor));
handle_call({add_value, GivenName, GivenDate, GivenType, GivenValue}, _, Monitor) ->
  check(Monitor, pollution:addValue( GivenName, GivenDate, GivenType, GivenValue, Monitor));
handle_call({remove_value, GivenName, GivenDate, GivenType}, _, Monitor) ->
  check(Monitor, pollution:removeValue(GivenName, GivenDate, GivenType, Monitor));
handle_call({get_one_value, GivenName, GivenDate, GivenType}, _, Monitor) ->
  check(Monitor, pollution:getOneValue(GivenName, GivenDate, GivenType, Monitor));
handle_call({get_minimum, GivenType}, _, Monitor) ->
  check(Monitor, pollution:getMinimumPollutionStation(GivenType, Monitor)).

handle_cast(stop, Monitor) ->
  {stop, normal, Monitor};
handle_cast(crash, Monitor) ->
  1/0,
  {noreply, Monitor}.

handle_info(_Message, Monitor) ->
  {noreply, Monitor}.

code_change(_OldVsn, Monitor, _Extra) ->
  {ok, Monitor}.

terminate(Reason, _Value) ->
  io:format("Server stopped.~n"),
  Reason.

check(Monitor, {error, Description}) ->
  {reply, {error, Description}, Monitor};
check(_, NewMonitor) ->
  {reply, ok, NewMonitor}.

