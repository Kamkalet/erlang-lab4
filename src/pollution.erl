%%%-------------------------------------------------------------------
%%% @author AD
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. kwi 2017 12:57
%%%-------------------------------------------------------------------
-module(pollution).
-author("AD").

%% API
-export([createMonitor/0, addStation/3, ifStationExists/3,
  compareNames/2, compareCords/2, removeValue/4,
  getOneValue/4, addValue/5,  getMinimumPollutionStation/2, flatten/2, getMeasures/1, getDailyMean/3]).

-record(station, {name, coordinates, measures=[]}).
-record(measure, { date, type, value}).

%%#################### HELPING FUNCTIONS ##################################


%%findMinValType(GivenType, Monitor) ->
%%  lists:filter(fun (X) -> filterByType(GivenType,X) end, Monitor).

%%findMinValType(GivenType, Stations) ->
%%  (filterByType(GivenType, hd([getMeasures(X) || X <- Stations ]))).

sumDailyStatMean(MType, MDay, Measurements) ->
  lists:foldl(fun(X,Y) -> X + Y end, 0, mapTuples(filterByType(MType, filterDay(MDay, Measurements)))).


sumDailyMean(MType, MDay, Stations)
  -> lists:foldl(fun(X, Y) -> X + Y end, 0,[sumDailyStatMean(MType, MDay, getMeasures(X)) || X <- Stations ]).


countDailyStatMean(MType, MDay, Measurements) ->
  lists:foldl(fun(X,Y) -> X + Y end, 0, mapToOneDigit(filterByType(MType, filterDay(MDay, Measurements)))).


countDailyMean(MType, MDay, Stations)
  -> lists:foldl(fun(X, Y) -> X + Y end, 0,[countDailyStatMean(MType, MDay, getMeasures(X)) || X <- Stations ]).


anyTDStations (Type, Day, Stations) ->
  lists:any(fun (X) -> anyTimeDateMeasurements(Type, Day, getMeasures(X)) end, Stations).


compareType(Type, #measure{type=Type}) -> true;
compareType(_,_) -> false.

compareDate(Date, #measure{date=Date}) -> true;
compareDate(_,_) -> false.

compareStationName(Name, #station{name = Name}) -> true;
compareStationName(_,_) -> false.

compareCoords(Coords, #station{coordinates = Coords}) -> true;
compareCoords(_,_) -> false.

%%compareNames(Name, Station) ->
%%  case Name =:= Station#station.name of
%%    true -> true;
%%    _ -> false
%%  end.

 %Alternative below... with pattern matching
compareNames(Name, #station{name = Name}) -> true;
compareNames(_,_) -> false.

%%> #person{name = Name} = P3, Name.
%%"Joe"
compareCords(Coords, #station{coordinates = Coords}) -> true;
compareCords(_,_) -> false.

ifStationExists(Name, Coords, Monitor) ->
  lists:any(fun (X) -> compareNames(Name, X) or compareCoords(Coords, X) end, Monitor).


addValueToStation(GivenDate, GivenType, GivenValue, Measures) ->
  case anyTimeDateMeasurements(GivenType,GivenDate, Measures) of
    false ->  [#measure{date=GivenDate , type=GivenType, value=GivenValue} | Measures] ;
    _ -> Measures
  end.

anyTypeStations(Type, Stations) ->
  lists:any(fun (X) -> anyTypeMeasures(Type, getMeasures(X)) end, Stations).

anyTypeMeasures(Type, Measures) -> lists:any(
  fun (X) -> compareType(Type, X) end, Measures).

anyTimeDateMeasurements (Type, Date, Measures) ->
  lists:any(fun(X) -> compareDate(Date, X) and compareType(Type, X) end, Measures).

flatten(X)               -> flatten(X,[]).

flatten([],Acc)          -> Acc;
flatten([[]|T],Acc)      -> flatten(T, Acc);
flatten([[_|_]=H|T],Acc) -> flatten(T, flatten(H,Acc));
flatten([H|T],Acc)       -> flatten(T,Acc++[H]) .


getValues(GivenType,Stations) ->
  Tuples = flatten(([ (getMeasures( X)) || X <- Stations ] )),
  lists:foldl(fun(X, Y) -> erlang:min(X,Y) end,
    hd(mapTuples(filterByType(GivenType, Tuples))), tl(mapTuples(filterByType(GivenType, Tuples)))).


findMinValType(GivenType, Stations) ->
   (getValues(GivenType,Stations)).

%%findMinValType(GivenType, Stations) ->
%%  lists:foldl(fun(X, Y) -> erlang:min(X,Y) end, hd(getValues(GivenType,Stations)),
%%    tl(getValues(GivenType,Stations))).

remove(Date, Type, Measures) ->
  lists:filter(fun (X) -> not compareDate(Date, X) orelse not compareType(Type, X) end, Measures).

getOneValueFromStation(Date, Type, Measures) ->
  lists:foldl(fun(X, Y) -> X + Y end, 0, mapTuples(lists:filter(
    fun (X) -> compareDate(Date, X) and compareType(Type, X) end, Measures))).

mapTuples(Measurements) -> [mapValue(X) || X <- Measurements].

mapValue(#measure{value = Value}) -> Value;
mapValue(_) -> 0.

mapToOneDigit(Measures) -> [1 || _ <- Measures].

countAverage(Type, Measures) ->
  case anyTypeMeasures(Type, Measures) of
    true ->  getSumOfElements(Type, Measures) /
      getNumberOfElements(Type, Measures);
    _ -> 0
  end.

getSumOfElements(Type, Measures) ->
  lists:foldl(fun(X,Y) -> X + Y end, 0,
    mapTuples(filterByType(Type, Measures))).
getNumberOfElements(Type, Measures) ->
  lists:foldl(fun(X,Y) -> X + Y end, 0,
    mapToOneDigit(filterByType(Type, Measures))).

getMeasures(#station{measures = Measures}) -> Measures.

isStation(X, Y) -> compareStationName(X, Y) or compareCoords(X, Y).

compareDay(MDay, #measure{date={MDay,_}}) -> true;
compareDay(_,_) -> false.

compareHour(MHour, #measure{date={_, MHour}}) -> true;
compareHour(_,_) -> false.


filterByType(Type, Measures) ->
  [X || X <- Measures, compareType(Type, X)].
filterDay(MDay, Measurements) ->
  [X || X <- Measurements, compareDay(MDay, X)].
filterHour(MHour, Measurements) ->
  [X || X <- Measurements, compareHour(MHour, X)].




%%###################################################################

createMonitor() ->
  [].

%%dodaje do monitora wpis o nowej stacji pomiarowej
%%(nazwa i współrzędne geograficzne), zwraca zaktualizowany monitor
addStation(Name, Coords, Monitor) ->
  case ifStationExists(Name, Coords, Monitor) of
    true ->  {error, "already in monitor"};
    _ ->  [#station{name=Name, coordinates=Coords} | Monitor]
  end.


addValue(_, _, _, _, []) -> [];
addValue(GivenName, GivenDate, GivenType, GivenValue,
    [Station=#station{name=Name, coordinates=Coords, measures=Measure} | Other])
  when GivenName == Name orelse GivenName == Coords ->
  [Station#station{measures=addValueToStation(GivenDate, GivenType, GivenValue, Measure)} | Other];
addValue(GivenName, GivenDate, GivenType, GivenValue, [Stat|Other]) ->
    [Stat] ++ addValue(GivenName, GivenDate, GivenType, GivenValue, Other).


removeValue(_, _, _, []) -> [];
removeValue(GivenName, GivenDate, GivenType, [Station=#station{name=Name, coordinates=Coords, measures=Measure} | Other])
  when GivenName == Name orelse GivenName == Coords ->
  [Station#station{measures = remove(GivenDate, GivenType, Measure)} | Other];
removeValue(GivenName, GivenDate, GivenType, [Station|Other]) -> [Station | removeValue(GivenName, GivenDate, GivenType, Other)].

%%zwraca wartość pomiaru o zadanym typie, z zadanej daty i stacji;
getOneValue(_, _, _, []) -> false;
getOneValue(GivenName, GivenDate, GivenType, [#station{name=Name, coordinates=Coords, measures= Measure} | _])
  when Name == GivenName orelse GivenName == Coords -> getOneValueFromStation( GivenDate, GivenType, Measure);
getOneValue(GivenName, GivenDate, GivenType, [_|Other]) -> getOneValue(GivenName, GivenDate, GivenType, Other).

%wraca średnią wartość parametru danego typu z zadanej stacji;
%%getStationMean(GivenName, GivenType, St) ->
%%  countAverage(GivenType, getMeasures(lists:filter(fun(X) -> (ifStation(GivenName,X)) end, St))).

getMinimumPollutionStation(GivenType, Stations) ->
  case anyTypeStations(GivenType, Stations) of
    true -> findMinValType(GivenType, Stations);
    _ -> false
  end.

getDailyMean(MType, MDay, Stations) ->
  case anyTDStations(MType, MDay, Stations) of
    true -> sumDailyMean(MType, MDay, Stations) ;
    _ -> 0
  end.





