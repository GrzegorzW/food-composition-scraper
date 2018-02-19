-module(scraper_app).

-behaviour(application).

-export([start/2, stop/1, scraper/0, scrap/1, scrap_food_details/1]).

-define(RETRY_TIMEOUT, 10000).

start(_StartType, _StartArgs) ->
  spawn(?MODULE, scraper, []),
  scraper_sup:start_link().

stop(_State) ->
  ok.

scraper() ->
  process_flag(trap_exit, true),
  Offset = 0,
  loop(Offset).

loop(Offset) ->
  spawn_link(?MODULE, scrap, [Offset]),
  receive
    {'EXIT', _From, normal} ->
      NewOffset = Offset + 50,
      loop(NewOffset);
    {'EXIT', _From, too_many_requests} ->
      io:format("[~p] Too many requests. Offset: ~p. Try again after ~pms~n", [self(), Offset, ?RETRY_TIMEOUT]),
      try_again_after(Offset, ?RETRY_TIMEOUT);
    {'EXIT', _From, empty_list} ->
      io:format("[~p] List is empty. Offset: ~p~n", [self(), Offset]);
    Else ->
      erlang:display(received_in_loop),
      erlang:display(Else)
  end.

try_again_after(Offset, Timeout) ->
  receive
  after Timeout ->
    loop(Offset)
  end.

scrap(Offset) ->
  process_flag(trap_exit, true),
  {ok, Connection} = gun:open("api.nal.usda.gov", 443),
  {ok, _Protocol} = gun:await_up(Connection),

  Url = erlang:iolist_to_binary([
    "/ndb/search",
    "?sort=n",
    "&api_key=", get_api_key(),
    "&ds=Standard+Reference",
    "&format=JSON",
    "&offset=", integer_to_list(Offset)
  ]),

  io:format("~n=====================================================================~n"),
  io:format("[~p] Getting list. Offset: ~p~n", [self(), Offset]),
  io:format("=====================================================================~n"),

  StreamRef = gun:get(Connection, Url),

  case gun:await(Connection, StreamRef) of
    {response, fin, _Status, _Headers} ->
      no_data;
    {response, nofin, 200, _Headers} ->
      {ok, Body} = gun:await_body(Connection, StreamRef),
      FoodList = jiffy:decode(Body, [return_maps]),
      scrap_food_list(FoodList);
    {response, nofin, 429, _Headers} ->
      exit(too_many_requests)
  end.

get_api_key() ->
  {ok, ApiKey} = application:get_env(scraper, api_key),
  ApiKey.

scrap_food_list(#{<<"list">> := #{<<"item">> := []}} = _List) ->
  exit(empty_list);
scrap_food_list(#{<<"list">> := #{<<"item">> := Items}} = _List) ->
  scrap_food_list(Items);
scrap_food_list([]) ->
  ok;
scrap_food_list(FoodItems) ->
  scrap_food_list(FoodItems, 0).

scrap_food_list([], SpawnedWorkers) ->
  wait_for_workers(SpawnedWorkers);
scrap_food_list(FoodItems, SpawnedWorkers) when length(FoodItems) >= 10 ->
  {FirstChunk, RestChunks} = lists:split(10, FoodItems),
  spawn_link(?MODULE, scrap_food_details, [FirstChunk]),
  scrap_food_list(RestChunks, SpawnedWorkers + 1);
scrap_food_list(FoodItems, SpawnedWorkers) ->
  spawn_link(?MODULE, scrap_food_details, [FoodItems]),
  scrap_food_list([], SpawnedWorkers + 1).

wait_for_workers(SpawnedWorkers) ->
  receive
    {'EXIT', _FromPid, too_many_requests} -> exit(too_many_requests);
    {'EXIT', _FromPid, _} when SpawnedWorkers =:= 1 -> ok;
    {'EXIT', _FromPid, _} -> wait_for_workers(SpawnedWorkers - 1);
    Else -> exit(Else)
  end.

scrap_food_details(FoodItems) ->
  {ok, Connection} = gun:open("api.nal.usda.gov", 443),
  {ok, _Protocol} = gun:await_up(Connection),
  scrap_food_details(FoodItems, Connection).

scrap_food_details([], _Connection) ->
  ok;
scrap_food_details([#{<<"ndbno">> := Ndbno} = _FoodItem | Rest], Connection) ->
  case scraper_storage:contains(Ndbno) of
    true -> io:format("[~p] Omitted: ~s~n", [self(), Ndbno]);
    false -> insert_food_details(Ndbno, Connection)
  end,
  scrap_food_details(Rest, Connection).

insert_food_details(Ndbno, Connection) ->
  io:format("[~p] Getting details: ~s~n", [self(), Ndbno]),

  Url = erlang:iolist_to_binary([
    "/ndb/reports",
    "?type=b",
    "&api_key=", get_api_key(),
    "&ndbno=", Ndbno
  ]),

  StreamRef = gun:get(Connection, Url),

  case gun:await(Connection, StreamRef) of
    {response, fin, _Status, _Headers} ->
      no_data;
    {response, nofin, 200, _Headers} ->
      {ok, Body} = gun:await_body(Connection, StreamRef),
      #{<<"report">> := #{<<"food">> := FoodDetails}} = jiffy:decode(Body, [return_maps]),
      scraper_storage:update(FoodDetails);
    {response, nofin, 429, _Headers} ->
      exit(too_many_requests)
  end.
