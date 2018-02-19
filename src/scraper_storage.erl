-module(scraper_storage).

-behavior(gen_server).

-export([start_link/0, init/1, handle_cast/2, handle_call/3]).

-export([update/1, contains/1]).

-define(COLLECTION, <<"food">>).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  application:ensure_all_started(mongodb),
  Args = get_connection_args(),
  mc_worker_api:connect(Args).

get_connection_args() ->
  [
    {host, get_env(mongo_host)},
    {port, get_env(mongo_port)},
    {database, get_env(mongo_database)}
  ].

get_env(Key) ->
  {ok, Value} = application:get_env(scraper, Key),
  Value.

update(FoodDetails) ->
  gen_server:cast(?MODULE, {update, FoodDetails}).

contains(NdbNo) ->
  gen_server:call(?MODULE, {contains, {ndbno, NdbNo}}).

handle_cast({update, #{<<"ndbno">> := Ndbno} = FoodDetails}, Connection) ->
  io:format("[~p] Updating: ~s~n", [self(), Ndbno]),
  Command = #{<<"$set">> => FoodDetails},
  mc_worker_api:update(Connection, ?COLLECTION, #{<<"ndbno">> => Ndbno}, Command, true, false),
  {noreply, Connection}.

handle_call({contains, {ndbno, NdbNo}}, _From, Connection) ->
  Res = mc_worker_api:find_one(Connection, ?COLLECTION, #{<<"ndbno">> => NdbNo}),
  Contains = case Res of
               undefined -> false;
               _Else -> true
             end,

  io:format("[~p] Is ~s in storage: ~p~n", [self(), NdbNo, Contains]),

  {reply, Contains, Connection}.
