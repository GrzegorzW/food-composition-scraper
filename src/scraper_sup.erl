-module(scraper_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  ChildSpecs = [
    {storage, {scraper_storage, start_link, []}, permanent, 5000, worker, [scraper_storage]}
  ],
  {ok, {{one_for_one, 5, 60}, ChildSpecs}}.
