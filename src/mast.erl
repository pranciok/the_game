-module(mast).
-export([start/0, stop/0]).
-export([init/0]).

-include("settings.hrl").


start()->
  spawn(?MODULE, init, []).

stop() -> mast ! stop.

init() ->
  loop().

loop() ->
  Mast = calculate_mast_position(),
  rpc:call(?ADMIRAL, admiral, mast, [node(), Mast]),
  receive
    {get_mast, Pid} ->
          Pid ! {mast, Mast},
          loop();
    stop -> ok
  after 5000 -> loop()
  end.

calculate_mast_position() ->
  Match = [{#wards{id = '$1',pid = '_', node = node(), weight = '$2'},
            [],
            [{{'$1','$2'}}]}],
  MyWards = mnesia:dirty_select(wards, Match),

  {TotalX, TotalY, TotalWeight} = get_totals(MyWards, {0, 0, 0}),
  case TotalWeight of
    0 -> {0, 0};
    _-> {TotalX/TotalWeight, TotalY/TotalWeight}
  end.

get_totals([], Result) -> Result;
get_totals([{{X, Y}, Weight}|Wards], {AccX, AccY, AccWeight})->
  get_totals(Wards, {(X * Weight) + AccX, (Y * Weight) + AccY, Weight + AccWeight}).
