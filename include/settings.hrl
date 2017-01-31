%%nodes%%
-define(GAME_NODES, [
  {{0,0},'gnode1@game.cluster'},
  {{1,0},'gnode2@game.cluster'},
  {{2,0},'gnode3@game.cluster'},
  {{0,1},'gnode4@game.cluster'},
  {{1,1},'gnode5@game.cluster'},
  {{2,1},'gnode6@game.cluster'},
  {{0,2},'gnode7@game.cluster'},
  {{1,2},'gnode8@game.cluster'},
  {{2,2},'gnode9@game.cluster'}
]).

-define(COLOURS, [
  {'gnode1@game.cluster', {0.0, 0.6, 0.0, 0.6}},
  {'gnode2@game.cluster', {0.7, 0.0, 0.2, 0.6}},
  {'gnode3@game.cluster', {0.7, 0.7, 0.0, 0.6}},
  {'gnode4@game.cluster', {0.7, 0.0, 0.7, 0.6}},
  {'gnode5@game.cluster', {0.2, 0.9, 0.0, 0.6}},
  {'gnode6@game.cluster', {0.0, 0.7, 0.7, 0.6}},
  {'gnode7@game.cluster', {0.9, 0.6, 0.3, 0.8}},
  {'gnode8@game.cluster', {0.1, 0.0, 0.7, 0.8}},
  {'gnode9@game.cluster', {0.2, 0.2, 0.2, 0.8}}
]).


-record(wards, {id, pid, node, weight}).

%% usefull globals %%
-define(MAX_X, 495000).
-define(MAX_Y, 495000).
-define(NO_OF_WARDS_TOTAL, 9081).
-define(NO_OF_WARDS, 1089).
-define(SQRT_OF_WARDS, 99).

-define(WARD_SIZE, 5000).
-define(PLAYER_VISIBILITY, 1000).

-define(SUPER_NODE, 'admiral@game.cluster').

-define(NO_OF_NODES, 9).

-define(PING_TRESHOLD, 10).
%%-----------------%%

%% help for calculating wards affected %%
-define(N, {0,1}).
-define(NE, {0.707, 0.707}). %% sin(45 deg), cos(45 deg)
-define(E, {1, 0}).
-define(SE, {0.707, -0.707}).
-define(S, {0, -1}).
-define(SW, {-0.707, -0.707}).
-define(W, {-1, 0}).
-define(NW, {-0.707, 0.707}).
%%------------------------------------%%
