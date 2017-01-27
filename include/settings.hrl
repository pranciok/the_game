%%nodes%%
-define(THIS_NODE, gnode1).
-define(GAME_NODES, [
  {{0,0},gnode1},
  {{1,0},gnode2},
  {{2,0},gnode3},
  {{0,1},gnode4},
  {{1,1},gnode5},
  {{2,1},gnode6},
  {{0,2},gnode7},
  {{1,2},gnode8},
  {{2,2},gnode9}
]).


%% usefull globals %%
-define(MAX_X, 495000).
-define(MAX_Y, 495000).
-define(NO_OF_WARDS_TOTAL, 9081).
-define(NO_OF_WARDS, 1089).
-define(SQRT_OF_WARDS, 99).

-define(WARD_SIZE, 5000).
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
