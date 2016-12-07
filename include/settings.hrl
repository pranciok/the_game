
%% usefull globals %%
-define(MAX_X, 10000).
-define(MAX_Y, 10000).
-define(NO_OF_WARDS, 100).
-define(SQRT_NO_OF_WARDS, 10).
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
