%%% @author Knut Nesheim <knutin@gmail.com>
%%% @copyright (C) 2010, Knut Nesheim
%%% @doc
%%% Mapping of URL to module and function
%%% @end
%%% Created : 14 Aug 2010 by Knut Nesheim <knutin@gmail.com>

-module(urlconf).

-export([urlconf/0]).

%% TODO: Compile every regex and cache it
urlconf() ->
    [
     %% Exact match, one group so foo_controller:show/2 is used
     {"^/foobar/(\\d+)/$", foo_controller, show},
     %% Exact match, two groups, foo_controller:show/3 is used
     {"^/foobar/(\\d+)/page/(\\d+)/$", foo_controller, show},

     %% Non-exact match, everything that starts with /test will match
     {"^/test", foo_controller, test}
    ].

