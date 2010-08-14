%%% @author Knut Nesheim <knutin@gmail.com>
%%% @copyright (C) 2010, Knut Nesheim
%%% @doc
%%% Yaws-interface to the URL dispatcher. Run this as an appmod under /
%%% @end
%%% Created : 14 Aug 2010 by Knut Nesheim <knutin@gmail.com>

-module(yaws_dispatcher).

-include_lib("/usr/local/lib/yaws/include/yaws_api.hrl").

-export([out/1]).

out(Arg) ->
    Url = "/" ++ Arg#arg.appmoddata,
    %% Dummy request
    Request = [{arg, Arg}, {session, foo}],
    dispatcher:dispatch(Request, Url, urlconf:urlconf()).
