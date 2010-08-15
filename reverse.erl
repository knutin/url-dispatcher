%%% @author Knut Nesheim <knutin@gmail.com>
%%% @copyright (C) 2010, Knut Nesheim
%%% @doc
%%% Reverse URL lookups.
%%% @end
%%% Created : 15 Aug 2010 by Knut Nesheim <knutin@gmail.com>

-module(reverse).

-export([reverse/4, reverse/0]).

-define(GROUP_RE, "\\(([^\\)]+)\\)").

reverse() ->
    %%reverse(urlconf:urlconf(), foo_controller, show, ["1234"]).
    reverse(urlconf:urlconf(), foo_controller, show, ["1234", "3"]).
    %%reverse(urlconf:urlconf(), foo_controller, test, []).

%% @doc: Returns a URL mapping to module and function with arguments.
reverse(UrlConf, Module, Function, Args) ->
    UrlRegex = reverse_search(UrlConf, Module, Function, length(Args)),
    CleanUrl = clean(UrlRegex),
    insert_args(CleanUrl, Args).

%% @doc: Replace regex groups with positional arguments.
insert_args(Url, []) ->
    Url;
insert_args(Url, [Arg|T]) ->
    NewUrl = re:replace(Url, ?GROUP_RE, Arg, [{return, list}]),
    insert_args(NewUrl, T).

%% @doc: Search the URL Conf for the URL that maps to module, function and arguments.
reverse_search([], _, _, _) ->
    "/";
reverse_search([{UrlRe, Module, Function} | T], Module, Function, ArgsLength) ->
    %% Match regex groups
    Options = [global, {capture, all_but_first, list}],

    case re:run(UrlRe, ?GROUP_RE, Options) of
        {match, Groups} -> 
            %% If the number of groups in the URL regex 
            %% is the same as ArgsLength, we got a match
            if length(Groups) =:= ArgsLength ->
                    UrlRe;
               true ->
                    reverse_search(T, Module, Function, ArgsLength)
            end;
        nomatch when ArgsLength =:= 0 -> 
            UrlRe;
        nomatch -> 
            reverse_search(T, Module, Function, ArgsLength)
    end;

reverse_search([_H|T], Module, Function, ArgsLength) ->
    reverse_search(T, Module, Function, ArgsLength).


%% @doc: Remove any special regex control characters from the URL
clean(Url) ->
    re:replace(Url, "[\\^|\\$]", "", [global, {return, list}]).
