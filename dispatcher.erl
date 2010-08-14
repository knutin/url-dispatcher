%%% @author Knut Nesheim <knutin@gmail.com>
%%% @copyright (C) 2010, Knut Nesheim
%%% @doc
%%% A Django-style URL dispatcher.
%%% The idea is to map a URL to a function and have the dispatcher find the
%%% correct function, extract any arguments from the URL and call said function.
%%%
%%% How it works:
%%%  1. Search the UrlConf and find the matching module and function
%%%  2. From the URL, extract any arguments specified by regular expression groups. 
%%%  3. Use the arguments and a generic Request variable to make the function signature
%%%  4. Call the function and return the result.
%%% @end
%%% Created : 14 Aug 2010 by Knut Nesheim <knutin@gmail.com>

-module(dispatcher).

-export([dispatch/3,
         error_404/2]).


%% @doc: Dispatch control to the correct controller
dispatch(Request, Url, UrlConfs) ->
    {Module, Function, MatchResults} = search_urls(Url, UrlConfs),

    Args = case MatchResults of
               [] -> [Request];
               _  -> [Request] ++ MatchResults
           end,
    erlang:apply(Module, Function, Args).


%% @doc: Find the first regex that matches the URL. 
%%       Returns the module, function and even the result from the match.
search_urls(Url, []) ->
    %% No match found, call the 404 handler
    {dispatcher, error_404, [Url]};

search_urls(Url, [{Regex, Module, Function}|T]) ->
    %% Make run/3 return a list of all captured groups, but not the URL itself
    Options = [{capture, all_but_first, list}],

    case re:run(Url, Regex, Options) of
        {match, MatchResult} -> {Module, Function, MatchResult};
        nomatch               -> search_urls(Url, T)
    end.

error_404(_Request, _Url) ->
    {ehtml, "HTTP 404"}.
    
