%%
%% delback_store --
%%      keep a database storing information about processed files
%%
%% ----------- 
%%
%% Copyright (c) 2011 Daniel Carlsson
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(delback_store).
-export([open/1, close/0, store/2, lookup/1]).

open(File) ->
    %Exists = filelib:is_file(File),
    case dets:open_file(?MODULE, [{file, File}]) of
	{ok, ?MODULE} -> true;
	{error, Reason} -> 
	    io:format("*** Error open dets: ~p~n", [Reason]),
	    exit(error_dets_open)
    end.

close() ->
    dets:close(?MODULE).

store(Key, Filename) ->
    dets:insert(?MODULE, [{Key, Filename}]).

lookup(Key) ->
    case dets:lookup(?MODULE, Key) of
	[] -> error;
	[{_, Filename}] -> Filename
    end.

%% TEST
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


open_test() ->
    open("test_store.db").

close_test() ->
    close().

store_test() ->
    open("test_store.db"),
    store("test", "checksum"),
    close().

lookup_test() ->
    open("test_store.db"),
    store("key", "value"),
    ?assert(lookup("key") =:= "value"),
    close().


-endif.
