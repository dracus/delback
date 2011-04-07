%%
%% delback --
%%      Dracus Erlang Learning - BACKup program
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

-module(delback).
-export([start/0, stop/0, copy_files/1]).

-include_lib("kernel/include/file.hrl").

%% 
%% Exported functions
%%

-spec start() -> ok.
start() ->
    delback_store:open("delback.db").

-spec stop() -> ok.
stop() ->
    delback_store:close().

-spec copy_files([{From::file:filename(), To::file:filename()}]) -> done.
copy_files([]) ->
    done;
copy_files([{From, To}|T]) ->
    copy_files(From, To),
    copy_files(T).

%%
%% Internal functions
%%

copy_files(Dir, To) ->
    file:make_dir(To),
    io:format("Dir: ~p~n", [Dir]),
    case file:list_dir(Dir) of
	{ok, Files} -> [io:format("File: ~p~n", [File]) || File <- Files],
	               [backup_file(Dir, To, File) 
			|| File <- Files, filelib:is_regular(Dir ++ File)],
		       [copy_files(Dir ++ File ++ "/", To ++ File ++ "/") 
			|| File <- Files, filelib:is_dir(Dir ++ File)];
	{error, Reason} -> io:format("***Error: ~p~n", [Reason])
    end,
    filelib:is_dir(Dir).


%% only copy file if it do not exist or have another checksum from its data
backup_file(From, To, File) ->
    SrcFile = filename:join(From, File),
    DstFile = filename:join(To, File),
    {ok, SrcInfo} = file:read_file_info(SrcFile),
    case filelib:is_file(DstFile) of
	true ->
	    {ok, DstData} = file:read_file(DstFile),
	    DstValue = crypto:sha(DstData),
	    case delback_store:lookup(SrcFile) of
		error ->
		    {ok, SrcData} = file:read_file(SrcFile),
		    delback_store:store(SrcFile, crypto:sha(SrcData));
		    %,
		    %do_file_copy(SrcFile, DstFile, SrcInfo);
		Value ->
		    if 
			Value =/= DstValue ->
			    do_file_copy(SrcFile, DstFile, SrcInfo);
			true ->
			    io:format("No need to backup file: ~p~n", [File])
		    end
	    end;
	false ->
	    do_file_copy(SrcFile, DstFile, SrcInfo)
    end.


do_file_copy(Src, Dst, Info) ->
    file:copy(Src, Dst),
    file:write_file_info(Dst, Info).

%%
%% TEST
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

copy_test() ->
    start(),
    clean_up(),
    generate_test_data(),
    copy_files("../test/src/", "../test/dst/"),
    stop().

clean_up() ->
    clean_up(src, "../test/src/"),
    clean_up(dst, "../test/dst/"),
    file:del_dir("../test").

generate_test_data() ->
    file:make_dir("../test"),
    file:make_dir("../test/src"),
    file:make_dir("../test/src/dir"),
    file:write_file("../test/src/file.dat", "some data"),
    file:write_file("../test/src/test.txt", "Hi"),
    file:write_file("../test/src/dir/fil2.txt", "2 Hi").

clean_up(src, Dir) ->
    file:delete(Dir ++ "file.dat"),
    file:delete(Dir ++ "test.txt"),
    file:delete(Dir ++ "dir/fil2.txt"),
    file:del_dir(Dir ++ "dir");
clean_up(dst, Dir) ->
    file:delete(Dir ++ "file.dat"),
    file:delete(Dir ++ "dir/fil2.txt"),
    file:del_dir(Dir ++ "dir").

-endif.
