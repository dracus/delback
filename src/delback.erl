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
-export([copy_files/1]).

-include_lib("kernel/include/file.hrl").

copy_files([]) ->
    done;
copy_files([{From, To}|T]) ->
    copy_files(From, To),
    copy_files(T).

copy_files(Dir, To) ->
    file:make_dir(To),
    io:format("Dir: ~p~n", [Dir]),
    case file:list_dir(Dir) of
	{ok, Files} -> [io:format("File: ~p~n", [File]) || File <- Files],
	               [backup_file(Dir, To, File) 
			|| File <- Files, filelib:is_file(Dir ++ File)],
		       [copy_files(Dir ++ File ++ "/", To ++ File ++ "/") 
			|| File <- Files, filelib:is_dir(Dir ++ File)];
	{error, Reason} -> io:format("***Error: ~p~n", [Reason])
    end,
    filelib:is_dir(Dir).


%% only copy file if it do not exist or have another modified date
backup_file(From, To, File) ->
    {ok, SrcInfo} = file:read_file_info(From ++ File),
    case file:read_file_info(To ++ File) of
	{ok, DstInfo} -> 
	    DstModifiedTime = DstInfo#file_info.mtime,
	    SrcModifiedTime = SrcInfo#file_info.mtime,
	    io:format("DstMTime: ~p~n", [DstModifiedTime]),
	    io:format("SrcMTime: ~p~n", [SrcModifiedTime]),
	    if 
		DstModifiedTime =/= SrcModifiedTime ->
		    do_file_copy(From ++ File, To ++ File, SrcInfo);
		true ->
		    io:format("No need to backup file: ~p~n", [File])
	    end;
	{error, enoent} -> 
	    do_file_copy(From ++ File, To ++ File, SrcInfo)
    end.

do_file_copy(Src, Dst, Info) ->
    file:copy(Src, Dst),
    file:write_file_info(Dst, Info).


%% TEST
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


copy_test() ->
    clean_up(),
    generate_test_data(),
    copy_files("../test/src/", "../test/dst/").

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
