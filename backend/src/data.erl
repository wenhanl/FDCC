-module(data).
-export([sql_init/0, lookup/1, get_hosts_name/0, get_hosts_ip/0, add_node/2,
  get_hosts/0, get_from_host/1,
  remove/1, update_compile/4, update_job/5, update_user/3, get_jobs_user/2, update_status/4]).
%%-include("/usr/lib/erlang/lib/mysql/include/mysql.hrl").
%%-include("/usr/lib/erlang/lib/mysql/ebin/mysql_auth.beam").
%%-include("/usr/lib/erlang/lib/mysql/ebin/mysql.beam").
%%-include("/usr/lib/erlang/lib/mysql/ebin/mysql_conn.beam").
%%-include("/usr/lib/erlang/lib/mysql/ebin/mysql_recv.beam").
%%-record(todo,{data=,rows=[],affectedrows=0,insertid=0,error="",errcode=0,errsqlstate=""}).
%%-record(todo,{{_,{_,_,U,_,_,_,_,_}}}).
-define(Parser, {{_, {_, _, U, _, _, _, _, _}}}).

%%initialize mysql %%
sql_init() ->
%%io:fwrite("hello, world\n").
  mysql:start_link(conn, "localhost", "root", "root", "erlang").

lookup(Name) ->
  NameStr = atom_to_list(Name),
  QStr = "SELECT hostname FROM nodes WHERE hostname like \"" ++ NameStr ++ "\"",
  {_, {_, _, Result1, _, _, _, _, _}} = mysql:fetch(conn, list_to_binary(QStr)),
  Result1.

%% retrieve a list of all nodes, with their ids and ip addresses %%  
get_hosts_name() ->
  {_, {_, _, Result1, _, _, _, _, _}} =
    mysql:fetch(conn, <<"SELECT hostname FROM nodes">>),
  Result1.

get_hosts_ip() ->
  {_, {_, _, Result1, _, _, _, _, _}} =
    mysql:fetch(conn, <<"SELECT ip_addr FROM nodes">>),
  Result1.

get_hosts() ->
  {_, {_, _, Result1, _, _, _, _, _}} =
    mysql:fetch(conn, <<"SELECT hostname, ip_addr FROM nodes">>),
  Result1.

add_node(Name, Ip) ->
  NameStr = atom_to_list(Name),
  A = "insert into nodes (hostname, ip_addr) values(\"" ++
    NameStr ++ "\",\"" ++ Ip ++ "\")",
  %%B = string:concat(A,"\")"),
  %%io:format("this ~s~n",[B]),
  mysql:fetch(conn, list_to_binary(A)).

update_compile(Jobid, Cnid, Filename, Status) ->
  C = "insert into compile values(\"" ++ Jobid ++
    "\",\"" ++ Cnid ++ "\",\"" ++ Filename ++ "\",\"" ++ Status ++ "\")",
  mysql:fetch(conn, list_to_binary(C)).

update_job(Jobid, Src, Scnid, Uid, Downloadloc) ->
  D = "insert into job values(\"" ++ Jobid ++ "\",\"" ++ Src ++
    "\",\"" ++ Scnid ++ "\",\"" ++ Uid ++ "\",\"" ++ Downloadloc ++ "\")",
  mysql:fetch(conn, list_to_binary(D)).

update_user(Userid, Password, Totaljobs) ->
  E = "insert into user values(\"" ++
    Userid ++ "\",\"" ++ Password ++ "\",\"" ++ Totaljobs ++ "\")",
  mysql:fetch(conn, list_to_binary(E)).

%%get the total_jobs submitted by a user %% 
get_jobs_user(Userid, Password) ->
  Jobs = "select total_jobs from user where user_id = \"" ++
    Userid ++ "\" and password = \"" ++ Password ++ "\"",
  {_, {_, _, Ret, _, _, _, _, _}} = mysql:fetch(conn, list_to_binary(Jobs)),
  {Ret}.



update_status(Jobid, Cnid, Filename, Status) ->
  %%C = "insert into compile values(\""++Jobid++"\",\""++Cnid++"\",\""++Filename++"\",\""++Status++"\")",
  F = "update compile set status = \"" ++ Status ++ "\" where job_id = \"" ++
    Jobid ++
    "\" and cn_id = \"" ++ Cnid ++ "\" and filename = \"" ++ Filename ++ "\"",
  %%Qstr = string:join(["select * from nodes where host_id =",Id]," "),
  %%B = "select * from nodes where host_id ="++Id,
  mysql:fetch(conn, list_to_binary(F)).
%%io:format("Result: ~p~n", [Res]),

get_from_host(Host) ->
  Temp = "select host_id from nodes where hostname =\"" ++ Host ++ "\"",
  R = mysql:fetch(conn, list_to_binary(Temp)),
  {R}.

remove(Host_id) ->
  Temp = "delete from nodes where hostname =\"" ++ Host_id ++ "\"",
  R = mysql:fetch(conn, list_to_binary(Temp)),
  {R}.
