crdt
=====

A simple crdt server.

Build
-----

    $ rebar3 compile

Test
----

   $ rebar3 eunit
   $ rebar3 ct --dir ct

Manual test
-----------
```sh
$ rebar3 shell --sname foo
```
```erl
(foo@host) 1>
```
```sh
$ rebar3 shell --sname bar
```
```erl
(bar@host) 1> crdt:connect(foo@host).
ok
```
```erl
(bar@host) 2> crdt:add(9999).
ok
```
```erl
(foo@host) 1> crdt:member(9999).
true
(foo@host) 2> crdt:add(8888).
ok
```
```erl
(bar@host) 3> crdt:member(8888).
true
(bar@host) 4> crdt:remove(8888).
ok
```
```erl
(foo@host) 3> crdt:member(8888).
false
```
