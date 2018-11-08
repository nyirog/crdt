-module(crdt_dist_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([foo/1]).

all() -> [foo].

init_per_testcase(_TestName, Config) -> Config.

end_per_testcase(_TestName, _Config) -> ok.

foo(_Config) -> ok.
