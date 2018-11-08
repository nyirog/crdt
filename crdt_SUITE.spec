{logdir, "_build/test/logs"}.

{node, node_a, a@zbox}.
{node, node_b, b@zbox}.
{node, node_c, c@zbox}.

{suites, node_a, "ct", [crdt_dist_SUITE]}.

{init, node_a, {node_start, []}}.
{init, node_b, {node_start, []}}.
{init, node_c, {node_start, []}}.
