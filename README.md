# supervisor-protocol

MIT. The process supervisor core of the cl-stack coding-agent harness (H8a): one in-image
registry for every child the harness owns (shell jobs, rpc/MCP and language servers, spawned
Lisp images, subagent threads), with owner scopes, caps and LRU eviction, restart policies,
graceful-then-forced tree stops, a replayable state-change event stream, and durable unit
files swept for orphans after a crash. H8b (0.2.0): every unit's stdout / stderr goes to
rotated, size-capped segment files (one record per line, time- and stream-stamped, CRC-checked,
crash-safe) with a per-segment offset index and a per-supervisor total cap, read by line range,
tail and follow across rotations, also after a crash. H8c (0.3.0): `supervisor-protocol/search`, an
incremental BM25 index over those logs (a tokenizer for code and log text, structured hits for
errors, warnings, stack traces, test failures and exits, per-segment index files that go with
their segments), plus `restart-unit` and `close-unit-input`. 0.3.1: a pump read that fills its buffer
mid-line no longer splits the line into two records, so line numbers (and refs) depend on the
bytes a program printed, not on read timing. Brief:
[cl-stack/docs/capabilities/supervisor.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/supervisor.md).

## License

MIT
