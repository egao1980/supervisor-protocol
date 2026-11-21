# supervisor-protocol

MIT. The process supervisor core of the cl-stack coding-agent harness (H8a): one in-image
registry for every child the harness owns (shell jobs, rpc/MCP and language servers, spawned
Lisp images, subagent threads), with owner scopes, caps and LRU eviction, restart policies,
graceful-then-forced tree stops, a replayable state-change event stream, and durable unit
files swept for orphans after a crash. Brief:
[cl-stack/docs/capabilities/supervisor.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/supervisor.md).

## License

MIT
