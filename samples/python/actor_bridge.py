"""
Python actor that communicates with ActorForth actors via message passing.

This module demonstrates the bidirectional actor pattern:
- ActorForth spawns an actor that owns some state
- Python code calls into that actor via registered Erlang functions
- The actor processes requests and returns results

Usage from ActorForth:
    py-start
    "samples/python" py-import
    "actor_bridge" "setup_bridge" 0 py-call drop
"""

import erlang


class PythonWorker:
    """
    A Python-side worker that maintains state and processes requests.
    Registered functions allow ActorForth actors to interact with it.
    """

    def __init__(self):
        self.memory = {}
        self.history = []

    def store(self, key, value):
        """Store a key-value pair."""
        self.memory[key] = value
        self.history.append(("store", key))
        return "ok"

    def fetch(self, key):
        """Fetch a value by key."""
        return self.memory.get(key, "not_found")

    def get_history(self):
        """Return operation history."""
        return self.history

    def process(self, operation, *args):
        """Generic operation dispatcher."""
        if operation == "store" and len(args) == 2:
            return self.store(args[0], args[1])
        elif operation == "fetch" and len(args) == 1:
            return self.fetch(args[0])
        elif operation == "history":
            return self.get_history()
        elif operation == "count":
            return len(self.memory)
        else:
            return {"error": f"unknown operation: {operation}"}


# Global worker instance
_worker = None


def setup_bridge():
    """
    Initialize the bridge and register functions callable from Erlang/ActorForth.
    Call this once from ActorForth: "actor_bridge" "setup_bridge" 0 py-call
    """
    global _worker
    _worker = PythonWorker()

    # Register Python functions as callable from Erlang
    erlang.register_function("py_store",
        lambda key, value: _worker.store(key, value))
    erlang.register_function("py_fetch",
        lambda key: _worker.fetch(key))
    erlang.register_function("py_history",
        lambda: _worker.get_history())
    erlang.register_function("py_count",
        lambda: _worker.process("count"))

    return "bridge_ready"


def teardown_bridge():
    """Clean up registered functions."""
    global _worker
    _worker = None
    for name in ["py_store", "py_fetch", "py_history", "py_count"]:
        try:
            erlang.unregister_function(name)
        except Exception:
            pass
    return "bridge_torn_down"
