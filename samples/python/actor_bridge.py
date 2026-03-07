"""
Python actor bridge — module-level state pattern for ActorForth interop.

The erlang_python library runs py:exec in isolated scopes, so Python-side
state must live at module level (not in py:exec blocks). This module
demonstrates the working pattern: module-level objects with plain functions
that ActorForth calls via py-call.

Usage from ActorForth:
    py-start
    "samples/python" py-import
    actor_bridge setup 0 py-call drop      # initialize
    "name" "Ada" actor_bridge store 2 py-call drop
    "name" actor_bridge fetch 1 py-call     # -> String("Ada")
    actor_bridge count 0 py-call            # -> Int(1)
    actor_bridge history 0 py-call          # -> List of operations
    actor_bridge clear 0 py-call drop       # reset
"""

_data = {}
_history = []


def setup():
    """Initialize/reset the bridge state."""
    global _data, _history
    _data = {}
    _history = []
    return "ready"


def store(key, value):
    """Store a key-value pair."""
    _data[key] = value
    _history.append(("store", key, value))
    return "ok"


def fetch(key):
    """Fetch a value by key. Returns 'not_found' if missing."""
    return _data.get(key, "not_found")


def delete(key):
    """Delete a key. Returns the old value or 'not_found'."""
    val = _data.pop(key, "not_found")
    if val != "not_found":
        _history.append(("delete", key))
    return val


def count():
    """Return number of stored items."""
    return len(_data)


def keys():
    """Return list of all keys."""
    return list(_data.keys())


def values():
    """Return list of all values."""
    return list(_data.values())


def history():
    """Return operation history as list of tuples."""
    return _history


def clear():
    """Clear all data and history."""
    global _data, _history
    _data = {}
    _history = []
    return "ok"


def to_dict():
    """Return a copy of the entire store as a dict."""
    return dict(_data)
