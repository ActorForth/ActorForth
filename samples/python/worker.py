"""
Simple Python worker that maintains state, callable from ActorForth.
State is held at module level so it persists across py:call invocations.
"""

_data = {}

def store(key, value):
    """Store a key-value pair."""
    _data[key] = value
    return "ok"

def fetch(key):
    """Fetch a value by key."""
    return _data.get(key, "not_found")

def count():
    """Return number of stored items."""
    return len(_data)

def clear():
    """Clear all stored data."""
    _data.clear()
    return "ok"

def keys():
    """Return list of keys."""
    return list(_data.keys())
