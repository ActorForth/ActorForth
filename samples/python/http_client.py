"""
HTTP client for ActorForth — call REST APIs from the BEAM.

Provides GET/POST with JSON support, rate limiting, and error handling.
All state (headers, base_url) lives at module level for persistence
across py-call invocations.

Usage from ActorForth:
    py-start
    "samples/python" py-import

    # Simple GET
    "https://httpbin.org/get" http_client get 1 py-call

    # Set a base URL for repeated calls
    "https://api.example.com" http_client set_base_url 1 py-call drop
    "/users" http_client get_path 1 py-call

    # Set auth header
    "Bearer sk-xxx" http_client set_auth 1 py-call drop

    # POST with JSON body
    "/data" "{\"key\": \"value\"}" http_client post_json 2 py-call
"""

import json
import time
import urllib.request
import urllib.error
import urllib.parse

_base_url = ""
_headers = {"Content-Type": "application/json"}
_delay = 0.1  # seconds between requests
_last_request = 0
_stats = {"requests": 0, "errors": 0}


def set_base_url(url):
    """Set the base URL for path-based requests."""
    global _base_url
    _base_url = url.rstrip("/")
    return _base_url


def set_auth(token):
    """Set Authorization header (e.g., 'Bearer sk-xxx')."""
    _headers["Authorization"] = token
    return "ok"


def set_header(name, value):
    """Set a custom header."""
    _headers[name] = value
    return "ok"


def clear_headers():
    """Reset headers to defaults."""
    global _headers
    _headers = {"Content-Type": "application/json"}
    return "ok"


def stats():
    """Return request statistics."""
    return _stats


def _throttle():
    global _last_request
    elapsed = time.time() - _last_request
    if elapsed < _delay:
        time.sleep(_delay - elapsed)
    _last_request = time.time()


def get(url):
    """GET a URL, return parsed JSON or raw string."""
    _throttle()
    _stats["requests"] += 1
    req = urllib.request.Request(url)
    for k, v in _headers.items():
        if k != "Content-Type":
            req.add_header(k, v)
    try:
        with urllib.request.urlopen(req, timeout=30) as resp:
            body = resp.read()
            try:
                return json.loads(body)
            except (json.JSONDecodeError, ValueError):
                return body.decode("utf-8", errors="replace")
    except urllib.error.HTTPError as e:
        _stats["errors"] += 1
        return {"error": e.code, "message": str(e.reason)}
    except Exception as e:
        _stats["errors"] += 1
        return {"error": 0, "message": str(e)}


def get_path(path):
    """GET base_url + path."""
    return get(_base_url + path)


def post_json(path, json_str):
    """POST JSON to base_url + path. json_str is a JSON string."""
    _throttle()
    _stats["requests"] += 1
    url = _base_url + path if not path.startswith("http") else path
    data = json_str.encode("utf-8") if isinstance(json_str, str) else json_str
    req = urllib.request.Request(url, data=data, method="POST")
    for k, v in _headers.items():
        req.add_header(k, v)
    try:
        with urllib.request.urlopen(req, timeout=30) as resp:
            body = resp.read()
            try:
                return json.loads(body)
            except (json.JSONDecodeError, ValueError):
                return body.decode("utf-8", errors="replace")
    except urllib.error.HTTPError as e:
        _stats["errors"] += 1
        try:
            body = e.read().decode("utf-8", errors="replace")
            return {"error": e.code, "message": body}
        except Exception:
            return {"error": e.code, "message": str(e.reason)}
    except Exception as e:
        _stats["errors"] += 1
        return {"error": 0, "message": str(e)}
