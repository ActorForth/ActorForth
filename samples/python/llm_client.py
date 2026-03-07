"""
LLM client for ActorForth — call OpenAI/Anthropic from the BEAM.

Provides chat completion, embeddings, and structured output.
Requires: pip install openai   (or pip install anthropic)

Set your API key before use:
    "sk-xxx" llm_client set_api_key 1 py-call drop

Usage from ActorForth:
    py-start
    "samples/python" py-import

    # Set API key
    "sk-xxx" llm_client set_api_key 1 py-call drop

    # Simple chat
    "What is Erlang?" llm_client chat 1 py-call print

    # Chat with system prompt
    "You are a helpful Erlang expert" "Explain OTP supervisors"
        llm_client chat_with_system 2 py-call print

    # Generate embeddings
    "ActorForth is a BEAM language" llm_client embed 1 py-call

    # Structured JSON output
    "List 3 BEAM languages with descriptions"
        llm_client chat_json 1 py-call
"""

import json
import os

_api_key = os.environ.get("OPENAI_API_KEY")
_model = "gpt-4o"
_embed_model = "text-embedding-3-small"
_temperature = 0.7
_max_tokens = 1024
_client = None
_provider = "openai"  # "openai" or "anthropic"


def set_api_key(key):
    """Set the API key and initialize the client."""
    global _api_key, _client
    _api_key = key
    _client = None  # force re-init
    return "ok"


def set_model(model):
    """Set the chat model (e.g., 'gpt-4o', 'claude-sonnet-4-20250514')."""
    global _model
    _model = model
    return _model


def set_provider(provider):
    """Set provider: 'openai' or 'anthropic'."""
    global _provider, _client
    _provider = provider
    _client = None
    return _provider


def set_temperature(temp):
    """Set temperature for chat completions."""
    global _temperature
    _temperature = float(temp)
    return _temperature


def set_max_tokens(tokens):
    """Set max tokens for chat completions."""
    global _max_tokens
    _max_tokens = int(tokens)
    return _max_tokens


def _get_client():
    global _client
    if _client is not None:
        return _client
    if not _api_key:
        raise RuntimeError("API key not set. Call set_api_key first.")
    if _provider == "openai":
        from openai import OpenAI
        _client = OpenAI(api_key=_api_key)
    elif _provider == "anthropic":
        import anthropic
        _client = anthropic.Anthropic(api_key=_api_key)
    else:
        raise RuntimeError(f"Unknown provider: {_provider}")
    return _client


def chat(prompt):
    """Simple chat completion. Returns the response text."""
    return chat_with_system("You are a helpful assistant.", prompt)


def chat_with_system(system_prompt, user_prompt):
    """Chat with a system prompt. Returns the response text."""
    client = _get_client()
    if _provider == "openai":
        response = client.chat.completions.create(
            model=_model,
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ],
            temperature=_temperature,
            max_tokens=_max_tokens,
        )
        return response.choices[0].message.content
    elif _provider == "anthropic":
        response = client.messages.create(
            model=_model,
            system=system_prompt,
            messages=[{"role": "user", "content": user_prompt}],
            temperature=_temperature,
            max_tokens=_max_tokens,
        )
        return response.content[0].text


def chat_json(prompt):
    """Chat that returns structured JSON. Parses the response."""
    client = _get_client()
    system = "You are a helpful assistant. Always respond with valid JSON."
    if _provider == "openai":
        response = client.chat.completions.create(
            model=_model,
            messages=[
                {"role": "system", "content": system},
                {"role": "user", "content": prompt},
            ],
            temperature=_temperature,
            max_tokens=_max_tokens,
            response_format={"type": "json_object"},
        )
        text = response.choices[0].message.content
    elif _provider == "anthropic":
        response = client.messages.create(
            model=_model,
            system=system,
            messages=[{"role": "user", "content": prompt}],
            temperature=_temperature,
            max_tokens=_max_tokens,
        )
        text = response.content[0].text
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        return {"raw": text}


def embed(text):
    """Generate an embedding vector for text. Returns list of floats."""
    client = _get_client()
    if _provider != "openai":
        raise RuntimeError("Embeddings only supported with OpenAI provider")
    response = client.embeddings.create(
        model=_embed_model,
        input=text,
    )
    return response.data[0].embedding


def embed_batch(texts):
    """Generate embeddings for multiple texts. Returns list of vectors."""
    client = _get_client()
    if _provider != "openai":
        raise RuntimeError("Embeddings only supported with OpenAI provider")
    response = client.embeddings.create(
        model=_embed_model,
        input=texts,
    )
    return [item.embedding for item in response.data]


def similarity(text1, text2):
    """Compute cosine similarity between two texts via embeddings."""
    v1 = embed(text1)
    v2 = embed(text2)
    dot = sum(a * b for a, b in zip(v1, v2))
    mag1 = sum(a * a for a in v1) ** 0.5
    mag2 = sum(b * b for b in v2) ** 0.5
    if mag1 == 0 or mag2 == 0:
        return 0.0
    return dot / (mag1 * mag2)
