"""
AI/ML stub module demonstrating the pattern for AI bot integration.
Uses only stdlib — swap in real AI libraries (transformers, openai, etc.)
once you install them in the venv.

This module shows the calling conventions that ActorForth actors
would use to communicate with Python AI services.
"""

import json
import hashlib


# --- Embeddings (stub) ---

def embed_text(text):
    """
    Generate a fake embedding vector from text.
    Replace with: sentence_transformers.SentenceTransformer('all-MiniLM-L6-v2').encode(text)
    """
    h = hashlib.sha256(text.encode()).hexdigest()
    # Generate a 8-dimensional "embedding" from the hash
    return [int(h[i:i+4], 16) / 65535.0 for i in range(0, 32, 4)]


def cosine_similarity(vec1, vec2):
    """Compute cosine similarity between two vectors."""
    dot = sum(a * b for a, b in zip(vec1, vec2))
    mag1 = sum(a * a for a in vec1) ** 0.5
    mag2 = sum(b * b for b in vec2) ** 0.5
    if mag1 == 0 or mag2 == 0:
        return 0.0
    return dot / (mag1 * mag2)


def find_most_similar(query, documents):
    """
    Find the most similar document to query.
    Returns (index, similarity_score, document).
    """
    q_vec = embed_text(query)
    best_idx = 0
    best_score = -1.0
    for i, doc in enumerate(documents):
        score = cosine_similarity(q_vec, embed_text(doc))
        if score > best_score:
            best_score = score
            best_idx = i
    return {"index": best_idx, "score": best_score, "document": documents[best_idx]}


# --- Chat completion (stub) ---

def chat_complete(prompt):
    """
    Stub chat completion. Takes a string prompt (not message list).
    Replace with real LLM calls via llm_client.py for production use.
    """
    if isinstance(prompt, list):
        # Handle legacy message-list format
        last_msg = prompt[-1].get("content", "") if prompt else ""
    else:
        last_msg = str(prompt)
    return {
        "role": "assistant",
        "content": f"[stub] I received: {last_msg}"
    }


# --- JSON processing ---

def to_json(data):
    """Convert data to JSON string."""
    return json.dumps(data, indent=2)


def from_json(text):
    """Parse JSON string to data."""
    return json.loads(text)


# --- Pipeline pattern ---

def run_pipeline(steps, initial_data):
    """
    Run a sequence of named processing steps.
    Each step is a function name in this module.
    Demonstrates the pattern for AI pipelines callable from ActorForth.
    """
    data = initial_data
    results = []
    for step_name in steps:
        fn = globals().get(step_name)
        if fn and callable(fn):
            data = fn(data)
            results.append({"step": step_name, "result": data})
        else:
            results.append({"step": step_name, "error": "unknown function"})
    return results
