"""
Text processing tools callable from ActorForth via py-call.
Demonstrates Python string/text capabilities exposed to BEAM.
"""

def word_count(text):
    """Count words in a string."""
    return len(text.split())

def reverse_words(text):
    """Reverse the order of words in a string."""
    return " ".join(text.split()[::-1])

def capitalize_words(text):
    """Capitalize each word."""
    return text.title()

def slugify(text):
    """Convert text to URL-friendly slug."""
    import re
    text = text.lower().strip()
    text = re.sub(r'[^\w\s-]', '', text)
    text = re.sub(r'[\s_-]+', '-', text)
    return text.strip('-')

def extract_numbers(text):
    """Extract all numbers from text."""
    import re
    return [int(n) if '.' not in n else float(n)
            for n in re.findall(r'-?\d+\.?\d*', text)]

def levenshtein(s1, s2):
    """Compute edit distance between two strings."""
    if len(s1) < len(s2):
        return levenshtein(s2, s1)
    if len(s2) == 0:
        return len(s1)
    prev = range(len(s2) + 1)
    for i, c1 in enumerate(s1):
        curr = [i + 1]
        for j, c2 in enumerate(s2):
            curr.append(min(prev[j + 1] + 1, curr[j] + 1,
                           prev[j] + (c1 != c2)))
        prev = curr
    return prev[-1]
