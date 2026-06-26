"""Python preset: ty (type checking) + ruff (linting/formatting diagnostics).

Overrides the bundled 'python' preset to run ty and ruff via `uvx` rather
than expecting them on PATH. This means only `rassumfrassum` itself needs to
be installed globally (`uv tool install rassumfrassum`); ty and ruff are
fetched on demand and cached by uv in ~/.local/share/uv/tools/.

`uvx` respects per-project ruff/ty configuration (pyproject.toml, ruff.toml,
ty.toml) even though the tools run from their own isolated environments,
because they analyse the project files directly.
"""


def servers():
    return [
        ["uvx", "ty", "server"],
        ["uvx", "ruff", "server"],
    ]
