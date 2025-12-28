# version-string

`version-string` produces version strings derived from your system's
ASDF version and state extracted from git.

To use it, be sure to set a `:version` in your project's ASDF system definition.
Now, do this:

```
(version-string:define-version-parameter +version+ :MY-SYSTEM)
```

This will define the parameter `+version+` and set it to a string
representing the ASDF version for `:MY-SYSTEM` augmented by
information extracted from git.

The version string logic uses `git describe` and works thusly:

- If exactly on a tag: `v1.2.3`
- If 5 commits after a tag: `v1.2.3-5-g1a2b3c4`
- If no tags exist: `1a2b3c4` (just the commit hash)
- With uncommitted changes, appends `+dirty` (e.g., `v1.2.3-5-g1a2b3c4+dirty`)
- If git isn't available, it falls back to the base version from the
  .asd file (and `0.0.0` if no `:version` was provided).

## Author and License

`cl-version-string` was written by Anthony Green and is distributed
under the terms of the MIT license.
