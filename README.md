# version-string

`version-string` is a Common Lisp system for setting version strings
that incorporate state determined from git.

To use it, be sure to set a `:version` in your project's ASDF system definition.
Now, do this:

```
(version-string:define-version-parameter +version+ :MY-SYSTEM)
```

This will define the parameter `+version+` and set it to a string
representing the ASDF version augmented by information extracted from
git.  Currently, this is just the commit hash, and whether or not
there are uncommitted local changes ("+dirty").

## Author and License

`cl-version-string` was written by Anthony Green and is distributed
under the terms of the MIT license.
