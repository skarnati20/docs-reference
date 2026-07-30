# Common Lisp Documentation Evaluation

This folder contains the evaluation set for a set of queries and a set of links we would expect retrieve from. The chunks from these relevant documents will be scored higher compared to other documents.

The documentation for Common Lisp was chosen because small models would struggle with the language without an external knowledge base.

We retrieve links from the following command:

```
(rla "https://lispcookbook.github.io/cl-cookbook/")
```

We would expect to load the following links and index the following number of chunks:


```
https://lispcookbook.github.io/cl-cookbook/ (14 chunks)
https://lispcookbook.github.io/cl-cookbook/license.html (4 chunks)
https://lispcookbook.github.io/cl-cookbook/getting-started.html (48 chunks)
https://lispcookbook.github.io/cl-cookbook/editor-support.html (27 chunks)
https://lispcookbook.github.io/cl-cookbook/emacs-ide.html (105 chunks)
https://lispcookbook.github.io/cl-cookbook/lispworks.html (55 chunks)
https://lispcookbook.github.io/cl-cookbook/vscode-alive.html (36 chunks)
https://lispcookbook.github.io/cl-cookbook/variables.html (47 chunks)
https://lispcookbook.github.io/cl-cookbook/functions.html (57 chunks)
https://lispcookbook.github.io/cl-cookbook/data-structures.html (139 chunks)
https://lispcookbook.github.io/cl-cookbook/strings.html (80 chunks)
https://lispcookbook.github.io/cl-cookbook/regexp.html (15 chunks)
https://lispcookbook.github.io/cl-cookbook/numbers.html (32 chunks)
https://lispcookbook.github.io/cl-cookbook/equality.html (31 chunks)
https://lispcookbook.github.io/cl-cookbook/iteration.html (100 chunks)
https://lispcookbook.github.io/cl-cookbook/arrays.html (64 chunks)
https://lispcookbook.github.io/cl-cookbook/dates_and_times.html (39 chunks)
https://lispcookbook.github.io/cl-cookbook/pattern_matching.html (16 chunks)
https://lispcookbook.github.io/cl-cookbook/io.html (19 chunks)
https://lispcookbook.github.io/cl-cookbook/files.html (56 chunks)
https://lispcookbook.github.io/cl-cookbook/clos.html (128 chunks)
https://lispcookbook.github.io/cl-cookbook/packages.html (26 chunks)
https://lispcookbook.github.io/cl-cookbook/systems.html (16 chunks)
https://lispcookbook.github.io/cl-cookbook/error_handling.html (57 chunks)
https://lispcookbook.github.io/cl-cookbook/debugging.html (70 chunks)
https://lispcookbook.github.io/cl-cookbook/macros.html (84 chunks)
https://lispcookbook.github.io/cl-cookbook/type.html (36 chunks)
https://lispcookbook.github.io/cl-cookbook/process.html (172 chunks)
https://lispcookbook.github.io/cl-cookbook/performance.html (65 chunks)
https://lispcookbook.github.io/cl-cookbook/testing.html (67 chunks)
https://lispcookbook.github.io/cl-cookbook/scripting.html (79 chunks)
https://lispcookbook.github.io/cl-cookbook/streams.html (46 chunks)
https://lispcookbook.github.io/cl-cookbook/misc.html (8 chunks)
https://lispcookbook.github.io/cl-cookbook/os.html (41 chunks)
https://lispcookbook.github.io/cl-cookbook/databases.html (61 chunks)
https://lispcookbook.github.io/cl-cookbook/ffi.html (7 chunks)
https://lispcookbook.github.io/cl-cookbook/dynamic-libraries.html (42 chunks)
https://lispcookbook.github.io/cl-cookbook/gui.html (83 chunks)
https://lispcookbook.github.io/cl-cookbook/sockets.html (15 chunks)
https://lispcookbook.github.io/cl-cookbook/websockets.html (25 chunks)
https://lispcookbook.github.io/cl-cookbook/web.html (88 chunks)
https://lispcookbook.github.io/cl-cookbook/web-scraping.html (26 chunks)
https://lispcookbook.github.io/cl-cookbook/win32.html (146 chunks)
```