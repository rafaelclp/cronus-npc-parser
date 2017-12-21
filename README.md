# cronus-npc-parser
A JS parser for Cronus/rAthena npcs.

I don't know if there is any decent use case worth developing with it. A few
cases I thought about are:
- Build a JS script interpreter with a decent UI/UX for I/O;
- Build an IDE with debugging utilities (superset of the idea above);
- Build a prettify and/or obfuscation tool;
- Build a tool to help with translations;
- Build a script creator like RPG Maker's script creator.

## Goals
- Being able to parse script expressions and script code, expecting
the exact same result rAthena/Cronus would produce.
- Being able to parse npc headers to list npcs in a file. This **should not**
parse script code, but instead just skip and keep it as a string.

### Secondary goals
- Produce better error messages during lexical/syntactical analysis.
- Store line (and maybe even column) number on the original file for each node
in the AST.
- Include, in the AST, the comments found in the script code.

