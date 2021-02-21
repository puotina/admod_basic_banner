
# Batsh

[![Build Status](https://travis-ci.org/darrenldl/Batsh.svg?branch=master)](https://travis-ci.org/darrenldl/Batsh)

## Notes from maintainers

This repo was transferred from the original author @BYVoid,
and has been upgraded to build on more recent OCaml versions (buildable on at least OCaml 4.08.1).
You can see the discussion thread which spawned this fork effort
[here](https://discuss.ocaml.org/t/compiling-batsh/4700/).

Note that this project is currently in minimum maintenance mode.
Issues, PR may not be actively dealt with.

The following sections may contain out of date info as we are still
in the process of going through the repo

## Project description

Batsh is a simple programming language that compiles to Bash and Windows [Batch](http://www.microsoft.com/resources/documentation/windows/xp/all/proddocs/en-us/batch.mspx).
It enables you to write your script once runs on all platforms without **any** additional dependency.

Both Bash and Batch are messy to read and tricky to write due to historical reasons.
You have to spend a lot of time learning either of them and write platform-dependent code for each operating system.
I have wasted lots of time in my life struggling with bizarre syntaxes and unreasonable behaviors of them, and do not want to waste any more.

If you happen to be a maintainer of a cross-platform tool which relies on Bash on Linux/Mac and Batch on Windows as "glue code", and found it painful to "synchronize" between them, you would definitely like to try Batsh.

## How to get it

### The easiest way

[Try it online: http://batsh.org](http://batsh.org/)

### Install from OPAM

Batsh is implemented in OCaml and managed by [OPAM](http://opam.ocaml.org/pkg/batsh/0.0.5/).

1. Install OPAM. See [instructions](http://opam.ocaml.org/doc/Install.html).
2. Switch to the latest version (or at least 4.00.1) of OCaml by running `opam switch`.
3. Install Batsh: `opam install batsh`

### Build from source

You have to install OCaml (version 4.00.1 or higher) development environment before compiling Batsh from source code, and follow steps below:

1. Download source code of Batsh from [releases](https://github.com/BYVoid/Batsh/releases) or clone with git.
2. Uncompress source code tarball.
3. `make`
4. `make install`
5. Run: `batsh`

#### Dependencies

If there is any missing dependency, you can install them by running `opam install dune core_kernel ounit dlist cmdliner`

* [dune](https://dune.build/): Build framework.
* [core_kernel](https://github.com/janestreet/core_kernel/): Core_kernel is the system-independent part of Core, Jane Street's industrial-strength alternative to the OCaml standard library.
* [ounit](http://ounit.forge.ocamlcore.org/): Unit test framework.
* [dlist](https://github.com/BYVoid/Dlist): A purely functional list-like data structure supporting O(1) concatenation.
* [cmdliner](http://erratique.ch/software/cmdliner): Command line interfaces parser.

## Syntax

The syntax of Batsh is [C-based](https://en.wikipedia.org/wiki/List_of_C-based_programming_languages) (derived from C programming language).
If you have learned C, Java, C++ or JavaScript, Batsh is quite easy for you.

### Assignment

```javascript
a = 1;
b = "string";
c = [1, 2, "str", true, false];
```

### Expression

```javascript
a = 1 + 2;
b = a * 7;
c = "Con" ++ "cat";
d = c ++ b;
```

### Command

```javascript
// On UNIX
output = ls();
// On Windows
output = dir();
// Platform independent
output = readdir();

// Test existence
ex = exists("file.txt");
```

### If condition