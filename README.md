An Ocaml program that mimics the behavior of `grep`.


## How to Compile
Compile with `ocamlc -c regExpr.ml ogrep.ml`.

## Background 
Large software systems typically produce huge amounts of debugging and processing data scattered across dozens or more files and directories.  When developers or administrators need to debug these systems, they typically need to search through these files for messages or inputs that help find what kind and where the problem happened.  One of the most useful (and longest-existing) tools for this kind of search is the command-line program `grep`. The `grep` program takes command-line inputs of a string to look for and the names of one or more files to search in.  When `grep` finds a line containing the string, it prints the name of the file, followed by a colon (`:`) and the matching line.

Part of what makes grep more useful than "File search" is that in addition to searching for fixed strings, we can specify a pattern that we want grep to search for using a **regular expression**.  Regular expressions are a class of programs that specify a set of matching strings.

## Regular Expressions, Lexing, and Parsing

Regular expressions are a fundamental computational tool, used in programs from web applications, databases and network security to genome sequencing and computational linguistics.   In order to process regular expressions, we first have to obtain them from input, and we usually do this from strings (in the case of `grep`, the string specifying a regular expression comes from a command line argument).  In this homework, we'll write code to convert strings that represent grep-syntax regular expressions into data structures representing these expressions, and code that checks whether a string matches a regular expression.

### Lexing
Reading structures like expressions or programs is usually
separated into two phases, _lexing_ and _parsing_.  In _lexing_, a
string is converted into a list of *tokens*, which are the important
lexical components of a program.  Here, the important components are:

### Parsing

In _parsing_, a list of tokens is converted to a structured
representation of an expression, often called an "abstract syntax
tree."  In order to do this, we need to define the syntax of regular
expressions and their abstract representation.  We do so inductively, as
follows.  A regular expression is either:

+ a single character, or

+ a wildcard character, or

+ a bracket character, or

+ a star operator applied to a regular expression, or

+ The concatenation of two regular expressions, consisting of tokens `<re1><re2>` or

+ The union of two regular expressions, consisting of tokens `<re1> U <re2>`.

When parsing a sequence of tokens into a syntax tree, we will often need to read sub-expressions from the sequence, and keep track of the remaining tokens after this sub-expression, e.g. after parsing the `<re1>` above we'll need to know the remaining sequence of tokens after the `U`nion token to parse `<re2>`; or if we parse a sub-expression that starts with an open paren, we'll need to check that it was followed by a closing paren.

### Matching

Once we have a syntax tree for a regular expression, checking whether a string matches the expression can also be a little tricky.  There are two primary approaches, *finite state machines* and *search with backtracking* or *continuation*.  For complex regular expressions, finite state machines are faster but require more processing (and theory) before checking. I use the *continuation* approach: whenever we try to match a (sub)-string with a (sub)-regular expression, we always supply a function that remembers how to "continue" trying to match the expression.  Initially the continuation checks that we've reached the end of the string, but when checking, for instance, whether a string matches the concatenation of two subexpressions, I recursively check the first subexpression and pass along a new function that checks whether the rest of the string matches the second subexpression.


