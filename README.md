[![Travis][travis badge]][travis]
[![CodeCov][codecov badge]][codecov]
[![Erlang Versions][erlang versions badge]][erlang]
[![License][license badge]][license]
[![Latest Release][release badge]][release]
[![Last Commit][commit badge]][commit]

Contact information and license
-------------------------------

PropEr (PROPerty-based testing tool for ERlang) is a QuickCheck-inspired
open-source property-based testing tool for Erlang, developed by Manolis
Papadakis, Eirini Arvaniti and Kostis Sagonas. The base PropEr system was
written mainly by Manolis Papadakis, and the stateful code testing subsystem
by Eirini Arvaniti. Kostis Sagonas has been actively maintaining its code
base since 2012.

You can reach PropEr's developers in the following ways:

*   on the web: at [the project's home page](http://proper-testing.github.io)
    or [the project's github page](https://github.com/proper-testing/proper)
*   by email: take the tool's name (all lowercase), add a @ followed by
    softlab dot ntua dot gr

We welcome user contributions and feedback (comments, suggestions, feature
requests, bug reports, patches, etc.).

Copyright 2010-2020 by Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas.

This program is distributed under the [GPL](http://www.gnu.org/licenses/gpl.html),
version 3 or later. Please see the [COPYING][license] file for details.


Introduction
------------

Traditional testing methodologies essentially involve software testers writing a
series of test inputs for their programs, along with their corresponding
expected outputs, then running the program with these inputs and observing
whether it behaves as expected. This method of testing, while simple and easy to
automate, suffers from a few problems, such as:

*   Writing test cases by hand is tedious and time consuming.
*   It is hard to know whether the test suite covers all aspects of the software
    under test.

Property-based testing is a novel approach to software testing, where the tester
needs only specify the generic structure of valid inputs for the program under
test, plus certain properties (regarding the program's behaviour and the
input-output relation) which are expected to hold for every valid input.
A property-based testing tool, when supplied with this information, should randomly
produce progressively more complex valid inputs, then apply those inputs to the
program while monitoring its execution, to ensure that it behaves according to
its specification, as outlined in the supplied properties.

Here are a few examples of simple properties a user may wish to test, expressed
in natural language:

*   The program should accept any character string and convert all lowercase
    letters inside the string to uppercase.
*   The program should accept any list of integers. If the input list is at
    least 4 elements long, the program should return the 4th largest integer in
    the list, else it should throw an exception.

PropEr is such a property-based testing tool, designed to test programs written
in the Erlang programming language. Its focus is on testing the behaviour of
pure functions. On top of that, it is equipped with two library modules that can
be used for testing stateful code. The input domain of functions is specified
through the use of a type system, modeled closely after the type system of the
language itself. Properties are written using Erlang expressions, with the help
of a few predefined macros.

PropEr is also tightly integrated with Erlang's type language:

*   Types expressed in the Erlang type language can be used instead of
    generators written in PropEr's own type system as input data specifications.
*   Generators for ADTs can be constructed automatically using the ADTs' API
    functions.
*   PropEr can test functions automatically, based solely on information
    provided in their specs.


Quickstart guide
----------------

*   Obtain a copy of PropEr's sources. You can either get a tagged version of
    the tool (look under `Tags` on github) or you can clone the current code
    base:

    ```shell
        git clone git://github.com/proper-testing/proper.git
    ```
*   Compile PropEr: Simply run `make` if you just want to build PropEr.
    If you want to do some changes to PropEr or submit some pull request you
    most likely will want to issue a `make test` to run its unit tests and
    a `make dialyzer` call to also run dialyzer on PropEr's code base.
    To do the above but also build PropEr's documentation issue a `make all`
    call; in that case, you are going to need the `syntax_tools` application
    and a recent version of `EDoc`).
*   If you are using [Homebrew](https://brew.sh), you can simply:

    ```shell
        brew install proper
    ```
    and continue following the instructions below.
*   Add PropEr's base directory to your Erlang library path, using one of the
    following methods:
    1.   `ERL_LIBS` environment variable: Add the following line to your shell
         startup file (`~/.bashrc` in the case of the Bash shell):

         ```shell
             export ERL_LIBS=/full/path/to/proper
         ```
    2.   Erlang resource file: Add the following line to your `~/.erlang` file:

         ```erlang
             code:load_abs("/full/path/to/proper").
         ```
*   Add the following include line to all source files that contain properties:

    ```erlang
    -include_lib("proper/include/proper.hrl").
    ```

*   Compile those source files, preferably with `debug_info` enabled.
*   For each property, run:

    ```erlang
    proper:quickcheck(your_module:some_property()).
    ```
    See also the section common problems below if you want to run
    PropEr from EUnit.


Where to go from here
---------------------

To get started on using PropEr, see the tutorials and testing tips provided on
[PropEr's home page](http://proper-testing.github.io). On the same site you can
find a copy of PropEr's API documentation (you can also build this from source
if you prefer, by running `make doc`), as well as links to more resources on
property-based testing.


Common problems
---------------

### Using PropEr in conjunction with EUnit

The main issue is that both systems define a **`?LET`** macro. To avoid a
potential clash, simply include PropEr's header file before EUnit's. That
way, any instance of **`?LET`** will count as a PropEr **`?LET`**.

Another issue is that [EUnit captures standard output][eunit stdout],
so normally PropEr output is not visible when `proper:quickcheck()` is
invoked from EUnit. You can work around this by passing the option
`{to_file, user}` to `proper:quickcheck/2`. For example:
```erlang
?assertEqual(true, proper:quickcheck(your_mod:some_prop(), [{to_file, user}])).
```
This will make PropEr properties visible also when invoked from EUnit.


Incompatibilities with QuviQ's QuickCheck
-----------------------------------------

PropEr's notation and output format has been kept quite similar to that of
QuviQ's QuickCheck in order to ease the reuse of existing testing code written
for that tool. However, incompatibilities are to be expected, since we never
run or owned a copy of QuviQ's QuickCheck and the two programs probably bear
little resemblance under the hood. Here we provide a nonexhaustive list of
known incompatibilities:

*   **`?SUCHTHATMAYBE`** behaves differently in PropEr.
*   `proper_gen:pick/1` differs from `eqc_gen:pick/1` in return value format.
*   PropEr handles `size` differently from QuickCheck.
*   `proper:module/2` accepts options in the second argument instead of the
    first; this is for consistency with other `module/2` functions in Erlang/OTP.

All the above are from circa 2010. Most likely, there exist many more
incompatibilities between the two tools by now.


<!-- Links (alphabetically) -->
[codecov]: https://codecov.io/gh/xspirus/proper
[commit]: https://github.com/xspirus/proper/commit/HEAD
[erlang]: http://www.erlang.org
[eunit stdout]: http://erlang.org/doc/apps/eunit/chapter.html#Running_EUnit
[license]: ./COPYING
[release]: https://github.com/proper-testing/proper/releases/latest
[travis]: https://travis-ci.org/xspirus/proper

<!-- Badges (alphabetically) -->
[codecov badge]: https://codecov.io/gh/xspirus/proper/branch/develop/graph/badge.svg
[commit badge]: https://img.shields.io/github/last-commit/xspirus/proper/develop.svg?style=flat-square
[erlang versions badge]: https://img.shields.io/badge/erlang-19.0%20to%2022.3-blue.svg?style=flat-square
[license badge]: https://img.shields.io/github/license/proper-testing/proper.svg?style=flat-square
[release badge]: https://img.shields.io/github/release/proper-testing/proper.svg?style=flat-square
[travis badge]: https://travis-ci.org/xspirus/proper.svg?branch=develop
