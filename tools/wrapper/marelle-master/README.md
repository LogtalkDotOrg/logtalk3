# Marelle ("hopscotch")

[![Build Status](https://travis-ci.org/larsyencken/marelle.png)](https://travis-ci.org/larsyencken/marelle) [![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/larsyencken/marelle/trend.png)](https://bitdeli.com/free "Bitdeli Badge") [![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/larsyencken/marelle)


Test-driven system administration in SWI-Prolog, in the style of [Babushka](https://github.com/benhoskings/babushka).

Marelle uses [logic programming](https://en.wikipedia.org/wiki/Logic_programming) to describe system targets and rules by which these targets can be met. Prolog's built-in search mechanism makes writing and using these dependencies elegant. Anecdotally, writing deps for Marelle has the feel of teaching it about types of packages, rather than the feel of writing package templates.

![Hopscotch for Seniors](https://raw.github.com/wiki/larsyencken/marelle/img/HopscotchForSeniors.jpg)

## Current status

Experimental but working.

## Features

Marelle has some features common to other configuration management frameworks:

- Checking and meeting dependencies (preconditions)
- Testing whether a target installed correctly (post-conditions)
- Ability to use platform-dependent instructions

It also has some interesting differences:

- Can write checks (`met` predicates) without needing to say how to meet them (`meet` predicates)
- The dependencies of a target can vary by platform
- Succinct definition of new classes of packages using logical rules

## Installing marelle

### Quickstart

Pick a bootstrap script from the options below. If you're not sure, choose the stable version.

Version | Bootstrap command
------- | -----------------
_0.1.0 (stable)_ | `bash -c "$(curl -fsSL https://raw.githubusercontent.com/larsyencken/marelle/versions/0.1.0/bootstrap.sh)"`
_master (dev)_ | `bash -c "$(curl -fsSL https://raw.githubusercontent.com/larsyencken/marelle/master/bootstrap.sh)"`

This will install marelle for all users, putting the executable in `/usr/local/bin/marelle`.

### Manual version

1. Get Prolog
    - On OS X, with Homebrew: `brew install swi-prolog`
    - On Ubuntu, with apt-get: `sudo apt-get install swi-prolog-nox`
    - On FreeBSD, with pkgng: `sudo pkg install swi-pl`
2. Get git
    - On OS X, with Homebrew: `brew install git`
    - On Ubuntu, with apt-get: `sudo apt-get install git`
    - On FreeBSD, with pkgng: `sudo pkg install git`
3. Clone and set up marelle

```bash
# clone the repo
mkdir -p ~/.local
git clone https://github.com/larsyencken/marelle ~/.local/marelle

# set up an executable in ~/.local/bin
mkdir -p ~/.local/bin
cat >~/.local/bin/marelle <<EOF
#!/bin/sh
exec swipl -q -t main -s ~/.local/marelle/marelle.pl "$@"
EOF
chmod a+x ~/.local/bin/marelle

# add ~/.local/bin to your PATH
# (the exact commands depend on the shell you use)
echo 'export PATH=~/.local/bin:$PATH' >>~/.profile
source ~/.profile
```

## Writing deps

Make a `marelle-deps/` folder inside your project repo. Each package has two components, a `met/2` goal which checks if the dependency is met, and an `meet/2` goal with instructions on how to actually meet it if it's missing.

For example, suppose I want to write a dep for Python that works on recent Ubuntu flavours. I might write:

```prolog
% python is a target to meet
pkg(python).

% it's installed if it exists at /usr/bin/python
met(python, linux(_)) :- exists_file('/usr/bin/python').

% we can install by running apt-get in shell
meet(python, linux(_)) :-
    % could also use: install_apt('python-dev')
    sh('sudo apt-get install -y python-dev').
```

To install python on a machine, I'd now run `marelle meet python`.

To install pip, I might write:

```prolog
pkg(pip).

% pip is installed if we can run it
met(pip, _) :- which(pip).

% on all flavours of linux, try to install the python-pip package
meet(pip, linux(_)) :- install_apt('python-pip').

% on all platforms, pip depends on python
depends(pip, _, [python]).
```
Note our our use of platform specifiers and the `_` wildcard in their place. To see your current platform as described by marelle, run `marelle platform`. Examples include: `osx`, `linux(precise)` and `linux(raring)`.

## Running deps

### See available deps

This runs every `met/2` statement that's valid for your platform.

`marelle scan`

### Install something

This will run the `meet/2` clause for your package, provided a valid one exists for your current platform.

`marelle meet python`

### See your platform

To find the right platform code to use in deps you're writing, run:

`marelle platform`

It reports the code for the platform you're currently on.

## Where to put your deps

Like both Babushka and Babashka, Marelle looks for deps in `~/.marelle/deps` and in a folder called `marelle-deps` in the current directory, if either exists. This allows you to set up a personal set of deps for your environment, as well as project-specific deps.

## Examples

See my [marelle-deps](https://github.com/larsyencken/marelle-deps) repo for working examples.

## Developing

Run `make test` to run the test suite.
