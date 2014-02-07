ec
==

*ec* (pronounced Easy) is a Erlang Configuration library, designed to
 make end users of your application a pleasant one, whenever they need
 to mess around with configuration.


What it looks like
------------------

An easy config file (which should use a `.ec` suffix) may look like this:

    # Comments start with hash, and runs until end of line
    
    # A simple key-value pair
    simple = "My value"
    
    # Some key-value pairs with not so simple values
    tuple = {this, is, 'a tuple'}
    
    duplicates = "first value is a string"
    duplicates = other_value_is_an_atom

    # Values run until end of line.
    # Escape the new line for multi-line values..
    duplicates = [\
        third, \
        'is an', \
        array \
    ]

    empty =
    
    # object like properties
    obj {
        a_key = 123
        also_array = [12, foo, "bar, baz", {tuple, 42}]
    }
    
    nested {
        foo {
            bar = baz
        }
    }

#### A few noteworthy things

  * Backslash is used as escape char.
  * White space (of any kind) around `=` and between key and `{` is
    allowed.
  * Keys and object names are atoms.
  * Values are either numbers, atoms, strings, tuples or arrays (or,
    any Erlang term, really).
  * Duplicated keys result in multiple instances of it, rather than
    any one value overriding the others.
  * For most parts, it's still Erlang syntax, so keys being atoms
    means that `'@#$%\\nhuh'` is a valid key.


How it works
------------

`ec` compiles `.ec` files into `.beam` modules which can be
queried. As the config is loaded into code, this implies that there
can be multiple versions of the same config at any time. This is
useful when the application code needs to stay in sync with config
file changes during hot code upgrade. In this mode, the application
should start a "config" server to interact with to get a fixed config
version.

`ec` also supports parsing `.ec` files to plain erlang proplists
(which is a intermediate step when compiling to .beam modules).


ec api
======

Brief overview of the `ec` api:

  * `ec:compile_file(Filename) :: {ok, Module} | error` Compile `.ec` file to `.beam`
    module.
  * `ec:parse_file(Filename) :: {ok, [term()]} | error` Parse `.ec` file into list of
    `term()` values.

These functions also support an optional second argument with a list
of options to use **NYI**:

  * `report`, `return`, `*_errors`, `*_warnings` Report and/or return
    errors and/or warnings.
  * `warnings_as_errors` Treat warnings as errors.
  * `verbose` Be verbose.
  * `out_dir` If the compiled module should be saved to disk.
  * `force` Recompile, even if an up-to-date `.beam` module is found
    on disk.

In case of returning errors/warnings, the result tuple looks like:

    {ok, Module|[term()], Warnings} | {error, Errors, Warnings}
