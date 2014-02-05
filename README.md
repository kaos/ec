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
    tuple = this, is, 'a tuple'
    
    array = "first value is a string"
    array = other_value_is_an_atom
    array = \
        third, \
        is a, \
        tuple

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
  * White space (of any kind) around `=` and between key and `{` is allowed.
  * Keys and object names are atoms.
  * Values are either numbers, atoms, strings, tuples or arrays.
  * Any key can be turned into an array by defining additional values
    to it (even objects).


How it works
------------

`ec` compiles `.ec` files into `.beam` modules which can be
queried. As the config is loaded into code, this implies that there
can be multiple versions of the same config at any time. This is
useful when the application code needs to stay in sync with config
file changes during hot code upgrade. In this mode, the application
should start a "config" server to interact with to get a fixed config
version.

`ec` also supports compiling `.ec` files to plain erlang `term()`
values, much in the same way `file:consult/1` works.


ec api
======

Brief overview of the `ec` api.

  * `ec:compile(File) :: {ok, Module} | error` Compile `.ec` file to `.beam`
    module.
  * `ec:parse(File) :: {ok, [term()]} | error` Parse `.ec` file into list of
    `term()` values.

These functions also support a optional second argument with a list of
options to use:

  * `report`, `return`, `*_errors`, `*_warnings`, `warnings_as_errors`
    Report and/or return errors and/or warnings. And if warnings
    should be treated as errors.
  * `verbose` Be verbose.
  * `out_dir` If the compiled module should be saved to disk.
  * `force` Recompile, even if an up-to-date `.beam` module is found
    on disk.

In case of returning errors/warnings, the result tuple looks like:

    {ok, Module|[term()], Warnings} | {error, Errors, Warnings}
