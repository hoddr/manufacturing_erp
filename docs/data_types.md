# Data Types

Every major data type can be found in the [types source file](../src/APITypes.hs). This is where all new data types should be added to prevent circular references for types. As
explicit instances were written, there is little that needs explanation in the file. Some data types have a `ToRow` instance; others do not. Note that this affects the default to
row behavior, and specific behavior can be specified on a per function basis.

There are some generic utility functions for returning bad requests, generic success messages, and handling control flow (as the `Either` monad is omnipresent). The `wasFound`
function provides a generic, safe check for an empty query. `gen` provides a generic helper for a new instance after input into the database. There are a number of other generic
helper functions at the end of the source file.

The functions `flattenData` and `flattenErrors` provide an abstraction to coallesce over a "loop" of monadic actions and gather the
results. Check where they are used for a few examples.
