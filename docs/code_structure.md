# Code Base

The code base is laid out to permit use of the server in tests. All non-test,
non-web source code resides [here](../src/.). The [cabal file](../erp.cabal)
details the source code structure, additional libraries, and what modules are included
in the server package. The key Haskell libraries are:

- [wai](https://hackage.haskell.org/package/wai)
- [warp](https://hackage.haskell.org/package/warp)
  - web server
- [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple-0.6.2)
  - postgres database interface
- [aeson](https://hackage.haskell.org/package/aeson)
  - json de/serialization
- [cassava](https://hackage.haskell.org/package/cassava)
  - csv de/serialization
- [lucid](https://hackage.haskell.org/package/lucid)
  - html template generation

Each library has its quirks, but as automatic instance generation was largely avoided, the code base
should provide ample examples of how to make use of each library.

## Layout

- [Entrypoint/Executable/main](../app/Main.hs)
- [Routing](../src/Lib.hs)
- [Controllers/Handles](../src/Controllers.hs)
- [Data Types/Utilities](../src/APITypes.hs)
- [Database Wrappers](../src/Database.hs)
- [Reports/HTML Gen](../src/Reports.hs)
- [Error Log](../src/ErrorLog.hs)
- [Authorization/Users](../src/Auth.hs)
- [Email Client](../src/Mail.hs)
- [UI/UX/Frontend](../web/.)
- [HTML](../web/src/html/.)
  - [Javascript](../web/src/js/.)
  - [CSS](../web/src/css/.)

The remaining source files focus on key business logic and are attached
where needed in other source files, with most being linked to their respective
controller/handle in `src/Controllers.hs`.

## Web UI/UX

The frontend leverages vanilla JS, CSS, and HTML. No frameworks are used. The key logic files are:

- [utilities](../web/src/js/main.js)
- [api/client http reqs](../web/src/js/api.js)

As per the present examples, the source files must be "loaded" in a specific order
as their is no bundling done on the build, just minify. First is `main.js`, second is `api.js`,
then whichever sources file(s) are specific to the current view. There are plenty of examples.

There are a few trickier abstractions, namely those dealing with filling in "edit" forms with
data from a structure, and extracing + parsing + constructing a data type from a form.
The logic for these resides in `main.js`. In general, restricing the code base to vanilla JS -
combined with numerous examples - should make extending, maintaining, or modifying the UI/UX rather straightforward, if tedious at times.
