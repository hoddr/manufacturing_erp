# Controllers

The controller/handle functions provide the external interface. Most of the sanity checks (those not guaranteed by Haskell) are addressed here. There are a few helper functions for
parsing query strings, request bodies, and more. All static and non-static handles are present here.

## Notes

There are a few parts of the [source file](../src/Controllers.hs) that are a bit trickier than normal. The first is the use of the `>=>` operator, which - in essence - passes the
argument to the first statement and then passes the *result* of the first statement to the second.

Most of the more complex business code is related to writing orders and quotes to the system, as it involves inventory, pricing, projects, and more. Take great care when altering
any logic or ordering of these controllers, as it is also the logic used most frequently and affects accurate billing. The good news is these routes are also the most tested by
use.

Purchase orders are the other more complex section.
