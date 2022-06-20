# Router

The [router](../src/Lib.hs) is more or less as expected. There are three preceding steps for each request prior to hitting the router:

1. retrieving a database connection (`getConn`)
2. generate a random request id
3. authentication middleware

The router functions as expected, with the typical "gotcha" present where order of routes matters. The only thing to really note is the `pcm n up_<foo>` sections for each route.
These indicate the required user permission level for the given route. Note that pattern matching on `innerApp` permits a "catch-all" `404` handler as the very last route in the
file. This prevents A LOT of boilerplate and repetitive code.

## Middleware

1. `authMiddleware`
  - authentication middleware
  - `forwardS` indicates bypassing authentication, i.e. "forward static route"
  - auth is handled via a standard `Authorization: Bearer <token>` header for each request
2. `pcm`
  - permissions check middleware
  - new permission sections require addition to database, api type, and relevant sql sections
