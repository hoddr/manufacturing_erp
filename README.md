# ERP

Project for custom ERP (enterprise resource planning) system. Will work in conjunction
with QB desktop, leading to necessity for "in-office" scripts that will be run on internal workstations
(only way to have access to QB desktop without hosting app from within office). These can be
found in a separate repo.

hoddr, 2022
No outside use permitted without explicit permission from hoddr

## MAYBES

- simplify using new IO monad with state attached for less explicit state threading
  - custom IO monad that prohibits all but certain permitted IO ops
- support for transactions on all routes (or at least business logic critical routes)

## DATABASE DUMPS

```
pg_dump -d erp --clean --no-owner > erp.sql
```

## NOTES

- the `Thread killed by timeout manager` exception is thrown due to the `keep-alive` connections
  that are the default behavior in HTTP/1.1. Firefox has a default keep alive time of 115 seconds,
  whereas the warp server default is only 30. These can be safely ignored for the most part
- using `curl` will show that these exceptions are not thrown there, where the header `keep-alive`
  is not sent

## LINER/INSULATION

Recording the key specs for liner currently supported.

- Armaflex (1")
  - 1" x 48" by 35' roll
  - 4.5 lbs/ft^3
- 0.5" Liner
  - 0.5" x 59" x 100' roll
  - 1.5 lbs/ft^3
- 1" Liner
  - 1" x 59" x 100' roll
  - 1.5 lbs/ft^3
- 1.5" Liner
  - 1.5" x 59" x 50' roll
  - 1.5 lbs/ft^3
- 2.0" Liner
  - 2.0" x 59" x 50' roll
  - 1.5 lbs/ft^3
- Double-wall insulation
  - 1" x 60" x 115' sprial SG roll
  - 575 sq ft roll
  - 3.00 lbs/ft^3?
