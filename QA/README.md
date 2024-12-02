---
title: Documentation
---

Want to contribute to library?

- [development](/development)
- [usage](/usage)
- [testing](/testing)

# Design

ClickHaskell was designed to **avoid boilerplate** code\
and **decouple business logic** from DBMS protocol implementation

The key idea is to specilize database **table**/**query**/**view** interface\
as a Haskell type and then to constuct a correspondence\
(**decoder**/**encoder** and **query**) from **record generic representation**

For example in case of
```text
Table              <--    Record
├name1 : Type1   encoder  ├name1 : Type1
├name2 : Type2            ├name2 : Type2
...              decoder  ...        
└nameN : TypeN     -->    └nameM : TypeM
```
we can construct a decoders/encoders for server/client\
packets with data

Also we can construct queries like
```sql
SELECT name1, name2, ..., nameN FROM tableName
SELECT name1, name2, ..., nameN FROM viewName(...)
INSERT name1, name2, ..., nameN INTO tableName
```
and combine it with encoder/decoder to generate\
end-to-end database communication functions which\
only takes a data on runtime
