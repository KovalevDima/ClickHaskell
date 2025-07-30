# <img width="24px" height="24px" src="./documentation/app/public/assets/logo.svg"> ClickHaskell

Haskell implementation of [ClickHouse](https://clickhouse.com/) DBMS Native protocol and client

## Why ClickHouse+Haskell?

ClickHouse is a well known open source DBMS for building data intensive apps

Its design philosophy is close to functional programming due to `append-only`, support of `lambdas` and `higher-order functions`

It's a best-in-class database for storing data in event-driven architecture

## Why ClickHaskell?

Pros:

<ul type="square">
    <li>unique type-safe and low boilerplate API</li>
    <li>low dependency footprint</li>
    <li>thread-safe and well documented network code</li>
    <li>extensible CI/CD pipeline</li>
    <li>partially formalized <a href="https://clickhouse.com/docs/native-protocol/basics">Native protocol</a> based implementation</li>
</ul>

Cons:
<ul type="square">
  <li>single active maintainer</li>
  <li>limited support for protocol features</li>
  <li>API is too static</li>
</ul>

## Project structure

<pre>
├contribution/     Development related code
├library/          ClickHaskell source code
├ecosystem/        Integrations with external libraries
└documentation/    <a href="https://clickhaskell.dev/">clickhaskell.dev/</a> source code
</pre>

## General state of ClickHouse+Haskell

There are no other actively maintained open source alternatives to ClickHaskell at this moment.

All of the abandoned projects have hard-to-maintain codebases and poorer QA:

<ul type=square>
  <li><a href="https://github.com/2049foundation/clickhouse-haskell/">clickhouse-haskell</a></li>
  Provides much more dynamic API
  <li><a href="https://github.com/zaneli/hdbc-clickhouse/tree/master">hdbc-clickhouse</a></li>
  Based on RDBMS engine HDBC<br>
  Has never been released on Hackage
  <li><a href="https://github.com/Diamondy4/persistent-clickhouse">persistent-clickhouse</a></li>
  RDBMS backend of Persistent library<br>
  Has never been released on Hackage
</ul>
