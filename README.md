# <img width="24px" height="24px" src="./documentation/assets/logo.svg"> ClickHaskell

Haskell implementation of [ClickHouse](https://clickhouse.com/) DBMS Native protocol and client

## Why ClickHouse+Haskell?

ClickHouse is a well known open source DBMS for building data intensive apps

It's design philosophy are close to functional programming due to `append-only`, support of `lambdas` and `high order functions`

It's best in class database for storing data in event-driven architecture

## Why ClickHaskell?

Pros:

<ul type="square">
    <li>unique type safe and low boilerplate API</li>
    <li>low dependency footprint</li>
    <li>thread-safe and well documented network code</li>
    <li>rich and extensible CI/CD</li>
    <li>partially formalized <a href="https://clickhouse.com/docs/native-protocol/basics">Native protocol</a> based implementation</li>
</ul>

Cons:
<ul type="square">
  <li>single active maintainer</li>
  <li>limited protocol features supports</li>
  <li>too static API</li>
</ul>

## Project structure

<pre>
├contribution/     Development related code
├library/          ClickHaskell source code
└documentation/    <a href="https://clickhaskell.dev/">clickhaskell.dev/</a> source code
</pre>

## General state of ClickHouse+Haskell

There is no other activly maintained open source alternatives to ClickHaskell at this moment.

All of abandoned projects has sophisticated codebase and poorer QA:

<ul type=square>
  <li><a href="https://github.com/2049foundation/clickhouse-haskell/">clickhouse-haskell</a></li>
  Provides much more dynamic API
  <li><a href="https://github.com/zaneli/hdbc-clickhouse/tree/master">hdbc-clickhouse</a></li>
  Based on RDBMS engine HDBC<br>
  Never was released on Hackage
  <li><a href="https://github.com/Diamondy4/persistent-clickhouse">persistent-clickhouse</a></li>
  RDMBS backend of Persistent library<br>
  Never was released on Hackage
</ul>
