import React from 'react'
import { Link, Outlet } from 'react-router'


export default function HomePage() {
  return (
    <>
      <nav>
        <ul>
          <li><Link to="/contribution">/contribution</Link></li>
          <li><Link to="/testing">/testing</Link></li>
          <li><Link to="/usage/">/usage</Link></li>
          {/* <li><Link to="/performance">/performance</Link></li> */}
          <li><a href="/protocol/server/">/protocol/server</a></li>
          <li><a href="/protocol/client/">/protocol/client</a></li>
        </ul>
      </nav>
      <h1>Efficiency</h1>
      <p>
        Powerful Haskell runtime system with the fastest analytical database.<br />
        ClickHaskell is designed to:
      </p>
      <ul>
        <li>Handle millions of rows of data in constant memory</li>
        <li>Efficiently utilize Haskell concurrent runtime</li>
      </ul>
      <h2>Real-time analytics example</h2>
      <p>You are receiving data via WebSockets and generating<br />
        it by loading any page from a unique IP address
      </p>
      {/* <canvas id="visitsChart" style="background-color: #1e1e1e"></canvas> */}
      <Outlet />
      <footer>
      </footer>
    </>
  );
}
