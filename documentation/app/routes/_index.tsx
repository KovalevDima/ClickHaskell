import React from 'react'
import ReactDOM from 'react-dom/client'
import { Link } from 'react-router'

export default function HomePage() {
  return (
    <>
      <header>
        <div>
          <a style={{
            fontSize: "24px",
            color: "rgb(240, 246, 252)",
            textDecoration: "none",
            backgroundColor: "rgb(30, 30, 30)",
            gap: "8px"
          }}
            href="/"
          >
            <img alt="logo" width="28" height="28" src="/assets/logo.svg" />
            ClickHaskell
          </a>
        </div>
        <div>
          <a href="https://hackage.haskell.org/package/ClickHaskell">
            <img alt="hckg" width="36" height="36" src="/assets/hackage.svg" />
          </a>
          <a id="stars" href="https://git.clickhaskell.dev">
            <img alt="hckg" width="36" height="36" src="/assets/git.svg" />
          </a>
        </div>
      </header>
      <nav>
        <ul>
          <li><Link to="/contribution.html">/contribution</Link></li>
          <li><Link to="/testing">/testing</Link></li>
          <li><Link to="/usage/">/usage</Link></li>
          <li><Link to="/performance">/performance</Link></li>
          <li><a href="/#/protocol/server/">/protocol/server</a></li>
          <li><a href="/#/protocol/client/">/protocol/client</a></li>
        </ul>
      </nav>
      <main role="main">
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
      </main>
      <footer>
      </footer>
    </>
  );
}
