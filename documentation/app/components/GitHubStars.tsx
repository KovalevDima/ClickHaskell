import React, { useEffect, useState } from 'react';

function GitHubStars() {
  const [stars, setStars] = useState(null);

  useEffect(() => {
    fetch('https://api.github.com/repos/KovalevDima/ClickHaskell')
      .then(res => res.ok ? res.json() : Promise.reject())
      .then(data => {
        setStars(data.stargazers_count);
      })
      .catch(() => {
        // ничего не делаем при ошибке
      });
  }, []);

  if (stars === null) {
    return null; // или можно вернуть <p>Загрузка...</p>
  }

  return(
  <>
    <span>{stars}</span>
    <svg aria-hidden="true" height="18" width="18" viewBox="0 0 16 16" version="1.1" data-view-component="true">
      <g fill="rgb(227, 179, 65)">
        <path
          d="M8 .25a.75.75 0 0 1 .673.418l1.882 3.815 4.21.612a.75.75 0 0 1 .416 1.279l-3.046 2.97.719 4.192a.751.751 0 0 1-1.088.791L8 12.347l-3.766 1.98a.75.75 0 0 1-1.088-.79l.72-4.194L.818 6.374a.75.75 0 0 1 .416-1.28l4.21-.611L7.327.668A.75.75 0 0 1 8 .25Z">
        </path>
      </g>
    </svg>
  </>
  );
}

export default GitHubStars;
