const url = "https://api.github.com/repos/KovalevDima/ClickHaskell";
    fetch(url)
    .then(async response => {
        return({data: await response.json(), ok: response.ok})
    })
    .then(handledResponse => {
        if (handledResponse.ok) {
            var div = document.getElementById('stars');
            div.innerHTML += handledResponse.data.stargazers_count;
            div.innerHTML += `
                <svg aria-hidden="true" height="20" width="20" viewBox="0 0 16 16" version="1.1" width="16" data-view-component="true">
                    <g fill="rgb(227, 179, 65)">
                        <path d="M8 .25a.75.75 0 0 1 .673.418l1.882 3.815 4.21.612a.75.75 0 0 1 .416 1.279l-3.046 2.97.719 4.192a.751.751 0 0 1-1.088.791L8 12.347l-3.766 1.98a.75.75 0 0 1-1.088-.79l.72-4.194L.818 6.374a.75.75 0 0 1 .416-1.28l4.21-.611L7.327.668A.75.75 0 0 1 8 .25Z"></path>
                        </g>
                </svg>
            `
        }
    })
    .catch(console.error);
