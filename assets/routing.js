document.addEventListener('DOMContentLoaded', () => {
    const main = document.querySelector('main');

    async function loadPage(path) {
        try {
            const res = await fetch(path);
            const text = await res.text();
            const doc = new DOMParser().parseFromString(text, 'text/html');
            const newMain = doc.querySelector('main');
            main.innerHTML = newMain ? newMain.innerHTML : text;

            document.querySelectorAll('pre code').forEach(block => {
                hljs.highlightElement(block);
            });
        } catch (err) {
            main.innerHTML = `<p>Ошибка загрузки страницы</p>`;
        }
    }

    function handleHash() {
        const path = location.hash.slice(1);
        if (path) {
            loadPage(path);
        }
    }

    window.addEventListener('hashchange', handleHash);
    handleHash();
});
