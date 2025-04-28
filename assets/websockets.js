let socket;

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

      if (document.getElementById('visitsChart')) {
        initChart();
      }

    } catch (err) {
      main.innerHTML = `<p>Ошибка загрузки страницы</p>`;
    }
  }

  function handleHash() {
    const path = location.hash.slice(1);
    if (path) { loadPage(path); } else { loadPage('/'); }
  }

  function initWebSocket() {
    if (socket) return;

    socket = new WebSocket(`${window.location.protocol === 'https:' ? 'wss' : 'ws'}://${location.host}`);
    socket.onopen = () => console.log('WebSocket connected');
    socket.onmessage = event => {
      const data = JSON.parse(event.data);
      if (data.history) {
        chartData.labels = data.history.map(item => formatHour(item.hour));
        chartData.datasets[0].data = data.history.map(item => item.visits);
      } else if (data.realtime) {
        const formattedHour = formatHour(data.realtime.hour);
        const index = chartData.labels.indexOf(formattedHour);
        if (index !== -1) {
          chartData.datasets[0].data[index] = data.realtime.visits;
        }
      }
      visitsChart.update();
    };
    socket.onerror = error => console.error('WebSocket error:', error);
    socket.onclose = () => console.log('WebSocket closed');
  }

  window.addEventListener('hashchange', handleHash);
  handleHash();
  initWebSocket();
});
