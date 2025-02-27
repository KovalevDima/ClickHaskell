<script src="https://cdn.jsdelivr.net/npm/chart.js"></script>


# ClickHaskell

Haskell implementation of [ClickHouse](https://clickhouse.com/) DBMS Native protocol and client

Surf across [Home page](https://clickhaskell.dev/) to learn everything you need

## ⚠️ Current version is unstable
ClickHaskell 1.0.0 will be released after most of the negative case tests have been implemented. Until then, you may encounter unexpected behavior

<canvas id="visitsChart" style="background-color: #1e1e1e"></canvas>
<script>
const ctx = document.getElementById('visitsChart').getContext('2d');
let chartData = {
    labels: [],
    datasets: [{ label: 'Visits', data: [], backgroundColor: '#121212' }]
};

const visitsChart = new Chart(ctx, {
    type: 'bar',
    data: chartData,
    options: { scales: { y: { beginAtZero: true } } }
});

function formatHour(posixTime) {
    const date = new Date(posixTime * 1000);
    return `${date.getHours()}:00`;
}

const socket = new WebSocket(`ws://${location.host}/ws`);

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
</script>
