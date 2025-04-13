const ctx = document.getElementById('visitsChart').getContext('2d');
let chartData = {
    labels: [],
    datasets: [{ label: 'Visitors', data: [], backgroundColor: '#121212' }]
};

const visitsChart = new Chart(ctx, {
    type: 'bar',
    data: chartData,
    options: {
      
      scales: {
        x: {
          title: {
            display: true,
            text: "Hours"
          },
        },
        y: { beginAtZero: true }
      }
    }
});

function formatHour(posixTime) {
    const date = new Date(posixTime * 1000);
    return `${date.getHours()}`.padStart(2, '0');
}

const socket = new WebSocket(`${window.location.protocol === 'https:' ? 'wss' : 'ws'}://${location.host}`);

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