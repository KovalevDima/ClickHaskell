let visitsChart;
let chartData = {
  labels: [],
  datasets: [{ label: 'Visitors', data: [], backgroundColor: '#121212' }]
};

function initChart() {
  const canvas = document.getElementById('visitsChart');
  if (!canvas) return;

  const ctx = canvas.getContext('2d');

  if (visitsChart) {
    visitsChart.destroy();
  }

  visitsChart = new Chart(ctx, {
    type: 'bar',
    data: chartData,
    options: {
      scales: {
        x: { title: { display: true, text: "Hours" } },
        y: { beginAtZero: true }
      }
    }
  });
}

function formatHour(posixTime) {
  const date = new Date(posixTime * 1000);
  return `${date.getHours()}`.padStart(2, '0');
}
