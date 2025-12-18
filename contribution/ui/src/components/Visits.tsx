import { useEffect, useRef, useState } from "react";
import { Bar, BarChart, CartesianGrid, XAxis } from "recharts";
import {
  ChartConfig,
  ChartContainer,
  ChartTooltip,
  ChartTooltipContent,
} from "@/components/ui/chart";

const chartConfig = {
} satisfies ChartConfig;


function formatHour(posixTime: number) {
  const date = new Date(posixTime * 1000);
  return `${date.getHours()}`.padStart(2, '0');
}

type DataPoint = { hour: number; visits: number };

export default function VisitsChart() {
  const [chartData, setChartData] = useState<{ hour: string; visits: number }[]>([]);

  const socketRef = useRef<WebSocket | null>(null);

  useEffect(() => {
    const protocol = window.location.protocol === "https:" ? "wss" : "ws";
    const wsUrl = `${protocol}://${window.location.host}/visits`;
    socketRef.current = new WebSocket(wsUrl);

    socketRef.current.onopen = () => {
      console.log("WebSocket connected");
    };

    socketRef.current.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data);

        if (data.history) {
          const history = data.history as DataPoint[];
          const newData = history
            .slice()
            .sort((a, b) => a.hour - b.hour)
            .map(
            (item: DataPoint) =>
              ({...item, hour: formatHour(item.hour)})
        );
          console.log(data.history);
          setChartData(newData);
        }

        if (data.realtime) {
          const hour = formatHour(data.realtime.hour);
          setChartData((prev) =>
            prev.map((entry) =>
              entry.hour === hour
                ? { ...entry, visits: data.realtime.visits }
                : entry
            )
          );
        }
      } catch (err) {
        console.error("WebSocket data parsing error", err);
      }
    };

    socketRef.current.onerror = (err) => {
      console.error("WebSocket error:", err);
    };

    socketRef.current.onclose = () => {
      console.log("WebSocket closed");
    };

    return () => {
      socketRef.current?.close();
    };
  }, []);

  return (
    <ChartContainer config={chartConfig}>
      <BarChart accessibilityLayer data={chartData}>
        <CartesianGrid vertical={false} />
        <XAxis
          dataKey="hour"
          tickLine={false}
          tickMargin={10}
          axisLine={false}
        />
        <ChartTooltip cursor={false} content={<ChartTooltipContent hideLabel />} />
        <Bar dataKey="visits" fill="var(--color-chart-1)" radius={0} />
      </BarChart>
    </ChartContainer>
  );
}
