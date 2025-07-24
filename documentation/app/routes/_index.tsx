import { Link, Outlet } from 'react-router'
import { Bar, BarChart, CartesianGrid, XAxis } from "recharts"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { ChartConfig, ChartContainer, ChartTooltip, ChartTooltipContent } from '@/components/ui/chart'

const chartData = [
  { month: "January", desktop: 186 },
  { month: "February", desktop: 305 },
  { month: "March", desktop: 237 },
  { month: "April", desktop: 73 },
  { month: "May", desktop: 209 },
  { month: "June", desktop: 214 },
]

const chartConfig = {
  desktop: {
    label: "Desktop",
    color: "var(--chart-3)",
  },
} satisfies ChartConfig


export default function HomePage() {
  return (
    <>
      <Card className="p-6">
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
      </Card>
      <Card>
        <CardHeader>
          <CardTitle>Bar Chart</CardTitle>
          <CardDescription>January - June 2024</CardDescription>
        </CardHeader>
        <CardContent>
          <ChartContainer config={chartConfig}>
            <BarChart accessibilityLayer data={chartData}>
              <CartesianGrid vertical={false} />
              <XAxis
                dataKey="month"
                tickLine={false}
                tickMargin={10}
                axisLine={false}
                tickFormatter={(value) => value.slice(0, 3)}
              />
              <ChartTooltip
                cursor={false}
                content={<ChartTooltipContent hideLabel />}
              />
              <Bar dataKey="desktop" fill="var(--color-desktop)" radius={0} />
            </BarChart>
          </ChartContainer>
        </CardContent>
      </Card>
      <footer>
      </footer>
    </>
  );
}
