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
    color: "var(--chart-1)",
  },
} satisfies ChartConfig


export default function HomePage() {
  return (
    <div className="flex flex-col gap-y-3">
      <Card className="p-6 ">
          <h1>Efficiency</h1>
          <p>
            ClickHaskell is designed to:
          </p>
          <ul>
            <li>Handle millions of rows of data in constant memory</li>
            <li>Efficiently utilize Haskell concurrent runtime</li>
          </ul>
      </Card>

      <div className="flex flex-row gap-3">
        <Card className="p-6 w-75">

          <h2>Real-time visits</h2>
          <p>You are receiving data via WebSockets and generating<br />
            it by loading any page from a unique IP address
          </p>
        </Card>
        <Card className="w-100">
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
      </div>
      <footer>
      </footer>
    </div>
  );
}
