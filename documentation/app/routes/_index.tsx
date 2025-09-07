import { Card, CardContent } from '@/components/ui/card'
import VisitsChart from '~/components/Visits'

export default function HomePage() {
  return (
    <div className="flex flex-col gap-y-3">
      <Card className="p-6">
        <p>
          ClickHaskell is a pure Haskell implementation
          of ClickHouse Native protocol
        </p>
      </Card>

      <div className="flex flex-row gap-3">
        <Card className="p-6 max-w-75">
          <h2>Real-time visits</h2>
          <p>
            You are receiving data via WebSockets, generated
            by loading the root page from a unique IP address.
          </p>
        </Card>
        <Card className="min-w-100">
          <CardContent>
            <VisitsChart />
          </CardContent>
        </Card>
      </div>
      <footer>
      </footer>
    </div>
  );
}
