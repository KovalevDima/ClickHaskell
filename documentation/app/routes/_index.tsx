import { Card, CardContent } from '@/components/ui/card'
import VisitsChart from '~/components/Visits'

export default function HomePage() {
  return (
    <div className="grid auto-rows-max gap-4 md:grid-cols-3">
      <Card className="p-6">
        <p>
          ClickHaskell is a pure Haskell implementation
          of ClickHouse Native protocol
        </p>
      </Card>

      <div className="grid auto-rows-max gap-3">
        <Card className="p-6 ">
          <h2>Real-time visits</h2>
          <p>
            You are receiving data via WebSockets, generated
            by loading the root page from a unique IP address.
          </p>
        </Card>
        <Card>
          <CardContent>
            <VisitsChart />
          </CardContent>
        </Card>
      </div>
    </div>
  );
}
