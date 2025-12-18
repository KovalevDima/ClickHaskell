import { Card } from '@clickhaskell/ui/components/ui/card';
import html from './protocol/client.html?raw';

export default function Component() {
  return (
    <Card className="p-5">
      <div
        className="flex flex-col gap-5"
        dangerouslySetInnerHTML={{ __html: html as string }}
      />
    </Card>
  );
}
