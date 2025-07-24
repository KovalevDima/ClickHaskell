import { Card } from '~/components/ui/card';
import usage from '../../usage/index.lhs?raw';

import HighlightedHTML from "@/components/Highlight";

export default function Usage() {
  return (
    <HighlightedHTML html={usage}/>
  );
}
