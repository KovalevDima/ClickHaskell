import { Card } from '@clickhaskell/ui/components/ui/card';
import usage from '../../usage/index.lhs?raw';

import HighlightedHTML from "@clickhaskell/ui/components/Highlight";

export default function Usage() {
  return (
    <HighlightedHTML html={usage}/>
  );
}
