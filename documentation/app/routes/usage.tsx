import testing from '../../usage/index.lhs?raw';

import HighlightedHTML from "app/components/Highlight";

export default function Usage() {
  return (
    <HighlightedHTML html={testing} />
  );
}
