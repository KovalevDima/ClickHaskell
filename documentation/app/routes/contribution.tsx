import HighlightedHTML from "@/components/Highlight";
import contribution from '../../contribution.html?raw';

export default function Component() {
  return (
    <HighlightedHTML html={contribution}/>
  );
}
