import { useEffect, useRef } from "react";

import hljs from "highlight.js";
import haskell from "highlight.js/lib/languages/haskell";
import "highlight.js/styles/vs2015.css";
import { Card } from "./ui/card";

hljs.registerLanguage("haskell", haskell);

type Props = {
  html: string;
};

const HighlightedHTML: React.FC<Props> = ({ html }) => {
  const containerRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (containerRef.current) {
      const codeBlocks = containerRef.current.querySelectorAll("pre code");
      codeBlocks.forEach((block) => hljs.highlightElement(block as HTMLElement));
    }
  }, [html]);

  return (
  
    <Card className="p-5">
      <div ref={containerRef} dangerouslySetInnerHTML={{ __html: html }} />
    </Card>
  )
};

export default HighlightedHTML;
