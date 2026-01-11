import { MDXContent } from 'mdx/types';
import { JSX } from 'react';
import { useParams } from 'react-router';
import { cn } from "@clickhaskell/ui/lib/utils"

const matches = import.meta.glob('../../docs/**/*.{mdx,lhs}', {
  eager: true,
}) as Record<string, { default: () => JSX.Element;}>;

const slug = (route: string) => [
  `../../docs/${route}.mdx`,
  `../../docs/${route}.lhs`,
];

export default function Component() {
  const params = useParams();
  const route = params['*'];
  if (!route) return null;

  const match = slug(route).map(path => matches[path]).find(Boolean);
  if (!match) return null;

  const MdxComponent : MDXContent = match.default;

  return (
    <MdxComponent components={{
      code({...props}) {
        return <code {...props}/>
      },
      pre({className, ...props}) {
        return <pre className={cn(className, 'p-3')} {...props} />
      }
    }}/>
  );
}
