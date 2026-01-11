import { CodeBlock } from '@clickhaskell/ui/components/ui/code-block';
import { MDXProvider } from '@mdx-js/react';
import { MDXContent } from 'mdx/types';
import { JSX } from 'react';
import { useParams } from 'react-router';

import { type BundledLanguage, codeToHtml, type ShikiTransformer } from "shiki";

const matches = import.meta.glob('../../docs/**/*.mdx', {
  eager: true,
}) as Record<string, { default: () => JSX.Element;}>;

const slug = (route: string) => `../../docs/${route}.mdx`;


export default function Component() {
  const params = useParams();
  const route = params['*'];
  if (!route) {
    return null;
  }

  const MdxComponent : MDXContent = matches[slug(route)].default;

  return (
    <MdxComponent components={{
      code({...props}) {
        return <CodeBlock code = {`${props.children}`} language="bash"/>
      }
    }}/>
  );
}
