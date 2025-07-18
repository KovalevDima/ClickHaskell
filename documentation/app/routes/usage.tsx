import { Outlet, useLoaderData } from "react-router";
import testing from '../../testing/index.lhs?raw';

export default function Usage() {
  return (
    <div dangerouslySetInnerHTML={{__html: testing}} />
  );
}
