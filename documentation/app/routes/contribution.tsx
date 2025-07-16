import type { Route } from "./+types/performance";
import { useLoaderData } from "react-router";

export async function clientLoader({  }) {
  let title = localStorage.getItem("title") || "No Title";
  return { title };
}

export default function Component() {
  let data = useLoaderData();
  return (
    <div>
      <h1>{data.title}</h1>
    </div>
  );
}
