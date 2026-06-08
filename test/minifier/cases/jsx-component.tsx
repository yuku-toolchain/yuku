const Foo = ({ children }: { children?: unknown }) => ["Foo", children];
const M = { Header: ({ title }: { title: string }) => ["Header", title] };

export function run() {
  return {
    component: <Foo>hi</Foo>,
    member: <M.Header title="t" />,
    intrinsic: <div className="x" />,
    namespaced: <svg:path d="M0 0" />,
  };
}
