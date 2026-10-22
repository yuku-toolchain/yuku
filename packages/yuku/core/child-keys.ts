export type ChildVisit<State> = (state: State, value: unknown, key: string) => boolean;

export type ChildVisitor = <State>(
  node: Readonly<Record<string, unknown>>,
  visit: ChildVisit<State>,
  state: State,
) => boolean;

export const childKeys: Record<string, string[]> = Object.create(null);

export const widened: Set<string> = new Set();

export function addChildKeys(type: string, keys: string[]): void {
  const existing = childKeys[type];
  if (existing === undefined) {
    childKeys[type] = keys;
    return;
  }
  for (const key of keys) {
    if (!existing.includes(key)) {
      existing.push(key);
      widened.add(type);
    }
  }
}
