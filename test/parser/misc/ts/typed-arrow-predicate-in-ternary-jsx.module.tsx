const ok = true;
const values: unknown[] = [];

export const clientLoader = () =>
  ok
    ? values.filter((value): value is string => Boolean(value))
    : [];

export default function Route() {
  return (
    <main>
      {clientLoader().map(value => (
        <span key={value}>{value}</span>
      ))}
    </main>
  );
}
