type Tag = "section" | "article";

export function Panel({ as, title }: { as: Tag; title: string }) {
    return (
        <{as} className="panel">
            <h2>{title}</h2>
        </{as}>
    );
}

export const Icon = ({ as }: { as: Tag }) => <{as} />;

export const WithWhitespace = ({ as }: { as: Tag }) => (
    <{ as }>
        <span />
    </{as}>
);
