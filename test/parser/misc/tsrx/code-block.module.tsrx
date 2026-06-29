const view = (
    <section>
        before
        @{ const label = "ready"; const count = 1; }
        after
    </section>
);

const rendered = (
    <section>
        @{ const label = "ready"; <span>{label}</span> }
    </section>
);

export { view, rendered };
