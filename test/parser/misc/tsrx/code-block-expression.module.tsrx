const value = @{ const label = "expr"; <span>{label}</span> };

const nested = (
    <section>
        @{ const label = "outer"; @{ const inner = label; <span>{inner}</span> } }
    </section>
);

export { value, nested };
