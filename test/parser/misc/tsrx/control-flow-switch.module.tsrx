declare const status: "ready" | "empty" | "error";

const view = (
    <section>
        @switch (status) {
            @case "ready": {
                <span>ready</span>
            }
            @case "empty": {
                const label = "empty";
                <span>{label}</span>
            }
            @default: {
                <span>error</span>
            }
        }
    </section>
);

const value = @switch (status) {
    @case "ready": {
        <span>ready</span>
    }
    @default: {
        <span>fallback</span>
    }
};
