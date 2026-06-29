declare const ready: boolean;

const view = (
    <section>
        @try {
            const label = ready ? "ready" : "waiting";
            <span>{label}</span>
        } @pending {
            <span>pending</span>
        } @catch (error: Error, reset) {
            <button onClick={reset}>{error.message}</button>
        }
    </section>
);

const pending_only = @try {
    <span>ready</span>
} @pending {
    <span>pending</span>
};

const catch_only = @try {
    <span>ready</span>
} @catch {
    <span>failed</span>
};
