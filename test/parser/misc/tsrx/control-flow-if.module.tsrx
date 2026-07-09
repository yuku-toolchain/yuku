declare const ready: boolean;
declare const fallback: boolean;

const child = (
    <section>
        @if (ready) {
            const label = "ready";
            <span>{label}</span>
        } @else @if (fallback) {
            <span>fallback</span>
        } @else {
            <span>empty</span>
        }
    </section>
);

const value = @if (ready) {
    <span>yes</span>
} @else {
    <span>no</span>
};

const block = @{
    const ok = ready;
    @if (ok) {
        <span>ok</span>
    }
};
