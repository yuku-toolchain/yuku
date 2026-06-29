declare const items: string[];
declare const keyed: { id: string }[];

const list = (
    <section>
        @for (const item of items) {
            const label = item.toUpperCase();
            <span>{label}</span>
        } @empty {
            <span>empty</span>
        }
    </section>
);

const indexed = @for (const item of items; index item_index) {
    <span>{item_index}:{item}</span>
};

const keyed_list = @for (const item of keyed; index item_index; key item.id) {
    <span>{item_index}:{item.id}</span>
};

const counted = @for (let i = 0; i < items.length; i++) {
    <span>{items[i]}</span>
};
