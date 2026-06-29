const view = (
    <section>
        <style>
            .card {
                color: red;
            }

            /* stays in css */
            @media (min-width: 40rem) {
                .card {
                    display: grid;
                }
            }
        </style>
        <article className="card">content</article>
    </section>
);

const standalone = <style>.inline { color: blue; }</style>;

const from_code_block = @{
    const tone = "green";
    <style>.from-code-block { color: green; }</style>
};

export { view, standalone, from_code_block };
