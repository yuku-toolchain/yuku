declare const status: "ready" | "empty" | "loop";

const view = @switch (status) {
    @case "ready": {
        break;
    }
    @case "empty": {
        if (status) {
            return <span>empty</span>;
        }
    }
    @case "loop": {
        for (const item of []) {
            break;
        }
        <span>loop</span>
    }
};
