module server {
    export function load() {
        return "ok";
    }
}

import { load } from server;

export function View(): unknown @{
    const value = load();
    <span>{value}</span>
}
