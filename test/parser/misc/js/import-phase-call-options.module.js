// every import call phase accepts the optional options argument
import("foo", { with: { type: "json" } });
import.defer("foo", { with: { type: "json" } });
import.source("foo", { with: { type: "json" } });
import.defer("foo", { with: { type: "json" } },);
import.source("foo");
