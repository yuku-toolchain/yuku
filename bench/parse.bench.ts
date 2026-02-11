import { readFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { bench, describe, beforeAll } from "vitest";

const __dirname = dirname(fileURLToPath(import.meta.url));

// WASM interface types matching the parser exports
interface WasmExports {
  alloc: (size: number) => number;
  free: (ptr: number, size: number) => void;
  parse: (
    sourcePtr: number,
    sourceLen: number,
    sourceType: number,
    lang: number,
  ) => bigint;
  memory: WebAssembly.Memory;
}

// Source type and lang enums matching the parser
const SourceType = { Script: 0, Module: 1 } as const;
const Lang = { JS: 0, TS: 1, JSX: 2, TSX: 3, DTS: 4 } as const;

let wasm: WasmExports;

function parseSource(
  source: string,
  sourceType: number,
  lang: number,
): unknown {
  const encoder = new TextEncoder();
  const sourceBytes = encoder.encode(source);
  const sourceLen = sourceBytes.length;

  const sourcePtr = sourceLen > 0 ? wasm.alloc(sourceLen) : 0;

  if (sourceLen > 0 && !sourcePtr) {
    throw new Error("Failed to allocate memory for source code");
  }

  try {
    if (sourceLen > 0 && sourcePtr) {
      const wasmMemory = new Uint8Array(
        wasm.memory.buffer,
        sourcePtr,
        sourceLen,
      );
      wasmMemory.set(sourceBytes);
    }

    const result = wasm.parse(sourcePtr, sourceLen, sourceType, lang);

    if (result === 0n) {
      throw new Error("Failed to parse source code");
    }

    const resultPtr = Number(result & 0xffffffffn);
    const jsonLen = Number(result >> 32n);

    if (resultPtr + jsonLen > wasm.memory.buffer.byteLength) {
      throw new Error("Invalid result pointer from WASM parser");
    }

    try {
      const decoder = new TextDecoder();
      const jsonBytes = new Uint8Array(wasm.memory.buffer, resultPtr, jsonLen);
      const jsonStr = decoder.decode(jsonBytes);
      return JSON.parse(jsonStr);
    } finally {
      wasm.free(resultPtr, jsonLen);
    }
  } finally {
    if (sourceLen > 0 && sourcePtr) {
      wasm.free(sourcePtr, sourceLen);
    }
  }
}

// Sample JavaScript sources of varying complexity
const smallSource = `const x = 42;`;

const mediumSource = `
function fibonacci(n) {
  if (n <= 1) return n;
  return fibonacci(n - 1) + fibonacci(n - 2);
}

const results = [];
for (let i = 0; i < 20; i++) {
  results.push(fibonacci(i));
}

export default results;
`;

const largeSource = `
import { EventEmitter } from "events";

class TaskRunner extends EventEmitter {
  #queue = [];
  #running = 0;
  #concurrency;

  constructor(concurrency = 4) {
    super();
    this.#concurrency = concurrency;
  }

  add(task, priority = 0) {
    this.#queue.push({ task, priority });
    this.#queue.sort((a, b) => b.priority - a.priority);
    this.#run();
    return this;
  }

  async #run() {
    while (this.#running < this.#concurrency && this.#queue.length > 0) {
      const { task } = this.#queue.shift();
      this.#running++;
      this.emit("taskStart", task);

      try {
        const result = await task();
        this.emit("taskComplete", result);
      } catch (error) {
        this.emit("taskError", error);
      } finally {
        this.#running--;
        this.#run();
      }
    }

    if (this.#running === 0 && this.#queue.length === 0) {
      this.emit("drain");
    }
  }

  get pending() {
    return this.#queue.length;
  }

  get active() {
    return this.#running;
  }
}

const runner = new TaskRunner(2);

const delay = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

const createTask = (id, duration) => async () => {
  console.log("Starting task " + id);
  await delay(duration);
  console.log("Completed task " + id);
  return { id, duration };
};

runner.on("taskComplete", ({ id }) => {
  console.log("Task " + id + " finished");
});

runner.on("drain", () => {
  console.log("All tasks complete");
});

for (let i = 0; i < 10; i++) {
  runner.add(createTask(i, Math.random() * 1000), Math.floor(Math.random() * 5));
}

export { TaskRunner, createTask };
`;

const jsxSource = `
import React, { useState, useEffect, useCallback } from "react";

function TodoApp() {
  const [todos, setTodos] = useState([]);
  const [input, setInput] = useState("");
  const [filter, setFilter] = useState("all");

  useEffect(() => {
    const saved = localStorage.getItem("todos");
    if (saved) {
      setTodos(JSON.parse(saved));
    }
  }, []);

  useEffect(() => {
    localStorage.setItem("todos", JSON.stringify(todos));
  }, [todos]);

  const addTodo = useCallback(() => {
    if (input.trim()) {
      setTodos((prev) => [
        ...prev,
        { id: Date.now(), text: input.trim(), completed: false },
      ]);
      setInput("");
    }
  }, [input]);

  const toggleTodo = useCallback((id) => {
    setTodos((prev) =>
      prev.map((todo) =>
        todo.id === id ? { ...todo, completed: !todo.completed } : todo,
      ),
    );
  }, []);

  const filtered = todos.filter((todo) => {
    if (filter === "active") return !todo.completed;
    if (filter === "completed") return todo.completed;
    return true;
  });

  return (
    <div className="app">
      <h1>Todo App</h1>
      <div className="input-group">
        <input
          type="text"
          value={input}
          onChange={(e) => setInput(e.target.value)}
          onKeyDown={(e) => e.key === "Enter" && addTodo()}
          placeholder="Add a todo..."
        />
        <button onClick={addTodo}>Add</button>
      </div>
      <div className="filters">
        {["all", "active", "completed"].map((f) => (
          <button
            key={f}
            className={filter === f ? "active" : ""}
            onClick={() => setFilter(f)}
          >
            {f}
          </button>
        ))}
      </div>
      <ul>
        {filtered.map((todo) => (
          <li
            key={todo.id}
            className={todo.completed ? "completed" : ""}
            onClick={() => toggleTodo(todo.id)}
          >
            {todo.text}
          </li>
        ))}
      </ul>
      <p>{todos.filter((t) => !t.completed).length} items left</p>
    </div>
  );
}

export default TodoApp;
`;

beforeAll(async () => {
  const wasmPath = resolve(__dirname, "../zig-out/bin/yuku.wasm");
  const wasmBytes = readFileSync(wasmPath);
  const wasmModule = await WebAssembly.instantiate(wasmBytes);
  wasm = wasmModule.instance.exports as unknown as WasmExports;
});

describe("parse JavaScript", () => {
  bench("small source (single statement)", () => {
    parseSource(smallSource, SourceType.Script, Lang.JS);
  });

  bench("medium source (functions and loops)", () => {
    parseSource(mediumSource, SourceType.Module, Lang.JS);
  });

  bench("large source (classes, async, modules)", () => {
    parseSource(largeSource, SourceType.Module, Lang.JS);
  });
});

describe("parse JSX", () => {
  bench("React component with hooks", () => {
    parseSource(jsxSource, SourceType.Module, Lang.JSX);
  });
});

describe("parse modes", () => {
  bench("script mode", () => {
    parseSource(mediumSource, SourceType.Script, Lang.JS);
  });

  bench("module mode", () => {
    parseSource(mediumSource, SourceType.Module, Lang.JS);
  });
});
