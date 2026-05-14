import { useCallback, useEffect, useMemo, useState } from "react";
import { createPortal } from "react-dom";
import * as Icons from "./icons";

export interface TodoItem {
  id: string;
  title: string;
  done: boolean;
}

interface TodoListProps {
  items: TodoItem[];
  onToggle: (id: string, done: boolean) => void;
  onRemove?: (id: string) => void;
  filter?: "all" | "active" | "done";
}

const isItem = (value: unknown): value is TodoItem => {
  return (
    !!value &&
    typeof value === "object" &&
    "id" in value &&
    typeof (value as TodoItem).id === "string"
  );
};

export function TodoList({ items, onToggle, onRemove, filter = "all" }: TodoListProps) {
  const [hoverId, setHoverId] = useState<string | null>(null);

  const visible = useMemo(() => {
    if (filter === "active") return items.filter((i) => !i.done);
    if (filter === "done") return items.filter((i) => i.done);
    return items;
  }, [items, filter]);

  const handleToggle = useCallback(
    (id: string, currentlyDone: boolean) => {
      onToggle(id, !currentlyDone);
    },
    [onToggle],
  );

  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if (e.key === "Escape") setHoverId(null);
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, []);

  if (visible.length === 0) {
    return (
      <div className="empty">
        <Icons.Empty />
        <p>nothing here</p>
      </div>
    );
  }

  return (
    <ul className="todo-list">
      {visible.map((item) => {
        const hovered = hoverId === item.id;
        return (
          <li
            key={item.id}
            className={hovered ? "todo todo-hover" : "todo"}
            onMouseEnter={() => setHoverId(item.id)}
            onMouseLeave={() => setHoverId(null)}
          >
            <label>
              <input
                type="checkbox"
                checked={item.done}
                onChange={() => handleToggle(item.id, item.done)}
              />
              <span className={item.done ? "title done" : "title"}>{item.title}</span>
            </label>
            {onRemove && hovered && (
              <button type="button" onClick={() => onRemove(item.id)}>
                <Icons.Trash />
              </button>
            )}
          </li>
        );
      })}
      {typeof document !== "undefined" &&
        createPortal(
          <div className="todo-count">{visible.length} item(s)</div>,
          document.body,
        )}
    </ul>
  );
}

export function pickFirstItem(values: unknown[]): TodoItem | null {
  for (const v of values) if (isItem(v)) return v;
  return null;
}
