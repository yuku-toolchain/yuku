declare const name: string;
declare function getTag(): string;

const call_tag = <{getTag()} />;
const concat_tag = <{"x" + name} />;
const interpolated_tag = <{`x${name}`} />;
const object_tag = <{{ tag: "div" }} />;
const undefined_tag = <{undefined} />;
const void_tag = <{void 0} />;
