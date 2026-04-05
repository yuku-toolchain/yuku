// ESTree decoder for Yuku parser binary transfer format.
// reads the ArrayBuffer produced by the native parser and constructs
// plain JS objects matching the ESTree / typescript-eslint AST spec.

"use strict";

const HEADER_SIZE = 36;
const NODE_SIZE = 32;
const COMMENT_SIZE = 20;
const NULL = 0xFFFFFFFF;

const BINARY_OPS = ["==", "!=", "===", "!==", "<", "<=", ">", ">=", "+", "-", "*", "/", "%", "**", "|", "^", "&", "<<", ">>", ">>>", "in", "instanceof"];
const LOGICAL_OPS = ["&&", "||", "??"];
const UNARY_OPS = ["-", "+", "!", "~", "typeof", "void", "delete"];
const UPDATE_OPS = ["++", "--"];
const ASSIGNMENT_OPS = ["=", "+=", "-=", "*=", "/=", "%=", "**=", "<<=", ">>=", ">>>=", "|=", "^=", "&=", "||=", "&&=", "??="];
const VAR_KINDS = ["var", "let", "const", "using", "await using"];
const PROPERTY_KINDS = ["init", "get", "set"];
const METHOD_KINDS = ["constructor", "method", "get", "set"];
const FUNCTION_TYPES = ["FunctionDeclaration", "FunctionExpression", "TSDeclareFunction", "TSEmptyBodyFunctionExpression"];
const CLASS_TYPES = ["ClassDeclaration", "ClassExpression"];
const IMPORT_PHASES = [null, "source", "defer"];
const COMMENT_TYPES = ["Line", "Block"];
const SEVERITY_NAMES = ["error", "warning", "hint", "info"];

// module-level state, set per decode() call.
// avoids closures -- V8 optimizes module-level reads better than closure captures.
let _u8, _u32, _dv, _source, _sourceLen, _nodesOff, _extraBase, _spOff;

function decode(buffer, source) {
  _u8 = new Uint8Array(buffer);
  _dv = new DataView(buffer);
  const alignedLen = (buffer.byteLength >> 2) << 2;
  _u32 = new Uint32Array(buffer, 0, alignedLen >> 2);
  _source = source;

  const nodeCount = _u32[2];
  const extraCount = _u32[3];
  const stringPoolLen = _u32[4];
  _sourceLen = _u32[5];
  const commentCount = _u32[6];
  const diagCount = _u32[7];
  const programIndex = _u32[8];

  _nodesOff = HEADER_SIZE;
  const extraOffset = _nodesOff + nodeCount * NODE_SIZE;
  _extraBase = extraOffset >> 2;
  _spOff = extraOffset + extraCount * 4;
  const commentsOffset = _spOff + stringPoolLen;
  const diagsOffset = commentsOffset + commentCount * COMMENT_SIZE;

  // pre-decode string pool into a single JS string for fast slicing
  let _poolStr = "";
  if (stringPoolLen > 0) {
    _poolStr = textDecoder.decode(_u8.subarray(_spOff, _spOff + stringPoolLen));
  }

  // inline string resolver
  function str(s, e) {
    if (s === e) return "";
    if (s < _sourceLen) return _source.slice(s, e);
    return _poolStr.slice(s - _sourceLen, e - _sourceLen);
  }

  // node field accessors -- computed once per node via local offset
  function node(i) {
    const o = _nodesOff + i * NODE_SIZE;
    const tag = _u8[o];
    const flags = _u8[o + 1];
    const f0 = _u8[o + 2] | (_u8[o + 3] << 8); // u16 LE
    const b = o >> 2;
    const f1 = _u32[b + 1];
    const f2 = _u32[b + 2];
    const f3 = _u32[b + 3];
    const f4 = _u32[b + 4];
    const f5 = _u32[b + 5];
    const start = _u32[b + 6];
    const end = _u32[b + 7];

    switch (tag) {
      case 74: { // program
        const st = (flags & 1) ? "module" : "script";
        return {
          type: "Program", start, end,
          sourceType: st,
          hashbang: (flags & 2) ? str(f2, f3) : null,
          body: nodeArr(f1, f0),
        };
      }

      // identifiers
      case 42: case 44: case 45: case 46: // identifier_reference, binding_identifier, identifier_name, label_identifier
        return { type: "Identifier", start, end, name: str(f1, f2) };

      case 43: // private_identifier
        return { type: "PrivateIdentifier", start, end, name: str(f1, f2) };

      // literals
      case 33: // string_literal
        return { type: "Literal", start, end, value: str(f1, f2), raw: _source.slice(start, end) };

      case 34: { // numeric_literal
        const raw = _source.slice(start, end);
        const v = +raw;
        return { type: "Literal", start, end, value: v === v && isFinite(v) ? v : null, raw };
      }

      case 35: { // bigint_literal
        const raw = _source.slice(start, end);
        return { type: "Literal", start, end, value: `(BigInt) ${raw}`, raw, bigint: str(f1, f2).replace(/_/g, "") };
      }

      case 36: // boolean_literal
        return { type: "Literal", start, end, value: !!(flags & 1), raw: (flags & 1) ? "true" : "false" };

      case 37: // null_literal
        return { type: "Literal", start, end, value: null, raw: "null" };

      case 39: { // regexp_literal
        const p = str(f1, f2), fl = str(f3, f4);
        return { type: "Literal", start, end, value: `(RegExp) /${p}/${fl}`, raw: `/${p}/${fl}`, regex: { pattern: p, flags: fl.split("").sort().join("") } };
      }

      case 38: // this_expression
        return { type: "ThisExpression", start, end };

      case 32: // super
        return { type: "Super", start, end };

      // expressions
      case 0: // sequence_expression
        return { type: "SequenceExpression", start, end, expressions: nodeArr(f1, f0) };

      case 1: // parenthesized_expression
        return node(f1);

      case 2: { // arrow_function_expression
        const prm = f1 !== NULL ? fnParams(f1) : [];
        return { type: "ArrowFunctionExpression", start, end, id: null, generator: false, async: !!(flags & 2), params: prm, body: node(f2), expression: !!(flags & 1) };
      }

      case 8: // binary_expression
        return { type: "BinaryExpression", start, end, left: node(f1), operator: BINARY_OPS[flags], right: node(f2) };

      case 9: // logical_expression
        return { type: "LogicalExpression", start, end, left: node(f1), operator: LOGICAL_OPS[flags], right: node(f2) };

      case 10: // conditional_expression
        return { type: "ConditionalExpression", start, end, test: node(f1), consequent: node(f2), alternate: node(f3) };

      case 11: // unary_expression
        return { type: "UnaryExpression", start, end, operator: UNARY_OPS[flags], prefix: true, argument: node(f1) };

      case 12: // update_expression
        return { type: "UpdateExpression", start, end, operator: UPDATE_OPS[flags & 3], prefix: !!(flags & 4), argument: node(f1) };

      case 13: // assignment_expression
        return { type: "AssignmentExpression", start, end, operator: ASSIGNMENT_OPS[flags], left: node(f1), right: node(f2) };

      case 14: // array_expression
        return { type: "ArrayExpression", start, end, elements: nodeArrHoles(f1, f0) };

      case 15: // object_expression
        return { type: "ObjectExpression", start, end, properties: nodeArr(f1, f0) };

      case 16: // spread_element
        return { type: "SpreadElement", start, end, argument: node(f1) };

      case 17: // object_property
        return { type: "Property", start, end, kind: PROPERTY_KINDS[flags & 3], key: node(f1), value: node(f2), method: !!(flags & 4), shorthand: !!(flags & 8), computed: !!(flags & 16) };

      case 18: // member_expression
        return { type: "MemberExpression", start, end, object: node(f1), property: node(f2), computed: !!(flags & 1), optional: !!(flags & 2) };

      case 19: // call_expression
        return { type: "CallExpression", start, end, callee: node(f1), arguments: nodeArr(f2, f0), optional: !!(flags & 1) };

      case 20: // chain_expression
        return { type: "ChainExpression", start, end, expression: node(f1) };

      case 21: // tagged_template_expression
        return { type: "TaggedTemplateExpression", start, end, tag: node(f1), quasi: node(f2) };

      case 22: // new_expression
        return { type: "NewExpression", start, end, callee: node(f1), arguments: nodeArr(f2, f0) };

      case 23: // await_expression
        return { type: "AwaitExpression", start, end, argument: node(f1) };

      case 24: // yield_expression
        return { type: "YieldExpression", start, end, delegate: !!(flags & 1), argument: f1 !== NULL ? node(f1) : null };

      case 25: // meta_property
        return { type: "MetaProperty", start, end, meta: node(f1), property: node(f2) };

      case 40: // template_literal
        return { type: "TemplateLiteral", start, end, quasis: nodeArr(f1, f0), expressions: nodeArr(f2, f3) };

      case 41: { // template_element
        const raw = normalizeLineEndings(_source.slice(start, end));
        return { type: "TemplateElement", start, end, value: { raw, cooked: (flags & 2) ? null : str(f1, f2) }, tail: !!(flags & 1) };
      }

      // functions
      case 3: { // function
        const ft = flags & 3;
        const prm = f2 !== NULL ? fnParams(f2) : [];
        const r = { type: FUNCTION_TYPES[ft], start, end, id: f1 !== NULL ? node(f1) : null, generator: !!(flags & 4), async: !!(flags & 8), params: prm, body: f3 !== NULL ? node(f3) : null, expression: false };
        if (ft === 2) r.declare = true;
        return r;
      }

      case 4: // function_body
        return { type: "BlockStatement", start, end, body: nodeArr(f1, f0) };

      case 5: // block_statement
        return { type: "BlockStatement", start, end, body: nodeArr(f1, f0) };

      case 6: // formal_parameters (direct access)
        return { params: fnParams(i) };

      case 7: // formal_parameter (unwrap to pattern)
        return node(f1);

      // statements
      case 47: // expression_statement
        return { type: "ExpressionStatement", start, end, expression: node(f1) };

      case 68: // directive
        return { type: "ExpressionStatement", start, end, expression: node(f1), directive: str(f2, f3) };

      case 48: // if_statement
        return { type: "IfStatement", start, end, test: node(f1), consequent: node(f2), alternate: f3 !== NULL ? node(f3) : null };

      case 49: // switch_statement
        return { type: "SwitchStatement", start, end, discriminant: node(f1), cases: nodeArr(f2, f0) };

      case 50: // switch_case
        return { type: "SwitchCase", start, end, test: f1 !== NULL ? node(f1) : null, consequent: nodeArr(f2, f0) };

      case 51: // for_statement
        return { type: "ForStatement", start, end, init: f1 !== NULL ? node(f1) : null, test: f2 !== NULL ? node(f2) : null, update: f3 !== NULL ? node(f3) : null, body: node(f4) };

      case 52: // for_in_statement
        return { type: "ForInStatement", start, end, left: node(f1), right: node(f2), body: node(f3) };

      case 53: // for_of_statement
        return { type: "ForOfStatement", start, end, left: node(f1), right: node(f2), body: node(f3), await: !!(flags & 1) };

      case 54: // while_statement
        return { type: "WhileStatement", start, end, test: node(f1), body: node(f2) };

      case 55: // do_while_statement
        return { type: "DoWhileStatement", start, end, body: node(f1), test: node(f2) };

      case 56: // break_statement
        return { type: "BreakStatement", start, end, label: f1 !== NULL ? node(f1) : null };

      case 57: // continue_statement
        return { type: "ContinueStatement", start, end, label: f1 !== NULL ? node(f1) : null };

      case 58: // labeled_statement
        return { type: "LabeledStatement", start, end, label: node(f1), body: node(f2) };

      case 59: // with_statement
        return { type: "WithStatement", start, end, object: node(f1), body: node(f2) };

      case 60: // return_statement
        return { type: "ReturnStatement", start, end, argument: f1 !== NULL ? node(f1) : null };

      case 61: // throw_statement
        return { type: "ThrowStatement", start, end, argument: node(f1) };

      case 62: // try_statement
        return { type: "TryStatement", start, end, block: node(f1), handler: f2 !== NULL ? node(f2) : null, finalizer: f3 !== NULL ? node(f3) : null };

      case 63: // catch_clause
        return { type: "CatchClause", start, end, param: f1 !== NULL ? node(f1) : null, body: node(f2) };

      case 64: // debugger_statement
        return { type: "DebuggerStatement", start, end };

      case 65: // empty_statement
        return { type: "EmptyStatement", start, end };

      // declarations
      case 66: // variable_declaration
        return { type: "VariableDeclaration", start, end, kind: VAR_KINDS[flags], declarations: nodeArr(f1, f0) };

      case 67: // variable_declarator
        return { type: "VariableDeclarator", start, end, id: node(f1), init: f2 !== NULL ? node(f2) : null };

      // patterns
      case 69: // assignment_pattern
        return { type: "AssignmentPattern", start, end, left: node(f1), right: node(f2) };

      case 70: // binding_rest_element
        return { type: "RestElement", start, end, argument: node(f1) };

      case 71: { // array_pattern
        const el = nodeArrHoles(f1, f0);
        if (f2 !== NULL) el.push(node(f2));
        return { type: "ArrayPattern", start, end, elements: el };
      }

      case 72: { // object_pattern
        const pr = nodeArr(f1, f0);
        if (f2 !== NULL) pr.push(node(f2));
        return { type: "ObjectPattern", start, end, properties: pr };
      }

      case 73: // binding_property
        return { type: "Property", start, end, kind: "init", key: node(f1), value: node(f2), method: false, shorthand: !!(flags & 1), computed: !!(flags & 2) };

      // class
      case 27: // class
        return { type: CLASS_TYPES[flags & 1], start, end, decorators: nodeArr(f1, f0), id: f2 !== NULL ? node(f2) : null, superClass: f3 !== NULL ? node(f3) : null, body: node(f4) };

      case 28: // class_body
        return { type: "ClassBody", start, end, body: nodeArr(f1, f0) };

      case 29: // method_definition
        return { type: "MethodDefinition", start, end, decorators: nodeArr(f1, f0), key: node(f2), value: node(f3), kind: METHOD_KINDS[flags & 3], computed: !!(flags & 4), static: !!(flags & 8) };

      case 30: { // property_definition
        const acc = !!(flags & 4);
        return { type: acc ? "AccessorProperty" : "PropertyDefinition", start, end, decorators: nodeArr(f1, f0), key: node(f2), value: f3 !== NULL ? node(f3) : null, computed: !!(flags & 1), static: !!(flags & 2) };
      }

      case 31: // static_block
        return { type: "StaticBlock", start, end, body: nodeArr(f1, f0) };

      case 26: // decorator
        return { type: "Decorator", start, end, expression: node(f1) };

      // module
      case 75: { // import_expression
        const hp = flags & 1;
        return { type: "ImportExpression", start, end, source: node(f1), options: f2 !== NULL ? node(f2) : null, phase: hp ? IMPORT_PHASES[(flags >> 1) + 1] : null };
      }

      case 76: { // import_declaration
        const hp = flags & 1;
        return { type: "ImportDeclaration", start, end, specifiers: nodeArr(f1, f0), source: node(f2), phase: hp ? IMPORT_PHASES[(flags >> 1) + 1] : null, attributes: nodeArr(f3, f4) };
      }

      case 77: // import_specifier
        return { type: "ImportSpecifier", start, end, imported: node(f1), local: node(f2) };

      case 78: // import_default_specifier
        return { type: "ImportDefaultSpecifier", start, end, local: node(f1) };

      case 79: // import_namespace_specifier
        return { type: "ImportNamespaceSpecifier", start, end, local: node(f1) };

      case 80: // import_attribute
        return { type: "ImportAttribute", start, end, key: node(f1), value: node(f2) };

      case 81: // export_named_declaration
        return { type: "ExportNamedDeclaration", start, end, declaration: f1 !== NULL ? node(f1) : null, specifiers: nodeArr(f2, f0), source: f3 !== NULL ? node(f3) : null, attributes: nodeArr(f4, f5) };

      case 82: // export_default_declaration
        return { type: "ExportDefaultDeclaration", start, end, declaration: node(f1) };

      case 83: // export_all_declaration
        return { type: "ExportAllDeclaration", start, end, exported: f1 !== NULL ? node(f1) : null, source: node(f2), attributes: nodeArr(f3, f0) };

      case 84: // export_specifier
        return { type: "ExportSpecifier", start, end, local: node(f1), exported: node(f2) };

      case 85: // ts_export_assignment
        return { type: "TSExportAssignment", start, end, expression: node(f1) };

      case 86: // ts_namespace_export_declaration
        return { type: "TSNamespaceExportDeclaration", start, end, id: node(f1) };

      // jsx
      case 87: // jsx_element
        return { type: "JSXElement", start, end, openingElement: node(f1), children: nodeArr(f2, f0), closingElement: f3 !== NULL ? node(f3) : null };

      case 88: // jsx_opening_element
        return { type: "JSXOpeningElement", start, end, name: node(f1), attributes: nodeArr(f2, f0), selfClosing: !!(flags & 1) };

      case 89: // jsx_closing_element
        return { type: "JSXClosingElement", start, end, name: node(f1) };

      case 90: // jsx_fragment
        return { type: "JSXFragment", start, end, openingFragment: node(f1), children: nodeArr(f2, f0), closingFragment: node(f3) };

      case 91: // jsx_opening_fragment
        return { type: "JSXOpeningFragment", start, end, attributes: [], selfClosing: false };

      case 92: // jsx_closing_fragment
        return { type: "JSXClosingFragment", start, end };

      case 93: // jsx_identifier
        return { type: "JSXIdentifier", start, end, name: str(f1, f2) };

      case 94: // jsx_namespaced_name
        return { type: "JSXNamespacedName", start, end, namespace: node(f1), name: node(f2) };

      case 95: // jsx_member_expression
        return { type: "JSXMemberExpression", start, end, object: node(f1), property: node(f2) };

      case 96: // jsx_attribute
        return { type: "JSXAttribute", start, end, name: node(f1), value: f2 !== NULL ? node(f2) : null };

      case 97: // jsx_spread_attribute
        return { type: "JSXSpreadAttribute", start, end, argument: node(f1) };

      case 98: // jsx_expression_container
        return { type: "JSXExpressionContainer", start, end, expression: node(f1) };

      case 99: // jsx_empty_expression
        return { type: "JSXEmptyExpression", start, end };

      case 100: { // jsx_text
        const t = str(f1, f2);
        return { type: "JSXText", start, end, value: t, raw: t };
      }

      case 101: // jsx_spread_child
        return { type: "JSXSpreadChild", start, end, expression: node(f1) };
    }
  }

  function nodeArr(s, len) {
    const r = new Array(len);
    for (let j = 0; j < len; j++) r[j] = node(_u32[_extraBase + s + j]);
    return r;
  }

  function nodeArrHoles(s, len) {
    const r = new Array(len);
    for (let j = 0; j < len; j++) {
      const idx = _u32[_extraBase + s + j];
      r[j] = idx !== NULL ? node(idx) : null;
    }
    return r;
  }

  function fnParams(paramIdx) {
    const po = _nodesOff + paramIdx * NODE_SIZE;
    const itemsLen = _u8[po + 2] | (_u8[po + 3] << 8);
    const pb = po >> 2;
    const itemsStart = _u32[pb + 1];
    const restIdx = _u32[pb + 2];
    const params = new Array(itemsLen + (restIdx !== NULL ? 1 : 0));
    for (let j = 0; j < itemsLen; j++) {
      const fpIdx = _u32[_extraBase + itemsStart + j];
      // unwrap formal_parameter -> pattern
      const fpOff = _nodesOff + fpIdx * NODE_SIZE;
      params[j] = node(_u32[(fpOff >> 2) + 1]);
    }
    if (restIdx !== NULL) params[itemsLen] = node(restIdx);
    return params;
  }

  // comments
  const comments = new Array(commentCount);
  for (let j = 0; j < commentCount; j++) {
    const off = commentsOffset + j * COMMENT_SIZE;
    comments[j] = {
      type: COMMENT_TYPES[_u8[off]],
      value: str(_dv.getUint32(off + 12, true), _dv.getUint32(off + 16, true)),
      start: _dv.getUint32(off + 4, true),
      end: _dv.getUint32(off + 8, true),
    };
  }

  // diagnostics
  const diagnostics = new Array(diagCount);
  let doff = diagsOffset;
  for (let j = 0; j < diagCount; j++) {
    const severity = SEVERITY_NAMES[_u8[doff]]; doff += 1;
    const ds = _dv.getUint32(doff, true); doff += 4;
    const de = _dv.getUint32(doff, true); doff += 4;
    const ml = _dv.getUint32(doff, true); doff += 4;
    const message = textDecoder.decode(_u8.subarray(doff, doff + ml)); doff += ml;
    const hh = _u8[doff]; doff += 1;
    let help = null;
    if (hh) {
      const hl = _dv.getUint32(doff, true); doff += 4;
      help = textDecoder.decode(_u8.subarray(doff, doff + hl)); doff += hl;
    }
    const lc = _dv.getUint32(doff, true); doff += 4;
    const labels = new Array(lc);
    for (let k = 0; k < lc; k++) {
      const ls = _dv.getUint32(doff, true); doff += 4;
      const le = _dv.getUint32(doff, true); doff += 4;
      const lml = _dv.getUint32(doff, true); doff += 4;
      labels[k] = { start: ls, end: le, message: textDecoder.decode(_u8.subarray(doff, doff + lml)) };
      doff += lml;
    }
    diagnostics[j] = { severity, message, start: ds, end: de, help, labels };
  }

  const program = node(programIndex);

  // release refs
  _u8 = _u32 = _dv = _source = null;

  return { program, comments, diagnostics };
}

const textDecoder = new TextDecoder("utf-8");

function normalizeLineEndings(s) {
  if (s.indexOf("\r") === -1) return s;
  return s.replace(/\r\n?/g, "\n");
}

module.exports = { decode };
