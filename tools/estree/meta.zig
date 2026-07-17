// shared metadata for the decoder and encoder generators, one source so they agree

const std = @import("std");
const parser = @import("parser");
const ast = parser.ast;

pub const BINARY_OPS = [_][]const u8{
    "==", "!=",         "===", "!==", "<", "<=", ">", ">=", "+",  "-",
    "*",  "/",          "%",   "**",  "|", "^",  "&", "<<", ">>", ">>>",
    "in", "instanceof",
};
pub const LOGICAL_OPS = [_][]const u8{ "&&", "||", "??" };
pub const UNARY_OPS = [_][]const u8{ "-", "+", "!", "~", "typeof", "void", "delete" };
pub const UPDATE_OPS = [_][]const u8{ "++", "--" };
pub const ASSIGNMENT_OPS = [_][]const u8{
    "=",   "+=",   "-=", "*=", "/=", "%=",  "**=", "<<=",
    ">>=", ">>>=", "|=", "^=", "&=", "||=", "&&=", "??=",
};
pub const VAR_KINDS = [_][]const u8{ "var", "let", "const", "using", "await using" };
pub const PROPERTY_KINDS = [_][]const u8{ "init", "get", "set" };
pub const METHOD_KINDS = [_][]const u8{ "constructor", "method", "get", "set" };
pub const FUNCTION_TYPES = [_][]const u8{
    "FunctionDeclaration",
    "FunctionExpression",
    "TSDeclareFunction",
    "TSEmptyBodyFunctionExpression",
};
pub const CLASS_TYPES = [_][]const u8{ "ClassDeclaration", "ClassExpression" };
pub const COMMENT_TYPES = [_][]const u8{ "Line", "Block" };
pub const SEVERITY = [_][]const u8{ "error", "warning", "hint", "info" };

// tables with non-string elements, decoder writes them raw and the encoder inverts
pub const IMPORT_EXPORT_KINDS_RAW = [_][]const u8{ "\"value\"", "\"type\"" };
pub const ACCESSIBILITY_RAW = [_][]const u8{ "null", "\"public\"", "\"private\"", "\"protected\"" };
pub const TS_TYPE_OPERATORS_RAW = [_][]const u8{ "\"keyof\"", "\"unique\"", "\"readonly\"" };
pub const TS_METHOD_SIGNATURE_KINDS_RAW = [_][]const u8{ "\"method\"", "\"get\"", "\"set\"" };
pub const TS_MODULE_KINDS_RAW = [_][]const u8{ "\"namespace\"", "\"module\"" };
pub const TS_MAPPED_OPTIONAL_RAW = [_][]const u8{ "false", "true", "\"+\"", "\"-\"" };
pub const TS_MAPPED_READONLY_RAW = [_][]const u8{ "null", "true", "\"+\"", "\"-\"" };

pub fn enumTableName(comptime E: type) []const u8 {
    if (E == ast.BinaryOperator) return "BINARY_OPS";
    if (E == ast.LogicalOperator) return "LOGICAL_OPS";
    if (E == ast.UnaryOperator) return "UNARY_OPS";
    if (E == ast.UpdateOperator) return "UPDATE_OPS";
    if (E == ast.AssignmentOperator) return "ASSIGNMENT_OPS";
    if (E == ast.VariableKind) return "VAR_KINDS";
    if (E == ast.PropertyKind) return "PROPERTY_KINDS";
    if (E == ast.MethodDefinitionKind) return "METHOD_KINDS";
    if (E == ast.FunctionType) return "FUNCTION_TYPES";
    if (E == ast.ClassType) return "CLASS_TYPES";
    if (E == ast.ImportOrExportKind) return "IMPORT_EXPORT_KINDS";
    if (E == ast.Accessibility) return "ACCESSIBILITY";
    if (E == ast.TSTypeOperatorKind) return "TS_TYPE_OPERATORS";
    if (E == ast.TSMethodSignatureKind) return "TS_METHOD_SIGNATURE_KINDS";
    if (E == ast.TSModuleDeclarationKind) return "TS_MODULE_KINDS";
    @compileError("no lookup table for enum: " ++ @typeName(E));
}

// true when an enum's inverse must be a JS function rather than an object map
pub fn enumNeedsInverseFn(comptime E: type) bool {
    return E == ast.Accessibility or E == ast.TSMappedTypeModifier;
}

// ESTree name overrides where snake to pascal is wrong (ts_jsdoc_ to TSJSDoc)
const NAME_OVERRIDES = [_]struct { z: []const u8, e: []const u8 }{
    .{ .z = "function_body", .e = "BlockStatement" },
    .{ .z = "binding_rest_element", .e = "RestElement" },
    .{ .z = "object_property", .e = "Property" },
    .{ .z = "identifier_reference", .e = "Identifier" },
    .{ .z = "binding_identifier", .e = "Identifier" },
    .{ .z = "identifier_name", .e = "Identifier" },
    .{ .z = "label_identifier", .e = "Identifier" },
    .{ .z = "ts_bigint_keyword", .e = "TSBigIntKeyword" },
    .{ .z = "ts_jsdoc_nullable_type", .e = "TSJSDocNullableType" },
    .{ .z = "ts_jsdoc_non_nullable_type", .e = "TSJSDocNonNullableType" },
    .{ .z = "ts_jsdoc_unknown_type", .e = "TSJSDocUnknownType" },
};

pub fn estreeType(comptime name: []const u8) []const u8 {
    inline for (NAME_OVERRIDES) |o| if (comptime std.mem.eql(u8, name, o.z)) return o.e;
    if (comptime std.mem.startsWith(u8, name, "jsx_")) {
        return "JSX" ++ snakeConvert(name[4..], true);
    }
    if (comptime std.mem.startsWith(u8, name, "ts_")) return "TS" ++ snakeConvert(name[3..], true);
    return snakeConvert(name, true);
}

pub fn estreeField(comptime tag: []const u8, comptime field: []const u8) []const u8 {
    if (comptime std.mem.eql(u8, tag, "variable_declaration") and
        std.mem.eql(u8, field, "declarators")) return "declarations";
    // const is a zig keyword so the field is is_const, ESTree renders it as const
    if (comptime std.mem.eql(u8, tag, "ts_enum_declaration") and
        std.mem.eql(u8, field, "is_const")) return "const";
    return snakeConvert(field, false);
}

// arrays that allow holes, sparse elements become null in ESTree
pub fn isHoleyArray(comptime tag: []const u8, comptime field: []const u8) bool {
    return std.mem.eql(u8, tag, "array_expression") and std.mem.eql(u8, field, "elements");
}

pub fn snakeConvert(comptime name: []const u8, comptime pascal: bool) []const u8 {
    comptime {
        @setEvalBranchQuota(200_000);
        var result: [name.len]u8 = undefined;
        var len: usize = 0;
        var cap = pascal;
        for (name) |c| {
            if (c == '_') {
                cap = true;
            } else {
                result[len] = if (cap) std.ascii.toUpper(c) else c;
                cap = false;
                len += 1;
            }
        }
        const final = result[0..len].*;
        return &final;
    }
}

// identifier dispatch role, ESTree has one Identifier but yuku has five variants
pub const Role = enum {
    // ESTree Identifier becomes identifier_reference
    auto,
    // binding position becomes binding_identifier, carries decorators optional type in TS
    binding,
    // label slot for break, continue, or a labeled statement
    label,
    // non-binding name slot becomes identifier_name, else the minifier corrupts these names
    name,
};

const ROLES = [_]struct { node: []const u8, field: []const u8, role: Role }{
    .{ .node = "variable_declarator", .field = "id", .role = .binding },
    .{ .node = "assignment_pattern", .field = "left", .role = .binding },
    .{ .node = "binding_rest_element", .field = "argument", .role = .binding },
    .{ .node = "binding_property", .field = "value", .role = .binding },
    .{ .node = "catch_clause", .field = "param", .role = .binding },
    .{ .node = "import_specifier", .field = "local", .role = .binding },
    .{ .node = "import_default_specifier", .field = "local", .role = .binding },
    .{ .node = "import_namespace_specifier", .field = "local", .role = .binding },
    .{ .node = "ts_type_parameter", .field = "name", .role = .binding },
    .{ .node = "ts_type_alias_declaration", .field = "id", .role = .binding },
    .{ .node = "ts_interface_declaration", .field = "id", .role = .binding },
    .{ .node = "ts_enum_declaration", .field = "id", .role = .binding },
    .{ .node = "ts_import_equals_declaration", .field = "id", .role = .binding },
    .{ .node = "ts_parameter_property", .field = "parameter", .role = .binding },
    .{ .node = "break_statement", .field = "label", .role = .label },
    .{ .node = "continue_statement", .field = "label", .role = .label },
    .{ .node = "labeled_statement", .field = "label", .role = .label },
    .{ .node = "import_specifier", .field = "imported", .role = .name },
    .{ .node = "import_attribute", .field = "key", .role = .name },
    .{ .node = "export_all_declaration", .field = "exported", .role = .name },
    .{ .node = "export_specifier", .field = "exported", .role = .name },
    .{ .node = "ts_qualified_name", .field = "right", .role = .name },
    .{ .node = "ts_import_type", .field = "qualifier", .role = .name },
};

pub fn fieldRole(comptime tag: []const u8, comptime field: []const u8) Role {
    inline for (ROLES) |r| {
        if (comptime std.mem.eql(u8, r.node, tag) and
            std.mem.eql(u8, r.field, field)) return r.role;
    }
    return .auto;
}

// encoder special-case set, nodes the generic struct to object mapping can't express
const SPECIAL = [_][]const u8{
    "formal_parameter",              "formal_parameters",                  "function",
    "arrow_function_expression",     "program",                            "directive",
    "string_literal",                "numeric_literal",                    "bigint_literal",
    "boolean_literal",               "null_literal",                       "regexp_literal",
    "template_element",              "class",                              "method_definition",
    "property_definition",           "unary_expression",                   "binding_property",
    "array_pattern",                 "object_pattern",                     "jsx_text",
    "ts_function_type",              "ts_constructor_type",                "ts_method_signature",
    "ts_call_signature_declaration", "ts_construct_signature_declaration", "ts_mapped_type",
    "ts_module_declaration",         "ts_global_declaration",              "ts_this_parameter",
    "member_expression",             "object_property",                    "ts_property_signature",
    "ts_enum_member",                "ts_index_signature",
};

pub fn isSpecial(comptime name: []const u8) bool {
    comptime {
        @setEvalBranchQuota(200_000);
        for (SPECIAL) |s| if (s.len == name.len and std.mem.eql(u8, s, name)) return true;
        return false;
    }
}
