const ast = @import("../../ast.zig");
const parser_module = @import("../../parser.zig");
const Parser = parser_module.Parser;
const ParseExpressionOptions = @import("../expressions.zig").ParseExpressionOptions;

pub const Speculation = packed struct {
    rule: Rule,
    position: u32,
    context: parser_module.Context,
    ts_context: parser_module.TsContext,
    options: ParseExpressionOptions,

    pub const Rule = enum(u2) { arrow, type_arguments, call_type_arguments, infer_constraint };

    pub fn at(parser: *const Parser, rule: Rule, options: ParseExpressionOptions) Speculation {
        return .{
            .rule = rule,
            .position = parser.current_token.span.start,
            .context = parser.context,
            .ts_context = parser.ts_context,
            .options = options,
        };
    }

    pub fn isRejected(self: Speculation, parser: *const Parser) bool {
        return parser.ts_rejected_speculations.contains(self);
    }

    pub fn reject(self: Speculation, parser: *Parser) void {
        parser.ts_rejected_speculations.putNoClobber(parser.allocator(), self, {}) catch
            ast.outOfMemory();
    }
};
