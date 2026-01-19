const ast = @import("../ast.zig");
const Parser = @import("../parser.zig").Parser;
const Error = @import("../parser.zig").Error;

pub fn parseJsxElement(parser: *Parser) Error!?ast.NodeIndex {
    _ = try parseJsxOpeningElement(parser) orelse return null;

    return null;
}

pub fn parseJsxOpeningElement(parser: *Parser) Error!?ast.NodeIndex {
    parser.setLexerMode(.jsx_identifier);

    try parser.advance() orelse return null; // consume '<'

    if(parser.current_token.type == .jsx_identifier) {}

    return null;
}
