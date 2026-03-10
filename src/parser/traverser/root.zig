/// Traverser layers, from simplest to most capable:
///   basic:   path tracking only
///   scoped:  path + JavaScript lexical scopes
///   symbols: path + scopes + symbol/reference collection
pub const basic = @import("basic.zig");
pub const scoped = @import("scoped.zig");
pub const symbols = @import("symbols.zig");

pub const walk = @import("walk.zig").walk;
pub const Layer = @import("walk.zig").Layer;
pub const dispatch = @import("walk.zig").dispatch;
pub const Action = @import("walk.zig").Action;
pub const NodePath = @import("walk.zig").NodePath;
