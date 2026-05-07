const codegen = @import("parser").codegen;

/// Top-level minifier options.
pub const Options = struct {
    /// Identifier renaming. Set `mangle = .{ .enabled = false }` to disable.
    mangle: MangleOptions = .{},
    /// Output format. Defaults to compact.
    format: FormatOptions = .{},
};

/// Identifier mangling configuration.
pub const MangleOptions = struct {
    /// Enable identifier renaming.
    enabled: bool = true,

    /// Rename top-level (module-scope) bindings too. Off by default in
    /// script source type because globals may be referenced from outside
    /// the file. On by default in module source type.
    ///
    /// `null` means: infer from the tree's source type.
    toplevel: ?bool = null,

    /// Skip renaming `function foo()` declarations and named function
    /// expressions. Production code often relies on `Function.prototype.name`.
    keep_fnames: bool = false,

    /// Skip renaming `class Foo {}` declarations and named class expressions.
    /// Many runtimes use `obj.constructor.name` for `instanceof`-style
    /// dispatch, toggling this off can break those checks.
    keep_classnames: bool = false,

    /// Names that must never be used as a mangled output name. Useful when
    /// the host page exposes globals you want left untouched.
    reserved: []const []const u8 = &.{},
};

/// Output format options.
pub const FormatOptions = struct {
    format: codegen.Format = .compact,
    quotes: codegen.Quotes = .double,
    final_newline: bool = false,
};
