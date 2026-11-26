const std = @import("std");
const ast = @import("../ast.zig");
const static_semantics = @import("spec").static_semantics;
const sc = @import("scope.zig");
const lexical = @import("../lexical.zig");

const Allocator = std.mem.Allocator;
const String = ast.String;
const NodeIndex = ast.NodeIndex;
const ScopeId = sc.ScopeId;

/// Identifier for a `Symbol`. `.none` means absent.
pub const SymbolId = enum(u32) { none = std.math.maxInt(u32), _ };

/// Identifier for a `Reference`. `.none` means absent.
pub const ReferenceId = enum(u32) { none = std.math.maxInt(u32), _ };

const NodeLink = packed struct(u32) {
    id: u31,
    reference: bool,

    const none: NodeLink = .{ .id = std.math.maxInt(u31), .reference = true };
};

const ScopeMap = struct {
    slots: [*]Slot = @constCast(&no_slots),
    mask: u32 = 0,
    len: u32 = 0,

    const empty: ScopeMap = .{};

    const Slot = struct {
        name: [*]const u8,
        hash: u32,
        id: SymbolId,

        const free: Slot = .{ .name = "", .hash = 0, .id = .none };
    };

    const no_slots = [1]Slot{.free};

    const capacity_min = 4;

    inline fn get(
        self: *const ScopeMap,
        symbols: []const Symbol,
        name: []const u8,
        hash: NameHash,
    ) ?SymbolId {
        const index = self.find(symbols, name, hash) orelse return null;
        return self.slots[index].id;
    }

    inline fn find(
        self: *const ScopeMap,
        symbols: []const Symbol,
        name: []const u8,
        hash: NameHash,
    ) ?u32 {
        const tag = hash.tag();
        var index = tag & self.mask;
        while (true) : (index = (index + 1) & self.mask) {
            const slot = self.slots[index];
            if (slot.id == .none) return null;
            if (slot.hash != tag) continue;
            const bound = symbols[@backingInt(slot.id)].name;
            if (bound.end - bound.start != name.len) continue;
            if (nameEql(slot.name[0..name.len], name)) return index;
        }
    }

    fn putNew(
        self: *ScopeMap,
        allocator: Allocator,
        name: []const u8,
        hash: NameHash,
        id: SymbolId,
    ) Allocator.Error!void {
        std.debug.assert(id != .none);
        if (self.len == (self.mask + 1) / 4 * 3) try self.grow(allocator);
        const tag = hash.tag();
        var index = tag & self.mask;
        while (self.slots[index].id != .none) index = (index + 1) & self.mask;
        self.slots[index] = .{ .name = name.ptr, .hash = tag, .id = id };
        self.len += 1;
    }

    noinline fn grow(self: *ScopeMap, allocator: Allocator) Allocator.Error!void {
        const old = self.slots[0 .. self.mask + 1];
        std.debug.assert(old.len < std.math.maxInt(u32) / 2);
        const capacity: u32 = @max(capacity_min, (self.mask + 1) * 2);
        const slots = try allocator.alloc(Slot, capacity);
        @memset(slots, Slot.free);
        const mask = capacity - 1;
        for (old) |slot| {
            if (slot.id == .none) continue;
            var index = slot.hash & mask;
            while (slots[index].id != .none) index = (index + 1) & mask;
            slots[index] = slot;
        }
        if (self.mask != 0) allocator.free(old);
        self.slots = slots.ptr;
        self.mask = mask;
    }

    fn removeAt(self: *ScopeMap, index: u32) void {
        std.debug.assert(index <= self.mask);
        std.debug.assert(self.slots[index].id != .none);
        var hole = index;
        var next = (hole + 1) & self.mask;
        while (self.slots[next].id != .none) : (next = (next + 1) & self.mask) {
            const home = self.slots[next].hash & self.mask;
            if ((next -% home) & self.mask >= (next -% hole) & self.mask) {
                self.slots[hole] = self.slots[next];
                hole = next;
            }
        }
        self.slots[hole].id = .none;
        self.len -= 1;
    }

    inline fn iterator(self: *const ScopeMap) Iterator {
        return .{ .slots = self.slots[0 .. self.mask + 1] };
    }

    /// Yields the symbols declared directly in a scope.
    const Iterator = struct {
        slots: []const Slot,
        index: usize = 0,

        /// The next symbol id, or `null` when done.
        pub fn next(self: *Iterator) ?SymbolId {
            while (self.index < self.slots.len) {
                const id = self.slots[self.index].id;
                self.index += 1;
                if (id != .none) return id;
            }
            return null;
        }
    };
};

const Range = struct {
    start: u32 = 0,
    len: u32 = 0,

    fn layout(ranges: []Range) u32 {
        var total: u32 = 0;
        for (ranges) |*range| {
            const len = range.len;
            range.* = .{ .start = total };
            total += len;
        }
        return total;
    }

    inline fn push(self: *Range) u32 {
        defer self.len += 1;
        return self.start + self.len;
    }
};

/// A declared binding, shared by every declaration that legally merges under its name.
pub const Symbol = struct {
    name: String,
    flags: Flags,
    /// The scope the symbol lands in, the hoist target for a hoisting `var`.
    scope: sc.ScopeId,

    pub const Flags = packed struct(u32) {
        function_scoped_var: bool = false,
        block_scoped_var: bool = false,
        function: bool = false,
        class: bool = false,
        regular_enum: bool = false,
        const_enum: bool = false,
        value_module: bool = false,
        interface: bool = false,
        type_alias: bool = false,
        type_parameter: bool = false,
        namespace_module: bool = false,
        import: bool = false,
        type_import: bool = false,
        const_var: bool = false,
        ambient: bool = false,
        parameter: bool = false,
        catch_var: bool = false,
        exported: bool = false,
        is_default: bool = false,
        enum_member: bool = false,
        _: u12 = 0,

        /// Whether `a` and `b` share a flag.
        pub inline fn intersects(a: Flags, b: Flags) bool {
            return @backingInt(a) & @backingInt(b) != 0;
        }

        /// The union of two flag sets.
        pub inline fn merge(a: Flags, b: Flags) Flags {
            return @fromBackingInt(@backingInt(a) | @backingInt(b));
        }

        fn without(a: Flags, b: Flags) Flags {
            return @fromBackingInt(@backingInt(a) & ~@backingInt(b));
        }

        /// Whether this is a `var` that hoists, not a parameter or catch variable.
        pub inline fn isHoistingVar(self: Flags) bool {
            return self.function_scoped_var and !self.parameter and !self.catch_var;
        }

        /// Whether the binding is visible in `space`, as an import always is.
        pub inline fn visibleIn(self: Flags, space: Reference.Space) bool {
            return self.intersects(any_import.merge(switch (space) {
                .value, .typeof => value_space,
                .type => type_space,
                .namespace => namespace_space,
                .any => return true,
            }));
        }

        /// Whether a hoisting `var` cannot pass through the binding.
        pub inline fn isBlockScopedLike(self: Flags) bool {
            return self.block_scoped_var or self.class or self.function;
        }

        /// The declaration kind as diagnostics name it.
        pub fn toString(self: Flags) []const u8 {
            if (self.function) return "function";
            if (self.class) return "class";
            if (self.regular_enum or self.const_enum) return "enum";
            if (self.value_module or self.namespace_module) return "namespace";
            if (self.interface) return "interface";
            if (self.type_alias) return "type alias";
            if (self.type_import) return "type import";
            if (self.import) return "import";
            if (self.parameter) return "parameter";
            if (self.catch_var) return "catch parameter";
            if (self.type_parameter) return "type parameter";
            if (self.enum_member) return "enum member";
            return "variable";
        }
    };

    // mirrored into the JS decoder's BindingFlags

    /// Any variable, parameter, or catch binding.
    pub const variable: Flags = .{ .function_scoped_var = true, .block_scoped_var = true };

    /// Any value or type-only import binding.
    pub const any_import: Flags = .{ .import = true, .type_import = true };

    /// Declarations that exist at runtime.
    pub const value_space: Flags = .{
        .function_scoped_var = true,
        .block_scoped_var = true,
        .function = true,
        .class = true,
        .regular_enum = true,
        .const_enum = true,
        .value_module = true,
        .enum_member = true,
    };

    /// Declarations that are TypeScript types.
    pub const type_space: Flags = .{
        .class = true,
        .regular_enum = true,
        .const_enum = true,
        .interface = true,
        .type_alias = true,
        .type_parameter = true,
        .enum_member = true,
    };

    /// Declarations a dotted type name can start from.
    pub const namespace_space: Flags = .{
        .value_module = true,
        .namespace_module = true,
        .regular_enum = true,
        .const_enum = true,
    };

    /// The flags of the symbols each kind of declaration cannot merge into.
    pub const Excludes = struct {
        pub const block_scoped_var: Flags = value_space;
        pub const function_scoped_var: Flags =
            value_space.without(.{ .function_scoped_var = true, .function = true });
        /// A TypeScript function.
        pub const function: Flags =
            value_space.without(.{ .function = true, .value_module = true });
        pub const class: Flags =
            value_space.merge(type_space).without(.{ .value_module = true, .interface = true });
        pub const interface: Flags = type_space.without(.{ .interface = true, .class = true });
        pub const type_alias: Flags = type_space;
        pub const regular_enum: Flags =
            value_space.merge(type_space).without(.{ .regular_enum = true, .value_module = true });
        pub const const_enum: Flags =
            value_space.merge(type_space).without(.{ .const_enum = true });
        pub const value_module: Flags = value_space.without(.{
            .function = true,
            .class = true,
            .regular_enum = true,
            .value_module = true,
        });
        pub const namespace_module: Flags = .{};
        pub const import_binding: Flags = any_import;
        pub const parameter: Flags = value_space.without(.{ .function_scoped_var = true });
        pub const catch_var: Flags = value_space;
        // multiple `infer T` in one conditional unify, `<T, T>` is caught structurally
        pub const type_parameter: Flags = type_space.without(.{ .type_parameter = true });
        pub const enum_member: Flags = value_space.merge(type_space);
    };
};

/// A use of a name by an identifier reference, a JSX tag, or a type predicate parameter.
pub const Reference = struct {
    name: String,
    /// The scope the reference appears in.
    scope: sc.ScopeId,
    /// The referencing node.
    node: ast.NodeIndex,
    /// The resolved symbol, or `.none`.
    symbol: SymbolId = .none,
    flags: Flags = .{},

    pub const Flags = packed struct(u8) {
        /// Whether the reference assigns its binding.
        write: bool = false,
        /// The declaration space this position resolves in.
        space: Space = .value,
        /// Whether the use is erased with the types.
        type_position: bool = false,
        _: u3 = 0,
    };

    /// The declaration space a reference resolves in, as in TypeScript.
    pub const Space = enum(u3) {
        /// A runtime use.
        value,
        /// A type use.
        type,
        /// The qualifier of a dotted name, `ns` in `ns.T`.
        namespace,
        /// A value use inside a type, as in `typeof v`.
        typeof,
        /// A position that accepts every space, as in `export { x }`.
        any,
    };
};

/// The semantic model of a tree, valid for the lifetime of its arena.
pub const Semantic = struct {
    /// Every scope, indexed by `ScopeId`.
    scopes: sc.ScopeTree,
    /// Every symbol in declaration order, indexed by `SymbolId`.
    symbols: []const Symbol,
    /// Every reference in source order, indexed by `ReferenceId`.
    references: []const Reference,

    decl_nodes: []const NodeIndex,
    decl_ranges: []const Range,
    use_ids: []const ReferenceId,
    use_ranges: []const Range,
    scope_maps: []const ScopeMap,
    scope_names: []const u64,
    hoisting_variables: []const ScopeMap,
    /// Per scope, the next body of the same namespace or enum in a cycle, or `.none`.
    next_bodies: []const ScopeId,
    /// Per namespace or enum symbol, a body on its `next_bodies` cycle.
    owner_bodies: std.AutoHashMapUnmanaged(SymbolId, ScopeId),
    node_scopes: []const ScopeId,
    node_parents: []const NodeIndex,
    node_links: []const NodeLink,

    /// Yields the symbols declared directly in a scope.
    pub const BindingIterator = ScopeMap.Iterator;

    /// Yields node indices from a starting node up to the root.
    pub const AncestorIterator = struct {
        node_parents: []const NodeIndex,
        current: NodeIndex,

        /// The next node up the chain, or `null` past the root.
        pub fn next(self: *AncestorIterator) ?NodeIndex {
            const node = self.current;
            if (node == .null) return null;
            self.current = self.node_parents[@backingInt(node)];
            return node;
        }
    };

    /// The symbol with the given id.
    pub inline fn symbol(self: Semantic, id: SymbolId) Symbol {
        return self.symbols[@backingInt(id)];
    }

    /// The reference with the given id.
    pub inline fn reference(self: Semantic, id: ReferenceId) Reference {
        return self.references[@backingInt(id)];
    }

    /// The symbol declared at `node` or resolved by its reference, or `null`.
    pub fn symbolOf(self: Semantic, node: NodeIndex) ?SymbolId {
        const link = self.node_links[@backingInt(node)];
        if (link == NodeLink.none) return null;
        if (!link.reference) return @fromBackingInt(link.id);
        const resolved = self.references[link.id].symbol;
        return if (resolved != .none) resolved else null;
    }

    /// The reference at `node`, or `null`.
    pub fn referenceOf(self: Semantic, node: NodeIndex) ?ReferenceId {
        const link = self.node_links[@backingInt(node)];
        return if (link == NodeLink.none or !link.reference) null else @fromBackingInt(link.id);
    }

    /// The innermost scope of `node`, the one it creates if any.
    pub inline fn scopeOf(self: Semantic, node: NodeIndex) ScopeId {
        return self.node_scopes[@backingInt(node)];
    }

    /// The structural parent of `node`, or `null` at the root.
    pub fn parentOf(self: Semantic, node: NodeIndex) ?NodeIndex {
        const parent = self.node_parents[@backingInt(node)];
        return if (parent != .null) parent else null;
    }

    /// Iterates from `node` up to the root.
    pub fn ancestors(self: Semantic, node: NodeIndex) AncestorIterator {
        return .{ .node_parents = self.node_parents, .current = node };
    }

    /// The name node of every declaration of `id` in source order, conflicting ones included.
    pub fn decls(self: Semantic, id: SymbolId) []const NodeIndex {
        const range = self.decl_ranges[@backingInt(id)];
        return self.decl_nodes[range.start..][0..range.len];
    }

    /// Every reference to `id` in source order.
    pub fn uses(self: Semantic, id: SymbolId) []const ReferenceId {
        const range = self.use_ranges[@backingInt(id)];
        return self.use_ids[range.start..][0..range.len];
    }

    /// The binding of `name` in the scope alone, a hoisting `var` passing through included.
    pub fn binding(self: Semantic, scope_id: ScopeId, name: []const u8) ?SymbolId {
        const hash: NameHash = .of(name);
        const index = @backingInt(scope_id);
        return self.scope_maps[index].get(self.symbols, name, hash) orelse
            self.hoisting_variables[index].get(self.symbols, name, hash);
    }

    /// Iterates the symbols declared directly in the scope.
    pub fn bindings(self: Semantic, scope_id: ScopeId) BindingIterator {
        return self.scope_maps[@backingInt(scope_id)].iterator();
    }

    /// The nearest binding of `name` visible in `space` from the scope.
    pub fn lookup(
        self: Semantic,
        scope_id: ScopeId,
        name: []const u8,
        space: Reference.Space,
    ) ?SymbolId {
        return self.resolveFrom(scope_id, name, space, null);
    }

    /// The member `name` of the namespace or enum `owner` visible in `space`, as `owner.name`
    /// resolves across every merged body.
    pub fn member(
        self: Semantic,
        owner: SymbolId,
        name: []const u8,
        space: Reference.Space,
    ) ?SymbolId {
        const first = self.owner_bodies.get(owner) orelse return null;
        const members = self.bodies();
        const hash: NameHash = .of(name);
        var body = first;
        while (true) {
            if (members.shared(body, body, name, hash, space)) |id| return id;
            body = self.next_bodies[@backingInt(body)];
            if (body == first) return null;
        }
    }

    fn bodies(self: *const Semantic) Bodies {
        return .{
            .scopes = self.scopes.list,
            .scope_maps = self.scope_maps,
            .symbols = self.symbols,
            .next = self.next_bodies,
        };
    }

    const Position = struct { tree: *const ast.Tree, reference: *const Reference };

    inline fn resolve(
        self: *const Semantic,
        at: Position,
        named_above: []const ScopeId,
        shares_members: bool,
    ) SymbolId {
        const ref = at.reference;
        const name = at.tree.string(ref.name);
        const hash: NameHash = .of(name);
        const space = ref.flags.space;
        const bit = hash.bit();

        var scope = ref.scope;
        if ((space == .value or space == .typeof) and lexical.isArguments(name)) {
            return self.resolveFrom(scope, name, space, at) orelse .none;
        }
        while (scope != .none) : (scope = named_above[@backingInt(scope)]) {
            const index = @backingInt(scope);
            if (self.scope_names[index] & bit == 0) continue;
            if (self.scope_maps[index].get(self.symbols, name, hash)) |id| {
                if (plainlyVisible(self.symbols[@backingInt(id)].flags, ref.flags)) return id;
                break;
            }
            if (shares_members and self.next_bodies[index] != .none) break;
        }
        if (scope == .none) return .none;
        return self.resolveFrom(scope, name, space, at) orelse .none;
    }

    noinline fn resolveFrom(
        self: *const Semantic,
        start: ScopeId,
        name: []const u8,
        space: Reference.Space,
        position: ?Position,
    ) ?SymbolId {
        const hash: NameHash = .of(name);
        const members = self.bodies();
        // an implicit arguments object hides outer bindings but not its own scope's (10.2.11)
        const arguments_barrier = position != null and
            (space == .value or space == .typeof) and lexical.isArguments(name);

        var it = self.scopes.ancestors(start);
        while (it.next()) |ancestor| {
            if (hash.find(self.scope_maps, self.scope_names, self.symbols, ancestor, name)) |id| {
                const sym = self.symbol(id);
                if (sym.flags.visibleIn(space)) {
                    if (position == null or self.visibleAt(position.?, sym)) return id;
                }
            }
            if (members.sharedMember(ancestor, name, hash, space)) |id| return id;
            if (arguments_barrier) {
                if (isArgumentsBarrier(position.?.tree, self.scopes.get(ancestor))) return null;
            }
        }
        return null;
    }

    // infer variables exist only in their conditional's true branch, class type
    // parameters are hidden in static members (TS2302) and computed keys (TS2467)
    fn visibleAt(self: *const Semantic, at: Position, sym: Symbol) bool {
        const tree = at.tree;
        const node = at.reference.node;
        const scope_node = self.scopes.get(sym.scope).node;
        if (sym.flags.type_parameter) return switch (tree.data(scope_node)) {
            .ts_conditional_type => |cond| self.inBranch(node, scope_node, cond.true_type),
            .class, .ts_interface_declaration => !self.hidesTypeParameters(tree, node, scope_node),
            else => true,
        };
        if (!at.reference.flags.type_position) return true;
        if (sym.flags.parameter) {
            const type_parameters = typeParametersOf(tree, scope_node);
            return type_parameters == .null or !self.hasAncestor(node, type_parameters);
        }
        if (sym.flags.isHoistingVar()) {
            const body = switch (tree.data(scope_node)) {
                inline .function, .arrow_function_expression => |f| f.body,
                else => return true,
            };
            return self.hasAncestor(node, body);
        }
        return true;
    }

    fn hasAncestor(self: *const Semantic, node: NodeIndex, ancestor: NodeIndex) bool {
        std.debug.assert(ancestor != .null);
        var it = self.ancestors(self.node_parents[@backingInt(node)]);
        while (it.next()) |current| {
            if (current == ancestor) return true;
        }
        return false;
    }

    fn inBranch(self: *const Semantic, node: NodeIndex, root: NodeIndex, branch: NodeIndex) bool {
        var it = self.ancestors(node);
        var child = it.next().?;
        while (it.next()) |parent| : (child = parent) {
            if (parent == root) return child == branch;
        }
        return false;
    }

    fn hidesTypeParameters(
        self: *const Semantic,
        tree: *const ast.Tree,
        node: NodeIndex,
        owner: NodeIndex,
    ) bool {
        var it = self.ancestors(node);
        var child = it.next().?;
        while (it.next()) |parent| : (child = parent) {
            switch (tree.data(parent)) {
                .class => |cls| if (parent == owner and child == cls.super_class) return true,
                inline .method_definition,
                .property_definition,
                .ts_property_signature,
                .ts_method_signature,
                => |member_node| {
                    const body = self.node_parents[@backingInt(parent)];
                    if (member_node.computed and member_node.key == child and body != .null and
                        self.node_parents[@backingInt(body)] == owner) return true;
                },
                .class_body => {
                    if (self.node_parents[@backingInt(parent)] != owner) continue;
                    return switch (tree.data(child)) {
                        inline .method_definition,
                        .property_definition,
                        .ts_index_signature,
                        => |m| m.static,
                        .static_block => true,
                        else => false,
                    };
                },
                else => {},
            }
        }
        return false;
    }
};

const Bodies = struct {
    scopes: []const sc.Scope,
    scope_maps: []const ScopeMap,
    symbols: []const Symbol,
    next: []const ScopeId,

    fn shared(
        self: Bodies,
        from: ScopeId,
        body: ScopeId,
        name: []const u8,
        hash: NameHash,
        space: Reference.Space,
    ) ?SymbolId {
        const map = &self.scope_maps[@backingInt(body)];
        const id = map.get(self.symbols, name, hash) orelse return null;
        const flags = self.symbols[@backingInt(id)].flags;
        const in_enum = self.scopes[@backingInt(from)].kind != .ts_module;
        const is_shared = if (in_enum) flags.enum_member else flags.exported;
        return if (is_shared and flags.visibleIn(space)) id else null;
    }

    fn sharedMember(
        self: Bodies,
        scope_id: ScopeId,
        name: []const u8,
        hash: NameHash,
        space: Reference.Space,
    ) ?SymbolId {
        // the tracker sizes `next` on demand, past its end no scope is a body
        if (@backingInt(scope_id) >= self.next.len) return null;
        var body = self.next[@backingInt(scope_id)];
        if (body == .none) return null;
        while (body != scope_id) : (body = self.next[@backingInt(body)]) {
            if (self.shared(scope_id, body, name, hash, space)) |id| return id;
        }
        return null;
    }
};

const NameHash = struct {
    value: u64,

    const keys = [3]u64{ 0xa0761d6478bd642f, 0xe7037ed1a0b428db, 0x8ebc6af09c88c6e3 };

    inline fn of(name: []const u8) NameHash {
        const len = name.len;
        var low: u64 = 0;
        var high: u64 = 0;
        if (len >= 8) {
            low = word(u64, name, 0);
            high = word(u64, name, len - 8);
            var offset: usize = 8;
            while (offset + 8 < len) : (offset += 8) {
                low = mix(low ^ word(u64, name, offset), keys[2]);
            }
        } else if (len >= 4) {
            low = word(u32, name, 0);
            high = word(u32, name, len - 4);
        } else if (len > 0) {
            low = @as(u64, name[0]) | @as(u64, name[len >> 1]) << 8 | @as(u64, name[len - 1]) << 16;
        }
        return .{ .value = mix(low ^ keys[0], high ^ keys[1] ^ len) };
    }

    inline fn mix(a: u64, b: u64) u64 {
        const product = @as(u128, a) *% b;
        return @as(u64, @truncate(product)) ^ @as(u64, @truncate(product >> 64));
    }

    inline fn bit(self: NameHash) u64 {
        return @as(u64, 1) << @intCast(self.value >> 58);
    }

    inline fn tag(self: NameHash) u32 {
        return @truncate(self.value);
    }

    // a scope's binding of the name, its map skipped when its names rule the name out
    inline fn find(
        self: NameHash,
        maps: []const ScopeMap,
        names: []const u64,
        symbols: []const Symbol,
        scope: ScopeId,
        name: []const u8,
    ) ?SymbolId {
        const index = @backingInt(scope);
        if (names[index] & self.bit() == 0) return null;
        return maps[index].get(symbols, name, self);
    }
};

inline fn nameEql(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    const len = a.len;
    if (len >= 8) {
        var offset: usize = 0;
        while (offset + 8 < len) : (offset += 8) {
            if (word(u64, a, offset) != word(u64, b, offset)) return false;
        }
        return word(u64, a, len - 8) == word(u64, b, len - 8);
    }
    if (len >= 4) {
        return word(u32, a, 0) == word(u32, b, 0) and word(u32, a, len - 4) == word(u32, b, len - 4);
    }
    for (a, b) |x, y| {
        if (x != y) return false;
    }
    return true;
}

inline fn word(comptime T: type, bytes: []const u8, offset: usize) T {
    return std.mem.readInt(T, bytes[offset..][0..@sizeOf(T)], .little);
}

/// What a semantic walk records.
pub const Mode = enum {
    /// Scopes and bindings, all the early-error checker reads.
    check,
    /// Also references and the scope and parent of every node, for the `Semantic` model.
    analyze,
};

/// Collects symbols and references during a walk and finalizes them into a `Semantic`.
pub const SymbolTracker = struct {
    tree: *const ast.Tree,
    allocator: Allocator,
    symbols: std.ArrayList(Symbol) = .empty,
    references: std.ArrayList(Reference) = .empty,
    decl_pairs: std.ArrayList(DeclPair) = .empty,
    // parallel to `symbols`
    first_decls: std.ArrayList(NodeIndex) = .empty,
    scope_maps: std.ArrayList(ScopeMap) = .empty,
    // per scope, one bit per name hash bound there, so a resolution skips most maps
    scope_names: std.ArrayList(u64) = .empty,
    hoisting_variables: std.ArrayList(ScopeMap) = .empty,
    next_bodies: std.ArrayList(ScopeId) = .empty,
    export_contexts: std.ArrayList(bool) = .empty,
    last_bodies: std.AutoHashMapUnmanaged(SymbolId, ScopeId) = .empty,
    last_module_bodies: std.StringHashMapUnmanaged(ScopeId) = .empty,
    shares_members: bool = false,
    body_owner: SymbolId = .none,
    global_body: ScopeId = .none,
    block_functions: std.ArrayList(BlockFunction) = .empty,

    /// What the next `binding_identifier` declares.
    pending: PendingBinding = .{},
    /// How the next `binding_identifier` is exported.
    export_state: ExportState = .none,
    ambient: bool = false,
    excluded_imports: Symbol.Flags,

    saved_stack: std.ArrayList(SavedContext) = .empty,
    namespace_bodies: std.ArrayList(NodeIndex) = .empty,

    hashed_name: []const u8 = "",
    hashed_value: u64 = 0,

    pub const ExportState = enum { none, named, default };

    /// The declaration context for the next `binding_identifier`.
    pub const PendingBinding = struct {
        flags: Symbol.Flags = .{},
        excludes: Symbol.Flags = .{},
        scope: ScopeId = .root,
        block_function: bool = false,

        fn of(
            comptime kind: std.meta.DeclEnum(Symbol.Excludes),
            ambient: bool,
            scope: ScopeId,
        ) PendingBinding {
            var flags: Symbol.Flags = .{
                .ambient = ambient,
                .function_scoped_var = kind == .parameter or kind == .catch_var,
                .namespace_module = kind == .value_module,
            };
            @field(flags, @tagName(kind)) = true;
            const excludes = @field(Symbol.Excludes, @tagName(kind));
            return .{ .flags = flags, .excludes = excludes, .scope = scope };
        }

        /// Whether the declaration cannot merge into a symbol with `flags`.
        pub fn conflicts(self: PendingBinding, flags: Symbol.Flags) bool {
            var excludes = self.excludes;
            // a class and a function merge only when the class is ambient
            if (self.flags.class and self.flags.ambient) excludes.function = false;
            if (self.flags.function and flags.class and flags.ambient) excludes.class = false;
            return flags.intersects(excludes);
        }
    };

    const DeclPair = struct { symbol: SymbolId, node: NodeIndex };

    const BlockFunction = struct { symbol: SymbolId, block: ScopeId };

    const SavedContext = struct {
        pending: PendingBinding,
        export_state: ExportState,
        ambient: bool,
    };

    const saves_context = std.EnumSet(ast.NodeTag).initMany(&.{
        .variable_declaration,
        .function,
        .class,
        .formal_parameters,
        .import_declaration,
        .ts_import_equals_declaration,
        .import_specifier,
        .catch_clause,
        .ts_interface_declaration,
        .ts_type_alias_declaration,
        .ts_enum_declaration,
        .ts_module_declaration,
        .ts_global_declaration,
        .ts_namespace_export_declaration,
        .ts_type_parameter,
        .ts_mapped_type,
        .ts_index_signature,
    });

    pub fn init(tree: *ast.Tree, mode: Mode) Allocator.Error!SymbolTracker {
        const alloc = tree.allocator();
        var self = SymbolTracker{
            .tree = tree,
            .allocator = alloc,
            .ambient = tree.lang == .dts,
            .excluded_imports = if (tree.isTs()) .{} else Symbol.any_import,
        };

        const nodes: u32 = @intCast(tree.nodes.len);
        try self.symbols.ensureTotalCapacity(alloc, @max(16, nodes / 12));
        try self.first_decls.ensureTotalCapacity(alloc, @max(16, nodes / 12));
        if (mode == .analyze) try self.references.ensureTotalCapacity(alloc, @max(16, nodes / 4));
        try self.decl_pairs.ensureTotalCapacity(alloc, @max(16, nodes / 12));
        try self.scope_maps.ensureTotalCapacity(alloc, @max(8, nodes / 16));
        try self.hoisting_variables.ensureTotalCapacity(alloc, @max(8, nodes / 16));
        try self.saved_stack.ensureTotalCapacity(alloc, 32);

        if (exportsImplicitly(tree)) try self.setExportContext(sc.module_scope, true);
        return self;
    }

    /// Records the binding context a node sets for its `binding_identifier`s.
    pub inline fn setBindingContext(
        self: *SymbolTracker,
        comptime tag: ast.NodeTag,
        data: ast.NodeData,
        path: []const NodeIndex,
        scope: *const sc.ScopeTracker,
    ) Allocator.Error!void {
        std.debug.assert(path.len > 0);
        const parent: NodeIndex = if (path.len > 1) path[path.len - 2] else .null;
        if (comptime saves_context.contains(tag)) try self.saved_stack.append(self.allocator, .{
            .pending = self.pending,
            .export_state = self.export_state,
            .ambient = self.ambient,
        });
        switch (tag) {
            .export_named_declaration => {
                const decl = data.export_named_declaration;
                // a type-only specifier list must not tag the value side
                if (decl.declaration != .null or decl.export_kind != .type) {
                    self.export_state = .named;
                }
            },
            .export_default_declaration => self.export_state = .default,

            .variable_declaration => {
                const decl = data.variable_declaration;
                const ambient = decl.declare or self.ambient;
                switch (decl.kind) {
                    .@"var" => {
                        const target = scope.get(scope.current).hoist_target;
                        self.pending = .of(.function_scoped_var, ambient, target);
                        // ts and module-level functions never merge with `var`
                        self.pending.excludes.function =
                            self.tree.isTs() or scope.get(target).kind == .module;
                    },
                    .let, .@"const", .using, .await_using => {
                        self.pending = .of(.block_scoped_var, ambient, scope.current);
                        self.pending.flags.const_var = decl.kind != .let;
                    },
                }
                self.pending.excludes = self.pending.excludes.merge(self.excluded_imports);
            },

            .function => {
                const func = data.function;
                const is_decl = func.type == .function_declaration or
                    func.type == .ts_declare_function;
                const outer = scope.get(scope.current).parent;
                const target = if (is_decl and scope.get(outer).kind == .function_body)
                    scope.get(outer).hoist_target
                else
                    outer;
                const var_like = switch (scope.get(target).kind) {
                    .function, .function_body, .global, .static_block => true,
                    else => false,
                };
                self.pending = .{
                    .flags = .{
                        .function = true,
                        .ambient = func.declare or func.type == .ts_declare_function or
                            func.type == .ts_empty_body_function_expression or self.ambient,
                    },
                    .excludes = (if (self.tree.isTs())
                        Symbol.Excludes.function
                    else if (var_like)
                        Symbol.Excludes.function_scoped_var
                    else
                        Symbol.Excludes.block_scoped_var).merge(self.excluded_imports),
                    .scope = target,
                    // annex B also binds a sloppy block-level function where a var would (B.3.2)
                    .block_function = func.type == .function_declaration and !var_like and
                        !func.async and !func.generator and !self.tree.isTs() and
                        !scope.get(target).flags.strict and isInStatementList(self.tree, parent),
                };
                // expression names are local
                if (!is_decl) self.export_state = .none;
            },

            .class => {
                const cls = data.class;
                const target = scope.get(scope.current).parent;
                self.pending = .of(.class, cls.declare or self.ambient, target);
                self.pending.excludes = self.pending.excludes.merge(self.excluded_imports);
                // expression names are local
                if (cls.type != .class_declaration) self.export_state = .none;
            },

            .formal_parameters => {
                self.pending = .of(.parameter, self.pending.flags.ambient, scope.current);
                self.export_state = .none;
            },
            // its parameters are not `formal_parameters`
            .ts_index_signature => {
                const ambient = self.pending.flags.ambient or self.ambient;
                self.pending = .of(.parameter, ambient, scope.current);
                self.export_state = .none;
            },

            .class_body => self.export_state = .none,
            .ts_module_block, .ts_enum_body => try self.enterBody(data, parent, scope),

            .import_declaration, .ts_import_equals_declaration => {
                const type_only = @field(data, @tagName(tag)).import_kind == .type;
                self.pending = .{
                    .flags = .{
                        .import = !type_only,
                        .type_import = type_only,
                        .ambient = self.ambient,
                    },
                    .excludes = if (self.tree.isTs())
                        Symbol.Excludes.import_binding
                    else
                        Symbol.Excludes.import_binding.merge(Symbol.value_space),
                    .scope = scope.current,
                };
            },

            .import_specifier => if (data.import_specifier.import_kind == .type) {
                self.pending.flags = .{ .type_import = true, .ambient = self.ambient };
                self.pending.excludes = Symbol.Excludes.import_binding;
            },

            .catch_clause => self.pending = .of(.catch_var, self.ambient, scope.current),

            // the id binds outside the scope the tracker pushed for the body
            .ts_interface_declaration, .ts_type_alias_declaration => {
                const kind = if (tag == .ts_interface_declaration) .interface else .type_alias;
                const ambient = @field(data, @tagName(tag)).declare or self.ambient;
                self.pending = .of(kind, ambient, scope.get(scope.current).parent);
            },

            .ts_enum_declaration => {
                const decl = data.ts_enum_declaration;
                const ambient = decl.declare or self.ambient;
                self.pending = if (decl.is_const)
                    .of(.const_enum, ambient, scope.current)
                else
                    .of(.regular_enum, ambient, scope.current);
                if (decl.declare) self.ambient = true;
            },

            .ts_module_declaration => {
                const decl = data.ts_module_declaration;
                const ambient = decl.declare or self.ambient;
                self.body_owner = .none;
                self.pending = if (try self.isNamespaceInstantiated(decl.body))
                    .of(.value_module, ambient, scope.current)
                else
                    .of(.namespace_module, ambient, scope.current);
                if (decl.declare) self.ambient = true;
            },

            .ts_global_declaration => {
                self.body_owner = .none;
                self.ambient = true;
            },

            // `export as namespace N`
            .ts_namespace_export_declaration => {
                const id = data.ts_namespace_export_declaration.id;
                self.pending = .of(.namespace_module, true, .root);
                const name = self.tree.data(id).identifier_name.name;
                _ = try self.declare(name, id, scope.scopes.items);
            },

            // the key of a mapped type `[K in T]` is a type parameter
            .ts_type_parameter, .ts_mapped_type => {
                // an infer declares in the enclosing conditional so its true branch sees it
                const target = if (parent != .null and self.tree.data(parent) == .ts_infer_type)
                    inferScope(scope, path)
                else
                    scope.current;
                const ambient = self.pending.flags.ambient or self.ambient;
                self.pending = .of(.type_parameter, ambient, target);
                self.export_state = .none;
            },

            else => {},
        }
    }

    noinline fn enterBody(
        self: *SymbolTracker,
        data: ast.NodeData,
        parent: NodeIndex,
        scope: *const sc.ScopeTracker,
    ) Allocator.Error!void {
        self.export_state = .none;
        try self.syncScopeMaps(scope.scopes.items.len);
        if (parent != .null and self.tree.data(parent) == .ts_global_declaration) {
            self.global_body = scope.current;
        } else if (self.body_owner != .none) {
            try self.addBody(try self.lastBodyOf(self.body_owner), scope.current);
            self.body_owner = .none;
        } else if (try self.lastModuleBody(parent)) |last| {
            try self.addBody(last, scope.current);
        }
        if (data == .ts_module_block and self.ambient) {
            const body = self.tree.extra(data.ts_module_block.body);
            try self.setExportContext(scope.current, !hasExportStatement(self.tree, body));
        }
    }

    /// Restores the binding context a node saved on enter.
    pub inline fn exit(self: *SymbolTracker, comptime tag: ast.NodeTag) void {
        if (comptime saves_context.contains(tag)) {
            const saved = self.saved_stack.pop() orelse return;
            self.pending = saved.pending;
            self.ambient = saved.ambient;
            // a parameter list leaves the export state cleared, so its body exports nothing
            if (tag != .formal_parameters) self.export_state = saved.export_state;
        } else switch (tag) {
            .export_named_declaration, .export_default_declaration => self.export_state = .none,
            else => {},
        }
    }

    /// Declares the names a node binds, after its enter hooks.
    pub inline fn declareBindings(
        self: *SymbolTracker,
        comptime tag: ast.NodeTag,
        index: NodeIndex,
        data: ast.NodeData,
        scope: *const sc.ScopeTracker,
        type_position: bool,
    ) Allocator.Error!void {
        const scopes = scope.scopes.items;
        switch (tag) {
            .binding_identifier => {
                const id = data.binding_identifier;
                const flags = self.pending.flags;
                // in a type, only type parameters and parameters declare
                if (type_position and !flags.type_parameter and !flags.parameter) return;

                const sym_id = try self.declare(id.name, index, scopes);
                if (flags.namespace_module or flags.regular_enum or flags.const_enum) {
                    self.body_owner = sym_id;
                }
                if (self.pending.block_function) {
                    try self.block_functions.append(self.allocator, .{
                        .symbol = sym_id,
                        .block = self.pendingTarget(),
                    });
                }
                if (flags.isHoistingVar()) {
                    const name = self.tree.string(id.name);
                    const hash = self.prehash(name);
                    const from = scope.current;
                    try self.passThrough(scopes, from, self.pending.scope, name, hash, sym_id);
                }
            },
            .ts_module_declaration => {
                const decl = data.ts_module_declaration;
                if (self.tree.data(decl.id) == .ts_qualified_name) {
                    try self.declareQualifiedNamespace(decl, scope);
                }
            },
            // member names are not binding identifiers, and tsc resolves them lexically
            .ts_enum_member => {
                const member = data.ts_enum_member;
                const name = switch (self.tree.data(member.id)) {
                    .identifier_name => |id| id.name,
                    else => static_semantics.stringLiteralLike(self.tree, member.id) orelse return,
                };
                const saved = self.pending;
                self.pending = .of(.enum_member, self.ambient, scope.current);
                _ = try self.declare(name, member.id, scopes);
                self.pending = saved;
            },
            else => {},
        }
    }

    /// Records the names a node uses, with `identifier_flags` for an `identifier_reference`.
    pub inline fn addReferences(
        self: *SymbolTracker,
        comptime tag: ast.NodeTag,
        index: NodeIndex,
        data: ast.NodeData,
        scope: *const sc.ScopeTracker,
        identifier_flags: Reference.Flags,
    ) Allocator.Error!void {
        switch (tag) {
            .identifier_reference => {
                const name = data.identifier_reference.name;
                try self.addReference(name, scope.current, index, identifier_flags);
            },
            // the `v` of `v is T` is an identifier_name, but renamers need its parameter link
            .ts_type_predicate => {
                const node = data.ts_type_predicate.parameter_name;
                if (node == .null) return;
                const name = switch (self.tree.data(node)) {
                    .identifier_name => |id| id.name,
                    else => return,
                };
                const param = self.ownBinding(scope.current, self.tree.string(name)) orelse return;
                if (!self.symbol(param).flags.parameter) return;
                const flags: Reference.Flags = .{ .space = .typeof, .type_position = true };
                try self.addReference(name, scope.current, node, flags);
            },
            // a lowercase or dashed tag is an intrinsic element, as the JSX transforms treat it
            .jsx_opening_element, .jsx_closing_element => {
                const tag_name = @field(data, @tagName(tag)).name;
                const root = jsxTagRoot(self.tree, tag_name) orelse return;
                const id = self.tree.data(root).jsx_identifier;
                const text = self.tree.string(id.name);
                const is_value = if (self.tree.data(tag_name) == .jsx_member_expression)
                    !std.mem.eql(u8, text, "this")
                else
                    text.len > 0 and !std.ascii.isLower(text[0]);
                if (is_value and std.mem.findScalar(u8, text, '-') == null) {
                    try self.addReference(id.name, scope.current, root, .{});
                }
            },
            else => {},
        }
    }

    fn addReference(
        self: *SymbolTracker,
        name: String,
        scope: ScopeId,
        node: NodeIndex,
        flags: Reference.Flags,
    ) Allocator.Error!void {
        const ref: Reference = .{ .name = name, .scope = scope, .node = node, .flags = flags };
        try self.references.append(self.allocator, ref);
    }

    // `namespace A.B {}` as `namespace A { export namespace B {} }`
    noinline fn declareQualifiedNamespace(
        self: *SymbolTracker,
        decl: ast.TSModuleDeclaration,
        scope: *const sc.ScopeTracker,
    ) Allocator.Error!void {
        const export_state = self.export_state;
        defer self.export_state = export_state;
        var depth: u32 = 0;
        var head = decl.id;
        while (self.tree.data(head) == .ts_qualified_name) : (depth += 1) {
            head = self.tree.data(head).ts_qualified_name.left;
        }
        var owner: SymbolId = .none;
        var level: u32 = 0;
        while (level <= depth) : (level += 1) {
            var part = decl.id;
            var target = scope.current;
            var steps = depth - level;
            while (steps > 0) : (steps -= 1) {
                part = self.tree.data(part).ts_qualified_name.left;
                target = scope.get(target).parent;
            }
            const name, const node = switch (self.tree.data(part)) {
                .ts_qualified_name => |q| .{
                    self.tree.data(q.right).identifier_name.name,
                    q.right,
                },
                .binding_identifier => |id| .{ id.name, part },
                else => return,
            };
            if (owner != .none) try self.addBody(try self.lastBodyOf(owner), target);
            self.export_state = if (owner == .none) export_state else .named;
            self.pending.scope = target;
            owner = try self.declare(name, node, scope.scopes.items);
        }
        self.body_owner = owner;
    }

    // a conflicting declaration keeps the existing flags but still records `node`
    fn declare(
        self: *SymbolTracker,
        name: String,
        node: NodeIndex,
        scopes: []const sc.Scope,
    ) Allocator.Error!SymbolId {
        try self.syncScopeMaps(scopes.len);
        const target = self.pendingTarget();
        std.debug.assert(node != .null);
        const name_str = self.tree.string(name);
        const hash = self.prehash(name_str);

        const exported = self.pendingExported(target);
        const is_default = self.export_state == .default;
        // an export merges across the bodies of its namespace
        const own = self.bindingHashed(target, name_str, hash);
        const shared = if (own == null) self.sharedExport(target, name_str, hash, scopes) else null;
        if (shared) |existing| try self.bind(target, name_str, hash, existing);

        const id = if (own orelse shared) |existing| existing: {
            const sym = &self.symbols.items[@backingInt(existing)];
            if (!self.pending.conflicts(sym.flags)) {
                var merged = sym.flags.merge(self.pending.flags);
                merged.exported = merged.exported or exported;
                merged.is_default = merged.is_default or is_default;
                // an overload implementation emits at runtime, so the merge is non-ambient
                merged.ambient = sym.flags.ambient and self.pending.flags.ambient;
                sym.flags = merged;
            }
            break :existing existing;
        } else new: {
            const id: SymbolId = @fromBackingInt(@intCast(self.symbols.items.len));
            var sym: Symbol = .{ .name = name, .flags = self.pending.flags, .scope = target };
            sym.flags.exported = exported;
            sym.flags.is_default = is_default;
            try self.symbols.append(self.allocator, sym);
            try self.first_decls.append(self.allocator, node);
            try self.bind(target, name_str, hash, id);
            break :new id;
        };
        try self.decl_pairs.append(self.allocator, .{ .symbol = id, .node = node });
        return id;
    }

    /// The symbol the pending declaration of `name` merges into, the scope's own binding or an
    /// export another body of its namespace shares.
    pub fn prior(self: *SymbolTracker, name: []const u8, scopes: []const sc.Scope) ?SymbolId {
        const target = self.pendingTarget();
        const hash = self.prehash(name);
        return self.bindingHashed(target, name, hash) orelse
            self.sharedExport(target, name, hash, scopes);
    }

    fn pendingTarget(self: *const SymbolTracker) ScopeId {
        return if (self.pending.scope == self.global_body) .root else self.pending.scope;
    }

    fn pendingExported(self: *const SymbolTracker, target: ScopeId) bool {
        const index = @backingInt(target);
        const contexts = self.export_contexts.items;
        return self.export_state != .none or (index < contexts.len and contexts[index] and
            !self.pending.flags.intersects(Symbol.any_import));
    }

    fn sharedExport(
        self: *const SymbolTracker,
        target: ScopeId,
        name: []const u8,
        hash: NameHash,
        scopes: []const sc.Scope,
    ) ?SymbolId {
        if (!self.shares_members or !self.pendingExported(target)) return null;
        const bodies: Bodies = .{
            .scopes = scopes,
            .scope_maps = self.scope_maps.items,
            .symbols = self.symbols.items,
            .next = self.next_bodies.items,
        };
        return bodies.sharedMember(target, name, hash, .any);
    }

    /// The symbol with the given id.
    pub inline fn symbol(self: *const SymbolTracker, id: SymbolId) Symbol {
        return self.symbols.items[@backingInt(id)];
    }

    /// The name node of the first declaration of `id`.
    pub fn firstDeclOf(self: *const SymbolTracker, id: SymbolId) NodeIndex {
        return self.first_decls.items[@backingInt(id)];
    }

    /// The binding of `name` declared directly in `scope`.
    pub fn ownBinding(self: *const SymbolTracker, scope: ScopeId, name: []const u8) ?SymbolId {
        const index = @backingInt(scope);
        if (index >= self.scope_maps.items.len) return null;
        return self.scope_maps.items[index].get(self.symbols.items, name, .of(name));
    }

    /// The binding of `name` in `scope`, a hoisting `var` passing through included.
    pub fn binding(self: *const SymbolTracker, scope: ScopeId, name: []const u8) ?SymbolId {
        return self.bindingHashed(scope, name, .of(name));
    }

    inline fn prehash(self: *SymbolTracker, name: []const u8) NameHash {
        if (name.ptr != self.hashed_name.ptr or name.len != self.hashed_name.len) {
            self.hashed_name = name;
            self.hashed_value = NameHash.of(name).value;
        }
        return .{ .value = self.hashed_value };
    }

    fn bindingHashed(
        self: *const SymbolTracker,
        scope: ScopeId,
        name: []const u8,
        hash: NameHash,
    ) ?SymbolId {
        const index = @backingInt(scope);
        const symbols = self.symbols.items;
        if (index >= self.scope_maps.items.len) return null;
        return hash.find(self.scope_maps.items, self.scope_names.items, symbols, scope, name) orelse
            self.hoisting_variables.items[index].get(symbols, name, hash);
    }

    fn bind(
        self: *SymbolTracker,
        scope: ScopeId,
        name: []const u8,
        hash: NameHash,
        id: SymbolId,
    ) Allocator.Error!void {
        const map = &self.scope_maps.items[@backingInt(scope)];
        std.debug.assert(map.find(self.symbols.items, name, hash) == null);
        try map.putNew(self.allocator, name, hash, id);
        self.scope_names.items[@backingInt(scope)] |= hash.bit();
    }

    // a hoisting name is visible in every scope it passes through, so redeclarations see it
    fn passThrough(
        self: *SymbolTracker,
        scopes: []const sc.Scope,
        from: ScopeId,
        target: ScopeId,
        name: []const u8,
        hash: NameHash,
        id: SymbolId,
    ) Allocator.Error!void {
        var scope = from;
        while (scope != target and scope != .none) : (scope = scopes[@backingInt(scope)].parent) {
            const passing = &self.hoisting_variables.items[@backingInt(scope)];
            if (passing.find(self.symbols.items, name, hash) != null) continue;
            try passing.putNew(self.allocator, name, hash, id);
        }
    }

    inline fn syncScopeMaps(self: *SymbolTracker, count: usize) Allocator.Error!void {
        if (self.scope_maps.items.len < count) try self.growScopeMaps(count);
    }

    noinline fn growScopeMaps(self: *SymbolTracker, count: usize) Allocator.Error!void {
        try padTo(ScopeMap, &self.scope_maps, self.allocator, count, .empty);
        try padTo(u64, &self.scope_names, self.allocator, count, 0);
        try padTo(ScopeMap, &self.hoisting_variables, self.allocator, count, .empty);
    }

    fn setExportContext(self: *SymbolTracker, scope: ScopeId, value: bool) Allocator.Error!void {
        try padTo(bool, &self.export_contexts, self.allocator, @backingInt(scope) + 1, false);
        self.export_contexts.items[@backingInt(scope)] = value;
    }

    // body-less ambient modules are instantiated by spec
    fn isNamespaceInstantiated(self: *SymbolTracker, body: NodeIndex) Allocator.Error!bool {
        const queue = &self.namespace_bodies;
        queue.clearRetainingCapacity();
        try queue.append(self.allocator, body);
        while (queue.pop()) |next| {
            if (next == .null) return true;
            const block = switch (self.tree.data(next)) {
                .ts_module_block => |b| b,
                else => return true,
            };
            const statements = self.tree.extra(block.body);
            for (statements) |stmt| {
                if (try self.isInstantiatingStatement(stmt, statements)) return true;
            }
        }
        return false;
    }

    fn isInstantiatingStatement(
        self: *SymbolTracker,
        idx: NodeIndex,
        statements: []const NodeIndex,
    ) Allocator.Error!bool {
        switch (self.tree.data(idx)) {
            .ts_interface_declaration,
            .ts_type_alias_declaration,
            .ts_import_equals_declaration,
            => {},
            .ts_module_declaration => |m| try self.namespace_bodies.append(self.allocator, m.body),
            .ts_global_declaration => |g| try self.namespace_bodies.append(self.allocator, g.body),
            .export_named_declaration => |e| {
                // `export declare` is a type-only export of a value
                if (e.declaration != .null) {
                    if (self.tree.data(e.declaration) == .ts_import_equals_declaration) return true;
                    return self.isInstantiatingStatement(e.declaration, statements);
                }
                if (e.export_kind == .type) return false;
                return e.source != .null or try self.exportsValue(e.specifiers, statements);
            },
            // const enums included, as in tsc
            else => return true,
        }
        return false;
    }

    // `export { x }` exports a value unless x is declared only as types
    fn exportsValue(
        self: *SymbolTracker,
        specifiers: ast.IndexRange,
        statements: []const NodeIndex,
    ) Allocator.Error!bool {
        const tree = self.tree;
        for (tree.extra(specifiers)) |specifier| {
            const spec = tree.data(specifier).export_specifier;
            if (spec.export_kind == .type) continue;
            const name = switch (tree.data(spec.local)) {
                .identifier_reference => |id| tree.string(id.name),
                else => return true,
            };
            var found = false;
            for (statements) |stmt| {
                if (!declaresName(tree, stmt, name)) continue;
                found = true;
                if (tree.data(stmt) == .ts_import_equals_declaration) return true;
                if (try self.isInstantiatingStatement(stmt, statements)) return true;
            }
            if (!found) return true;
        }
        return false;
    }

    fn lastModuleBody(self: *SymbolTracker, declaration: NodeIndex) Allocator.Error!?*ScopeId {
        if (declaration == .null) return null;
        const id = switch (self.tree.data(declaration)) {
            .ts_module_declaration => |decl| decl.id,
            else => return null,
        };
        const name = switch (self.tree.data(id)) {
            .string_literal => |literal| self.tree.string(literal.value),
            else => return null,
        };
        return (try self.last_module_bodies.getOrPutValue(self.allocator, name, .none)).value_ptr;
    }

    fn lastBodyOf(self: *SymbolTracker, owner: SymbolId) Allocator.Error!*ScopeId {
        return (try self.last_bodies.getOrPutValue(self.allocator, owner, .none)).value_ptr;
    }

    fn addBody(self: *SymbolTracker, last: *ScopeId, body: ScopeId) Allocator.Error!void {
        std.debug.assert(last.* != body);
        try padTo(ScopeId, &self.next_bodies, self.allocator, @backingInt(body) + 1, .none);
        const next = self.next_bodies.items;
        std.debug.assert(next[@backingInt(body)] == .none);
        if (last.* == .none) {
            next[@backingInt(body)] = body;
        } else {
            next[@backingInt(body)] = next[@backingInt(last.*)];
            next[@backingInt(last.*)] = body;
            self.shares_members = true;
        }
        last.* = body;
    }

    // decided once every scope is complete, as a later `let` of the name rules it out (B.3.2)
    // https://tc39.es/ecma262/#sec-functiondeclarationinstantiation
    // https://tc39.es/ecma262/#sec-globaldeclarationinstantiation
    fn hoistBlockFunctions(self: *SymbolTracker, scopes: []const sc.Scope) Allocator.Error!void {
        const functions = self.block_functions.items;
        if (functions.len == 0) return;
        const hoists = try self.allocator.alloc(bool, functions.len);
        for (functions, hoists) |function, *hoist| {
            hoist.* = self.blockFunctionHoists(function, scopes);
        }

        var redirects: []SymbolId = &.{};
        for (functions, hoists) |function, hoist| {
            if (!hoist) continue;
            const into = try self.hoistBlockFunction(function, scopes) orelse continue;
            if (redirects.len == 0) {
                redirects = try self.allocator.alloc(SymbolId, self.symbols.items.len);
                @memset(redirects, .none);
            }
            redirects[@backingInt(function.symbol)] = into;
        }
        if (redirects.len > 0) try self.removeMergedSymbols(redirects);
    }

    // only where `var f` in its place is no early error and names no parameter, as 10.2.11 step 32
    // and 16.1.7 step 12 require, a simple catch parameter aside (B.3.4)
    fn blockFunctionHoists(
        self: *const SymbolTracker,
        function: BlockFunction,
        scopes: []const sc.Scope,
    ) bool {
        const name = self.tree.string(self.symbol(function.symbol).name);
        const block = scopes[@backingInt(function.block)];
        const target = block.hoist_target;
        std.debug.assert(target != function.block);
        // a function's `arguments` stays its arguments object (10.2.11 step 32)
        if (scopes[@backingInt(target)].kind != .global and lexical.isArguments(name)) return false;
        var scope_id = block.parent;
        while (true) {
            const scope = scopes[@backingInt(scope_id)];
            if (self.ownBinding(scope_id, name)) |existing_id| {
                const flags = self.symbol(existing_id).flags;
                if (scope_id == target) {
                    if (flags.parameter or flags.block_scoped_var or flags.class) return false;
                } else if (!flags.catch_var or !isSimpleCatch(self.tree, scope)) {
                    return false;
                }
            }
            if (scope.kind == .function or scope.kind == .global) return true;
            scope_id = scope.parent;
        }
    }

    // an existing var or function of the name is the binding the declaration writes when it runs
    fn hoistBlockFunction(
        self: *SymbolTracker,
        function: BlockFunction,
        scopes: []const sc.Scope,
    ) Allocator.Error!?SymbolId {
        const name = self.tree.string(self.symbol(function.symbol).name);
        const hash: NameHash = .of(name);
        const block_map = &self.scope_maps.items[@backingInt(function.block)];
        const slot = block_map.find(self.symbols.items, name, hash) orelse return null;
        if (block_map.slots[slot].id != function.symbol) return null;
        block_map.removeAt(slot);

        const target = scopes[@backingInt(function.block)].hoist_target;
        try self.passThrough(scopes, function.block, target, name, hash, function.symbol);
        if (self.ownBinding(target, name)) |existing| {
            const merged = &self.symbols.items[@backingInt(existing)];
            merged.flags = merged.flags.merge(self.symbol(function.symbol).flags);
            return existing;
        }
        self.symbols.items[@backingInt(function.symbol)].scope = target;
        try self.bind(target, name, hash, function.symbol);
        return null;
    }

    // bindings are listed by id, so a symbol merged away must leave no gap
    fn removeMergedSymbols(self: *SymbolTracker, redirects: []const SymbolId) Allocator.Error!void {
        std.debug.assert(self.last_bodies.count() == 0);
        const ids = try self.allocator.alloc(SymbolId, redirects.len);
        var kept: u32 = 0;
        for (redirects, 0..) |into, old| {
            if (into != .none) continue;
            ids[old] = @fromBackingInt(kept);
            self.symbols.items[kept] = self.symbols.items[old];
            self.first_decls.items[kept] = self.first_decls.items[old];
            kept += 1;
        }
        for (redirects, ids) |into, *id| {
            if (into == .none) continue;
            std.debug.assert(redirects[@backingInt(into)] == .none);
            id.* = ids[@backingInt(into)];
        }
        self.symbols.shrinkRetainingCapacity(kept);
        self.first_decls.shrinkRetainingCapacity(kept);

        for (self.decl_pairs.items) |*pair| pair.symbol = ids[@backingInt(pair.symbol)];
        for ([_][]ScopeMap{ self.scope_maps.items, self.hoisting_variables.items }) |maps| {
            for (maps) |*map| {
                for (map.slots[0 .. map.mask + 1]) |*slot| {
                    if (slot.id != .none) slot.id = ids[@backingInt(slot.id)];
                }
            }
        }
    }

    /// Finalizes the tracker into a `Semantic` that aliases its storage.
    pub noinline fn finalize(
        self: *SymbolTracker,
        scopes: sc.ScopeTree,
        node_scopes: []const ScopeId,
        node_parents: []const NodeIndex,
    ) Allocator.Error!Semantic {
        std.debug.assert(self.saved_stack.items.len == 0);
        std.debug.assert(self.export_state == .none);
        const allocator = self.allocator;

        try self.syncScopeMaps(scopes.list.len);
        try padTo(ScopeId, &self.next_bodies, allocator, scopes.list.len, .none);
        try self.hoistBlockFunctions(scopes.list);
        const symbols = self.symbols.items;
        const references = self.references.items;
        const decls = self.decl_pairs.items;

        const decl_ranges = try allocator.alloc(Range, symbols.len);
        @memset(decl_ranges, .{});
        for (decls) |decl| decl_ranges[@backingInt(decl.symbol)].len += 1;
        const decl_nodes = try allocator.alloc(NodeIndex, Range.layout(decl_ranges));
        for (decls) |decl| decl_nodes[decl_ranges[@backingInt(decl.symbol)].push()] = decl.node;

        const node_links = try allocator.alloc(NodeLink, self.tree.nodes.len);
        @memset(node_links, NodeLink.none);
        for (decls) |decl| {
            const id: u31 = @intCast(@backingInt(decl.symbol));
            node_links[@backingInt(decl.node)] = .{ .id = id, .reference = false };
        }
        for (references, 0..) |ref, i| {
            node_links[@backingInt(ref.node)] = .{ .id = @intCast(i), .reference = true };
        }

        const scope_names = self.scope_names.items;
        if (self.shares_members) {
            for (self.next_bodies.items, scope_names) |next, *names| {
                if (next != .none) names.* = std.math.maxInt(u64);
            }
        }
        const gpa = self.tree.arena.child_allocator;
        const named_above = try gpa.alloc(ScopeId, scopes.list.len);
        defer gpa.free(named_above);
        for (scopes.list, named_above) |scope, *above| {
            const parent = scope.parent;
            above.* = if (parent == .none or scope_names[@backingInt(parent)] != 0)
                parent
            else
                named_above[@backingInt(parent)];
        }

        const use_ranges = try allocator.alloc(Range, symbols.len);
        @memset(use_ranges, .{});
        var sem: Semantic = .{
            .scopes = scopes,
            .symbols = symbols,
            .references = references,
            .decl_nodes = decl_nodes,
            .decl_ranges = decl_ranges,
            .use_ids = &.{},
            .use_ranges = use_ranges,
            .scope_maps = self.scope_maps.items,
            .scope_names = scope_names,
            .hoisting_variables = self.hoisting_variables.items,
            .next_bodies = self.next_bodies.items,
            .owner_bodies = self.last_bodies,
            .node_scopes = node_scopes,
            .node_parents = node_parents,
            .node_links = node_links,
        };
        for (references) |*ref| {
            const at: Semantic.Position = .{ .tree = self.tree, .reference = ref };
            ref.symbol = sem.resolve(at, named_above, self.shares_members);
            if (ref.symbol != .none) use_ranges[@backingInt(ref.symbol)].len += 1;
        }
        const use_ids = try allocator.alloc(ReferenceId, Range.layout(use_ranges));
        for (references, 0..) |ref, i| {
            if (ref.symbol == .none) continue;
            use_ids[use_ranges[@backingInt(ref.symbol)].push()] = @fromBackingInt(@intCast(i));
        }
        sem.use_ids = use_ids;
        return sem;
    }
};

inline fn plainlyVisible(flags: Symbol.Flags, reference: Reference.Flags) bool {
    const placed: Symbol.Flags = if (reference.type_position)
        .{ .type_parameter = true, .parameter = true, .function_scoped_var = true }
    else
        .{ .type_parameter = true };
    return flags.visibleIn(reference.space) and !flags.intersects(placed);
}

fn padTo(
    comptime T: type,
    list: *std.ArrayList(T),
    allocator: Allocator,
    count: usize,
    value: T,
) Allocator.Error!void {
    if (list.items.len < count) try list.appendNTimes(allocator, value, count - list.items.len);
}

fn typeParametersOf(tree: *const ast.Tree, node: NodeIndex) NodeIndex {
    return switch (tree.data(node)) {
        inline .function,
        .arrow_function_expression,
        .ts_function_type,
        .ts_constructor_type,
        .ts_method_signature,
        .ts_call_signature_declaration,
        .ts_construct_signature_declaration,
        => |signature| signature.type_parameters,
        else => .null,
    };
}

fn inferScope(scope: *const sc.ScopeTracker, path: []const NodeIndex) ScopeId {
    var it = scope.ancestors(scope.current);
    while (it.next()) |id| switch (scope.tree.data(scope.get(id).node)) {
        .ts_conditional_type => |cond| {
            if (std.mem.findScalar(NodeIndex, path, cond.extends_type) != null) return id;
        },
        else => {},
    };
    return scope.current;
}

// only non-arrow functions hold an arguments object, static blocks have none at all
fn isArgumentsBarrier(tree: *const ast.Tree, scope: sc.Scope) bool {
    return switch (scope.kind) {
        .function => tree.data(scope.node) == .function,
        .static_block => true,
        else => false,
    };
}

fn jsxTagRoot(tree: *const ast.Tree, name: NodeIndex) ?NodeIndex {
    var current = name;
    while (true) switch (tree.data(current)) {
        .jsx_identifier => return current,
        .jsx_member_expression => |m| current = m.object,
        else => return null,
    };
}

// directly in a block or case, or alone in an if clause, where annex B applies (B.3.2, B.3.3)
// https://tc39.es/ecma262/#sec-block-level-function-declarations-web-legacy-compatibility-semantics
fn isInStatementList(tree: *const ast.Tree, parent: NodeIndex) bool {
    if (parent == .null) return false;
    return switch (tree.data(parent)) {
        .block_statement, .switch_case, .if_statement => true,
        else => false,
    };
}

/// Whether a catch scope's parameter is a plain identifier, whose name a var may share (B.3.4).
/// https://tc39.es/ecma262/#sec-variablestatements-in-catch-blocks
pub fn isSimpleCatch(tree: *const ast.Tree, scope: sc.Scope) bool {
    const param = tree.data(scope.node).catch_clause.param;
    return param != .null and tree.data(param) == .binding_identifier;
}

/// Whether a declaration module without export statements exports all but its imports.
pub fn exportsImplicitly(tree: *const ast.Tree) bool {
    if (tree.lang != .dts or !tree.isModule()) return false;
    const body = tree.extra(tree.data(tree.root).program.body);
    return hasModuleSyntax(tree, body) and !hasExportStatement(tree, body);
}

pub fn hasModuleSyntax(tree: *const ast.Tree, statements: []const NodeIndex) bool {
    for (statements) |stmt| switch (tree.data(stmt)) {
        .import_declaration,
        .export_named_declaration,
        .export_default_declaration,
        .export_all_declaration,
        .ts_export_assignment,
        => return true,
        .ts_import_equals_declaration => |decl| {
            if (tree.data(decl.module_reference) == .ts_external_module_reference) return true;
        },
        else => {},
    };
    return false;
}

// `export default function` is a declaration
fn hasExportStatement(tree: *const ast.Tree, statements: []const NodeIndex) bool {
    for (statements) |stmt| switch (tree.data(stmt)) {
        .export_named_declaration => |e| if (e.declaration == .null) return true,
        .export_all_declaration, .ts_export_assignment => return true,
        .export_default_declaration => |e| switch (tree.data(e.declaration)) {
            .function => |func| if (func.type == .function_expression) return true,
            .class => |cls| if (cls.type == .class_expression) return true,
            .ts_interface_declaration => {},
            else => return true,
        },
        else => {},
    };
    return false;
}

fn declaresName(tree: *const ast.Tree, stmt: NodeIndex, name: []const u8) bool {
    const id = switch (tree.data(stmt)) {
        inline .function,
        .class,
        .ts_interface_declaration,
        .ts_type_alias_declaration,
        .ts_enum_declaration,
        .ts_module_declaration,
        .ts_import_equals_declaration,
        => |decl| decl.id,
        .variable_declaration => |decl| {
            for (tree.extra(decl.declarators)) |declarator| {
                const target = tree.data(declarator).variable_declarator.id;
                if (namedIdentifier(tree, target, name)) return true;
            }
            return false;
        },
        .export_named_declaration => |e| {
            return e.declaration != .null and declaresName(tree, e.declaration, name);
        },
        else => return false,
    };
    return namedIdentifier(tree, id, name);
}

fn namedIdentifier(tree: *const ast.Tree, node: NodeIndex, name: []const u8) bool {
    if (node == .null) return false;
    return switch (tree.data(node)) {
        .binding_identifier => |id| std.mem.eql(u8, tree.string(id.name), name),
        else => false,
    };
}
