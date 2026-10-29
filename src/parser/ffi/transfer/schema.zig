//! The children each node may have, as the parser builds them. A field listing `.null` may be
//! absent, or for a list hold holes, and one listing tags holds only nodes of those tags.

const std = @import("std");
const ast = @import("parser").ast;

pub const Tag = std.meta.Tag(ast.NodeData);

const declaration = .{ .null, .ts_type_parameter_declaration };
const instantiation = .{ .null, .ts_type_parameter_instantiation };
const annotation = .{ .null, .ts_type_annotation };
const decorators = .{.decorator};
const names = .{ .identifier_name, .string_literal };
const jsx_names = .{ .jsx_identifier, .jsx_namespaced_name, .jsx_member_expression };
const jsx_children = .{
    .jsx_text,
    .jsx_expression_container,
    .jsx_spread_child,
    .jsx_element,
    .jsx_fragment,
};
const signatures = .{
    .ts_property_signature,
    .ts_method_signature,
    .ts_call_signature_declaration,
    .ts_construct_signature_declaration,
    .ts_index_signature,
};
const signature = .{
    .type_parameters = declaration,
    .params = .{.formal_parameters},
    .return_type = annotation,
};
const pattern = .{ .decorators = decorators, .type_annotation = annotation };

const fields = .{
    .arrow_function_expression = signature,
    .function = .{
        .id = .{ .null, .binding_identifier },
        .type_parameters = declaration,
        .params = .{.formal_parameters},
        .return_type = annotation,
        .body = .{ .null, .function_body },
    },
    .formal_parameters = .{
        .items = .{ .formal_parameter, .ts_parameter_property },
        .rest = .{ .null, .binding_rest_element },
    },
    .array_expression = .{ .elements = .{.null} },
    .object_expression = .{ .properties = .{ .object_property, .spread_element } },
    .call_expression = .{ .type_arguments = instantiation },
    .chain_expression = .{
        .expression = .{ .member_expression, .call_expression, .ts_non_null_expression },
    },
    .tagged_template_expression = .{
        .type_arguments = instantiation,
        .quasi = .{.template_literal},
    },
    .new_expression = .{ .type_arguments = instantiation },
    .yield_expression = .{ .argument = .{.null} },
    .meta_property = .{ .meta = .{.identifier_name}, .property = .{.identifier_name} },
    .class = .{
        .decorators = decorators,
        .id = .{ .null, .binding_identifier },
        .type_parameters = declaration,
        .super_class = .{.null},
        .super_type_arguments = instantiation,
        .implements = .{.ts_class_implements},
        .body = .{.class_body},
    },
    .class_body = .{
        .body = .{ .method_definition, .property_definition, .static_block, .ts_index_signature },
    },
    .method_definition = .{ .decorators = decorators, .value = .{.function} },
    .property_definition = .{
        .decorators = decorators,
        .type_annotation = annotation,
        .value = .{.null},
    },
    .template_literal = .{ .quasis = .{.template_element} },
    .binding_identifier = pattern,
    .if_statement = .{ .alternate = .{.null} },
    .switch_statement = .{ .cases = .{.switch_case} },
    .switch_case = .{ .@"test" = .{.null} },
    .for_statement = .{ .init = .{.null}, .@"test" = .{.null}, .update = .{.null} },
    .break_statement = .{ .label = .{ .null, .label_identifier } },
    .continue_statement = .{ .label = .{ .null, .label_identifier } },
    .labeled_statement = .{ .label = .{.label_identifier} },
    .return_statement = .{ .argument = .{.null} },
    .try_statement = .{
        .block = .{.block_statement},
        .handler = .{ .null, .catch_clause },
        .finalizer = .{ .null, .block_statement },
    },
    .catch_clause = .{ .param = .{.null}, .body = .{.block_statement} },
    .variable_declaration = .{ .declarators = .{.variable_declarator} },
    .variable_declarator = .{ .init = .{.null} },
    .directive = .{ .expression = .{.string_literal} },
    .assignment_pattern = pattern,
    .binding_rest_element = pattern,
    .array_pattern = .{
        .decorators = decorators,
        .elements = .{.null},
        .rest = .{ .null, .binding_rest_element },
        .type_annotation = annotation,
    },
    .object_pattern = .{
        .decorators = decorators,
        .properties = .{.binding_property},
        .rest = .{ .null, .binding_rest_element },
        .type_annotation = annotation,
    },
    .import_expression = .{ .options = .{.null} },
    .import_declaration = .{
        .specifiers = .{
            .import_specifier,
            .import_default_specifier,
            .import_namespace_specifier,
        },
        .source = .{.string_literal},
        .attributes = .{.import_attribute},
    },
    .import_specifier = .{ .imported = names, .local = .{.binding_identifier} },
    .import_default_specifier = .{ .local = .{.binding_identifier} },
    .import_namespace_specifier = .{ .local = .{.binding_identifier} },
    .import_attribute = .{ .key = names, .value = .{.string_literal} },
    .export_named_declaration = .{
        .declaration = .{.null},
        .specifiers = .{.export_specifier},
        .source = .{ .null, .string_literal },
        .attributes = .{.import_attribute},
    },
    .export_all_declaration = .{
        .exported = .{ .null, .identifier_name, .string_literal },
        .source = .{.string_literal},
        .attributes = .{.import_attribute},
    },
    .export_specifier = .{
        .local = .{ .identifier_reference, .identifier_name, .string_literal },
        .exported = names,
    },
    .ts_type_reference = .{
        .type_name = .{ .identifier_reference, .ts_qualified_name, .this_expression },
        .type_arguments = instantiation,
    },
    .ts_qualified_name = .{ .right = .{.identifier_name} },
    .ts_type_query = .{ .type_arguments = instantiation },
    .ts_import_type = .{
        .source = .{.string_literal},
        .options = .{ .null, .object_expression },
        .qualifier = .{ .null, .identifier_name, .ts_qualified_name },
        .type_arguments = instantiation,
    },
    .ts_type_parameter = .{
        .name = .{.binding_identifier},
        .constraint = .{.null},
        .default = .{.null},
    },
    .ts_type_parameter_declaration = .{ .params = .{.ts_type_parameter} },
    .ts_template_literal_type = .{ .quasis = .{.template_element} },
    .ts_named_tuple_member = .{ .label = .{.identifier_name} },
    .ts_infer_type = .{ .type_parameter = .{.ts_type_parameter} },
    .ts_function_type = .{
        .type_parameters = declaration,
        .params = .{.formal_parameters},
        .return_type = .{.ts_type_annotation},
    },
    .ts_constructor_type = .{
        .type_parameters = declaration,
        .params = .{.formal_parameters},
        .return_type = .{.ts_type_annotation},
    },
    .ts_type_predicate = .{
        .parameter_name = .{ .identifier_name, .ts_this_type },
        .type_annotation = annotation,
    },
    .ts_type_literal = .{ .members = signatures },
    .ts_mapped_type = .{
        .key = .{.binding_identifier},
        .name_type = .{.null},
        .type_annotation = .{.null},
    },
    .ts_property_signature = .{ .type_annotation = annotation },
    .ts_method_signature = signature,
    .ts_call_signature_declaration = signature,
    .ts_construct_signature_declaration = signature,
    .ts_index_signature = .{
        .parameters = .{.binding_identifier},
        .type_annotation = .{.ts_type_annotation},
    },
    .ts_type_alias_declaration = .{ .id = .{.binding_identifier}, .type_parameters = declaration },
    .ts_interface_declaration = .{
        .id = .{.binding_identifier},
        .type_parameters = declaration,
        .extends = .{.ts_interface_heritage},
        .body = .{.ts_interface_body},
    },
    .ts_interface_body = .{ .body = signatures },
    .ts_interface_heritage = .{ .type_arguments = instantiation },
    .ts_class_implements = .{ .type_arguments = instantiation },
    .ts_enum_declaration = .{ .id = .{.binding_identifier}, .body = .{.ts_enum_body} },
    .ts_enum_body = .{ .members = .{.ts_enum_member} },
    .ts_enum_member = .{
        .id = .{ .identifier_name, .string_literal, .template_literal },
        .initializer = .{.null},
    },
    .ts_module_declaration = .{
        .id = .{ .binding_identifier, .string_literal, .ts_qualified_name },
        .body = .{ .null, .ts_module_block },
    },
    .ts_global_declaration = .{ .id = .{.identifier_name}, .body = .{.ts_module_block} },
    .ts_parameter_property = .{
        .decorators = decorators,
        .parameter = .{ .binding_identifier, .assignment_pattern },
    },
    .ts_this_parameter = .{ .type_annotation = annotation },
    .ts_instantiation_expression = .{ .type_arguments = .{.ts_type_parameter_instantiation} },
    .ts_namespace_export_declaration = .{ .id = .{.identifier_name} },
    .ts_import_equals_declaration = .{
        .id = .{.binding_identifier},
        .module_reference = .{
            .ts_external_module_reference,
            .identifier_reference,
            .ts_qualified_name,
        },
    },
    .ts_external_module_reference = .{ .expression = .{.string_literal} },
    .jsx_element = .{
        .opening_element = .{.jsx_opening_element},
        .children = jsx_children,
        .closing_element = .{ .null, .jsx_closing_element },
    },
    .jsx_opening_element = .{
        .name = jsx_names,
        .type_arguments = instantiation,
        .attributes = .{ .jsx_attribute, .jsx_spread_attribute },
    },
    .jsx_closing_element = .{ .name = jsx_names },
    .jsx_fragment = .{
        .opening_fragment = .{.jsx_opening_fragment},
        .children = jsx_children,
        .closing_fragment = .{.jsx_closing_fragment},
    },
    .jsx_namespaced_name = .{ .namespace = .{.jsx_identifier}, .name = .{.jsx_identifier} },
    .jsx_member_expression = .{
        .object = .{ .jsx_identifier, .jsx_member_expression },
        .property = .{.jsx_identifier},
    },
    .jsx_attribute = .{
        .name = .{ .jsx_identifier, .jsx_namespaced_name },
        .value = .{
            .null,
            .string_literal,
            .jsx_expression_container,
            .jsx_element,
            .jsx_fragment,
        },
    },
};

pub const Field = struct {
    nullable: bool = false,
    /// Empty when any node is allowed.
    tags: []const Tag = &.{},

    pub inline fn allows(comptime field: Field, tag: u8) bool {
        if (field.tags.len == 0) return true;
        inline for (field.tags) |allowed| if (@backingInt(allowed) == tag) return true;
        return false;
    }
};

pub fn of(comptime tag: []const u8, comptime name: []const u8) Field {
    comptime {
        if (!@hasField(@TypeOf(fields), tag)) return .{};
        const rules = @field(fields, tag);
        if (!@hasField(@TypeOf(rules), name)) return .{};
        var field: Field = .{};
        for (@field(rules, name)) |kind| {
            if (kind == .null) {
                field.nullable = true;
            } else {
                field.tags = field.tags ++ &[_]Tag{kind};
            }
        }
        return field;
    }
}

comptime {
    @setEvalBranchQuota(100_000);
    for (@typeInfo(@TypeOf(fields)).@"struct".field_names) |tag| {
        std.debug.assert(@hasField(ast.NodeData, tag));
        const T = @FieldType(ast.NodeData, tag);
        for (@typeInfo(@TypeOf(@field(fields, tag))).@"struct".field_names) |name| {
            const Child = @FieldType(T, name);
            std.debug.assert(Child == ast.NodeIndex or Child == ast.IndexRange);
        }
    }
}
