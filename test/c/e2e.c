#include "yuku-c.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void walk_recursive(const uint8_t* buf, const char* source, uint32_t idx, int depth);

static const char* node_type_name(YukuNodeType t) {
    switch (t) {
        case YUKU_NODE_program: return "Program";
        case YUKU_NODE_expression_statement: return "ExpressionStatement";
        case YUKU_NODE_variable_declaration: return "VariableDeclaration";
        case YUKU_NODE_variable_declarator: return "VariableDeclarator";
        case YUKU_NODE_binding_identifier: return "BindingIdentifier";
        case YUKU_NODE_identifier_reference: return "IdentifierReference";
        case YUKU_NODE_binary_expression: return "BinaryExpression";
        case YUKU_NODE_call_expression: return "CallExpression";
        case YUKU_NODE_member_expression: return "MemberExpression";
        case YUKU_NODE_function: return "Function";
        case YUKU_NODE_function_body: return "FunctionBody";
        case YUKU_NODE_block_statement: return "BlockStatement";
        case YUKU_NODE_return_statement: return "ReturnStatement";
        case YUKU_NODE_string_literal: return "StringLiteral";
        case YUKU_NODE_numeric_literal: return "NumericLiteral";
        case YUKU_NODE_boolean_literal: return "BooleanLiteral";
        case YUKU_NODE_null_literal: return "NullLiteral";
        case YUKU_NODE_identifier_name: return "IdentifierName";
        case YUKU_NODE_formal_parameters: return "FormalParameters";
        case YUKU_NODE_formal_parameter: return "FormalParameter";
        default: return NULL;
    }
}

static void print_source_slice(const char* source, uint32_t start, uint32_t end) {
    printf("%.*s", end - start, source + start);
}

static void walk(const uint8_t* buf, const char* source, uint32_t idx, int depth) {
    if (yuku_is_null(idx)) return;
    YukuNodeType type = yuku_node_type(buf, idx);
    uint32_t start = yuku_node_span_start(buf, idx);
    uint32_t end = yuku_node_span_end(buf, idx);

    for (int i = 0; i < depth; i++) printf("  ");

    const char* name = node_type_name(type);
    if (name) {
        printf("%s", name);
    } else {
        printf("Node(%d)", type);
    }
    printf(" [");
    print_source_slice(source, start, end);
    printf("]\n");

    // for identifier/string nodes, also print the resolved name
    switch (type) {
        case YUKU_NODE_binding_identifier:
        case YUKU_NODE_identifier_reference:
        case YUKU_NODE_identifier_name:
        case YUKU_NODE_label_identifier:
        case YUKU_NODE_private_identifier: {
            YukuString s;
            switch (type) {
                case YUKU_NODE_binding_identifier: s = YUKU_binding_identifier_name(buf, idx); break;
                case YUKU_NODE_identifier_reference: s = YUKU_identifier_reference_name(buf, idx); break;
                case YUKU_NODE_identifier_name: s = YUKU_identifier_name_name(buf, idx); break;
                case YUKU_NODE_label_identifier: s = YUKU_label_identifier_name(buf, idx); break;
                case YUKU_NODE_private_identifier: s = YUKU_private_identifier_name(buf, idx); break;
                default: s = (YukuString){0, 0}; break;
            }
            size_t len;
            const char* ptr = yuku_str_ptr(source, buf, s, &len);
            for (int i = 0; i < depth + 1; i++) printf("  ");
            printf("-> \"%.*s\"\n", (int)len, ptr);
            break;
        }
        case YUKU_NODE_string_literal: {
            YukuString s = YUKU_string_literal_value(buf, idx);
            size_t len;
            const char* ptr = yuku_str_ptr(source, buf, s, &len);
            for (int i = 0; i < depth + 1; i++) printf("  ");
            printf("-> \"%.*s\"\n", (int)len, ptr);
            break;
        }
        default:
            break;
    }
}

static void walk_children(const uint8_t* buf, const char* source, uint32_t idx, int depth) {
    if (yuku_is_null(idx)) return;
    YukuNodeType type = yuku_node_type(buf, idx);

    switch (type) {
        case YUKU_NODE_program: {
            uint32_t s = YUKU_program_body_START(buf, idx);
            uint32_t len = YUKU_program_body_LEN(buf, idx);
            for (uint32_t i = 0; i < len; i++)
                walk_recursive(buf, source, yuku_extra(buf, s, i), depth);
            break;
        }
        case YUKU_NODE_expression_statement: {
            walk_recursive(buf, source, YUKU_expression_statement_expression(buf, idx), depth);
            break;
        }
        case YUKU_NODE_variable_declaration: {
            uint32_t s = YUKU_variable_declaration_declarators_START(buf, idx);
            uint32_t len = YUKU_variable_declaration_declarators_LEN(buf, idx);
            for (uint32_t i = 0; i < len; i++)
                walk_recursive(buf, source, yuku_extra(buf, s, i), depth);
            break;
        }
        case YUKU_NODE_variable_declarator: {
            walk_recursive(buf, source, YUKU_variable_declarator_id(buf, idx), depth);
            walk_recursive(buf, source, YUKU_variable_declarator_init(buf, idx), depth);
            break;
        }
        case YUKU_NODE_binary_expression: {
            walk_recursive(buf, source, YUKU_binary_expression_left(buf, idx), depth);
            walk_recursive(buf, source, YUKU_binary_expression_right(buf, idx), depth);
            break;
        }
        case YUKU_NODE_call_expression: {
            walk_recursive(buf, source, YUKU_call_expression_callee(buf, idx), depth);
            uint32_t s = YUKU_call_expression_arguments_START(buf, idx);
            uint32_t len = YUKU_call_expression_arguments_LEN(buf, idx);
            for (uint32_t i = 0; i < len; i++)
                walk_recursive(buf, source, yuku_extra(buf, s, i), depth);
            break;
        }
        case YUKU_NODE_function: {
            walk_recursive(buf, source, YUKU_function_id(buf, idx), depth);
            walk_recursive(buf, source, YUKU_function_params(buf, idx), depth);
            walk_recursive(buf, source, YUKU_function_body(buf, idx), depth);
            break;
        }
        case YUKU_NODE_block_statement: {
            uint32_t s = YUKU_block_statement_body_START(buf, idx);
            uint32_t len = YUKU_block_statement_body_LEN(buf, idx);
            for (uint32_t i = 0; i < len; i++)
                walk_recursive(buf, source, yuku_extra(buf, s, i), depth);
            break;
        }
        case YUKU_NODE_function_body: {
            uint32_t s = YUKU_function_body_body_START(buf, idx);
            uint32_t len = YUKU_function_body_body_LEN(buf, idx);
            for (uint32_t i = 0; i < len; i++)
                walk_recursive(buf, source, yuku_extra(buf, s, i), depth);
            break;
        }
        case YUKU_NODE_return_statement: {
            walk_recursive(buf, source, YUKU_return_statement_argument(buf, idx), depth);
            break;
        }
        case YUKU_NODE_formal_parameters: {
            uint32_t s = YUKU_formal_parameters_items_START(buf, idx);
            uint32_t len = YUKU_formal_parameters_items_LEN(buf, idx);
            for (uint32_t i = 0; i < len; i++)
                walk_recursive(buf, source, yuku_extra(buf, s, i), depth);
            break;
        }
        default:
            break;
    }
}

static void walk_recursive(const uint8_t* buf, const char* source, uint32_t idx, int depth) {
    if (yuku_is_null(idx)) return;
    walk(buf, source, idx, depth);
    walk_children(buf, source, idx, depth + 1);
}

int main(int argc, char** argv) {
    const char* filename = (argc > 1) ? argv[1] : "test/parser/misc/js/async-function.js";
    FILE* f = fopen(filename, "rb");
    if (!f) {
        fprintf(stderr, "cannot open %s\n", filename);
        return 1;
    }
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);
    char* source = (char*)malloc(fsize + 1);
    fread(source, 1, fsize, f);
    source[fsize] = 0;
    fclose(f);

    printf("parsing: %s (%ld bytes)\n\n", filename, fsize);

    YukuResult result = yuku_parse(source, fsize, YUKU_SOURCE_MODULE, YUKU_LANG_JS);
    if (!result.buf) {
        fprintf(stderr, "parse failed (allocation error?)\n");
        free(source);
        return 1;
    }

    printf("buffer size: %zu bytes\n", result.size);
    printf("nodes: %u\n", yuku_header_node_count(result.buf));
    printf("comments: %u\n", yuku_header_comment_count(result.buf));
    printf("diagnostics: %u\n", yuku_header_diagnostic_count(result.buf));
    if (result.has_errors) printf("** has parse errors **\n");
    printf("\n");

    uint32_t root = yuku_header_program_index(result.buf);
    walk_recursive(result.buf, source, root, 0);

    // print diagnostics if any
    uint32_t diag_count = yuku_header_diagnostic_count(result.buf);
    for (uint32_t i = 0; i < diag_count; i++) {
        YukuDiagnostic d;
        yuku_diagnostic(result.buf, i, &d);
        fprintf(stderr, "diag[%u]: %.*s\n", i, d.message_len, d.message);
    }

    yuku_free(&result);
    free(source);
    printf("\nOK\n");
    return 0;
}
