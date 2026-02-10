// implement semantic analysis
// scopes
// symbol tables
// etc etc
//
// TODO:
//
// Redeclaration checks.
// It is a Syntax Error if FunctionBodyContainsUseStrict of FunctionBody is true and IsSimpleParameterList of FormalParameters is false.
// It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the LexicallyDeclaredNames of FunctionBody.
// It is a Syntax Error if FormalParameters Contains SuperProperty is true.
// It is a Syntax Error if FunctionBody Contains SuperProperty is true.
// It is a Syntax Error if FormalParameters Contains SuperCall is true.
// It is a Syntax Error if FormalParameters Contains YieldExpression is true.
// It is a Syntax Error if FormalParameters Contains AwaitExpression is true.
// It is a Syntax Error if FunctionBody Contains SuperCall is true.
// 'evals' and 'arguments' in binding identifier and identifier reference.
// Reserved checks: https://tc39.es/ecma262/#prod-ReservedWord
// It is a Syntax Error if HasDirectSuper of MethodDefinition is true.
// It is a Syntax Error if ConciseBodyContainsUseStrict of ConciseBody is true and IsSimpleParameterList of ArrowParameters is false.
// It is a Syntax Error if any element of the BoundNames of ArrowParameters also occurs in the LexicallyDeclaredNames of ConciseBody.
// (delete unary) It is a Syntax Error if IsStrict(the UnaryExpression) is true and the derived UnaryExpression is PrimaryExpression : IdentifierReference , MemberExpression : MemberExpression . PrivateIdentifier , CallExpression : CallExpression . PrivateIdentifier , OptionalChain : ?. PrivateIdentifier , or OptionalChain : OptionalChain . PrivateIdentifier .
// (delete unary) It is a Syntax Error if the derived UnaryExpression is
// PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
// and CoverParenthesizedExpressionAndArrowParameterList ultimately derives a phrase that, if used in place of UnaryExpression, would produce a Syntax Error according to these rules. This rule is recursively applied.
// Note
// (delete unary) The last rule means that expressions such as delete (((foo))) produce early errors because of recursive application of the first rule.
// ImportMeta :
//  import.meta
//  It is a Syntax Error if the syntactic goal symbol is not Module.
// It is a Syntax Error if this BreakStatement is not nested, directly or indirectly (but not crossing function or static initialization block boundaries), within an IterationStatement or a SwitchStatement.
// It is a Syntax Error if this ContinueStatement is not nested, directly or indirectly (but not crossing function or static initialization block boundaries), within an IterationStatement.
// It is a Syntax Error if any element of the BoundNames of ForDeclaration also occurs in the VarDeclaredNames of Statement.
// It is a Syntax Error if the BoundNames of ForDeclaration contains any duplicate entries.
// In strict mode code, functions can only be declared at top level or inside a block
//
// ImportDeclaration:
// - It is a Syntax Error if the BoundNames of ImportDeclaration contains any duplicate entries.
//
// WithClause:
// - It is a Syntax Error if WithClauseToAttributes of WithClause has two different entries a and b such that a.[[Key]] is b.[[Key]].
//
// ExportDeclaration:
// - It is a Syntax Error if the ExportedNames of ModuleItemList contains any duplicate entries.
// - For each IdentifierName n in the ReferencedBindings of NamedExports:
//   It is a Syntax Error if the StringValue of n is a ReservedWord or the StringValue of n is one of
//   "implements", "interface", "let", "package", "private", "protected", "public", or "static".
//   Note: This is already checked in parser during export parsing for local exports without 'from'.
//
// module-level semantic checks:
// - It is a Syntax Error if the LexicallyDeclaredNames of ModuleItemList contains any duplicate entries.
// - It is a Syntax Error if any element of the LexicallyDeclaredNames of ModuleItemList also occurs in the VarDeclaredNames of ModuleItemList.
// - It is a Syntax Error if the ExportedBindings of ModuleItemList does not also occur in the VarDeclaredNames of ModuleItemList,
//   or the LexicallyDeclaredNames of ModuleItemList, or the ImportedLocalNames of ModuleItemList.
// - It is a Syntax Error if ModuleItemList Contains super.
// - It is a Syntax Error if ModuleItemList Contains NewTarget (except in functions).
// 'let' is reserved in strict mode code.
// export statements cannot be outside of a module.
// 'default' case cannot appear more than once in a switch statement.
// for-in/of loop variable declaration may not have an initializer
// 'with' statement is not allowed in strict mode
// '0'-prefixed octal literals in strict mode
// Decimals with leading zeros are not allowed in strict mode
// Octal escapes are not allowed in strict mode

// fn octalInString(raw: []const u8) bool {
//     var i: usize = 0;
//     while (i + 1 < raw.len) {
//         if (raw[i] != '\\') {
//             i += 1;
//             continue;
//         }

//         const escaped = raw[i + 1];
//         switch (escaped) {
//             '1'...'7', '8', '9' => return true,
//             '0' => {
//                 if (i + 2 < raw.len and isOctalDigit(raw[i + 2])) return true;
//             },
//             else => {},
//         }

//         i += 2;
//     }

//     return false;
// }
