const core = @import("types/core.zig");
const literal = @import("types/literal.zig");
const object = @import("types/object.zig");
const generics = @import("types/generics.zig");
const predicate = @import("types/predicate.zig");
const arrows = @import("arrows.zig");
const ts_expressions = @import("expressions.zig");

pub const parseType = core.parseType;
pub const parseTypeAliasBody = core.parseTypeAliasBody;
pub const isStartOfType = core.isStartOfType;
pub const extendQualifiedName = core.extendQualifiedName;

pub const parseTypeParameters = generics.parseTypeParameters;
pub const parseTypeArguments = generics.parseTypeArguments;
pub const isAngleOpen = generics.isAngleOpen;

pub const parseTypeAnnotation = predicate.parseTypeAnnotation;
pub const parseReturnTypeAnnotation = predicate.parseReturnTypeAnnotation;
pub const parseTypeOrTypePredicate = predicate.parseTypeOrTypePredicate;
pub const applyTypeAnnotationToPattern = predicate.applyTypeAnnotationToPattern;
pub const applyDecoratorsToPattern = predicate.applyDecoratorsToPattern;
pub const markPatternOptional = predicate.markPatternOptional;

pub const parseTypeLiteral = object.parseTypeLiteral;
pub const parseObjectTypeMembers = object.parseObjectTypeMembers;
pub const isStartOfMappedType = object.isStartOfMappedType;
pub const parseMappedType = object.parseMappedType;
pub const isIndexSignatureStart = object.isIndexSignatureStart;
pub const IndexSignatureModifiers = object.IndexSignatureModifiers;
pub const parseIndexSignature = object.parseIndexSignature;

pub const ArrowHead = arrows.ArrowHead;
pub const classifyArrowHead = arrows.classifyArrowHead;
pub const parseArrow = arrows.parseArrow;
pub const tryParseArrow = arrows.tryParseArrow;
pub const tryParseGenericArrow = arrows.tryParseGenericArrow;

pub const parseTypeAssertion = ts_expressions.parseTypeAssertion;
pub const parseAsOrSatisfiesExpression = ts_expressions.parseAsOrSatisfiesExpression;
pub const parseNonNullExpression = ts_expressions.parseNonNullExpression;
pub const parseTypeArgumentedCallOrInstantiation = ts_expressions.parseTypeArgumentedCallOrInstantiation;
pub const tryParseTypeArgumentsInExpression = ts_expressions.tryParseTypeArgumentsInExpression;

pub const parseLiteralType = literal.parseLiteralType;
pub const parseTemplateLiteralType = literal.parseTemplateLiteralType;
