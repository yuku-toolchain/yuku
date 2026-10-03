const core = @import("types/core.zig");
const object = @import("types/object.zig");
const generics = @import("types/generics.zig");
const predicate = @import("types/predicate.zig");
const arrows = @import("arrows.zig");
const ts_expressions = @import("expressions.zig");

pub const parseTypeAliasBody = core.parseTypeAliasBody;
pub const extendQualifiedName = core.extendQualifiedName;

pub const parseTypeParameters = generics.parseTypeParameters;
pub const parseTypeArguments = generics.parseTypeArguments;
pub const isAngleOpen = generics.isAngleOpen;

pub const parseTypeAnnotation = predicate.parseTypeAnnotation;
pub const parseReturnTypeAnnotation = predicate.parseReturnTypeAnnotation;
pub const parsePatternTypeAnnotation = predicate.parsePatternTypeAnnotation;
pub const applyDecoratorsToPattern = predicate.applyDecoratorsToPattern;
pub const markPatternOptional = predicate.markPatternOptional;
pub const checkDefiniteAssignment = predicate.checkDefiniteAssignment;

pub const parseObjectTypeMembers = object.parseObjectTypeMembers;
pub const isIndexSignatureStart = object.isIndexSignatureStart;
pub const parseIndexSignature = object.parseIndexSignature;

pub const classifyArrowHead = arrows.classifyArrowHead;
pub const parseArrow = arrows.parseArrow;
pub const tryParseArrow = arrows.tryParseArrow;
pub const tryParseGenericArrow = arrows.tryParseGenericArrow;

pub const parseTypeAssertion = ts_expressions.parseTypeAssertion;
pub const parseAsOrSatisfiesExpression = ts_expressions.parseAsOrSatisfiesExpression;
pub const parseNonNullExpression = ts_expressions.parseNonNullExpression;
pub const parseTypeArgumentedCallOrInstantiation =
    ts_expressions.parseTypeArgumentedCallOrInstantiation;
pub const tryParseTypeArgumentsInExpression = ts_expressions.tryParseTypeArgumentsInExpression;
