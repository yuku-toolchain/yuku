// the design inspired by: https://github.com/dtolnay/unicode-ident

const tables = @import("unicode-id-tables.zig");

/// Checks if a unicode code point is the valid identifier start
pub fn canStartIdentifier(cp: u32) bool {
    if (cp < 128) {
            return (cp >= 'a' and cp <= 'z') or
                (cp >= 'A' and cp <= 'Z') or
                cp == '_' or cp == '$';
        }

    const chunk_number = cp / 512;

    const chunk_offset = @as(u32, tables.id_start_root[chunk_number]) * 16;

    const c = cp - (chunk_number * 512);

    const piece_offset = chunk_offset + (c / 32);

    const bitpos_in_piece: u5 = @truncate(c % 32);

    const piece = tables.id_start_leaf[piece_offset];

    return (piece >> bitpos_in_piece) & 1 == 1;
}

/// Checks if a unicode code point is the valid identifier continuation
pub fn canContinueIdentifier(cp: u32) bool {
    if (cp < 128) {
            return (cp >= 'a' and cp <= 'z') or
                (cp >= 'A' and cp <= 'Z') or
                cp == '_' or cp == '$' or  // ← Add this!
                (cp >= '0' and cp <= '9');
        }

    const chunk_number = cp / 512;

    const chunk_offset = @as(u32, tables.id_continue_root[chunk_number]) * 16;

    const c = cp - (chunk_number * 512);

    const piece_offset = chunk_offset + (c / 32);

    const bitpos_in_piece: u5 = @truncate(c % 32);

    const piece = tables.id_continue_leaf[piece_offset];

    return (piece >> bitpos_in_piece) & 1 == 1;
}
