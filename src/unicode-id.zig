const tables = @import("unicode-id-tables.zig");

const chunk_size = 512;
const bits_per_word = 32;
const leaf_chunk_width = 16;

pub fn canStartIdentifier(cp: u32) bool {
    if (cp < 128) {
        return (cp >= 'a' and cp <= 'z') or
            (cp >= 'A' and cp <= 'Z') or
            cp == '_' or cp == '$';
    }

    return queryBitTable(cp, tables.id_start_root, tables.id_start_leaf);
}

pub fn canContinueIdentifier(cp: u32) bool {
    if (cp < 128) {
        return (cp >= 'a' and cp <= 'z') or
            (cp >= 'A' and cp <= 'Z') or
            cp == '_' or cp == '$' or
            (cp >= '0' and cp <= '9');
    }

    return queryBitTable(cp, tables.id_continue_root, tables.id_continue_leaf);
}

inline fn queryBitTable(cp: u32, root: []const u8, leaf: []const u64) bool {
    const chunk_idx = cp / chunk_size;
    const leaf_base = @as(u32, root[chunk_idx]) * leaf_chunk_width;
    const offset_in_chunk = cp - (chunk_idx * chunk_size);
    const word_idx = leaf_base + (offset_in_chunk / bits_per_word);
    const bit_position: u5 = @truncate(offset_in_chunk % bits_per_word);
    const word = leaf[word_idx];
    return (word >> bit_position) & 1 == 1;
}
