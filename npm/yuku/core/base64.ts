const ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

export function decodeBase64(text: string): Uint8Array<ArrayBuffer> {
  const values = new Uint8Array(128);
  for (let i = 0; i < ALPHABET.length; i++) values[ALPHABET.charCodeAt(i)] = i;
  const padding = text.endsWith("==") ? 2 : text.endsWith("=") ? 1 : 0;
  const bytes = new Uint8Array((text.length / 4) * 3 - padding);
  for (let i = 0, j = 0; i < text.length; i += 4, j += 3) {
    const quad =
      (values[text.charCodeAt(i)]! << 18) |
      (values[text.charCodeAt(i + 1)]! << 12) |
      (values[text.charCodeAt(i + 2)]! << 6) |
      values[text.charCodeAt(i + 3)]!;
    bytes[j] = quad >> 16;
    bytes[j + 1] = quad >> 8;
    bytes[j + 2] = quad;
  }
  return bytes;
}
