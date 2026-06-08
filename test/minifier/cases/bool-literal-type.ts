export type Flagged = { readonly isFresh: true };

export function isFreshType(): Flagged {
  return { isFresh: true };
}

export function run() {
  return isFreshType();
}
