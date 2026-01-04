/**
 * Tecendil Wrapper - Non-invasive wrapper for upstream tecendil.mjs
 *
 * This wrapper isolates the upstream tecendil.mjs from our code,
 * making it easy to update tecendil.mjs without breaking our implementation.
 */

import { transcribe, loadMode, toFont as originalToFont } from './tecendil.mjs';

/**
 * Wrapped toFont that fixes CSUR font uppercase issue.
 *
 * Problem: tecendil's "unicode" encoding (TengwarTelcontar) uses E080-E0FF
 * for uppercase letters, but most CSUR fonts only have glyphs at E000-E07F.
 *
 * Fix: Map CSUR fonts to use "unicode-lowercase" encoding (FreeMonoTengwar)
 * which only outputs E000-E07F codepoints for all letters.
 */
function toFont(literals, fontName) {
    // CSUR fonts that need unicode-lowercase encoding (E000-E07F only)
    const csurFontsNeedingLowercase = [
        "TengwarTelcontar"
    ];

    const mappedFont = csurFontsNeedingLowercase.includes(fontName)
        ? "FreeMonoTengwar"
        : fontName;

    return originalToFont(literals, mappedFont);
}

export { transcribe, loadMode, toFont };
