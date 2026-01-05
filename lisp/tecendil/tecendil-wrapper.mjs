/**
 * Tecendil Wrapper - Non-invasive wrapper for upstream tecendil.mjs
 *
 * This wrapper isolates the upstream tecendil.mjs from our code,
 * making it easy to update tecendil.mjs without breaking our implementation.
 */

import { transcribe, loadMode, toFont as originalToFont } from './tecendil.mjs';

/**
 * Fix corrupted Japanese characters in dansmith-alt font output.
 *
 * Problem: tecendil.mjs has corrupted font tables where doubled tehtar
 * (double-right-curl, double-left-curl, double-acute) output Japanese
 * hiragana instead of correct TengwarAnnatar characters.
 *
 * Corrupted mappings in G (dansmith-alt):
 *   double-right-curl: ["\u305E","\u3059","\u3048","\u304E"] (ぞすえぎ)
 *   double-left-curl:  ["\u3026","\u3055","\u304A","\u304D"] (ゆさおき)
 *   double-acute:      ["\u3024","\u3052","\u3046","\u3056"] (ゆげうざ)
 *
 * Correct values based on base font q patterns:
 *   right-curl: ["^","Y","H","N"], left-curl: ["&","U","J","M"], acute: ["$","R","F","V"]
 *   Doubled versions use two characters per position.
 */
const japaneseToTengwarFixes = {
    // double-right-curl fixes (based on right-curl ["^","Y","H","N"])
    // Pattern: position N uses char[N] + char[N+1] for visual separation
    "\u305E": "^Y",  // position 0 (wide-tengwa) - ぞ → pos0 + pos1
    "\u3059": "YH",  // position 1 (tengwa) - す → pos1 + pos2
    "\u3048": "HN",  // position 2 (extended-tengwa) - え → pos2 + pos3
    "\u304E": "NN",  // position 3 (narrow-tengwa) - ぎ → pos3 + pos3
    // double-left-curl fixes (based on left-curl ["&","U","J","M"])
    "\u3026": "&U",  // position 0 (wide-tengwa) - ゆ → pos0 + pos1
    "\u3055": "UJ",  // position 1 (tengwa) - さ → pos1 + pos2
    "\u304A": "JM",  // position 2 (extended-tengwa) - お → pos2 + pos3
    "\u304D": "MM",  // position 3 (narrow-tengwa) - き → pos3 + pos3
    // double-acute fixes (based on acute ["$","R","F","V"])
    "\u3024": "$R",  // position 0 (wide-tengwa) - ゆ variant → pos0 + pos1
    "\u3052": "RF",  // position 1 (tengwa) - げ → pos1 + pos2
    "\u3046": "FV",  // position 2 (extended-tengwa) - う → pos2 + pos3
    "\u3056": "VV",  // position 3 (narrow-tengwa) - ざ → pos3 + pos3
};

function fixCorruptedJapanese(output) {
    let result = output;
    for (const [japanese, tengwar] of Object.entries(japaneseToTengwarFixes)) {
        result = result.replaceAll(japanese, tengwar);
    }
    return result;
}

/**
 * Wrapped toFont that fixes:
 * 1. CSUR font uppercase issue (E080-E0FF -> E000-E07F)
 * 2. Corrupted Japanese characters in dansmith-alt font
 */
function toFont(literals, fontName) {
    // CSUR fonts that need unicode-lowercase encoding (E000-E07F only)
    const csurFontsNeedingLowercase = [
        "TengwarTelcontar"
    ];

    const mappedFont = csurFontsNeedingLowercase.includes(fontName)
        ? "FreeMonoTengwar"
        : fontName;

    let result = originalToFont(literals, mappedFont);

    // Fix corrupted Japanese characters for TengwarAnnatar (dansmith-alt encoding)
    if (fontName === "TengwarAnnatar" || fontName === "TengwarAnnatarBold" ||
        fontName === "Eldamar" || fontName === "Parmaite") {
        result = fixCorruptedJapanese(result);
    }

    return result;
}

export { transcribe, loadMode, toFont };
