#!/usr/bin/env bun
// NOTE: This script runs under bun, not tsc. IDE type errors are expected.
/**
 * Tengwar transliteration server
 * 
 * Modes:
 *   --server    Persistent mode: reads JSON lines from stdin, writes JSON lines to stdout
 *   (default)   One-shot mode: processes args and exits
 * 
 * Protocol (server mode):
 *   Input:  {"id": "...", "words": ["word1", "word2", ...], "language": "english", "format": "ascii"|"csur"}
 *   Output: {"id": "...", "results": ["trans1", "trans2", ...]}
 */

import { makeOptions } from "tengwar/general-use";
import generalUse from "tengwar/general-use";
import classical from "tengwar/classical";
import beleriand from "tengwar/beleriand";
import { createInterface } from "readline";

const args = process.argv.slice(2);
const isServer = args.includes("--server");

// Dan Smith ASCII to CSUR Unicode mapping
const DAN_TO_CSUR: Record<string, string> = {
    "1": "\uE000", "q": "\uE001", "a": "\uE002", "z": "\uE003",
    "2": "\uE004", "w": "\uE005", "s": "\uE006", "x": "\uE007",
    "3": "\uE008", "e": "\uE009", "d": "\uE00A", "c": "\uE00B",
    "4": "\uE00C", "r": "\uE00D", "f": "\uE00E", "v": "\uE00F",
    "5": "\uE010", "t": "\uE011", "g": "\uE012", "b": "\uE013",
    "6": "\uE014", "y": "\uE015", "h": "\uE016", "n": "\uE017",
    "7": "\uE018", "u": "\uE019", "j": "\uE01A", "m": "\uE01B",
    "8": "\uE01C", "i": "\uE01D", "k": "\uE01E", ",": "\uE01F",
    "9": "\uE020", "o": "\uE021", "l": "\uE022", ".": "\uE023",
    "½": "\uE024", "`": "\uE025", "~": "\uE026", "]": "\uE027",
    "!": "\uE000", "Q": "\uE001", "A": "\uE002", "Z": "\uE003",
    "@": "\uE004", "W": "\uE005", "S": "\uE006", "X": "\uE007",
    "#": "\uE040", "E": "\uE040", "D": "\uE040", "C": "\uE040", // a (3 dots)
    "$": "\uE046", "R": "\uE046", "F": "\uE046", "V": "\uE046", // e (acute)
    "%": "\uE044", "T": "\uE044", "G": "\uE044", "B": "\uE044", // i (dot)
    "^": "\uE04A", "Y": "\uE04A", "H": "\uE04A", "N": "\uE04A", // o (right curl)
    "&": "\uE04C", "U": "\uE04C", "J": "\uE04C", "M": "\uE04C", // u (left curl)
    "P": "\uE048", "p": "\uE048", // tilde/bar above
    ":": "\uE04E", ";": "\uE04E", // tilde/bar below
    "-": "\uE061", "=": "\uE060", // punctuation
    " ": " "
};

function convertToCSUR(ascii: string): string {
    return ascii.split("").map(c => DAN_TO_CSUR[c] || c).join("");
}

function getModeModule(modeName: string) {
    switch (modeName?.toLowerCase()) {
        case "classical": return classical;
        case "beleriand": return beleriand;
        case "general-use":
        default: return generalUse;
    }
}

function transcribe(word: string, language?: string, format?: string, mode?: string): string {
    const modeModule = getModeModule(mode || "general-use");
    const options: any = { plain: true };
    if (language) options.language = language;
    
    const result = modeModule.transcribe(word, options);
    return format === "csur" ? convertToCSUR(result) : result;
}

interface Request {
    id: string;
    words: string[];
    language?: string;
    format?: string;
    mode?: string;
}

interface Response {
    id: string;
    results: string[];
    error?: string;
}

async function runServer() {
    const rl = createInterface({
        input: process.stdin,
        output: process.stdout,
        terminal: false
    });

    // Signal ready
    console.log(JSON.stringify({ ready: true }));

    for await (const line of rl) {
        if (!line.trim()) continue;
        
        try {
            const req: Request = JSON.parse(line);
            const results = req.words.map(w => transcribe(w, req.language, req.format, req.mode));
            const resp: Response = { id: req.id, results };
            console.log(JSON.stringify(resp));
        } catch (e) {
            const error = e instanceof Error ? e.message : String(e);
            console.log(JSON.stringify({ id: "error", error }));
        }
    }
}

function runOneShot() {
    let modeName = "general-use";
    let format = "ascii";
    let language: string | undefined = undefined;
    let isJson = false;
    let text = "";

    for (let i = 0; i < args.length; i++) {
        if (args[i] === "--mode" && args[i + 1]) {
            modeName = args[++i];
        } else if (args[i] === "--format" && args[i + 1]) {
            format = args[++i];
        } else if (args[i] === "--language" && args[i + 1]) {
            language = args[++i];
        } else if (args[i] === "--json") {
            isJson = true;
        } else {
            text += args[i] + " ";
        }
    }
    text = text.trim();

    if (!text) {
        console.error("Usage: bun hyalo-tengwar.ts [--mode <mode>] [--format <fmt>] [--language <lang>] [--json] <text>");
        process.exit(1);
    }

    try {
        if (isJson) {
            const inputs = JSON.parse(text);
            if (Array.isArray(inputs)) {
                const results = inputs.map(w => transcribe(w, language, format, modeName));
                console.log(JSON.stringify(results));
            } else {
                console.error("JSON input must be an array of strings.");
                process.exit(1);
            }
        } else {
            console.log(transcribe(text, language, format, modeName));
        }
    } catch (e) {
        console.error("Error:", e);
        process.exit(1);
    }
}

// Entry point
if (isServer) {
    runServer();
} else {
    runOneShot();
}
