#!/usr/bin/env bun
/**
 * Tengwar transliteration server using Tecendil engine
 */

import { createInterface } from "readline";
import { transcribe as tecendilTranscribe, loadMode, toFont } from "./tecendil/tecendil.mjs";

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
    "#": "\uE040", "E": "\uE040", "D": "\uE040", "C": "\uE040",
    "$": "\uE046", "R": "\uE046", "F": "\uE046", "V": "\uE046",
    "%": "\uE044", "T": "\uE044", "G": "\uE044", "B": "\uE044",
    "^": "\uE04A", "Y": "\uE04A", "H": "\uE04A", "N": "\uE04A",
    "&": "\uE04C", "U": "\uE04C", "J": "\uE04C", "M": "\uE04C",
    "P": "\uE048", "p": "\uE048",
    ":": "\uE04E", ";": "\uE04E",
    "-": "\u0061", "=": "\u0060",
    " ": " ",
    // Add missing mappings for numbers/punctuation if tecendil outputs them
    "\u00F0": "\uE070", "\u00F1": "\uE071", "\u00F2": "\uE072", "\u00F3": "\uE073", 
    "\u00F4": "\uE074", "\u00F5": "\uE075", "\u00F6": "\uE076", "\u00F7": "\uE077", 
    "\u00F8": "\uE078", "\u00F9": "\uE079", "\u00FA": "\uE07A", "\u00FB": "\uE07B",
    "\u00BA": "\uE060", // Roman Period usually .
    "\u00A9": "\uE061", // Roman Comma usually ,
    "\u00C1": "\uE065", // Exclamation
    "\u00C0": "\uE066"  // Question
};

function convertToCSUR(ascii: string): string {
    return ascii.split("").map(c => DAN_TO_CSUR[c] || c).join("");
}

async function transcribe(word: string, language?: string, format?: string, mode?: string): Promise<string> {
    const lang = language || "english";
    const modeData = await loadMode(lang);
    if (!modeData) return "";
    
    const literals = await tecendilTranscribe(word, modeData);
    // Convert literals to Dan Smith font mapping (TengwarAnnatar)
    const danSmith = toFont(literals, "TengwarAnnatar");
    
    return format === "csur" ? convertToCSUR(danSmith) : danSmith;
}

interface Request {
    id: string;
    words: string[];
    language?: string;
    format?: string;
    mode?: string;
}

async function runServer() {
    // Pre-load English mode
    await loadMode("english");

    const rl = createInterface({
        input: process.stdin,
        output: process.stdout,
        terminal: false
    });

    console.log(JSON.stringify({ ready: true }));

    for await (const line of rl) {
        if (!line.trim()) continue;
        try {
            const req: Request = JSON.parse(line);
            const results = await Promise.all(req.words.map(w => transcribe(w, req.language, req.format, req.mode)));
            console.log(JSON.stringify({ id: req.id, results }));
        } catch (e) {
            console.log(JSON.stringify({ id: "error", error: String(e) }));
        }
    }
}

async function runOneShot() {
    let modeName = "general-use";
    let format = "ascii";
    let language: string | undefined = undefined;
    let isJson = false;
    let text = "";

    for (let i = 0; i < args.length; i++) {
        if (args[i] === "--mode" && args[i + 1]) modeName = args[++i];
        else if (args[i] === "--format" && args[i + 1]) format = args[++i];
        else if (args[i] === "--language" && args[i + 1]) language = args[++i];
        else if (args[i] === "--json") isJson = true;
        else text += args[i] + " ";
    }
    text = text.trim();

    if (!text) {
        console.error("Usage: bun hyalo-tengwar.ts [--mode <mode>] [--format <fmt>] [--language <lang>] [--json] <text>");
        process.exit(1);
    }

    try {
        if (isJson) {
            const inputs = JSON.parse(text);
            const results = await Promise.all(inputs.map((w: string) => transcribe(w, language, format, modeName)));
            console.log(JSON.stringify(results));
        } else {
            console.log(await transcribe(text, language, format, modeName));
        }
    } catch (e) {
        console.error("Error:", e);
        process.exit(1);
    }
}

if (isServer) runServer();
else runOneShot();
