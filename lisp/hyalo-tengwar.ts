#!/usr/bin/env bun
/**
 * Tengwar transliteration server using Tecendil engine
 */

import { createInterface } from "readline";
import { transcribe as tecendilTranscribe, loadMode, toFont } from "./tecendil/tecendil-wrapper.mjs";

const args = process.argv.slice(2);
const isServer = args.includes("--server");

async function transcribe(word: string, language?: string, format?: string, mode?: string): Promise<string> {
    const targetFont = format === "csur" ? "TengwarTelcontar" : "TengwarAnnatar";
    
    // Handle *tinco prefix for tengwar literals
    if (word.startsWith("*")) {
        const literal = `{${word.slice(1).toLowerCase()}}`;
        return toFont(literal, targetFont);
    }
    
    // Handle {{tinco}}[modifier] double-brace literals with optional tehta
    const doubleBraceMatch = word.match(/^\{\{([^}]+)\}\}(\[[^\]]+\])?$/);
    if (doubleBraceMatch) {
        const tehta = doubleBraceMatch[2] || "";
        const literal = `{${doubleBraceMatch[1].toLowerCase()}}${tehta}`;
        return toFont(literal, targetFont);
    }
    
    // Handle {tinco}[modifier] single-brace literals with optional tehta
    const singleBraceMatch = word.match(/^\{([^}]+)\}(\[[^\]]+\])?$/);
    if (singleBraceMatch) {
        const tehta = singleBraceMatch[2] || "";
        const literal = `{${singleBraceMatch[1].toLowerCase()}}${tehta}`;
        return toFont(literal, targetFont);
    }
    
    // Normal Transcription - mode file handles logograms via "words" section
    const lang = language || "english";
    const modeData = await loadMode(lang);
    if (!modeData) return "";
    
    // Tecendil engine handles preprocessing, r-rules, literals, and vowel placement
    const literals = await tecendilTranscribe(word, modeData);
    
    return toFont(literals, targetFont);
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
