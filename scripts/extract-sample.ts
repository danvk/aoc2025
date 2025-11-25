#!/usr/bin/env -S deno run --allow-read --allow-write
import {
  DOMParser,
} from "https://deno.land/x/deno_dom@v0.1.36-alpha/deno-dom-wasm.ts";
// import { assert } from "../util.ts";

// assert(Deno.args.length === 2);
const [inputFile, outputDir] = Deno.args;
const contents = await Deno.readTextFile(inputFile);
const doc = new DOMParser().parseFromString(contents, "text/html");
// assert(doc);

const claimed = new Set()
const samples = doc.querySelectorAll("pre > code");
for (let i = 0; i < samples.length; i++) {
  const sample = samples[i];
  const fileName = outputDir + '/sample' + (samples.length > 1 ? (i+1) : '') + '.txt';
  console.log(fileName, '\n', sample.textContent, '\n');
  Deno.writeTextFileSync(fileName, sample.textContent);
  claimed.add(sample)
}

let isFirst = true
const codes = doc.querySelectorAll("code");
for (const code of codes) {
  if (!claimed.has(code)) {
    if (isFirst) {
      console.log('unclaimed <code> elements:')
      isFirst = false
    }
    console.log(code.textContent)
  }
}
