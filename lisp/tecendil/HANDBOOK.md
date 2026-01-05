# Tengwar Handbook

This handbook demonstrates the Tengwar writing system using `hyalo-tengwar-partial-mode`.
Text within `@@...@@` delimiters will be rendered in Tengwar script when the mode is active.

Use `{{name}}` inside the delimiters to render specific tengwar literally (e.g., `@@{{tinco}}@@`).

Based on the [Tecendil Tengwar Handbook](https://www.tecendil.com/tengwar-handbook/).

---

## Introduction

The Tengwar is a writing system created by J.R.R. Tolkien. It is also known as the
Fëanorian system, from the name of its (fictional) inventor.

**Important:** Tengwar is not a language. It is a writing system that can be used
to write many languages, including Quenya, Sindarin, and English.

### Modes

A Tengwar mode is a set of conventions used to transcribe a specific language.
Here is the same sentence transcribed using different modes:

- **English orthographic:** @@Not all those who wander are lost@@
- **English phonemic:** @@Not ool dhouz huu wonder ar lost@@
- **Sindarin (Gondor):** @@Law ill idh reniar gwenwin@@

---

## Tengwar Letters

A Tengwar glyph is called a *tengwa* (plural: *tengwar*).

### The Primary Tengwar

The shape of the tengwa corresponds to the distinctive feature of the sound it represents.

| Tengwa | Name | Sound |
|--------|------|-------|
| @@{{tinco}}@@ | tinco | /t/ |
| @@{{parma}}@@ | parma | /p/ |
| @@{{calma}}@@ | calma | /k/ or /ch/ |
| @@{{quesse}}@@ | quesse | /kw/ or /k/ |
| @@{{ando}}@@ | ando | /d/ |
| @@{{umbar}}@@ | umbar | /b/ |
| @@{{anga}}@@ | anga | /g/ or /ng/ |
| @@{{ungwe}}@@ | ungwe | /gw/ or /g/ |
| @@{{thuule}}@@ | thúle | /th/ (voiceless) |
| @@{{formen}}@@ | formen | /f/ |
| @@{{harma}}@@ | harma | /ch/ or /sh/ |
| @@{{hwesta}}@@ | hwesta | /ch/ or /hw/ |
| @@{{anto}}@@ | anto | /th/ (voiced) |
| @@{{ampa}}@@ | ampa | /v/ |
| @@{{anca}}@@ | anca | /j/ or /zh/ |
| @@{{unque}}@@ | unque | /gh/ |
| @@{{nuumen}}@@ | númen | /n/ |
| @@{{malta}}@@ | malta | /m/ |
| @@{{noldo}}@@ | noldo | /nd/ |
| @@{{nwalme}}@@ | nwalme | /ng/ |
| @@{{oore}}@@ | óre | /r/ (final) |
| @@{{vala}}@@ | vala | /w/ |
| @@{{anna}}@@ | anna | /y/ |
| @@{{vilya}}@@ | vilya | /v/ or silent |
| @@{{roomen}}@@ | rómen | /r/ (initial) |
| @@{{arda}}@@ | arda | /rd/ |
| @@{{lambe}}@@ | lambe | /l/ |
| @@{{alda}}@@ | alda | /ld/ |
| @@{{silme}}@@ | silme | /s/ |
| @@{{silme-nuquerna}}@@ | silme nuquerna | /s/ |
| @@{{esse}}@@ | esse | /z/ |
| @@{{esse-nuquerna}}@@ | esse nuquerna | /z/ |
| @@{{hyarmen}}@@ | hyarmen | /h/ |
| @@{{hwesta-sindarinwa}}@@ | hwesta sindarinwa | /wh/ |
| @@{{yanta}}@@ | yanta | /y/ |
| @@{{uure}}@@ | úre | /w/ |

### Extended Tengwar

Some tengwar have an extended form with the stem extending above and below:

| Tengwa | Name | Usage |
|--------|------|-------|
| @@{{extended-tinco}}@@ | extended tinco | ordinal suffix (1st, 2nd) |
| @@{{extended-parma}}@@ | extended parma | /ph/ → /f/ |
| @@{{extended-calma}}@@ | extended calma | /kh/ → /k/ |
| @@{{extended-quesse}}@@ | extended quesse | /ch/ → /k/ |
| @@{{extended-ando}}@@ | extended ando | "the" shorthand |
| @@{{extended-umbar}}@@ | extended umbar | "of" shorthand |
| @@{{extended-anga}}@@ | extended anga | ??? |
| @@{{extended-ungwe}}@@ | extended ungwe | silent /gh/ |

### Carriers

If there is no suitable tengwa available, a "carrier" is used:

| Carrier | Name | Usage |
|---------|------|-------|
| @@{{telco}}@@ | telco (short carrier) | short vowel |
| @@{{aara}}@@ | ára (long carrier) | long vowel |

### Punctuation

| Mark | Usage |
|------|-------|
| @@{{pusta}}@@ | comma |
| @@{{double-pusta}}@@ | period |
| @@{{triple-pusta}}@@ | colon |
| @@{{exclamation-mark}}@@ | exclamation |
| @@{{question-mark}}@@ | question |

### Numbers

Numbers are typically written in base-12 (duodecimal), least significant digit first.

| Number | Tengwar |
|--------|---------|
| 0 | @@0@@ |
| 1 | @@1@@ |
| 2 | @@2@@ |
| 3 | @@3@@ |
| 4 | @@4@@ |
| 5 | @@5@@ |
| 6 | @@6@@ |
| 7 | @@7@@ |
| 8 | @@8@@ |
| 9 | @@9@@ |
| 10 | @@10@@ |
| 11 | @@11@@ |
| 12 | @@12@@ |
| 144 | @@144@@ |
| 1st | @@1st@@ |
| 2nd | @@2nd@@ |

---

## The English Mode

The English orthographic mode preserves the peculiarities of English orthography,
for example, ph or gh for a /f/ sound in "phone" or "cough".

### The Consonants

| Letter | Tengwa | Example |
|--------|--------|---------|
| b | @@{{umbar}}@@ | @@book@@ |
| c | @@{{quesse}}@@ | @@cat@@ |
| d | @@{{ando}}@@ | @@dog@@ |
| f | @@{{formen}}@@ | @@fish@@ |
| g | @@{{ungwe}}@@ | @@go@@ |
| h | @@{{hyarmen}}@@ | @@hat@@ |
| j | @@{{anga}}@@ | @@jump@@ |
| k | @@{{quesse}}@@ | @@king@@ |
| l | @@{{lambe}}@@ | @@love@@ |
| m | @@{{malta}}@@ | @@man@@ |
| n | @@{{nuumen}}@@ | @@no@@ |
| p | @@{{parma}}@@ | @@pen@@ |
| q | @@{{quesse}}@@ | @@queen@@ |
| r | @@{{oore}}@@ / @@{{roomen}}@@ | @@run@@ |
| s | @@{{silme}}@@ | @@sun@@ |
| t | @@{{tinco}}@@ | @@top@@ |
| v | @@{{ampa}}@@ | @@van@@ |
| w | @@{{vala}}@@ | @@win@@ |
| x | @@x@@ | @@box@@ |
| y | @@{{anna}}@@ | @@yes@@ |
| z | @@{{esse}}@@ | @@zoo@@ |

### Digraphs

| Digraph | Tengwa | Example |
|---------|--------|---------|
| ch | @@{{calma}}@@ | @@church@@ |
| sh | @@{{harma}}@@ | @@ship@@ |
| th (soft) | @@{{thuule}}@@ | @@thing@@ |
| th (hard) | @@{{anto}}@@ | @@this@@ |
| ng | @@{{nwalme}}@@ | @@ring@@ |
| wh | @@{{hwesta-sindarinwa}}@@ | @@white@@ |
| ph | @@{{extended-parma}}@@ | @@phone@@ |
| gh | @@{{unque}}@@ | @@ghost@@ |

### Doubled Consonants

Doubled consonants are indicated by a bar below the tengwa:

| Example | Tengwar |
|---------|---------|
| bb | @@bb@@ |
| dd | @@dd@@ |
| ff | @@ff@@ |
| gg | @@gg@@ |
| ll | @@ll@@ |
| mm | @@mm@@ |
| nn | @@nn@@ |
| pp | @@pp@@ |
| rr | @@rr@@ |
| ss | @@ss@@ |
| tt | @@tt@@ |

### Nasalized Consonants

Nasalized consonants (nt, mp, nk) have a bar above:

| Example | Tengwar |
|---------|---------|
| nt | @@nt@@ |
| mp | @@mp@@ |
| nd | @@nd@@ |
| mb | @@mb@@ |
| nc / nk | @@nk@@ |

### The Vowels

In this mode, vowels are represented by diacritics (tehtar) on the following consonant:

| Vowel | Tehta | Example |
|-------|-------|---------|
| a | @@{{telco}[triple-dot-above]@@ | @@cat@@ |
| e | @@{{telco}[acute]@@ | @@pet@@ |
| i | @@{{telco}[dot-above]@@ | @@sit@@ |
| o | @@{{telco}[right-curl]@@ | @@dog@@ |
| u | @@{{telco}[left-curl]@@ | @@cup@@ |

### Silent E

Silent final ⟨e⟩ is indicated by a dot below the preceding consonant:

| Word | Tengwar |
|------|---------|
| make | @@make@@ |
| time | @@time@@ |
| love | @@love@@ |
| bone | @@bone@@ |
| pure | @@pure@@ |

### Diphthongs

Vowel diphthongs use special combinations:

| Diphthong | Example |
|-----------|---------|
| ai / ay | @@main@@ @@day@@ |
| ei / ey | @@their@@ @@they@@ |
| oi / oy | @@coin@@ @@boy@@ |
| ou | @@found@@ |
| au | @@cause@@ |
| eu | @@feud@@ |
| iu | @@iu@@ |
| ui | @@quit@@ |

### The Letter Y

When ⟨y⟩ is a consonant: @@yes@@ (uses anna)

When ⟨y⟩ is a vowel: @@my@@ @@happy@@ (uses breve tehta)

### The R-Rule

**Rómen** @@{{roomen}}@@ is used before vowels (except final silent-e).

**Óre** @@{{oore}}@@ is used before consonants and at the end of words.

| Word | Tengwar |
|------|---------|
| run | @@run@@ |
| car | @@car@@ |
| far | @@far@@ |
| more | @@more@@ |

### Shorthand

Common words have shorthand representations:

| Word | Shorthand |
|------|-----------|
| the | @@the@@ |
| of | @@of@@ |
| and | @@and@@ |
| of the | @@"of the" rings@@ |

### Sample Words

| English | Tengwar |
|---------|---------|
| friend | @@friend@@ |
| ring | @@ring@@ |
| middle | @@middle@@ |
| earth | @@earth@@ |
| darkness | @@darkness@@ |
| shadow | @@shadow@@ |
| fire | @@fire@@ |
| water | @@water@@ |
| mountain | @@mountain@@ |
| valley | @@valley@@ |

### Sample Sentences

**The lord of the rings:**
@@The lord of the rings@@

**The ring of the elves:**
@@The ring of the elves@@

**One ring to rule them all:**
@@One ring to rule them all@@

**In the darkness bind them:**
@@In the darkness bind them@@

**You shall not pass:**
@@You shall not pass@@

---

## Test Your Mode

Try placing your cursor inside any `@@...@@` block - the original text should appear.
Move the cursor out, and the Tengwar transliteration should render again.

---

## Exercices

### English Mode: Writing 1

- mom: `@@mom@@`
- dad: `@@dad@@`
- cat: `@@cat@@`
- dog: `@@dog@@`
- orc: `@@orc@@`
- fig: `@@fig@@`
- big: `@@big@@`
- bug: `@@bug@@`
- bag: `@@bag@@`
- gold: `@@gold@@`
- gap: `@@gap@@`
- car: `@@car@@`
- ark: `@@ark@@`
- my: `@@my@@`
- was: `@@was@@`

### English Mode: Writing 2

- all: `@@all@@`
- wet: `@@wet@@`
- paw: `@@paw@@`
- zoo: `@@zoo@@` #FIXME
- toe: `@@toe@@`
- now: `@@now@@`
- then: `@@then@@`
- there: `@@there@@`
- look: `@@look@@` #FIXME
- good: `@@good@@` #FIXME
- only: `@@only@@`
- hole: `@@hole@@`
- from: `@@from@@`

## English Mode: Writing 3

- the: `@@the@@`
- of: `@@of@@`
- with: `@@with@@`
- one: `@@one@@`
- time: `@@time@@`
- think: `@@think@@`
- make: `@@make@@`
- ant: `@@ant@@`
- day: `@@day@@`
- bye: `@@bye@@`
- ink: `@@ink@@`
- mix: `@@mix@@`
- her: `@@her@@`
- ray: `@@ray@@`

## English Mode: Writing 4

- ball: `@@ball@@`
- door: `@@door@@` #FIXME
- house: `@@house@@`
- know: `@@know@@`
- again: `@@again@@`
- before: `@@before@@`
- when: `@@when@@`
- away: `@@away@@`
- eyes: `@@eyes@@`
- Frodo: `@@Frodo@@`
- Gandalf: `@@Gandalf@@`
- Aragorn: `@@Aragorn@@`
- hobbits: `@@hobbits@@`
- Gimli: `@@Gimli@@`
- Legolas: `@@Legolas@@`
- rings: `@@rings@@`

## English Mode: Reading


- @@In a hole in the ground there lived a hobbit@@
- **In a hole in the ground there lived a hobbit**

- @@You shall not pass@@
- **You shall not pass**

- @@It's the job that never started that takes the longest to finish@@
- **It's the job that never started that takes the longest to finish**

- @@Home is behind, the world ahead@@
- **Home is behind, the world ahead**

- @@All you have to decide is what to do with the time that is given to you@@
- **All you have to decide is what to do with the time that is given to you**

- @@A wizard is never late. Nor is he early. He arrives precisely when he means to@@
- **A wizard is never late. Nor is he early. He arrives precisely when he means to**

- @@Even the smallest person can change the course of the future@@
- **Even the smallest person can change the course of the future**

- @@One does not simply walk into Mordor@@
- **One does not simply walk into Mordor**

- @@Moonlight drowns out all but the brightest stars@@
- **Moonlight drowns out all but the brightest stars**

- @@But in the end it's only a passing thing, this shadow; even darkness must pass@@
- **But in the end it's only a passing thing, this shadow; even darkness must pass**

- @@His old life lay behind in the mists, dark adventure lay in front@@
- **His old life lay behind in the mists, dark adventure lay in front**
