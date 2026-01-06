;;; hyalo-tengwar-tutorial.el --- Interactive Tengwar reading tutorial -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026
;; Author: Julien Wintz <julien.wintz@inria.fr>
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: education, tengwar

;;; Commentary:

;; An interactive tutorial for learning to read Tengwar in English mode.
;; Progressive curriculum from vowels through consonants, digraphs, and special rules.
;; Monkeytype-inspired centered layout with 5x scaled Tengwar display.
;;
;; Usage:
;;   M-x hyalo/tengwar-tutorial      - Start or resume tutorial
;;   M-x hyalo/tengwar-tutorial-reset - Reset progress and start fresh
;;
;; The tutorial follows the HANDBOOK order:
;;   1. Vowels (tehtar)
;;   2. Primary Tengwar (consonants in groups of 4)
;;   3. Extended Tengwar
;;   4. Carriers
;;   5. Digraphs
;;   6. Doubled consonants
;;   7. Nasalized consonants
;;   8. Labialized consonants
;;   9. Diphthongs
;;  10. Numbers
;;  11. Special rules (R-rule, silent-e, y-vowel, shorthand)
;;  12. Word exercises
;;  13. LotR sentence reading

;;; Code:

(require 'cl-lib)
(require 'hyalo-tengwar)

(defgroup hyalo-tengwar-tutorial nil
  "Tengwar reading tutorial."
  :group 'hyalo-tengwar
  :prefix "hyalo-tengwar-tutorial-")

;;; ============================================================================
;;; Persistence
;;; ============================================================================

(defcustom hyalo-tengwar-tutorial-progress-file
  (expand-file-name "tengwar-tutorial-progress.el" user-emacs-directory)
  "File storing tutorial progress."
  :type 'file
  :group 'hyalo-tengwar-tutorial)

(defvar hyalo-tengwar-tutorial--state nil
  "Current tutorial state.
A plist with keys:
  :current-lesson - index of current lesson
  :completed-lessons - list of completed lesson IDs
  :learned-symbols - list of symbol IDs learned so far
  :session-history - list of combinations shown this session (to avoid repeats)")

(defun hyalo-tengwar-tutorial--save-progress ()
  "Save tutorial progress to file."
  (when hyalo-tengwar-tutorial--state
    (with-temp-file hyalo-tengwar-tutorial-progress-file
      (insert ";;; Tengwar Tutorial Progress -*- lexical-binding: t -*-\n")
      (insert ";; Auto-generated, do not edit manually\n\n")
      (prin1 hyalo-tengwar-tutorial--state (current-buffer))
      (insert "\n"))))

(defun hyalo-tengwar-tutorial--load-progress ()
  "Load tutorial progress from file."
  (if (file-exists-p hyalo-tengwar-tutorial-progress-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents hyalo-tengwar-tutorial-progress-file)
            (goto-char (point-min))
            (setq hyalo-tengwar-tutorial--state (read (current-buffer))))
        (error
         (message "Could not load progress: %s" (error-message-string err))
         (hyalo-tengwar-tutorial--init-state)))
    (hyalo-tengwar-tutorial--init-state)))

(defun hyalo-tengwar-tutorial--init-state ()
  "Initialize fresh tutorial state."
  (setq hyalo-tengwar-tutorial--state
        (list :current-lesson 0
              :completed-lessons nil
              :learned-symbols nil
              :session-history nil)))

;;; ============================================================================
;;; Curriculum Data
;;; ============================================================================

;; Each lesson: (:id :title :type :items :explanation :exercise-words)
;; Types: intro (just explanation), glyphs (learn new symbols), rule (special rule)
;; Items: list of (:glyph :name :sound :example :accepts)
;;   - :glyph is the tengwar name for {{...}} syntax
;;   - :accepts is list of acceptable answers
;;
;; IMPORTANT: Exercise words must ONLY use letters/rules introduced up to that lesson!
;; - No doubled consonants before Phase 6
;; - No ck before Phase 3 (Extended Tengwar)
;; - No silent-e before Phase 11
;; - Each lesson's words are manually curated to respect this constraint
;;
;; TODO: Future enhancement - dynamic word lookup from a dictionary with
;; constraint-based filtering (letters available, rules introduced)

(defconst hyalo-tengwar-tutorial--curriculum
  `(
    ;; =========================================================================
    ;; PHASE 1: VOWELS (Tehtar)
    ;; =========================================================================
    (:id vowels-intro
     :title "Introduction to Vowels"
     :type intro
     :explanation
     "In English Tengwar mode, vowels are written as diacritics (tehtar) placed above
the following consonant. When a vowel stands alone or at the end of a word,
it is placed on a carrier symbol called 'telco' (the short carrier)."
     :items nil)

    (:id vowels
     :title "The Five Vowels"
     :type glyphs
     :explanation
     "Each vowel has a distinct tehta (Tolkien's term for diacritic mark):
• A - three dots arranged in a triangle (triple-dot-above)
• E - an acute accent pointing up (acute)
• I - a single dot (dot-above)
• O - a curl opening to the right (right-curl)
• U - a curl opening to the left (left-curl)

Note: 'tehta' (plural 'tehtar') is the correct term, not 'theta'."
     :items
     ((:glyph "{{telco}[triple-dot-above]" :name "a (tehta)" :sound "/a/" :example "a in cat" :accepts ("a"))
      (:glyph "{{telco}[acute]" :name "e (tehta)" :sound "/e/" :example "e in pet" :accepts ("e"))
      (:glyph "{{telco}[dot-above]" :name "i (tehta)" :sound "/i/" :example "i in sit" :accepts ("i"))
      (:glyph "{{telco}[right-curl]" :name "o (tehta)" :sound "/o/" :example "o in dog" :accepts ("o"))
      (:glyph "{{telco}[left-curl]" :name "u (tehta)" :sound "/u/" :example "u in cup" :accepts ("u")))
     :exercise-words nil)  ;; No word exercises - consonants not learned yet

    ;; =========================================================================
    ;; PHASE 2: PRIMARY TENGWAR (Groups of 4)
    ;; RULES: No doubled consonants, no silent-e, no ck until taught
    ;; =========================================================================

    ;; Available: a e i o u + t p c k (NO: ch, ck, doubled, silent-e)
    (:id primary-1
     :title "Primary Tengwar: T, P, CH, C/K"
     :type glyphs
     :explanation
     "The first row of Tengwar. The shape encodes sound features:
• Tinco (t) - voiceless alveolar stop
• Parma (p) - voiceless bilabial stop
• Quesse (c/k) - used for c, k, and q sounds
• Calma (ch) - the 'church' sound (digraph)"
     :items
     ((:glyph "{{tinco}}" :name "tinco" :sound "/t/" :example "top" :accepts ("t" "tinco"))
      (:glyph "{{parma}}" :name "parma" :sound "/p/" :example "pup" :accepts ("p" "parma"))
      (:glyph "{{calma}}" :name "calma" :sound "/ch/" :example "chap" :accepts ("ch" "calma"))
      (:glyph "{{quesse}}" :name "quesse" :sound "/c/ or /k/" :example "cat" :accepts ("c" "k" "q" "quesse")))
     ;; Words: a e i o u t p c k ch (NO: ck, doubled, silent-e)
     :exercise-words ("at" "it" "up" "cup" "cat" "cut" "pat" "pit" "pot" "tap" "tip" "top" "cap" "pup" "chap" "chip" "chop"))

    ;; Available: + d b g j
    (:id primary-2
     :title "Primary Tengwar: D, B, J, G"
     :type glyphs
     :explanation
     "The voiced counterparts of the first row:
• Ando (d) - voiced version of tinco
• Umbar (b) - voiced version of parma
• Anga (j) - the 'jump' sound
• Ungwe (g) - hard g as in 'go'"
     :items
     ((:glyph "{{ando}}" :name "ando" :sound "/d/" :example "dog" :accepts ("d" "ando"))
      (:glyph "{{umbar}}" :name "umbar" :sound "/b/" :example "bud" :accepts ("b" "umbar"))
      (:glyph "{{anga}}" :name "anga" :sound "/j/" :example "jab" :accepts ("j" "anga"))
      (:glyph "{{ungwe}}" :name "ungwe" :sound "/g/" :example "gab" :accepts ("g" "ungwe")))
     ;; Words: + d b g j (NO: ck, doubled, silent-e)
     :exercise-words ("dog" "bad" "bag" "big" "bit" "bud" "but" "cub" "dab" "dig" "dot" "dug" "gab" "got" "gut" "jab" "jig" "job" "jog" "jug" "tab" "tub" "tug"))

    ;; Available: + th f sh (hwesta)
    (:id primary-3
     :title "Primary Tengwar: TH, F, SH, CH/K"
     :type glyphs
     :explanation
     "The aspirated consonants:
• Thúle (th) - voiceless 'th' as in 'thing' (not 'this')
• Formen (f) - the 'f' sound
• Harma (sh) - the 'sh' sound as in 'ship'
• Hwesta - variant ch/k sound, as in 'echo'"
     :items
     ((:glyph "{{thuule}}" :name "thúle" :sound "/th/ (voiceless)" :example "thud" :accepts ("th" "thule" "thuule"))
      (:glyph "{{formen}}" :name "formen" :sound "/f/" :example "fig" :accepts ("f" "formen"))
      (:glyph "{{harma}}" :name "harma" :sound "/sh/" :example "shag" :accepts ("sh" "harma"))
      (:glyph "{{hwesta}}" :name "hwesta" :sound "/ch/, /k/" :example "echo" :accepts ("ch" "k" "hwesta")))
     ;; Words: + th f sh (NO: m n s h z w y r l; no doubled)
     ;; Available: a e i o u t p ch c k d b j g th f sh
     :exercise-words ("fat" "fig" "fit" "fog" "fad" "fib" "fob" "fug" "thud" "thug" "shag" "ship" "shop" "shot" "shod" "shut" "gush" "bash" "cash" "dash" "gash" "josh" "posh" "tush"))

    ;; Available: + th(voiced) v (anca, unque rare)
    (:id primary-4
     :title "Primary Tengwar: TH (voiced), V, ZH, GH"
     :type glyphs
     :explanation
     "More voiced consonants:
• Anto (th) - voiced 'th' as in 'this' (not 'thing')
• Ampa (v) - the 'v' sound
• Anca (zh) - rare sound in 'mirage'
• Unque (gh) - the 'gh' as in 'ghost'"
     :items
     ((:glyph "{{anto}}" :name "anto" :sound "/th/ (voiced)" :example "this" :accepts ("th" "anto"))
      (:glyph "{{ampa}}" :name "ampa" :sound "/v/" :example "vat" :accepts ("v" "ampa"))
      (:glyph "{{anca}}" :name "anca" :sound "/zh/" :example "mirage" :accepts ("zh" "j" "anca"))
      (:glyph "{{unque}}" :name "unque" :sound "/gh/" :example "ghost" :accepts ("gh" "unque")))
     ;; Words: + v th(voiced) (NO: n m w y r l s z h; no silent-e)
     ;; Available: a e i o u t p ch c k d b j g th f sh + v
     :exercise-words ("vat" "vet" "via" "diva" "java" "that" "vivid" "divot"))

    ;; Available: + n m (noldo, nwalme)
    (:id primary-5
     :title "Primary Tengwar: N, M, ND, NG"
     :type glyphs
     :explanation
     "The nasal consonants:
• Númen (n) - the 'n' sound
• Malta (m) - the 'm' sound
• Noldo (nd) - combined n+d sound
• Nwalme (ng) - the 'ng' sound as in 'sing'"
     :items
     ((:glyph "{{nuumen}}" :name "númen" :sound "/n/" :example "nap" :accepts ("n" "numen" "nuumen"))
      (:glyph "{{malta}}" :name "malta" :sound "/m/" :example "map" :accepts ("m" "malta"))
      (:glyph "{{noldo}}" :name "noldo" :sound "/nd/" :example "and" :accepts ("nd" "noldo"))
      (:glyph "{{nwalme}}" :name "nwalme" :sound "/ng/" :example "sing" :accepts ("ng" "nwalme")))
     ;; Words: + n m nd ng (NO: s, h, w, y, r, l; no doubled mm/nn)
     ;; Available: a e i o u t p ch c k d b j g th f sh v + n m nd ng
     :exercise-words ("man" "map" "mat" "men" "met" "mob" "mop" "mud" "mug" "nab" "nag" "nap" "nib" "nod" "not" "nut" "net" "and" "band" "end" "fang" "gang" "bang" "dang" "bung" "dung" "gung" "kung" "tung"))

    ;; Available: + w y (óre, vilya)
    (:id primary-6
     :title "Primary Tengwar: R (final), W, Y, V (silent)"
     :type glyphs
     :explanation
     "Semi-vowels and special consonants:
• Óre (r) - used for 'r' before consonants or at word end
• Vala (w) - the 'w' sound
• Anna (y) - the consonant 'y' as in 'yes'
• Vilya - rarely used, can be 'v' or silent"
     :items
     ((:glyph "{{oore}}" :name "óre" :sound "/r/ (final)" :example "bar" :accepts ("r" "ore" "oore"))
      (:glyph "{{vala}}" :name "vala" :sound "/w/" :example "wag" :accepts ("w" "vala"))
      (:glyph "{{anna}}" :name "anna" :sound "/y/" :example "yap" :accepts ("y" "anna"))
      (:glyph "{{vilya}}" :name "vilya" :sound "/v/ or silent" :example "—" :accepts ("v" "vilya")))
     ;; Words: + w y r(final) (NO: s, h, x as singles; no doubled, silent-e)
     :exercise-words ("wag" "wan" "war" "way" "web" "wed" "wet" "wig" "win" "wit" "wok" "won" "wop" "yak" "yam" "yap" "yaw" "yob"))

    ;; Available: + r(initial) l (arda, alda)
    (:id primary-7
     :title "Primary Tengwar: R (initial), RD, L, LL/LD"
     :type glyphs
     :explanation
     "The R-L tengwar:
• Rómen (r) - used for 'r' before vowels (not final silent-e)
• Arda (rd) - combined r+d sound
• Lambe (l) - the 'l' sound
• Alda (ll/ld) - doubled l or l+d (taught later)"
     :items
     ((:glyph "{{roomen}}" :name "rómen" :sound "/r/ (initial)" :example "run" :accepts ("r" "romen" "roomen"))
      (:glyph "{{arda}}" :name "arda" :sound "/rd/" :example "bard" :accepts ("rd" "arda"))
      (:glyph "{{lambe}}" :name "lambe" :sound "/l/" :example "lap" :accepts ("l" "lambe"))
      (:glyph "{{alda}}" :name "alda" :sound "/ll/ or /ld/" :example "old" :accepts ("ll" "ld" "alda")))
     ;; Words: + r l (NO: doubled ll, silent-e)
     :exercise-words ("lab" "lad" "lag" "lap" "lid" "lip" "lit" "log" "lot" "lug" "ran" "rat" "rib" "rid" "rig" "rip" "rob" "rod" "rot" "rug" "run" "rut"))

    ;; Available: + s z
    (:id primary-8
     :title "Primary Tengwar: S, Z (and variants)"
     :type glyphs
     :explanation
     "The sibilants - S and Z each have two forms:
• Silme (s) - normal upright form
• Silme Nuquerna (s) - inverted form, used when tehta goes above
• Esse (z) - normal form
• Esse Nuquerna (z) - inverted form"
     :items
     ((:glyph "{{silme}}" :name "silme" :sound "/s/" :example "sun" :accepts ("s" "silme"))
      (:glyph "{{silme-nuquerna}}" :name "silme nuquerna" :sound "/s/" :example "sit" :accepts ("s" "silme"))
      (:glyph "{{esse}}" :name "esse" :sound "/z/" :example "zap" :accepts ("z" "esse"))
      (:glyph "{{esse-nuquerna}}" :name "esse nuquerna" :sound "/z/" :example "zig" :accepts ("z" "esse")))
     ;; Words: + s z (NO: x, doubled ss, silent-e)
     :exercise-words ("sad" "sag" "sap" "sat" "set" "sip" "sit" "sob" "sod" "sop" "sub" "sum" "sun" "zag" "zap" "zig" "zip" "bus" "gas"))

    ;; Available: + h wh (yanta, úre)
    (:id primary-9
     :title "Primary Tengwar: H, WH, Y (alt), W (alt)"
     :type glyphs
     :explanation
     "The final primary tengwar:
• Hyarmen (h) - the 'h' sound
• Hwesta Sindarinwa (wh) - the 'wh' sound as in 'when'
• Yanta (y) - variant y, often in diphthongs
• Úre (w) - variant w, often in diphthongs"
     :items
     ((:glyph "{{hyarmen}}" :name "hyarmen" :sound "/h/" :example "hat" :accepts ("h" "hyarmen"))
      (:glyph "{{hwesta-sindarinwa}}" :name "hwesta sindarinwa" :sound "/wh/" :example "when" :accepts ("wh" "hwesta sindarinwa"))
      (:glyph "{{yanta}}" :name "yanta" :sound "/y/" :example "—" :accepts ("y" "yanta"))
      (:glyph "{{uure}}" :name "úre" :sound "/w/" :example "—" :accepts ("w" "ure" "uure")))
     ;; Words: + h wh (NO: silent-e)
     :exercise-words ("had" "hag" "ham" "has" "hat" "hem" "hen" "hid" "him" "hip" "his" "hit" "hob" "hog" "hop" "hot" "hub" "hug" "hum" "hut" "whim" "whip" "whir"))

    ;; =========================================================================
    ;; PHASE 3: EXTENDED TENGWAR (Row 1: voiceless)
    ;; =========================================================================
    (:id extended-1
     :title "Extended Tengwar: Row 1"
     :type glyphs
     :explanation
     "Extended tengwar have stems that extend both above and below.
The first row mirrors the primary voiceless consonants:
• Extended Tinco - for special t combinations
• Extended Parma - for 'ph' (as in 'phone')
• Extended Calma - for special ch combinations
• Extended Quesse - for 'ck' (as in 'back')"
     :items
     ((:glyph "{{extended-tinco}}" :name "extended tinco" :sound "/t/ (special)" :example "—" :accepts ("t" "extended tinco"))
      (:glyph "{{extended-parma}}" :name "extended parma" :sound "/ph/ → /f/" :example "graph" :accepts ("ph" "extended parma"))
      (:glyph "{{extended-calma}}" :name "extended calma" :sound "/ch/ (special)" :example "—" :accepts ("ch" "extended calma"))
      (:glyph "{{extended-quesse}}" :name "extended quesse" :sound "/ck/" :example "back" :accepts ("ck" "extended quesse")))
     ;; NOW we can use ck words!
     :exercise-words ("back" "hack" "jack" "lack" "pack" "rack" "sack" "tack" "duck" "luck" "muck" "suck" "tuck" "buck" "kick" "lick" "nick" "pick" "sick" "tick" "wick" "dock" "lock" "rock" "sock"))

    ;; =========================================================================
    ;; PHASE 3b: EXTENDED TENGWAR (Row 2: voiced + shorthands)
    ;; =========================================================================
    (:id extended-2
     :title "Extended Tengwar: Row 2"
     :type glyphs
     :explanation
     "The second row of extended tengwar, mirroring voiced consonants:
• Extended Ando - shorthand for 'the'
• Extended Umbar - shorthand for 'of'
• Extended Anga - for special j combinations
• Extended Ungwe - for special g combinations"
     :items
     ((:glyph "{{extended-ando}}" :name "extended ando" :sound "the (shorthand)" :example "the" :accepts ("the" "extended ando"))
      (:glyph "{{extended-umbar}}" :name "extended umbar" :sound "of (shorthand)" :example "of" :accepts ("of" "extended umbar"))
      (:glyph "{{extended-anga}}" :name "extended anga" :sound "/j/ (special)" :example "—" :accepts ("j" "extended anga"))
      (:glyph "{{extended-ungwe}}" :name "extended ungwe" :sound "/g/ (special)" :example "—" :accepts ("g" "extended ungwe")))
     :exercise-words ("the" "of"))

    ;; =========================================================================
    ;; PHASE 4: CARRIERS
    ;; =========================================================================
    (:id carriers
     :title "Carrier Symbols"
     :type glyphs
     :explanation
     "Carriers hold vowel tehtar when no consonant follows:
• Telco (short carrier) - a short vertical stroke, for regular vowels
• Ára (long carrier) - a longer stroke, for long/stressed vowels"
     :items
     ((:glyph "{{telco}}" :name "telco" :sound "(short carrier)" :example "a, e, i, o, u alone" :accepts ("carrier" "telco" "short carrier"))
      (:glyph "{{aara}}" :name "ára" :sound "(long carrier)" :example "long vowels" :accepts ("carrier" "ara" "aara" "long carrier")))
     :exercise-words nil)

    ;; =========================================================================
    ;; PHASE 5: DIGRAPHS (Review)
    ;; =========================================================================
    (:id digraphs
     :title "Digraphs Review"
     :type glyphs
     :explanation
     "These tengwar represent English digraphs (two letters → one sound):
• CH → Calma (church)
• SH → Harma (ship)
• TH (voiceless) → Thúle (thing)
• TH (voiced) → Anto (this)
• NG → Nwalme (ring)
• WH → Hwesta Sindarinwa (when)
• PH → Extended Parma (graph)
• CK → Extended Quesse (back)"
     :items
     ((:glyph "{{calma}}" :name "calma" :sound "ch" :example "chap" :accepts ("ch" "calma"))
      (:glyph "{{harma}}" :name "harma" :sound "sh" :example "shun" :accepts ("sh" "harma"))
      (:glyph "{{thuule}}" :name "thúle" :sound "th (voiceless)" :example "thud" :accepts ("th" "thule"))
      (:glyph "{{anto}}" :name "anto" :sound "th (voiced)" :example "this" :accepts ("th" "anto")))
     ;; NO: doubled consonants, silent-e
     :exercise-words ("chap" "chat" "chin" "chip" "chop" "chug" "shag" "sham" "shin" "ship" "shop" "shot" "shun" "shut" "thud" "thug" "this" "that" "than" "then" "them" "thus" "with"))

    ;; =========================================================================
    ;; PHASE 6: DOUBLED CONSONANTS
    ;; =========================================================================
    (:id doubled
     :title "Doubled Consonants"
     :type rule
     :explanation
     "Doubled consonants (bb, dd, ff, ll, mm, nn, pp, rr, ss, tt) are written
by adding a bar below the tengwa. You see the same letter shape, but with
an underline-like mark beneath it.

Example: 'buff' - the 'ff' is written as formen with a bar below."
     :items
     ((:glyph "{{formen}}[bar-below]" :name "doubled f (ff)" :sound "/ff/" :example "buff" :accepts ("ff" "double f"))
      (:glyph "{{lambe}}[bar-below]" :name "doubled l (ll)" :sound "/ll/" :example "full" :accepts ("ll" "double l"))
      (:glyph "{{silme}}[bar-below]" :name "doubled s (ss)" :sound "/ss/" :example "boss" :accepts ("ss" "double s"))
      (:glyph "{{tinco}}[bar-below]" :name "doubled t (tt)" :sound "/tt/" :example "butt" :accepts ("tt" "double t")))
     ;; NOW we can use doubled consonants!
     :exercise-words ("buff" "cuff" "huff" "muff" "puff" "riff" "biff" "doff" "dull" "full" "gull" "hull" "lull" "mull" "null" "pull" "bass" "boss" "fuss" "hiss" "joss" "loss" "mass" "miss" "moss"))

    ;; =========================================================================
    ;; PHASE 7: NASALIZED CONSONANTS
    ;; =========================================================================
    (:id nasalized
     :title "Nasalized Consonants"
     :type rule
     :explanation
     "When a nasal (n or m) precedes certain consonants, they combine with a
tilde or bar above the consonant tengwa:
• NT → tinco with tilde above
• MP → parma with tilde above
• ND → ando with tilde above
• MB → umbar with tilde above
• NC/NK → quesse with tilde above

This makes reading easier: look for the tilde above!"
     :items
     ((:glyph "{{tinco}}[tilde-above]" :name "nt" :sound "/nt/" :example "ant" :accepts ("nt"))
      (:glyph "{{parma}}[tilde-above]" :name "mp" :sound "/mp/" :example "bump" :accepts ("mp"))
      (:glyph "{{ando}}[tilde-above]" :name "nd (nasalized)" :sound "/nd/" :example "and" :accepts ("nd"))
      (:glyph "{{umbar}}[tilde-above]" :name "mb" :sound "/mb/" :example "lamb" :accepts ("mb")))
     :exercise-words ("ant" "bent" "dent" "hint" "hunt" "lent" "mint" "rent" "sent" "tent" "vent" "went" "bump" "camp" "damp" "dump" "jump" "lamp" "pump"))

    ;; =========================================================================
    ;; PHASE 8: LABIALIZED CONSONANTS
    ;; =========================================================================
    (:id labialized
     :title "Labialized Consonants"
     :type rule
     :explanation
     "Labialized consonants (QU, TW, DW) are written with a special over-twist
diacritic. The 'Q' in English is essentially 'KW', so 'quit' shows quesse
with the labialization mark. Similarly TW and DW."
     :items
     ((:glyph "{{quesse}}[over-twist]" :name "qu" :sound "/kw/" :example "quit" :accepts ("qu" "kw"))
      (:glyph "{{tinco}}[over-twist]" :name "tw" :sound "/tw/" :example "twin" :accepts ("tw"))
      (:glyph "{{ando}}[over-twist]" :name "dw" :sound "/dw/" :example "dwell" :accepts ("dw"))
      (:glyph "{{silme}}[over-twist]" :name "sw" :sound "/sw/" :example "swim" :accepts ("sw")))
     ;; NO: silent-e (quest has it)
     :exercise-words ("quit" "quip" "quid" "twin" "twig" "twit" "twig" "dwell" "swig" "swim"))

    ;; =========================================================================
    ;; PHASE 9: DIPHTHONGS
    ;; =========================================================================
    (:id diphthongs
     :title "Diphthongs"
     :type rule
     :explanation
     "Diphthongs (two vowel sounds blending) have special representations:
• AI/AY - a-tehta + anna (or yanta)
• OI/OY - o-tehta + anna
• OU/OW - o-tehta + vala (or úre)
• AU/AW - a-tehta + vala

Look for the vowel tehta followed by anna (y) or vala/úre (w)."
     :items
     ((:glyph "{{anna}}[triple-dot-above]" :name "ay/ai" :sound "/eɪ/" :example "day" :accepts ("ay" "ai"))
      (:glyph "{{anna}}[right-curl]" :name "oy/oi" :sound "/ɔɪ/" :example "boy" :accepts ("oy" "oi"))
      (:glyph "{{vala}}[right-curl]" :name "ow/ou" :sound "/aʊ/" :example "cow" :accepts ("ow" "ou"))
      (:glyph "{{vala}}[triple-dot-above]" :name "aw/au" :sound "/ɔː/" :example "jaw" :accepts ("aw" "au")))
     ;; NO: silent-e
     :exercise-words ("day" "bay" "gay" "hay" "jay" "lay" "may" "nay" "pay" "ray" "say" "way" "boy" "coy" "joy" "soy" "toy" "cow" "bow" "how" "now" "row" "sow" "vow" "wow" "jaw" "law" "paw" "raw" "saw"))

    ;; =========================================================================
    ;; PHASE 10: NUMBERS
    ;; =========================================================================
    (:id numbers
     :title "Tengwar Numbers"
     :type glyphs
     :explanation
     "Tengwar numbers are written in base-12 (duodecimal), with the least
significant digit first (reversed from English). Each digit 0-11 has its
own symbol. Ordinals (1st, 2nd) use 'extended tinco' as a suffix.

For now, recognize that numbers have distinct circular/angular shapes
different from the letter tengwar."
     :items
     ((:glyph "{{zero}}" :name "zero" :sound "0" :example "0" :accepts ("0" "zero"))
      (:glyph "{{one}}" :name "one" :sound "1" :example "1" :accepts ("1" "one"))
      (:glyph "{{two}}" :name "two" :sound "2" :example "2" :accepts ("2" "two"))
      (:glyph "{{three}}" :name "three" :sound "3" :example "3" :accepts ("3" "three")))
     :exercise-words nil)

    ;; =========================================================================
    ;; PHASE 11: SPECIAL RULES
    ;; =========================================================================
    (:id rule-silent-e
     :title "The Silent E Rule"
     :type rule
     :explanation
     "Silent final 'e' (as in 'make', 'time') is indicated by a dot below
the preceding consonant. This is NOT a vowel tehta - it's underneath,
not above.

When you see a dot below a consonant at the end of a word, that consonant
is followed by a silent 'e'. Now you can read words like: give, dive, love!"
     :items
     ((:glyph "{{tinco}}[dot-below]" :name "t + silent e" :sound "/t_e/" :example "bake" :accepts ("te" "t-e"))
      (:glyph "{{quesse}}[dot-below]" :name "k + silent e" :sound "/k_e/" :example "make" :accepts ("ke" "k-e"))
      (:glyph "{{ampa}}[dot-below]" :name "v + silent e" :sound "/v_e/" :example "give" :accepts ("ve" "v-e"))
      (:glyph "{{nuumen}}[dot-below]" :name "n + silent e" :sound "/n_e/" :example "tone" :accepts ("ne" "n-e")))
     ;; NOW we can use silent-e words!
     :exercise-words ("bake" "cake" "fake" "lake" "make" "take" "wake" "bike" "hike" "like" "pike" "dime" "lime" "time" "bone" "cone" "lone" "tone" "zone" "cube" "tube" "dune" "tune" "give" "dive" "live" "five" "hive"))

    (:id rule-r
     :title "The R Rule"
     :type rule
     :explanation
     "English has two 'r' tengwar:
• Rómen - used when 'r' comes BEFORE a vowel (run, ring)
• Óre - used when 'r' comes BEFORE a consonant or at word END (car, far)

Exception: Rómen is NOT used before silent final 'e' (more, fire use óre)."
     :items
     ((:glyph "{{roomen}}" :name "rómen" :sound "/r/ (before vowel)" :example "run" :accepts ("r" "romen" "roomen"))
      (:glyph "{{oore}}" :name "óre" :sound "/r/ (final/before cons)" :example "car" :accepts ("r" "ore" "oore")))
     :exercise-words ("run" "ram" "rat" "rib" "rid" "rim" "rip" "rob" "rod" "rot" "rub" "rug" "car" "bar" "far" "jar" "tar" "war" "more" "core" "bore" "sore" "wore" "fire" "hire" "wire" "tire"))

    (:id rule-y
     :title "The Letter Y"
     :type rule
     :explanation
     "Y has two uses in English:
• As a CONSONANT (yes, yap) → uses Anna tengwa
• As a VOWEL (my, fly, shy) → uses a breve-like tehta

When reading: if you see anna at the start of a word or syllable, it's
consonant 'y'. The breve tehta indicates vowel 'y'."
     :items
     ((:glyph "{{anna}}" :name "anna (consonant y)" :sound "/j/" :example "yes" :accepts ("y" "anna"))
      (:glyph "{{telco}}[breve]" :name "y (vowel tehta)" :sound "/i/ or /aɪ/" :example "my" :accepts ("y" "vowel y")))
     :exercise-words ("yes" "yet" "yak" "yam" "yap" "yaw" "yob" "my" "by" "fly" "fry" "dry" "pry" "try" "cry" "sky" "spy" "sly" "shy" "ply" "sty" "why"))

    (:id rule-shorthand
     :title "Shorthand Words"
     :type rule
     :explanation
     "Common words have special shorthand forms:
• THE → extended ando (distinct tall shape)
• OF → extended umbar
• AND → special ligature
• OF THE → combined shorthand

These are worth memorizing as they appear frequently!"
     :items
     ((:glyph "{{extended-ando}}" :name "the (shorthand)" :sound "the" :example "the" :accepts ("the"))
      (:glyph "{{extended-umbar}}" :name "of (shorthand)" :sound "of" :example "of" :accepts ("of"))
      (:glyph "{{ando}}[bar-above][dot-below]" :name "and (shorthand)" :sound "and" :example "and" :accepts ("and")))
     :exercise-words ("the" "of" "and"))

    ;; =========================================================================
    ;; PHASE 12: WORD EXERCISES (all rules now available)
    ;; =========================================================================
    (:id words-1
     :title "Word Practice: Simple Words"
     :type words
     :explanation "Read these simple words. Focus on recognizing the consonants and vowel tehtar."
     :items nil
     :exercise-words ("mom" "dad" "cat" "dog" "sun" "run" "fun" "bun" "gun" "man" "can" "fan" "pan" "van" "bat" "hat" "mat" "rat" "sat"))

    (:id words-2
     :title "Word Practice: Digraphs"
     :type words
     :explanation "These words feature digraphs. Watch for ch, sh, th, ng, wh."
     :items nil
     :exercise-words ("ship" "shop" "shut" "shin" "shot" "shun" "wish" "fish" "dish" "gush" "hush" "rush" "thin" "than" "then" "them" "this" "that" "with" "bath" "math" "path" "moth" "both"))

    (:id words-3
     :title "Word Practice: Common Words"
     :type words
     :explanation "Practice reading common English words. Note the shorthand forms!"
     :items nil
     :exercise-words ("the" "and" "for" "but" "not" "you" "all" "can" "had" "her" "was" "one" "our" "out" "day" "get" "has" "him" "his" "how"))

    (:id words-4
     :title "Word Practice: LotR Names"
     :type words
     :explanation "Read names from The Lord of the Rings. These test multiple rules together."
     :items nil
     :exercise-words ("Frodo" "Sam" "Merry" "Pippin" "Gandalf" "Bilbo" "Gollum" "Sauron" "ring" "shire" "hobbit" "dwarf" "wizard"))

    ;; =========================================================================
    ;; PHASE 13: SENTENCE READING
    ;; =========================================================================
    (:id sentences
     :title "Sentence Reading: LotR Quotes"
     :type sentences
     :explanation "The final challenge: read complete sentences from The Lord of the Rings!"
     :items nil
     :exercise-sentences
     ("In a hole in the ground there lived a hobbit"
      "You shall not pass"
      "One ring to rule them all"
      "In the darkness bind them"
      "Home is behind, the world ahead"
      "Even the smallest person can change the course of the future"
      "One does not simply walk into Mordor"
      "Moonlight drowns out all but the brightest stars")))
  "Complete curriculum for Tengwar reading tutorial.")

;;; ============================================================================
;;; Buffer-local State
;;; ============================================================================

(defvar-local hyalo-tengwar-tutorial--current-lesson nil
  "Index of the current lesson in the curriculum.")

(defvar-local hyalo-tengwar-tutorial--current-item nil
  "Index of the current item within the lesson.")

(defvar-local hyalo-tengwar-tutorial--current-exercise nil
  "Index of the current exercise question.")

(defvar-local hyalo-tengwar-tutorial--exercise-pool nil
  "Pool of exercise items for current session (combinations/words).")

(defvar-local hyalo-tengwar-tutorial--session-shown nil
  "Items already shown this session (to avoid repeats).")

(defvar-local hyalo-tengwar-tutorial--in-lesson-intro nil
  "Non-nil if showing lesson introduction screen.")

(defvar-local hyalo-tengwar-tutorial--waiting-for-input nil
  "Non-nil if waiting for user answer input.")

(defvar-local hyalo-tengwar-tutorial--current-question nil
  "Current question data for reference during feedback.")

(defvar-local hyalo-tengwar-tutorial--lesson-complete-screen nil
  "Non-nil if showing the lesson complete screen.")

(defvar-local hyalo-tengwar-tutorial--deferred-refresh-pending nil
  "Non-nil if a deferred refresh is pending to prevent infinite loops.")

;;; ============================================================================
;;; Major Mode
;;; ============================================================================

(defvar hyalo-tengwar-tutorial-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'hyalo-tengwar-tutorial--quit)
    (define-key map (kbd "RET") #'hyalo-tengwar-tutorial--continue)
    (define-key map (kbd "SPC") #'hyalo-tengwar-tutorial--continue)
    (define-key map (kbd "r") #'hyalo-tengwar-tutorial--repeat-lesson)
    (define-key map (kbd "n") #'hyalo-tengwar-tutorial--next-lesson)
    (define-key map (kbd "p") #'hyalo-tengwar-tutorial--prev-lesson)
    map)
  "Keymap for `hyalo-tengwar-tutorial-mode'.")

(define-derived-mode hyalo-tengwar-tutorial-mode special-mode "Tengwar Tutorial"
  "Major mode for the Tengwar reading tutorial.

\\{hyalo-tengwar-tutorial-mode-map}"
  :group 'hyalo-tengwar-tutorial
  (setq-local mode-line-format nil)
  (setq-local cursor-type nil)
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  (buffer-disable-undo)
  ;; Add window resize hook
  (add-hook 'window-size-change-functions
            #'hyalo-tengwar-tutorial--on-window-resize nil t))

(defun hyalo-tengwar-tutorial--on-window-resize (frame)
  "Recompute layout when window is resized.
FRAME is the frame that changed."
  (when-let* ((buf (get-buffer "*Tengwar Tutorial*"))
              (win (get-buffer-window buf frame)))
    (with-current-buffer buf
      (when (eq major-mode 'hyalo-tengwar-tutorial-mode)
        (hyalo-tengwar-tutorial--refresh-display)))))

(defun hyalo-tengwar-tutorial--refresh-display ()
  "Refresh the current display based on tutorial state."
  (cond
   ;; Lesson intro screen
   (hyalo-tengwar-tutorial--in-lesson-intro
    (hyalo-tengwar-tutorial--show-lesson-intro))
   ;; Lesson complete screen
   (hyalo-tengwar-tutorial--lesson-complete-screen
    (hyalo-tengwar-tutorial--show-lesson-complete-screen))
   ;; Waiting for input (exercise)
   (hyalo-tengwar-tutorial--waiting-for-input
    (hyalo-tengwar-tutorial--show-exercise))
   ;; After feedback - just leave as is (don't refresh mid-feedback)
   ))

;;; ============================================================================
;;; Display Helpers
;;; ============================================================================

(defconst hyalo-tengwar-tutorial--glyph-height 5.0
  "Height multiplier for tengwar display (5x normal size).")

(defconst hyalo-tengwar-tutorial--title-height 1.8
  "Height multiplier for title text.")

(defface hyalo-tengwar-tutorial-title
  '((t :inherit bold :height 1.8))
  "Face for tutorial titles."
  :group 'hyalo-tengwar-tutorial)

(defface hyalo-tengwar-tutorial-tengwar
  '((t :height 5.0))
  "Face for large tengwar display."
  :group 'hyalo-tengwar-tutorial)

(defface hyalo-tengwar-tutorial-prompt
  '((t :inherit font-lock-comment-face))
  "Face for input prompts."
  :group 'hyalo-tengwar-tutorial)

(defface hyalo-tengwar-tutorial-correct
  '((t :inherit success :weight bold))
  "Face for correct answers."
  :group 'hyalo-tengwar-tutorial)

(defface hyalo-tengwar-tutorial-incorrect
  '((t :inherit error :weight bold))
  "Face for incorrect answers."
  :group 'hyalo-tengwar-tutorial)

(defface hyalo-tengwar-tutorial-explanation
  '((t :inherit font-lock-doc-face))
  "Face for explanations."
  :group 'hyalo-tengwar-tutorial)

(defface hyalo-tengwar-tutorial-progress
  '((t :inherit shadow))
  "Face for progress indicators."
  :group 'hyalo-tengwar-tutorial)

(defface hyalo-tengwar-tutorial-muted
  '((t :inherit shadow :slant italic))
  "Face for secondary/muted text."
  :group 'hyalo-tengwar-tutorial)

(defun hyalo-tengwar-tutorial--center-line (text)
  "Return TEXT with centering spacing prepended."
  (let* ((width (window-width))
         (text-width (string-width text))
         (padding (max 0 (/ (- width text-width) 2))))
    (concat (make-string padding ?\s) text)))

(defun hyalo-tengwar-tutorial--measure-text-pixel-width (text)
  "Measure the pixel width of TEXT by inserting it in a temp buffer."
  (let* ((window (selected-window))
         (orig-buffer (window-buffer window)))
    (unwind-protect
        (with-temp-buffer
          (set-window-buffer window (current-buffer))
          (insert text)
          (car (window-text-pixel-size window nil nil nil nil)))
      (set-window-buffer window orig-buffer))))

(defun hyalo-tengwar-tutorial--insert-centered (text &optional face)
  "Insert TEXT centered using pixel-based measurement.
Works correctly regardless of font size."
  (let* ((propertized-text (if face
                               (propertize text 'face face)
                             text))
         ;; Measure pixel width of the propertized text
         (text-pixel-width (hyalo-tengwar-tutorial--measure-text-pixel-width
                            propertized-text))
         ;; Get window body width in pixels
         (window-pixel-width (window-body-width nil t))
         ;; Calculate left padding in pixels
         (left-padding-pixels (max 0 (/ (- window-pixel-width text-pixel-width) 2)))
         ;; Create a display space spec for the padding
         (padding-spec `(space :width (,left-padding-pixels))))
    ;; Insert invisible space with pixel-based width, then the text
    (insert (propertize " " 'display padding-spec))
    (insert propertized-text)
    (insert "\n")))

(defun hyalo-tengwar-tutorial--insert-tengwar (glyph-spec)
  "Insert GLYPH-SPEC as large centered tengwar.
GLYPH-SPEC is like \"{{tinco}}\" or \"{{telco}[acute]\"."
  (let* ((font-family hyalo-tengwar-font)
         ;; Request transliteration for the glyph
         (text (format "@@%s@@" glyph-spec))
         ;; We need sync transliteration - check cache or request
         (cached (gethash glyph-spec hyalo-tengwar--cache)))
    ;; If not cached, we'll display placeholder and request async
    (unless cached
      (hyalo-tengwar-tutorial--request-glyph glyph-spec))
    (let* ((tengwar-text (or cached "⋯"))
           (display-string (propertize tengwar-text
                                       'face `(:family ,font-family
                                               :height ,hyalo-tengwar-tutorial--glyph-height))))
      (hyalo-tengwar-tutorial--insert-centered display-string))))

(defun hyalo-tengwar-tutorial--request-glyph (glyph-spec)
  "Request transliteration for GLYPH-SPEC synchronously."
  ;; Ensure process is running
  (unless (and hyalo-tengwar--process
               (process-live-p hyalo-tengwar--process))
    (hyalo-tengwar--start-process)
    ;; Wait briefly for ready (up to 5 seconds)
    (let ((timeout 50))
      (while (and (not hyalo-tengwar--ready) (> timeout 0))
        (sit-for 0.1)
        (cl-decf timeout))))
  ;; Request the glyph
  (when hyalo-tengwar--ready
    (hyalo-tengwar--fetch-words (list glyph-spec))
    ;; Wait briefly for result (up to 2 seconds)
    (let ((timeout 20))
      (while (and (not (gethash glyph-spec hyalo-tengwar--cache))
                  (> timeout 0))
        (accept-process-output hyalo-tengwar--process 0.1)
        (cl-decf timeout)))))

(defun hyalo-tengwar-tutorial--insert-word-tengwar (word)
  "Insert WORD as large centered tengwar."
  (let* ((font-family hyalo-tengwar-font)
         (cached (gethash word hyalo-tengwar--cache)))
    (unless cached
      (hyalo-tengwar-tutorial--request-glyph word))
    (let* ((tengwar-text (or (gethash word hyalo-tengwar--cache) "⋯"))
           (display-string (propertize tengwar-text
                                       'face `(:family ,font-family
                                               :height ,hyalo-tengwar-tutorial--glyph-height))))
      (hyalo-tengwar-tutorial--insert-centered display-string))))

(defun hyalo-tengwar-tutorial--vertical-space (lines)
  "Insert LINES of vertical spacing."
  (insert (make-string lines ?\n)))

;;; ============================================================================
;;; Lesson Display
;;; ============================================================================

(defun hyalo-tengwar-tutorial--get-lesson (index)
  "Get lesson at INDEX from curriculum."
  (nth index hyalo-tengwar-tutorial--curriculum))

(defun hyalo-tengwar-tutorial--lesson-count ()
  "Return total number of lessons."
  (length hyalo-tengwar-tutorial--curriculum))

(defun hyalo-tengwar-tutorial--show-lesson-intro ()
  "Display the introduction screen for current lesson."
  (let* ((lesson (hyalo-tengwar-tutorial--get-lesson hyalo-tengwar-tutorial--current-lesson))
         (title (plist-get lesson :title))
         (explanation (plist-get lesson :explanation))
         (items (plist-get lesson :items))
         (lesson-num (1+ hyalo-tengwar-tutorial--current-lesson))
         (total (hyalo-tengwar-tutorial--lesson-count)))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (hyalo-tengwar-tutorial--vertical-space 3)

      ;; Progress indicator
      (hyalo-tengwar-tutorial--insert-centered
       (format "Lesson %d of %d" lesson-num total)
       'hyalo-tengwar-tutorial-progress)
      (hyalo-tengwar-tutorial--vertical-space 1)

      ;; Title
      (hyalo-tengwar-tutorial--insert-centered title 'hyalo-tengwar-tutorial-title)
      (hyalo-tengwar-tutorial--vertical-space 2)

      ;; Explanation
      (let* ((wrapped (with-temp-buffer
                        (insert explanation)
                        (let ((fill-column (min 70 (- (window-width) 10))))
                          (fill-region (point-min) (point-max)))
                        (buffer-string)))
             (lines (split-string wrapped "\n")))
        (dolist (line lines)
          (hyalo-tengwar-tutorial--insert-centered line 'hyalo-tengwar-tutorial-explanation)))
      (hyalo-tengwar-tutorial--vertical-space 2)

      ;; If this lesson has items (glyphs to learn), show preview
      (when items
        (hyalo-tengwar-tutorial--insert-centered "── Glyphs in this lesson ──" 'hyalo-tengwar-tutorial-muted)
        (hyalo-tengwar-tutorial--vertical-space 1)
        ;; Collect all glyphs and names
        (let ((glyphs nil)
              (names nil))
          (dolist (item items)
            (let* ((glyph-spec (plist-get item :glyph))
                   (name (plist-get item :name))
                   (cached (gethash glyph-spec hyalo-tengwar--cache)))
              (unless cached
                (hyalo-tengwar-tutorial--request-glyph glyph-spec))
              (push (or (gethash glyph-spec hyalo-tengwar--cache) "?") glyphs)
              (push name names)))
          (setq glyphs (nreverse glyphs))
          (setq names (nreverse names))
          ;; Display glyphs large with proper spacing
          ;; Pixel-based centering handles font scaling automatically
          (let* ((glyph-str (mapconcat #'identity glyphs "    "))
                 (display-string (propertize glyph-str
                                             'face `(:family ,hyalo-tengwar-font
                                                     :height 3.0))))
            (hyalo-tengwar-tutorial--insert-centered display-string))
          (hyalo-tengwar-tutorial--vertical-space 1)
          ;; Display names with middle dot separator
          (let ((name-str (mapconcat #'identity names " · ")))
            (hyalo-tengwar-tutorial--insert-centered name-str 'hyalo-tengwar-tutorial-muted))))

      (hyalo-tengwar-tutorial--vertical-space 3)
      (hyalo-tengwar-tutorial--insert-centered
       "Press SPACE or ENTER to begin exercises"
       'hyalo-tengwar-tutorial-prompt)
      (hyalo-tengwar-tutorial--vertical-space 1)
      (hyalo-tengwar-tutorial--insert-centered
       "[n]ext lesson  [p]revious  [q]uit"
       'hyalo-tengwar-tutorial-muted))

    (setq hyalo-tengwar-tutorial--in-lesson-intro t)
    (setq hyalo-tengwar-tutorial--waiting-for-input nil)
    (setq hyalo-tengwar-tutorial--lesson-complete-screen nil)
    (goto-char (point-min))
    ;; Schedule deferred refresh to catch async glyphs that weren't cached
    ;; Only if not already pending (prevents infinite refresh loops)
    (unless hyalo-tengwar-tutorial--deferred-refresh-pending
      (setq hyalo-tengwar-tutorial--deferred-refresh-pending t)
      (let ((buf (current-buffer)))
        (run-at-time 0.3 nil
                     (lambda ()
                       (when (buffer-live-p buf)
                         (with-current-buffer buf
                           (setq hyalo-tengwar-tutorial--deferred-refresh-pending nil)
                           (when hyalo-tengwar-tutorial--in-lesson-intro
                             (hyalo-tengwar-tutorial--show-lesson-intro))))))))))

(defun hyalo-tengwar-tutorial--prepare-exercises ()
  "Prepare the exercise pool for current lesson."
  (let* ((lesson (hyalo-tengwar-tutorial--get-lesson hyalo-tengwar-tutorial--current-lesson))
         (type (plist-get lesson :type))
         (items (plist-get lesson :items))
         (words (plist-get lesson :exercise-words))
         (sentences (plist-get lesson :exercise-sentences)))
    (setq hyalo-tengwar-tutorial--session-shown nil)
    (setq hyalo-tengwar-tutorial--current-exercise 0)
    (cond
     ;; Intro lessons - no exercises, just advance
     ((eq type 'intro)
      (setq hyalo-tengwar-tutorial--exercise-pool nil))

     ;; Glyph lessons - quiz on items, then word exercises
     ((eq type 'glyphs)
      (let ((pool nil))
        ;; First: recognize each new glyph
        (dolist (item items)
          (push (list :type 'glyph :data item) pool))
        ;; Then: word exercises if available
        (when words
          (dolist (w (hyalo-tengwar-tutorial--random-subset words 5))
            (push (list :type 'word :data w) pool)))
        (setq hyalo-tengwar-tutorial--exercise-pool (nreverse pool))))

     ;; Rule lessons - word exercises
     ((eq type 'rule)
      (let ((pool nil))
        (when words
          (dolist (w (hyalo-tengwar-tutorial--random-subset words 5))
            (push (list :type 'word :data w) pool)))
        (setq hyalo-tengwar-tutorial--exercise-pool (nreverse pool))))

     ;; Word practice - all words
     ((eq type 'words)
      (let ((pool nil))
        (dolist (w (hyalo-tengwar-tutorial--random-subset words 10))
          (push (list :type 'word :data w) pool))
        (setq hyalo-tengwar-tutorial--exercise-pool (nreverse pool))))

     ;; Sentence reading
     ((eq type 'sentences)
      (let ((pool nil))
        (dolist (s sentences)
          (push (list :type 'sentence :data s) pool))
        (setq hyalo-tengwar-tutorial--exercise-pool pool))))))

(defun hyalo-tengwar-tutorial--random-subset (list n)
  "Return a random subset of N elements from LIST."
  (let ((shuffled (copy-sequence list)))
    (cl-loop for i from (1- (length shuffled)) downto 1
             do (let* ((j (random (1+ i)))
                       (tmp (nth i shuffled)))
                  (setf (nth i shuffled) (nth j shuffled))
                  (setf (nth j shuffled) tmp)))
    (seq-take shuffled (min n (length shuffled)))))

(defun hyalo-tengwar-tutorial--show-exercise ()
  "Display the current exercise question."
  (if (null hyalo-tengwar-tutorial--exercise-pool)
      (hyalo-tengwar-tutorial--lesson-complete)
    (let* ((exercise (car hyalo-tengwar-tutorial--exercise-pool))
           (type (plist-get exercise :type))
           (data (plist-get exercise :data))
           (lesson (hyalo-tengwar-tutorial--get-lesson hyalo-tengwar-tutorial--current-lesson))
           (lesson-title (plist-get lesson :title))
           (ex-num (1+ hyalo-tengwar-tutorial--current-exercise))
           (ex-total (+ hyalo-tengwar-tutorial--current-exercise
                        (length hyalo-tengwar-tutorial--exercise-pool))))
      (setq hyalo-tengwar-tutorial--current-question exercise)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (hyalo-tengwar-tutorial--vertical-space 2)

        ;; Progress
        (hyalo-tengwar-tutorial--insert-centered
         (format "%s  •  Exercise %d/%d" lesson-title ex-num ex-total)
         'hyalo-tengwar-tutorial-progress)
        (hyalo-tengwar-tutorial--vertical-space 3)

        (cond
         ;; Glyph recognition
         ((eq type 'glyph)
          (hyalo-tengwar-tutorial--insert-centered
           "What letter or sound does this tengwa represent?"
           'hyalo-tengwar-tutorial-prompt)
          (hyalo-tengwar-tutorial--vertical-space 2)
          (hyalo-tengwar-tutorial--insert-tengwar (plist-get data :glyph))
          (hyalo-tengwar-tutorial--vertical-space 3))

         ;; Word reading
         ((eq type 'word)
          (hyalo-tengwar-tutorial--insert-centered
           "Read this word:"
           'hyalo-tengwar-tutorial-prompt)
          (hyalo-tengwar-tutorial--vertical-space 2)
          (hyalo-tengwar-tutorial--insert-word-tengwar data)
          (hyalo-tengwar-tutorial--vertical-space 3))

         ;; Sentence reading
         ((eq type 'sentence)
          (hyalo-tengwar-tutorial--insert-centered
           "Read this sentence:"
           'hyalo-tengwar-tutorial-prompt)
          (hyalo-tengwar-tutorial--vertical-space 2)
          (hyalo-tengwar-tutorial--insert-word-tengwar data)
          (hyalo-tengwar-tutorial--vertical-space 3)))

        (hyalo-tengwar-tutorial--insert-centered
         "Type your answer and press ENTER"
         'hyalo-tengwar-tutorial-muted)
        (hyalo-tengwar-tutorial--vertical-space 1)
        (hyalo-tengwar-tutorial--insert-centered
         "[q]uit  [r]epeat lesson"
         'hyalo-tengwar-tutorial-muted))

      (setq hyalo-tengwar-tutorial--in-lesson-intro nil)
      (setq hyalo-tengwar-tutorial--waiting-for-input t)
      (goto-char (point-min))
      ;; Prompt for input
      (hyalo-tengwar-tutorial--prompt-for-answer))))

(defun hyalo-tengwar-tutorial--prompt-for-answer ()
  "Prompt user for answer in minibuffer."
  (when hyalo-tengwar-tutorial--waiting-for-input
    (let* ((answer (read-string "Your answer: "))
           (exercise hyalo-tengwar-tutorial--current-question)
           (type (plist-get exercise :type))
           (data (plist-get exercise :data)))
      (hyalo-tengwar-tutorial--check-answer answer type data))))

(defun hyalo-tengwar-tutorial--normalize-answer (str)
  "Normalize STR for comparison: lowercase, trim whitespace."
  (string-trim (downcase str)))

(defun hyalo-tengwar-tutorial--check-answer (answer type data)
  "Check ANSWER against expected for TYPE and DATA, show feedback."
  (let* ((normalized (hyalo-tengwar-tutorial--normalize-answer answer))
         (correct nil)
         (expected nil)
         (explanation nil))
    (cond
     ;; Glyph check
     ((eq type 'glyph)
      (let ((accepts (plist-get data :accepts)))
        (setq expected (car accepts))
        (setq correct (member normalized accepts))
        (setq explanation
              (format "%s (%s) represents the sound %s, as in '%s'"
                      (plist-get data :name)
                      (car accepts)
                      (plist-get data :sound)
                      (plist-get data :example)))))

     ;; Word check
     ((eq type 'word)
      (setq expected data)
      (setq correct (string= normalized (hyalo-tengwar-tutorial--normalize-answer data)))
      (setq explanation (format "The word is: %s" data)))

     ;; Sentence check
     ((eq type 'sentence)
      (setq expected data)
      ;; More lenient: ignore punctuation and extra spaces
      (let ((norm-expected (replace-regexp-in-string "[^a-z ]" ""
                            (hyalo-tengwar-tutorial--normalize-answer data)))
            (norm-answer (replace-regexp-in-string "[^a-z ]" "" normalized)))
        (setq correct (string= norm-answer norm-expected)))
      (setq explanation (format "The sentence is: %s" data))))

    ;; Show feedback
    (hyalo-tengwar-tutorial--show-feedback correct expected explanation)))

(defun hyalo-tengwar-tutorial--show-feedback (correct expected explanation)
  "Show feedback for answer: CORRECT status, EXPECTED answer, EXPLANATION."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (hyalo-tengwar-tutorial--vertical-space 1)

    (if correct
        (progn
          (hyalo-tengwar-tutorial--insert-centered "✓ Correct!" 'hyalo-tengwar-tutorial-correct)
          (hyalo-tengwar-tutorial--vertical-space 1)
          (hyalo-tengwar-tutorial--insert-centered explanation 'hyalo-tengwar-tutorial-explanation)
          ;; Auto-advance after brief pause
          (sit-for 1.5)
          (hyalo-tengwar-tutorial--advance-exercise))
      ;; Wrong answer
      (hyalo-tengwar-tutorial--insert-centered "✗ Not quite" 'hyalo-tengwar-tutorial-incorrect)
      (hyalo-tengwar-tutorial--vertical-space 1)
      (hyalo-tengwar-tutorial--insert-centered
       (format "Expected: %s" expected)
       'hyalo-tengwar-tutorial-muted)
      (hyalo-tengwar-tutorial--vertical-space 1)
      (hyalo-tengwar-tutorial--insert-centered explanation 'hyalo-tengwar-tutorial-explanation)
      (hyalo-tengwar-tutorial--vertical-space 2)
      (hyalo-tengwar-tutorial--insert-centered
       "Press SPACE to continue"
       'hyalo-tengwar-tutorial-prompt)
      (setq hyalo-tengwar-tutorial--waiting-for-input nil))))

(defun hyalo-tengwar-tutorial--advance-exercise ()
  "Move to the next exercise or complete lesson."
  (pop hyalo-tengwar-tutorial--exercise-pool)
  (cl-incf hyalo-tengwar-tutorial--current-exercise)
  (if (null hyalo-tengwar-tutorial--exercise-pool)
      (hyalo-tengwar-tutorial--lesson-complete)
    (hyalo-tengwar-tutorial--show-exercise)))

(defun hyalo-tengwar-tutorial--lesson-complete ()
  "Handle lesson completion."
  ;; Mark lesson complete
  (let ((lesson (hyalo-tengwar-tutorial--get-lesson hyalo-tengwar-tutorial--current-lesson)))
    (unless (member (plist-get lesson :id)
                    (plist-get hyalo-tengwar-tutorial--state :completed-lessons))
      (plist-put hyalo-tengwar-tutorial--state :completed-lessons
                 (cons (plist-get lesson :id)
                       (plist-get hyalo-tengwar-tutorial--state :completed-lessons))))
    ;; Add learned symbols
    (when-let* ((items (plist-get lesson :items)))
      (dolist (item items)
        (let ((name (plist-get item :name)))
          (unless (member name (plist-get hyalo-tengwar-tutorial--state :learned-symbols))
            (plist-put hyalo-tengwar-tutorial--state :learned-symbols
                       (cons name (plist-get hyalo-tengwar-tutorial--state :learned-symbols))))))))

  ;; Update current lesson
  (plist-put hyalo-tengwar-tutorial--state :current-lesson
             (1+ hyalo-tengwar-tutorial--current-lesson))
  (hyalo-tengwar-tutorial--save-progress)

  ;; Update state flags
  (setq hyalo-tengwar-tutorial--in-lesson-intro nil)
  (setq hyalo-tengwar-tutorial--waiting-for-input nil)
  (setq hyalo-tengwar-tutorial--lesson-complete-screen t)

  ;; Show completion screen
  (hyalo-tengwar-tutorial--show-lesson-complete-screen))

(defun hyalo-tengwar-tutorial--show-lesson-complete-screen ()
  "Display the lesson completion screen."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (hyalo-tengwar-tutorial--vertical-space 5)
    (hyalo-tengwar-tutorial--insert-centered "✦ Lesson Complete! ✦" 'hyalo-tengwar-tutorial-correct)
    (hyalo-tengwar-tutorial--vertical-space 2)

    (let ((learned (plist-get hyalo-tengwar-tutorial--state :learned-symbols)))
      (hyalo-tengwar-tutorial--insert-centered
       (format "You now know %d tengwar symbols" (length learned))
       'hyalo-tengwar-tutorial-explanation))

    (hyalo-tengwar-tutorial--vertical-space 3)

    (if (< (1+ hyalo-tengwar-tutorial--current-lesson) (hyalo-tengwar-tutorial--lesson-count))
        (progn
          (hyalo-tengwar-tutorial--insert-centered
           "Press SPACE to continue to the next lesson"
           'hyalo-tengwar-tutorial-prompt)
          (hyalo-tengwar-tutorial--insert-centered
           "[q]uit and save progress"
           'hyalo-tengwar-tutorial-muted))
      ;; Tutorial complete!
      (hyalo-tengwar-tutorial--insert-centered
       "Congratulations! You have completed the Tengwar Tutorial!"
       'hyalo-tengwar-tutorial-title)
      (hyalo-tengwar-tutorial--vertical-space 2)
      (hyalo-tengwar-tutorial--insert-centered
       "You can now read Tengwar in English mode."
       'hyalo-tengwar-tutorial-explanation)
      (hyalo-tengwar-tutorial--vertical-space 1) 
      (hyalo-tengwar-tutorial--insert-centered
       "Press [q] to exit"
       'hyalo-tengwar-tutorial-muted))

    (goto-char (point-min))))

;;; ============================================================================
;;; Commands
;;; ============================================================================

(defun hyalo-tengwar-tutorial--continue ()
  "Continue to next screen/exercise."
  (interactive)
  (cond
   ;; On lesson complete screen - go to next lesson
   (hyalo-tengwar-tutorial--lesson-complete-screen
    (setq hyalo-tengwar-tutorial--lesson-complete-screen nil)
    (if (< (1+ hyalo-tengwar-tutorial--current-lesson) (hyalo-tengwar-tutorial--lesson-count))
        (progn
          (cl-incf hyalo-tengwar-tutorial--current-lesson)
          (hyalo-tengwar-tutorial--show-lesson-intro))
      (hyalo-tengwar-tutorial--quit)))

   ;; In lesson intro - start exercises
   (hyalo-tengwar-tutorial--in-lesson-intro
    (hyalo-tengwar-tutorial--prepare-exercises)
    (if hyalo-tengwar-tutorial--exercise-pool
        (hyalo-tengwar-tutorial--show-exercise)
      ;; No exercises (intro lesson) - complete immediately
      (hyalo-tengwar-tutorial--lesson-complete)))

   ;; After wrong answer feedback - advance
   ((not hyalo-tengwar-tutorial--waiting-for-input)
    (hyalo-tengwar-tutorial--advance-exercise))))

(defun hyalo-tengwar-tutorial--next-lesson ()
  "Skip to next lesson."
  (interactive)
  (when (< (1+ hyalo-tengwar-tutorial--current-lesson) (hyalo-tengwar-tutorial--lesson-count))
    (cl-incf hyalo-tengwar-tutorial--current-lesson)
    (plist-put hyalo-tengwar-tutorial--state :current-lesson
               hyalo-tengwar-tutorial--current-lesson)
    (hyalo-tengwar-tutorial--show-lesson-intro)))

(defun hyalo-tengwar-tutorial--prev-lesson ()
  "Go to previous lesson."
  (interactive)
  (when (> hyalo-tengwar-tutorial--current-lesson 0)
    (cl-decf hyalo-tengwar-tutorial--current-lesson)
    (plist-put hyalo-tengwar-tutorial--state :current-lesson
               hyalo-tengwar-tutorial--current-lesson)
    (hyalo-tengwar-tutorial--show-lesson-intro)))

(defun hyalo-tengwar-tutorial--repeat-lesson ()
  "Repeat current lesson from the beginning."
  (interactive)
  (hyalo-tengwar-tutorial--show-lesson-intro))

(defun hyalo-tengwar-tutorial--quit ()
  "Quit tutorial and save progress."
  (interactive)
  (hyalo-tengwar-tutorial--save-progress)
  ;; Stop the tengwar subprocess to avoid orphaned process
  (hyalo-tengwar--stop-process)
  (message "Progress saved. Resume anytime with M-x hyalo/tengwar-tutorial")
  (kill-buffer))

(defun hyalo-tengwar-tutorial--check-font ()
  "Check if Tengwar font is available, return t if ok."
  (let ((font-family hyalo-tengwar-font))
    (if (and font-family (font-info font-family))
        t
      (message "Tengwar font '%s' not found. Please install it first." font-family)
      nil)))

;;;###autoload
(defun hyalo/tengwar-tutorial ()
  "Start or resume the Tengwar reading tutorial."
  (interactive)
  (unless (hyalo-tengwar-tutorial--check-font)
    (user-error "Tengwar font not available"))

  ;; Load saved progress
  (hyalo-tengwar-tutorial--load-progress)

  ;; Create tutorial buffer
  (let ((buf (get-buffer-create "*Tengwar Tutorial*")))
    (switch-to-buffer buf)
    (hyalo-tengwar-tutorial-mode)

    ;; Restore lesson position
    (setq hyalo-tengwar-tutorial--current-lesson
          (or (plist-get hyalo-tengwar-tutorial--state :current-lesson) 0))

    ;; Clamp to valid range
    (when (>= hyalo-tengwar-tutorial--current-lesson (hyalo-tengwar-tutorial--lesson-count))
      (setq hyalo-tengwar-tutorial--current-lesson 0)
      (plist-put hyalo-tengwar-tutorial--state :current-lesson 0))

    ;; Ensure tengwar subprocess is running
    (hyalo-tengwar--start-process)

    ;; Force window to be fully realized before measuring fonts
    ;; This ensures font metrics are available for centering calculations
    (set-window-buffer (selected-window) buf)
    (redisplay t)

    ;; Show current lesson after a brief delay to ensure fonts are loaded
    (run-at-time 0.05 nil
                 (lambda ()
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (hyalo-tengwar-tutorial--show-lesson-intro)))))))

;;;###autoload
(defun hyalo/tengwar-tutorial-reset ()
  "Reset tutorial progress and start fresh."
  (interactive)
  (when (yes-or-no-p "Reset all Tengwar tutorial progress? ")
    (hyalo-tengwar-tutorial--init-state)
    (hyalo-tengwar-tutorial--save-progress)
    (message "Progress reset.")
    (hyalo/tengwar-tutorial)))

;;;###autoload
(defun hyalo/tengwar-tutorial-lesson (n)
  "Jump to lesson number N (1-based)."
  (interactive "nLesson number: ")
  (hyalo-tengwar-tutorial--load-progress)
  (let ((index (1- n)))
    (if (and (>= index 0) (< index (hyalo-tengwar-tutorial--lesson-count)))
        (progn
          (setq hyalo-tengwar-tutorial--current-lesson index)
          (plist-put hyalo-tengwar-tutorial--state :current-lesson index)
          (let ((buf (get-buffer-create "*Tengwar Tutorial*")))
            (switch-to-buffer buf)
            (hyalo-tengwar-tutorial-mode)
            (hyalo-tengwar--start-process)
            (hyalo-tengwar-tutorial--show-lesson-intro)))
      (user-error "Invalid lesson number. Must be 1-%d"
                  (hyalo-tengwar-tutorial--lesson-count)))))

(provide 'hyalo-tengwar-tutorial)
;;; hyalo-tengwar-tutorial.el ends here
