# Sjsonnet Playground — Technical Specification

> This document describes the current state of the Sjsonnet Playground (`playground/index.html`),
> a single-file browser-based IDE for writing, evaluating, and debugging Jsonnet code.
> It is intended as a reference for AI agents and developers iterating on the playground.

---

## Architecture Overview

- **Single HTML file** (`playground/index.html`, ~2560 lines) containing all HTML, CSS, and JavaScript.
- **Sjsonnet engine** compiled to JavaScript via Scala.js, inlined at build time via the `/* SJSONNET_BUNDLE_PLACEHOLDER */` marker in a `<script>` tag.
- **CodeMirror 5.65.16** used for all editors (code, variables, output), loaded from CDN.
- **No backend** — everything runs client-side in the browser.
- **Build**: `mill playground.bundle` compiles Scala.js and inlines the JS bundle into the HTML.
- **All JS is wrapped in a single IIFE** `(function () { ... })();` — no global variables leak.

---

## File Structure

The playground consists of a single file plus this spec:

    playground/
    ├── index.html    # The entire playground (HTML + CSS + JS)
    └── SPEC.md       # This specification

The `build.mill` file defines the `playground` module with:
- `playgroundHtml` — source task pointing to `playground/index.html`
- `bundle` — compiles ScalaJS, wraps CommonJS output as browser-compatible IIFE, inlines into HTML replacing `/* SJSONNET_BUNDLE_PLACEHOLDER */`
- `start()` — builds and opens in default browser via `open` (macOS) / `xdg-open` (Linux) / `cmd /c start` (Windows)

---

## Layout & UI

### Header
- Title: "Sjsonnet Playground — Interactive Jsonnet Evaluator"
- Theme toggle button (dark 🌙 / light ☀️)
- "Preserve key order" checkbox (`preserveOrder`)
- "Auto-evaluate" checkbox (`autoEval`, default: checked)
- Status indicator pill (Ready / Evaluating… / Success / Error) with ARIA `role="status"` and `aria-live="polite"`

### Left Panel (flex: 55)
1. **File Sidebar** (220px default, resizable 80–300px)
   - Header with "Files" label and "+" add button
   - File list with click-to-switch, double-click to rename
   - Entry file marked with ▶ prefix and bold text
   - Each file item shows on hover: ✏ rename, ▶ set-as-entry (if not entry), × delete (if >1 file)
   - Delete requires `confirm()` dialog
   - New files auto-named `file1.jsonnet`, `file2.jsonnet`, etc.
   - File name validation: no empty, no `<>:"|?*` or control chars, no `.` or `..` only, max 255 chars
   - Drag-resize via `fileSidebarResize` handle (4px, col-resize cursor)

2. **Editor Area**
   - Panel header with toolbar buttons:
     - 📂 Open — triggers hidden `<input type="file">` (accepts `.jsonnet,.libsonnet,.json,.txt,*/*`)
     - 💾 Save — downloads current file to disk via `downloadFile()`
     - ↩ Undo / ↪ Redo — calls `jsonnetEditor.undo()` / `jsonnetEditor.redo()`
   - CodeMirror editor (`jsonnetEditor`) with:
     - Mode: `javascript` (approximation for Jsonnet syntax)
     - Match brackets, auto-close brackets
     - Tab size: 2, indent with spaces
     - Show-hint addon for autocomplete
     - `Ctrl-Space` triggers manual autocomplete
   - Copy button overlay (appears on hover, top-right corner)

3. **Variables Panel** (bottom, default 200px height, resizable 80px–70% of editor area, collapsible)
   - Toggle button (▼/▶) to expand/collapse; when collapsed, resize handle hidden, height set to `auto`
   - Two sections separated by border:
     - **External Variables (ext vars)** — JSON object editor (CodeMirror, JSON mode, no line numbers)
     - **Top-Level Arguments (TLA vars)** — JSON object editor (same config)
   - Each section has: section header, error display (`.var-error`, hidden by default), editor wrapper, copy button
   - Variable parsing: `parseVars()` validates JSON object, stringifies all values via `JSON.stringify()` (because sjsonnet parses extVar values as Jsonnet code)
   - Invalid JSON shows inline error: `⚠ <message>`
   - Resize handle between editor area and variables panel (`varsResizeHandle`, 4px, row-resize cursor)

### Right Panel (flex: 45)
- Panel header with: timing display (e.g., "12.3 ms"), ⬇ Save button, ⬇ Save All button (hidden in single-output mode)
- **Single output mode** (`outputWrapper`):
  - CodeMirror editor (read-only, JSON mode on success, text/plain on error)
  - Gets `error-state` class on errors → text turns red via `color: var(--error)`
- **Multi-file output mode** (`outputTabs` + `outputTabContent`):
  - Tab bar with keyboard navigation (ArrowLeft/Right/Up/Down, Enter/Space to select)
  - Each tab has ARIA `role="tab"`, `aria-selected`, `tabindex`
  - Per-tab CodeMirror editor with auto-detected mode via `detectCodeMirrorMode()`
  - Remembers active tab key across re-evaluations
- Copy button overlay on both output modes
- Download buttons:
  - Single output: downloads as `output.json` (or detected filename)
  - Save All: downloads each tab as a separate file

### Resize Handles
All resize handles are 4px wide/tall, use `var(--border)` background, turn `var(--accent)` on hover/active.

| Handle | Direction | Min | Max | Affects |
|--------|-----------|-----|-----|---------|
| `fileSidebarResize` | horizontal (col-resize) | 80px | 300px | File sidebar width |
| `varsResizeHandle` | vertical (row-resize) | 80px | 70% of editor area | Variables panel height |
| `resizeHandle` | horizontal (col-resize) | 20% | 80% | Left/right panel ratio |

Unified `mousemove` and `mouseup` handlers on `document` manage all three resize interactions.

---

## Theming

Two themes: **dark** (Catppuccin Mocha-inspired) and **light** (Catppuccin Latte-inspired).

- CSS custom properties on `:root` / `[data-theme="dark"]` and `[data-theme="light"]`
- CodeMirror themes: `dracula` (dark) / `default` (light)
- Theme preference persisted in `localStorage` under key `sjsonnet-playground-theme`
- `applyTheme(theme)` updates `data-theme` attribute, button text, and all CodeMirror editor themes

Key CSS variables:

| Variable | Dark | Light |
|----------|------|-------|
| `--bg-primary` | `#1e1e2e` | `#eff1f5` |
| `--bg-secondary` | `#181825` | `#e6e9ef` |
| `--bg-surface` | `#313244` | `#ccd0da` |
| `--text-primary` | `#cdd6f4` | `#4c4f69` |
| `--text-secondary` | `#a6adc8` | `#6c6f85` |
| `--accent` | `#89b4fa` | `#1e66f5` |
| `--accent-hover` | `#74c7ec` | `#2a6ef5` |
| `--success` | `#a6e3a1` | `#40a02b` |
| `--error` | `#f38ba8` | `#d20f39` |
| `--border` | `#45475a` | `#bcc0cc` |
| `--warning` | `#f9e2af` | `#df8e1d` |

---

## State Management

### State Shape (version 2)

    {
      version: 2,
      files: [
        { name: "main.jsonnet", code: "...", isEntry: true },
        { name: "lib.libsonnet", code: "...", isEntry: false }
      ],
      activeFileName: "main.jsonnet",
      extVars: '{"name": "World"}',
      tlaVars: '{}'
    }

### Persistence
- Stored in `localStorage` under key `sjsonnet-playground-state`
- Debounced save (300ms) on every editor change via `saveState()`
- Immediate save on `beforeunload` via `saveStateImmediate()`
- State validation on load via `validateState()`:
  - Must be an object with non-empty `files` array
  - Each file must have string `name` and `code`
  - `activeFileName` must point to an existing file (auto-corrected if not)
  - Exactly one entry file enforced (auto-corrected if 0 or >1)
  - Invalid ext/TLA vars JSON reset to defaults
- Migration support via `migrateState()` (currently v1→v2: adds version field)
- Falls back to `createDefaultState()` if stored state is invalid or missing

### Default State
- Single file `main.jsonnet` with a welcome example demonstrating: local variables, `std.extVar()`, object construction, array comprehension, nested objects
- ext vars: `{"name": "World"}`
- TLA vars: `{}`

### Key Functions
- `getActiveFile()` — returns the file matching `state.activeFileName`
- `getEntryFile()` — returns the file with `isEntry: true`
- `findFileByName(name)` — lookup by name
- `saveEditorToState()` — syncs editor content back to state (guarded by `suppressSave` flag)
- `loadFileIntoEditor(fileName)` — loads file content into editor, clears undo history

---

## Multi-File Support

- Users can create multiple `.jsonnet` / `.libsonnet` files
- One file is designated as the **entry file** (▶ prefix, bold text)
- Files are listed in the sidebar with click-to-switch
- File actions: rename (✏ button or double-click), delete (× button with confirm), set as entry (▶ button)
- `import` resolution works across files via `importResolver` and `importLoader` callbacks
- Cross-file identifier extraction for autocomplete

### File Operations
- **Add**: `addNewFile()` — creates `fileN.jsonnet`, switches to it, focuses editor
- **Delete**: `deleteFile(name)` — requires confirm, reassigns entry if needed, switches to nearest file
- **Rename**: `startRenameFile()` — inline input field, validates name, updates `activeFileName` if needed
- **Switch**: `switchToFile(name)` — saves current, loads new, re-renders sidebar, schedules eval
- **Set Entry**: `setEntryFile(name)` — clears all `isEntry`, sets target, re-renders, schedules eval

### Import Resolution
- `importResolver(wd, importName)`:
  - First tries direct match by file name
  - Then resolves relative paths (`./`, `../`) against working directory
  - Uses `normalizePath()` to handle `.` and `..` segments
  - Returns resolved file name or `null`
- `importLoader(path, binaryData)`:
  - Returns file content as string (for `import`/`importstr`)
  - Returns `Uint8Array` for binary imports (`importbin`) — uses `TextEncoder` with manual UTF-8 fallback
  - Throws `Error("File not found: " + path)` if file doesn't exist
- Both are passed to `SjsonnetMain.interpret()`

---

## Evaluation

### Flow
1. `evaluate()` is called on:
   - Editor change (debounced via `scheduleEvaluation()`)
   - Ctrl/Cmd+Enter keyboard shortcut
   - Auto-evaluate checkbox toggle (evaluates immediately when checked)
   - Preserve-order checkbox change
2. Calls `saveEditorToState()` to sync editor content
3. Parses ext vars and TLA vars via `parseVars()` — returns `null` on invalid JSON, sets status to "Variable Error"
4. Empty code → renders empty output, sets status to "Ready"
5. Calls `SjsonnetMain.interpret(code, extVars, tlaVars, "", importResolver, importLoader, preserveOrder)`
6. On success:
   - Multi-file result (JS object with ≥2 string-valued keys) → `renderOutputTabs(result)`
   - Single result → `JSON.stringify(result, null, 2)` → `renderSingleOutput(text, false)`
   - Calls `clearErrorMarkers()`
   - Sets status to "Success" with timing
7. On error:
   - Extracts `error.message || error.toString()`
   - `renderSingleOutput(message, true)` — shows error in red
   - `markErrors(message)` — highlights error locations in editor
   - Sets status to "Error" with timing

### Adaptive Debounce
- Delay = `max(400, min(2000, lastEvalDurationMs * 3))`
- Fast evaluations get snappy feedback; slow ones get more breathing room
- Only triggers if auto-evaluate checkbox is checked

### Multi-File Output Detection
`isMultiFileResult(result)` returns true when:
- Result is a non-null, non-array object
- Has ≥2 keys
- All values are strings
- All keys are non-empty, trimmed, and contain no `<>:"|?*` or control characters

### Output Mode Detection
`detectCodeMirrorMode(filename, content)` auto-detects CodeMirror mode for output tabs:
- By extension: `.json` → JSON, `.yaml`/`.yml`/`.conf` → YAML, `.ini`/`.cfg`/`.properties` → properties, `.sh`/`.bash`/`.zsh` → shell, `.toml` → TOML, `.xml`/`.html`/`.svg` → XML, `.js`/`.ts`/`.jsonnet`/`.libsonnet` → JavaScript
- By content heuristic: `{`/`[` → JSON, `#!/` → shell, `[section]` → properties, `<?xml`/`<` → XML, `:\n`/`: ` → YAML
- Fallback: `text/plain`

### Download Support
- `downloadFile(filename, content)` — creates Blob, generates object URL, triggers download via hidden anchor
- `getMimeType(filename)` — maps extensions to MIME types (json, yaml, xml, html, css, js, sh, ini/cfg/conf/toml/properties, fallback text/plain)
- Single output: "⬇ Save" button downloads as detected filename
- Multi-file output: "⬇ Save All" button downloads all tabs as separate files

---

## Error Marking

When evaluation fails, errors are visually marked in the editor:

### Error Message Parsing
- `parseErrorLocations(message)` — regex: `/\((?:memory|[^)]+)\):(\d+):(\d+)/g`
- Extracts line and column from patterns like `(memory):2:1` or `(filename.jsonnet):5:3`
- Returns array of `{ line, col }` objects (0-indexed)
- Supports multiple error locations in a single message (e.g., stack traces)

### Error Summary Extraction
- `extractErrorSummary(message)` — takes first line, strips `sjsonnet.ParseError:` or `sjsonnet.Error:` prefix

### Visual Indicators
1. **Line background highlight** — light red background on the error line (`.cm-error-line`)
   - Dark theme: `rgba(255, 60, 60, 0.15)`
   - Light theme: `rgba(255, 0, 0, 0.08)`
2. **Wavy underline** — red wavy underline on the error token (`.cm-error-mark`)
   - Token detection: tries to match `[a-zA-Z_]\w*` or single non-whitespace char at error position
3. **Inline widget** — error summary text below the error line via `addLineWidget()`
   - Styled: `color: var(--error); font-size: 12px; opacity: 0.85`
   - Text: `⚠ <error summary>`
4. **Hover tooltip** — `title` attribute on the marked text shows error summary

### Lifecycle
- `markErrors(message)` — called in catch block, parses error and applies all markers
- `clearErrorMarkers()` — called on successful evaluation, removes all `markText` markers, `removeLineClass`, and clears `addLineWidget` widgets
- State tracked in `errorMarkers[]` (TextMarker objects) and `errorLineWidgets[]` (line + widget pairs)

---

## Autocomplete System

### Architecture
- `jsonnetHint(editor)` — main hint function registered with CodeMirror's show-hint addon
- Returns `{ list: completions[], from: Pos, to: Pos }`
- Each completion: `{ text, displayText?, render?, hint?, funcName?, funcSig? }`
- Priority is determined by code order — first match that returns wins

### Trigger Mechanisms
- **Manual**: Ctrl-Space (via `extraKeys` config)
- **Auto-trigger** on `inputRead` event (only for `+input` origin) for specific patterns:
  - After `std.`, `self.`, `super.`, `$.` — member access
  - After `std.extVar("` or `std.extVar('` — extVar key
  - After `std.funcName(` or comma inside std call — parameter hints
  - After `\` inside a string — escape sequences
  - After `import`/`importstr`/`importbin` + quote — import paths
  - After closing `)]}'"\w` on `local`/`assert` lines — `;` terminator
  - After 2+ identifier characters — general identifier completion

### Completion Types (in priority order)

1. **`local funcName` definition template**
   - Regex: `/^\s*local\s+([a-zA-Z_]\w*)\s*$/`
   - Condition: end of line, no text after cursor
   - Suggests: `(` → "(  ) = ...;  (function definition)" or ` = ` → " = ...;  (variable definition)"

2. **String escape sequences**
   - Regex: `/(?:^|[^\\])["'](?:[^"'\\]|\\.)*\\([a-z]?)$/`
   - Suggests: `\n`, `\t`, `\r`, `\\`, `\"`, `\'`, `\/`, `\uXXXX`, `\0`
   - Filters by typed prefix after `\`

3. **String format placeholders**
   - Regex: `/["'](?:[^"'\\]|\\.)*%([a-z]?)$/`
   - Suggests: `%s`, `%d`, `%f`, `%g`, `%e`, `%i`, `%o`, `%x`, `%X`, `%%`
   - Case-insensitive prefix matching

4. **`if`/`then`/`else` contextual completion**
   - After `if <expr>` (no `then` yet) → suggests ` then `
   - After `then <expr>` (no `else` yet) → suggests ` else `
   - Only when no text after cursor

5. **Comprehension keywords**
   - `for`: regex `/[\[{]\s*[a-zA-Z_$][\w.[\]()]*\s+$/`, excludes lines with `for`, `:`, or `,`
   - `in`: regex `/\bfor\s+[a-zA-Z_]\w*\s+$/` → suggests `in `
   - `if` (filter): regex `/\bfor\s+\w+\s+in\s+.+\s+$/`, excludes lines already containing `if`

6. **Array comprehension templates**
   - Trigger: `/\[\s*$/` at end of line with no text after
   - Suggests: `[expr for x in arr]` and `[expr for x in arr if cond]`

7. **Object comprehension template**
   - Trigger: `/\{\s*$/` at end of line with no text after
   - Suggests: `{[key]: value for key in arr}`
   - Returns immediately (does not fall through to `std.` completion)

8. **`std.` function/constant completion**
   - Regex: `/\bstd\.([a-zA-Z_]\w*)?$/`
   - ~158 functions with full signatures (e.g., `format(str, vals)`)
   - 2 constants: `pi`, `thisFile`
   - Custom `render` function: function name in normal text + signature in muted `.cm-hint-signature` style
   - Matching: prefix match OR substring match (for non-empty prefix)
   - Sorted: exact prefix matches first, then substring matches, then alphabetical

9. **`std.funcName(` parameter hints**
   - Regex: `/\bstd\.(\w+)\s*\([^)]*$/` (excludes calls with string literal after `(`)
   - Condition: cursor right after `(` or `,` with optional whitespace
   - Shows current parameter: `▸ paramName =default  (N/total)`
   - Shows remaining parameters as reference: `  paramName  (N/total)`
   - Uses `hint: function () {}` (no-op) — informational only, doesn't insert text

10. **`std.extVar("` key completion**
    - Regex: `/\bstd\.extVar\s*\(\s*(['"])([^'"]*)?$/`
    - Suggests keys from the ext vars JSON editor via `extractVarKeys(extVarsEditor)`
    - Smart suffix detection: checks if `")` or `')` already exists after cursor
      - If closing exists → inserts only the key
      - If no closing → inserts `key` + quote + `)`

11. **`self.` / `super.` / `$.` field completion**
    - Regex: `/\b(?:self|super|\$)\.([a-zA-Z_]\w*)?$/`
    - Suggests object field names from current file (`extractIdentifiers().fields`)
    - Also includes cross-file identifiers (`extractCrossFileIdentifiers().locals`)
    - Sorted alphabetically, deduplicated

12. **Import path completion**
    - Regex: `/\b(?:import|importstr|importbin)\s+['"]([^'"]*)?$/`
    - Suggests all file names from `state.files` via `extractImportedFileNames()`
    - Prefix-filtered

13. **General identifier completion**
    - Regex: `/\b([a-zA-Z_]\w*)$/`
    - Minimum 2-character prefix required
    - Sources (in order, deduplicated via `addedIdents` map):
      1. `std` — with displayText "std (standard library)"
      2. Jsonnet keywords — with snippet hints from `jsonnetSnippets` (custom render for those with snippets)
      3. Local variables, function parameters, object fields from current file via `extractIdentifiers()`
      4. Cross-file identifiers via `extractCrossFileIdentifiers()`
      5. TLA variable keys via `extractVarKeys(tlaVarsEditor)`
    - Excludes the exact text already typed
    - Sorted alphabetically

14. **`;` statement terminator** (lowest priority — at bottom of function)
    - Regex: `/^\s*(?:local\s+.+=.+\S|assert\s+.+\S)\s*$/`
    - Additional check: last non-whitespace char must not be `;`, `,`, `{`, `.`, or `(`
    - Only when no text after cursor

### Autocomplete CSS Styling
- Hint popup: `var(--bg-surface)` background, `var(--border)` border, 6px border-radius, drop shadow
- Font: JetBrains Mono / Fira Code / Cascadia Code, 12px
- Max height: 240px, z-index: 100
- Active hint: `var(--accent)` background with dark text
- Signature text: `var(--text-secondary)`, 11px, 8px left margin

### Helper Functions
- `extractIdentifiers(text)` — regex-based extraction:
  - Locals: `/\blocal\s+([a-zA-Z_]\w*)\s*(?:\([^)]*\))?\s*=/g`
  - Function params: `/\bfunction\s*\(([^)]*)\)/g` → splits by comma, strips defaults
  - Object fields: `/(?:^|[{,;])\s*([a-zA-Z_]\w*)\s*(?:::?:?|[+(])/gm` → excludes keywords
- `extractCrossFileIdentifiers()` — aggregates locals + fields from all non-active files
- `extractImportedFileNames()` — returns all `state.files[].name`
- `extractVarKeys(cmEditor)` — `JSON.parse()` editor value, returns `Object.keys()`, wrapped in try/catch

### Data
- `jsonnetKeywords` — 18 keywords: assert, else, error, false, for, function, if, import, importbin, importstr, in, local, null, self, super, then, true, tailstrict
- `jsonnetSnippets` — 8 snippet entries with `displayText` hints: local, if, function, import, importstr, importbin, assert, error
- `stdFunctions` — array of `[name, signature]` pairs (~158 entries), organized by category: Array, String, Object, Math, Type, Encoding, Manifest, Set, Misc
- `stdConstants` — `["pi", "thisFile"]`

---

## Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| Ctrl/Cmd + Enter | Force evaluate |
| Ctrl/Cmd + N | Add new file |
| Ctrl/Cmd + S | Save current file to disk (download) |
| Ctrl + Space | Trigger autocomplete |
| Double-click file name | Rename file |
| Enter (in rename input) | Confirm rename |
| Escape (in rename input) | Cancel rename |

---

## Accessibility

- Status indicator: `role="status"`, `aria-live="polite"`
- Resize handles: `role="separator"`, `aria-orientation`, `aria-label`, `tabindex="0"`
- Output tabs: `role="tablist"` on container, `role="tab"` + `aria-selected` + `tabindex` on each tab
- Output tab content: `role="tabpanel"`
- Output wrapper: `role="region"`, `aria-label="Evaluation output"`
- Variables toggle: `aria-expanded`, `aria-controls`
- File action buttons: `aria-label` with file name context

---

## External Dependencies (CDN)

All loaded from `cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.16/`:

**CSS:**
- `codemirror.min.css`
- `theme/dracula.min.css`
- `addon/fold/foldgutter.min.css`
- `addon/hint/show-hint.min.css`

**JavaScript:**
- `codemirror.min.js`
- Modes: `javascript`, `yaml`, `properties`, `shell`, `toml`, `xml`
- Addons: `matchbrackets`, `closebrackets`, `foldcode`, `foldgutter`, `brace-fold`, `indent-fold`, `show-hint`

---

## Clipboard Support

- `copyToClipboard(text)` — uses `navigator.clipboard.writeText()` with fallback to `document.execCommand("copy")` via hidden textarea
- `setupCopyButton(btnId, getTextFn)` — wires up copy buttons with visual feedback ("Copied!" / "Failed")
- Copy buttons: code editor, ext vars, TLA vars, output, tab output (5 total)

---

## CodeMirror Editor Instances

| Variable | Purpose | Mode | Read-only | Line numbers |
|----------|---------|------|-----------|--------------|
| `jsonnetEditor` | Main code editor | javascript | No | Yes |
| `extVarsEditor` | External variables | javascript (json) | No | No |
| `tlaVarsEditor` | TLA variables | javascript (json) | No | No |
| `outputEditor` | Single output display | javascript (json) / text | Yes | Yes |
| `tabOutputEditor` | Tab output display | auto-detected | Yes | Yes |

All editors are tracked in `allEditors[]` array for bulk theme updates.

Output editors have code folding enabled (`foldGutter`, `brace-fold`, `indent-fold`).

---

## Known Limitations

1. **No Jsonnet syntax highlighting** — uses JavaScript mode as approximation; Jsonnet-specific syntax (e.g., `local`, `::`, `|||`, `self`, `$`) is not properly highlighted
2. **Text-based autocomplete** — uses regex pattern matching on the current line only, not AST/semantic analysis
3. **No scope awareness** — variables defined in inner scopes are suggested globally; no shadowing support
4. **No type inference** — cannot suggest methods based on variable types (e.g., array vs object)
5. **No object member tracking** — `local obj = {a: 1}; obj.` cannot suggest `a`
6. **No import content resolution for autocomplete** — `import "lib.jsonnet"` doesn't analyze lib's exports for `.` completion
7. **Single-line context** — `textBefore` only considers the current line; multi-line expressions are not analyzed
8. **No undo for file operations** — file delete/rename cannot be undone (only editor content has undo)
9. **No syntax validation without evaluation** — errors are only detected when the full evaluation runs

## Future Improvement Opportunities

1. **Semantic autocomplete** — the sjsonnet `Parser` is already compiled to JS via Scala.js; expose `parse()` via `@JSExport` in `SjsonnetMain.scala` (src-js) to get AST-based completions in the browser
2. **Object member tracking** — parse `local obj = { ... }` definitions and suggest fields after `obj.`
3. **Import-aware completion** — resolve imported file content and suggest its top-level fields after `lib.`
4. **Jsonnet-specific syntax mode** — create a custom CodeMirror mode for proper Jsonnet highlighting (keywords, `::`, `|||` text blocks, `$`, string interpolation)
5. **Error squiggles on partial parse** — use the parser to detect errors as-you-type without full evaluation
6. **Snippet expansion with tab stops** — expand `if` to `if |condition| then |value| else |default|` with tab navigation between placeholders
7. **Persistent UI state** — save panel sizes, variables panel collapsed state, and active output tab to localStorage
8. **Error marking for non-memory files** — currently only marks errors in `(memory)` locations; could also mark errors in imported files by switching to the relevant tab
