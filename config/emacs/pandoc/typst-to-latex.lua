--- typst-to-latex.lua --- Pandoc filter repairing Typst -> LaTeX conversion
---
--- Pandoc's Typst reader gets prose, math and citations right, but loses
--- three things that matter for journal submission.  Each is repaired below.
---
---   1. `@fig-a` reads as a Link of class "ref", which the LaTeX writer emits
---      as `\hyperref[fig-a]{{[}fig-a{]}}` -- a working hyperlink whose
---      visible text is the literal label rather than a number.
---
---   2. A trailing `<fig-a>` label reads as an *empty Span placed after* the
---      element it labels, so the writer emits `\label` outside the figure
---      environment, where it binds whatever counter was last incremented.
---
---   3. Display math with a label stays an unnumbered `\[..\]`, so there is
---      no number for a cross-reference to resolve to.
---
--- Cases 2 and 3 need separate filters: after a figure the Span lands in its
--- own Para (a Blocks-level fix), while after display math it shares a Para
--- with the Math, separated by a Space (an Inlines-level fix).

--------------------------------------------------------------------------
-- Knobs
--------------------------------------------------------------------------

-- Cross-reference macro.  `\Cref` needs the cleveref package and produces
-- "Figure 1"; set to "ref" for a bare `\ref` if the target class forbids
-- cleveref, which some journal templates do.
local REF_MACRO = "Cref"

-- Pandoc wraps images in `\pandocbounded{...}`, a macro it defines in its
-- own preamble.  That is fine for a standalone document but breaks the
-- moment the body is pasted into a journal class.  When true, emit a plain
-- `\includegraphics[width=\linewidth]` instead.
local PLAIN_INCLUDEGRAPHICS = false

--------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------

--- Return the identifier of an empty Span (Typst's `<label>`), else nil.
local function orphan_label(el)
  if el.t == "Span" and #el.content == 0 and el.identifier ~= "" then
    return el.identifier
  end
end

--- Wrap display math in a numbered equation environment carrying ID.
local function numbered_equation(math, id)
  return table.concat({
    "\\begin{equation}\\label{", id, "}\n", math.text, "\n\\end{equation}",
  })
end

--------------------------------------------------------------------------
-- 1. Cross-references
--------------------------------------------------------------------------

function Link(el)
  if el.classes:includes("ref") then
    local target = el.target:gsub("^#", "")
    return pandoc.RawInline("latex", "\\" .. REF_MACRO .. "{" .. target .. "}")
  end
end

--------------------------------------------------------------------------
-- 2. Labels trailing a figure
--------------------------------------------------------------------------

function Blocks(blocks)
  local out = pandoc.Blocks{}
  for _, blk in ipairs(blocks) do
    local id = (blk.t == "Para" and #blk.content == 1)
      and orphan_label(blk.content[1]) or nil
    local prev = out[#out]

    if id and prev and prev.t == "Figure" and prev.identifier == "" then
      -- Move the label onto the figure itself, so the writer emits it
      -- beside \caption and it binds the figure counter.
      prev.identifier = id
    else
      out:insert(blk)
    end
  end
  return out
end

--------------------------------------------------------------------------
-- 3. Labels trailing display math
--------------------------------------------------------------------------

function Inlines(inlines)
  local out = pandoc.Inlines{}
  for _, el in ipairs(inlines) do
    local id = orphan_label(el)
    local space, math = out[#out], out[#out - 1]

    if id and space and space.t == "Space"
       and math and math.t == "Math" and math.mathtype == "DisplayMath" then
      out:remove(#out)  -- the Space
      out:remove(#out)  -- the Math
      out:insert(pandoc.RawInline("latex", numbered_equation(math, id)))
    else
      out:insert(el)
    end
  end
  return out
end

--------------------------------------------------------------------------
-- 4. Optional: unwrap \pandocbounded
--------------------------------------------------------------------------

function Image(el)
  if PLAIN_INCLUDEGRAPHICS then
    return pandoc.RawInline(
      "latex", "\\includegraphics[width=\\linewidth]{" .. el.src .. "}")
  end
end

--------------------------------------------------------------------------
-- 5. Preamble repairs (standalone output only)
--------------------------------------------------------------------------

--- Recover the .bib path from `#bibliography("refs.bib")` in the source.
--- The reader discards that call entirely -- it reaches neither the AST nor
--- the metadata -- so without this the biblatex preamble has no
--- \addbibresource and every citation resolves to nothing.
local function bibliography_from_source()
  for _, path in ipairs(PANDOC_STATE.input_files or {}) do
    local handle = io.open(path, "r")
    if handle then
      local source = handle:read("a")
      handle:close()
      -- Matches #bibliography("refs.bib") and the keyword-argument form
      -- #bibliography(("a.bib", "b.bib"), style: ..) by taking each string.
      local call = source:match("#bibliography%s*%((.-)%)")
      if call then
        local found = {}
        for bib in call:gmatch('"([^"]+%.bib)"') do
          table.insert(found, bib)
        end
        if #found > 0 then return found end
      end
    end
  end
end

function Meta(meta)
  -- \Cref comes from cleveref, which pandoc's template does not load.
  -- cleveref insists on being loaded after hyperref, but pandoc injects
  -- header-includes *before* it, so defer the load to the end of the
  -- preamble rather than emitting a bare \usepackage here.
  if REF_MACRO == "Cref" or REF_MACRO == "cref" then
    local includes = meta["header-includes"] or pandoc.MetaList{}
    if includes.t ~= "MetaList" then
      includes = pandoc.MetaList{includes}
    end
    includes:insert(pandoc.RawBlock(
      "latex",
      "\\usepackage{etoolbox}\n\\AtEndPreamble{\\usepackage{cleveref}}"))
    meta["header-includes"] = includes
  end

  if not meta.bibliography then
    local bibs = bibliography_from_source()
    if bibs then
      local list = pandoc.MetaList{}
      for _, bib in ipairs(bibs) do
        list:insert(pandoc.MetaString(bib))
      end
      meta.bibliography = list
    end
  end

  return meta
end

-- Inlines must run before Blocks: the Blocks filter inspects Para contents
-- to find orphan labels, and would otherwise see Paras that Inlines has
-- already rewritten.  Link and Image are order-independent.
return {
  { Link = Link, Image = Image },
  { Inlines = Inlines },
  { Blocks = Blocks },
  { Meta = Meta },
}
