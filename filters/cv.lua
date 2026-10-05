--[[
cv.lua -- turn the format-neutral CV Markdown into HTML or Typst layout.

The R section functions (R/cv_markdown.R) emit fenced divs and spans with
`cv-*` classes. This filter is the only place that knows about output
formats:

  * HTML  -> semantic elements styled by styles/cv.css
  * Typst -> calls to the functions in typst/typst-template.typ

Structure handled (attributes in brackets):

  .cv-page                        page wrapper (HTML grid; unwrapped in Typst)
  .cv-main                        main column (unwrapped in Typst)
  .cv-aside                       sidebar
    .cv-picture                     portrait image
    .cv-block [#id, title]          titled sidebar block
      .cv-item [icon]                 row with an icon gutter
    .cv-disclaimer                  "Last updated ..." note
  .cv-header                      name + profile (+ HTML-only toolbar)
    .cv-name / .cv-profile
  .cv-section [#id, title, icon, break-after]
    .cv-entry [start, end]
      .cv-title / .cv-org / .cv-location / .cv-body / .cv-links
  span .cv-icon [icon]            inline SVG icon (assets/icons/<icon>.svg)
  span .cv-label                  bold sidebar label
]]

local is_typst = quarto.doc.is_format("typst")
local is_html = quarto.doc.is_format("html")

local input_dir = pandoc.path.directory(quarto.doc.input_file)

-- ---------------------------------------------------------------- helpers

local function has_class(el, class)
  return el.classes and el.classes:includes(class)
end

local function html_escape(s)
  return (s:gsub("&", "&amp;"):gsub("<", "&lt;"):gsub(">", "&gt;"):gsub('"', "&quot;"))
end

local function typst_string(s)
  return '"' .. s:gsub("\\", "\\\\"):gsub('"', '\\"') .. '"'
end

-- Markdown text (e.g. an attribute value) -> Inlines
local function md_inlines(s)
  return pandoc.utils.blocks_to_inlines(pandoc.read(s, "markdown").blocks)
end

-- Inlines / Blocks -> Typst markup, wrapped as a content block `[...]`
local function typst_content_inlines(inlines)
  local markup = pandoc.write(pandoc.Pandoc({ pandoc.Plain(inlines) }), "typst")
  return "[" .. markup:gsub("%s+$", "") .. "]"
end

local function typst_content_blocks(blocks)
  local markup = pandoc.write(pandoc.Pandoc(blocks), "typst")
  return "[" .. markup:gsub("%s+$", "") .. "]"
end

local function raw_typst(code)
  return pandoc.RawBlock("typst", code)
end

local function raw_html(code)
  return pandoc.RawBlock("html", code)
end

-- Split a container's children into the named `cv-*` field divs
local function fields(div)
  local out = {}
  for _, block in ipairs(div.content) do
    if block.t == "Div" then
      for _, class in ipairs(block.classes) do
        local name = class:match("^cv%-(.+)$")
        if name then out[name] = block end
      end
    end
  end
  return out
end

local function field_inlines(field)
  return field and pandoc.utils.blocks_to_inlines(field.content) or nil
end

-- All Link elements inside a block list, in order
local function collect_links(blocks)
  local links = {}
  pandoc.Blocks(blocks):walk({
    Link = function(link)
      table.insert(links, link)
    end,
  })
  return links
end

-- ------------------------------------------------------------------ icons

local icon_cache = {}

local function icon_svg(name)
  if icon_cache[name] == nil then
    local path = pandoc.path.join({ input_dir, "assets", "icons", name .. ".svg" })
    local file = io.open(path, "r")
    if not file then
      quarto.log.warning("cv.lua: missing icon '" .. name .. "' (" .. path .. ")")
      icon_cache[name] = false
    else
      local svg = file:read("a")
      file:close()
      -- Width from the viewBox aspect ratio; height is 1em via CSS
      local w, h = svg:match('viewBox="[%d.]+ [%d.]+ ([%d.]+) ([%d.]+)"')
      local width = (w and h) and string.format("%.3fem", tonumber(w) / tonumber(h)) or "1em"
      icon_cache[name] = svg:gsub(
        "^<svg ",
        string.format('<svg class="cv-icon cv-icon-%s" aria-hidden="true" style="width:%s" ', name, width),
        1
      ):gsub("%s+$", "")
    end
  end
  return icon_cache[name]
end

local function icon_inline(name)
  if is_typst then
    return pandoc.RawInline("typst", "#cv-icon(" .. typst_string(name) .. ")")
  end
  local svg = icon_svg(name)
  return svg and pandoc.RawInline("html", svg) or pandoc.Str("")
end

-- ------------------------------------------------------------------- spans

local function Span(span)
  if has_class(span, "cv-icon") then
    return icon_inline(span.attributes.icon or "link")
  end
  if has_class(span, "cv-label") and is_typst then
    local out = pandoc.Inlines({ pandoc.RawInline("typst", "#cv-label[") })
    out:extend(span.content)
    out:insert(pandoc.RawInline("typst", "]"))
    return out
  end
end

-- ------------------------------------------------------------------- divs

local function entry(div)
  local f = fields(div)
  local start, finish = div.attributes.start, div.attributes["end"]
  local links = f.links and collect_links(f.links.content) or {}

  if is_typst then
    local args = {}
    if start then table.insert(args, "start: " .. typst_string(start)) end
    if finish then table.insert(args, "end: " .. typst_string(finish)) end
    table.insert(args, "title: " .. typst_content_inlines(field_inlines(f.title) or {}))
    if f.org then table.insert(args, "org: " .. typst_content_inlines(field_inlines(f.org))) end
    if f.location then
      table.insert(args, "location: " .. typst_content_inlines(field_inlines(f.location)))
    end
    if f.body then table.insert(args, "body: " .. typst_content_blocks(f.body.content)) end
    if #links > 0 then
      local items = {}
      for _, link in ipairs(links) do
        table.insert(items, typst_content_inlines({ link }) .. ",")
      end
      table.insert(args, "links: (" .. table.concat(items, " ") .. ")")
    end
    return raw_typst("#cv-entry(\n  " .. table.concat(args, ",\n  ") .. ",\n)")
  end

  -- HTML
  local date = pandoc.Inlines({})
  if start then date:insert(pandoc.Span(start)) end
  if finish then date:insert(pandoc.Span(finish)) end

  local details = pandoc.Blocks({})
  if f.title then
    details:insert(raw_html('<h3 class="cv-title">'))
    details:insert(pandoc.Plain(field_inlines(f.title)))
    details:insert(raw_html("</h3>"))
  end
  if f.org or f.location then
    local meta = pandoc.Inlines({})
    if f.org then meta:insert(pandoc.Span(field_inlines(f.org), { class = "cv-org" })) end
    if f.location then
      local loc = pandoc.Inlines({ icon_inline("location-dot"), pandoc.Space() })
      loc:extend(field_inlines(f.location))
      meta:insert(pandoc.Span(loc, { class = "cv-location" }))
    end
    details:insert(pandoc.Div(pandoc.Plain(meta), { class = "cv-meta" }))
  end
  if f.body then details:insert(pandoc.Div(f.body.content, { class = "cv-body" })) end

  local blocks = pandoc.Blocks({
    pandoc.Div(pandoc.Plain(date), { class = "cv-date" }),
    pandoc.Div({}, { class = "cv-decorator", ["aria-hidden"] = "true" }),
    pandoc.Div(details, { class = "cv-details" }),
  })
  if #links > 0 then
    local items = pandoc.Inlines({})
    for _, link in ipairs(links) do
      link.classes:insert("cv-button")
      items:insert(link)
    end
    blocks:insert(pandoc.Div(pandoc.Plain(items), { class = "cv-links" }))
  end
  return pandoc.Div(blocks, { class = "cv-entry" })
end

local function section(div)
  local title = div.attributes.title or ""
  local icon = div.attributes.icon
  if is_typst then
    local args = { "title: " .. typst_content_inlines(md_inlines(title)) }
    if icon then table.insert(args, "icon: " .. typst_string(icon)) end
    local out = pandoc.Blocks({ raw_typst("#cv-section(" .. table.concat(args, ", ") .. ")[") })
    out:extend(div.content)
    out:insert(raw_typst("]"))
    if div.attributes["break-after"] == "true" then
      out:insert(raw_typst("#pagebreak(weak: true)"))
    end
    return out
  end

  local heading = '<h2 class="cv-section-title">'
    .. (icon and (icon_svg(icon) or "") or "")
    .. "<span>" .. html_escape(title) .. "</span></h2>"
  local out = pandoc.Blocks({
    raw_html(string.format('<section class="cv-section" id="%s">', html_escape(div.identifier))),
    raw_html(heading),
  })
  out:extend(div.content)
  out:insert(raw_html("</section>"))
  return out
end

local function block(div)
  local title = div.attributes.title or ""
  if is_typst then
    local out = pandoc.Blocks({
      raw_typst("#cv-block(title: " .. typst_content_inlines(md_inlines(title)) .. ")["),
    })
    out:extend(div.content)
    out:insert(raw_typst("]"))
    return out
  end
  local out = pandoc.Blocks({
    raw_html(string.format('<section class="cv-block" id="%s">', html_escape(div.identifier))),
    raw_html('<h2 class="cv-block-title">' .. html_escape(title) .. "</h2>"),
  })
  out:extend(div.content)
  out:insert(raw_html("</section>"))
  return out
end

local function item(div)
  local icon = div.attributes.icon or "link"
  local inlines = pandoc.utils.blocks_to_inlines(div.content)
  if is_typst then
    return raw_typst("#cv-item(" .. typst_string(icon) .. ", " .. typst_content_inlines(inlines) .. ")")
  end
  return pandoc.Div(
    pandoc.Plain({ icon_inline(icon), pandoc.Span(inlines, { class = "cv-item-text" }) }),
    { class = "cv-item" }
  )
end

local function picture(div)
  local image
  div:walk({ Image = function(img) image = image or img end })
  if not image then return {} end
  if is_typst then
    return raw_typst("#cv-picture(" .. typst_string(image.src) .. ")")
  end
  image.attributes.id = "picture"
  return pandoc.Div(pandoc.Plain({ image }), { class = "cv-picture" })
end

local function header(div)
  local f = fields(div)
  if is_typst then
    local args = {}
    if f.name then table.insert(args, "name: " .. typst_content_inlines(field_inlines(f.name))) end
    if f.profile then table.insert(args, "profile: " .. typst_content_blocks(f.profile.content)) end
    return raw_typst("#cv-header(" .. table.concat(args, ", ") .. ")")
  end
  local out = pandoc.Blocks({ raw_html('<header class="cv-header">') })
  for _, child in ipairs(div.content) do
    if child.t == "Div" and has_class(child, "cv-name") then
      out:insert(raw_html('<h1 class="cv-name">'))
      out:insert(pandoc.Plain(pandoc.utils.blocks_to_inlines(child.content)))
      out:insert(raw_html("</h1>"))
    else
      out:insert(child)
    end
  end
  out:insert(raw_html("</header>"))
  return out
end

local function wrap_typst(fn, div)
  local out = pandoc.Blocks({ raw_typst("#" .. fn .. "[") })
  out:extend(div.content)
  out:insert(raw_typst("]"))
  return out
end

local function Div(div)
  if has_class(div, "cv-entry") then return entry(div) end
  if has_class(div, "cv-section") then return section(div) end
  if has_class(div, "cv-block") then return block(div) end
  if has_class(div, "cv-item") then return item(div) end
  if has_class(div, "cv-picture") then return picture(div) end
  if has_class(div, "cv-header") then return header(div) end
  if is_typst then
    if has_class(div, "cv-aside") then return wrap_typst("cv-aside", div) end
    if has_class(div, "cv-disclaimer") then return wrap_typst("cv-disclaimer", div) end
    if has_class(div, "cv-main") or has_class(div, "cv-page") then return div.content end
  end
  if is_html and has_class(div, "cv-aside") then
    local out = pandoc.Blocks({ raw_html('<aside class="cv-aside">') })
    out:extend(div.content)
    out:insert(raw_html("</aside>"))
    return out
  end
end

-- Spans first (icons/labels become raw inlines), then divs bottom-up, so
-- every container sees already-converted children.
return {
  { Span = Span },
  { Div = Div },
}
