// ============================================================
// CV layout for the PDF output (Quarto -> Typst).
//
// filters/cv.lua turns the format-neutral CV Markdown into calls to
// the cv-* functions below; cv() is applied to the whole document by
// typst/typst-show.typ. Colours + fonts come from themes/themes.yml via
// the generated typst/_theme.typ (written by R/theme.R at render time).
//
// NB: this file is a Pandoc template partial -- a literal dollar sign
// must be written twice.
// ============================================================

#import "typst/_theme.typ": cv-theme, cv-palette, cv-fonts

// Page geometry: the sidebar is a full-height band on the right of every
// page; the main column fills the rest.
#let cv-dims = (
  page-width: 210mm,
  page-height: 297mm,
  sidebar: 64mm,     // band width (to the page edge)
  gap: 7mm,          // main column -> band
  pad: 7mm,          // band inner horizontal padding
  margin-x: 13mm,    // left page margin
  margin-y: 13mm,    // top/bottom page margin
  date: 16mm,        // date column
  deco: 3mm,         // date column -> timeline line
  rail: 4.5mm,       // timeline line -> entry text
  dot: 3.1pt,        // timeline dot radius
)

// Inline Font Awesome icon from assets/icons/, tinted with the current
// text colour (the SVGs use fill="currentColor").
#let cv-icon(name, height: 0.9em) = context {
  let fill = text.fill
  let hex = if type(fill) == color { fill.to-hex() } else { "#000000" }
  let svg = read("assets/icons/" + name + ".svg").replace("currentColor", hex)
  box(baseline: 0.13em, image(bytes(svg), format: "svg", height: height))
}

// ---------------------------------------------------------- sidebar

#let cv-label(body) = text(weight: 700, fill: cv-theme.sidebar-label, body)

#let cv-item(icon, body) = block(
  above: 0.62em,
  below: 0.62em,
  grid(
    columns: (1.25em, 1fr),
    column-gutter: 0.45em,
    align(center, text(fill: cv-theme.sidebar-label, cv-icon(icon))),
    body,
  ),
)

#let cv-block(title: none, body) = block(above: 7mm, below: 0mm, width: 100%, {
  block(
    below: 2.6mm,
    text(
      weight: 700,
      size: 10.5pt,
      tracking: 0.02em,
      fill: cv-theme.sidebar-heading,
      upper(title),
    ),
  )
  set par(leading: 0.48em, spacing: 0.75em)
  body
})

#let cv-picture(path) = align(
  center,
  box(
    radius: 50%,
    inset: 2.6pt,
    fill: gradient.linear(cv-palette.forest, cv-palette.burnt),
    box(
      radius: 50%,
      clip: true,
      stroke: 2.6pt + cv-palette.surface,
      image(path, width: 38mm, height: 38mm, fit: "cover"),
    ),
  ),
)

#let cv-disclaimer(body) = place(
  bottom + right,
  block(
    width: 100%,
    align(right, text(size: 7pt, style: "italic", fill: cv-theme.sidebar-muted, body)),
  ),
)

// The sidebar content is placed into the band of the page it starts on
// (the first page), outside the main text flow.
#let cv-aside(body) = place(
  top + left,
  dx: 100% + cv-dims.gap,
  dy: -cv-dims.margin-y,
  box(
    width: cv-dims.sidebar,
    height: cv-dims.page-height,
    inset: (x: cv-dims.pad, top: 11mm, bottom: cv-dims.margin-y),
    {
      set text(size: 8.2pt, fill: cv-theme.sidebar-text)
      show link: set text(fill: cv-theme.sidebar-link)
      body
    },
  ),
)

// ------------------------------------------------------- main column

#let cv-header(name: none, profile: none) = block(below: 2mm, {
  block(
    below: 3mm,
    text(
      weight: 900,
      size: 23pt,
      tracking: -0.02em,
      fill: cv-theme.heading,
      upper(name),
    ),
  )
  if profile != none {
    set par(leading: 0.55em, justify: true)
    text(size: 8.4pt, profile)
  }
})

// Shared column layout of section headings and entries: date | gap |
// timeline line + content.
#let cv-columns = (cv-dims.date, cv-dims.deco, 1fr)

#let cv-section(title: none, icon: none, body) = {
  block(
    above: 6.5mm,
    below: 0.5mm,
    sticky: true,
    grid(
      columns: cv-columns,
      [],
      grid.cell(
        colspan: 2,
        inset: (left: cv-dims.deco - 0.65em),
        text(size: 11.5pt, fill: cv-theme.primary, {
          box(width: 1.3em, align(center, cv-icon(icon, height: 0.95em)))
          h(cv-dims.rail - 0.65em)
          text(weight: 700, tracking: 0.01em, upper(title))
        }),
      ),
    ),
  )
  body
}

#let cv-entry(
  start: none,
  end: none,
  title: none,
  org: none,
  location: none,
  body: none,
  links: (),
) = {
  let date = {
    set text(size: 6.6pt, weight: 700, tracking: 0.02em, fill: cv-theme.date)
    align(center, {
      if start != none { start }
      if end != none {
        // thin vertical divider between start and end
        block(above: 0.3em, below: 0.3em, line(angle: 90deg, length: 0.8em, stroke: 0.5pt + cv-theme.muted))
        end
      }
    })
  }

  let dot = place(
    top + left,
    dx: -cv-dims.rail - cv-dims.dot,
    dy: 0.2em,
    circle(
      radius: cv-dims.dot,
      fill: cv-theme.dot,
      stroke: 1.2pt + cv-theme.dot-ring,
    ),
  )

  // Links sit in the sidebar band, level with the entry, so they take the
  // sidebar colours (the band is dark in the forest theme)
  let link-column = if links.len() > 0 {
    place(
      top + left,
      dx: 100% + cv-dims.gap + cv-dims.pad,
      box(
        width: cv-dims.sidebar - 2 * cv-dims.pad,
        {
          set text(size: 7.6pt, weight: 700, fill: cv-theme.sidebar-label)
          show link: set text(fill: cv-theme.sidebar-label)
          stack(dir: ttb, spacing: 0.75em, ..links)
        },
      ),
    )
  }

  let details = {
    dot
    link-column
    text(size: 8.8pt, weight: 700, fill: cv-theme.heading, title)
    if org != none or location != none {
      block(
        above: 0.55em,
        below: 0em,
        grid(
          columns: (1fr, auto),
          column-gutter: 3mm,
          text(size: 8.1pt, org),
          if location != none {
            text(size: 7.4pt, fill: cv-theme.muted, {
              cv-icon("location-dot")
              h(0.25em)
              location
            })
          },
        ),
      )
    }
    if body != none {
      block(above: 0.75em, below: 0em, {
        set text(size: 7.7pt)
        set par(leading: 0.5em, spacing: 0.6em)
        body
      })
    }
  }

  block(
    breakable: false,
    above: 0pt,
    below: 0pt,
    grid(
      columns: cv-columns,
      stroke: (x, y) => if x == 2 { (left: 0.6pt + cv-theme.rule) },
      inset: (x, y) => (
        top: 2.2mm,
        bottom: 2.2mm,
        left: if x == 2 { cv-dims.rail } else { 0pt },
      ),
      date, [], details,
    ),
  )
}

// ------------------------------------------------------------ document

#let cv(title: none, author: none, lang: "en", doc) = {
  set document(title: title, author: author)
  set page(
    width: cv-dims.page-width,
    height: cv-dims.page-height,
    margin: (
      left: cv-dims.margin-x,
      right: cv-dims.sidebar + cv-dims.gap,
      top: cv-dims.margin-y,
      bottom: cv-dims.margin-y,
    ),
    numbering: none,
    fill: cv-theme.page-bg,
    background: place(
      top + right,
      rect(width: cv-dims.sidebar, height: 100%, fill: cv-theme.sidebar-bg),
    ),
  )
  set text(
    font: (cv-fonts.sans,),
    size: 8.5pt,
    fill: cv-theme.text,
    lang: lang,
  )
  set par(leading: 0.5em, spacing: 0.7em)
  set list(
    marker: text(fill: cv-theme.accent, weight: 700)[•],
    indent: 0.5mm,
    body-indent: 1.6mm,
    spacing: 0.5em,
  )
  show emph: set text(font: (cv-fonts.serif,), size: 1.04em)
  show link: set text(fill: cv-theme.link)
  show underline: set underline(offset: 1.5pt, stroke: 0.5pt)
  doc
}
