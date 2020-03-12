# scrillex

## Introduction

**scrillex** intends to provide some scribble syntax for creating Open Office XML WordprocessingML elements, to be used in or with `.docx` documents.

## Markdown

For now, though, it extends the [3bmd](https://github.com/3b/3bmd) Markdown processor to produce WordprocessingML.

Using `(3bmd:parse-string-and-print-to-stream string stream :format :wml)`,
`(3bmd:parse-and-print-to-stream file stream :format :wml)` or
`(3bmd:print-doc-to-stream doc stream :format :wml)` (note the `:wml`) will produce
the expected output. (But see below.)

That is

```lisp
(3bmd:parse-string-and-print-to-stream "\"This is a **_fine_ mess** you've gotten _me_ into!\" he said." *standard-output* :format :wml)
```

yields

```xml
<w:p><w:r><w:t xml:space="preserve">&quot;This is a </w:t></w:r><w:r><w:rPr><w:i w:val="true"/><w:iCs w:val="true"/><w:b w:val="true"/><w:bCs w:val="true"/></w:rPr><w:t>fine</w:t></w:r><w:r><w:rPr><w:b w:val="true"/><w:bCs w:val="true"/></w:rPr><w:t xml:space="preserve"> mess</w:t></w:r><w:r><w:t xml:space="preserve"> you've gotten </w:t></w:r><w:r><w:rPr><w:i w:val="true"/><w:iCs w:val="true"/></w:rPr><w:t>me</w:t></w:r><w:r><w:t xml:space="preserve"> into!&quot; he said.</w:t></w:r></w:p>
```

**Smart quotes** (bind `3bmd:*smart-quotes*` to a non-`NIL` value) and **tables** (bind `3bmd-tables:*tables*` similarly) are supported.

*function* **MD->DOCX** `infile` `outfile`

Reads the Markdown file `infile` and produces a new Microsoft Word `.docx` file named `outfile`. Smart quotes and tables are enabled. Elements are styled with the following custom styles, which are added to `outfile`:

  * _MD Heading 1_ to _MD Heading 6_ inclusive for headings
  * _MD Code_ for code elements
  * _MD Link_ for hyperlinks
  * _MD Quote_ for block quotes
  * _MD Table_ for tables
  * _MD ListParagraph_ for paragraphs in lists

Ordered and unordered list numbering styles are provided.

These are deliberately fairly minimal: the idea is that you can modify the styles in your favourite wordprocessor later.

*special variable* **\*HTML-IS-WML\***

When bound to a non-`NIL` value treats `html`-like content as WordprocessingML markup and inserts it into the document. This makes it very easy to create an invalid document. When bound to `NIL` (the default), `html` content is ignored.

As an example the text `Some inline </w:t><w:br /><w:t> text` will be rendered as

> Some inline  text

when `*HTML-IS-WML*` is `NIL` (the default), and

> Some inline
>
> text

otherwise.

## Limitations

As you might expect, `html` is not supported.

Images are a work in progress.
