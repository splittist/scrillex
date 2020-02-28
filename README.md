# scrillex

## Introduction

**scrillex** intends to provide some scribble syntax for creating Open Office XML WordprocessingML elements, to be used in or with `.docx` documents.

## Markdown

For now, though, it extends the [3bmd](https://github.com/3b/3bmd) Markdown processor to produce WordprocessingML.

Using `(3bmd:parse-string-and-print-to-stream string stream :format :wml)`,
`(3bmd:parse-and-print-to-stream file stream :format :wml)` or
`(3bmd:print-doc-to-stream doc stream :format :wml)` (note the `:wml`) will produce
the expected output. (But see below.)

## Limitations

References, links etc. are not currently implemented. Neither is `html`, which is perhaps more to be expected.
Numbering of ordered lists is unlikely to behave as expected.
