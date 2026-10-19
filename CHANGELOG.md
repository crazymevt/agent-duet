# Changelog

## Unreleased

- Markdown blockquotes keep unquoted text only when it continues an open
  paragraph, including paragraphs inside nested quotes and list items.
  Text after quoted code, headings, tables or blank lines stays outside.
- Markdown previews read quote contents on demand so documents with many
  quotes ending at unquoted text do not repeatedly scan the remaining lines.
