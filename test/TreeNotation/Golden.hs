module TreeNotation.Golden (tests) where

import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import TreeNotation.Utils (readFilesAndProduceTreeDiff)

tests :: IO TestTree
tests =
  return $
    testGroup
      "Markdown Inputs → Annotated Diff Tree (Golden)"
      [ -- Inline-level
        testGroup
          "inline-text"
          [ goldenCase $ "inline-text" </> "insert-word",
            goldenCase $ "inline-text" </> "delete-word",
            goldenCase $ "inline-text" </> "replace-word",
            goldenCase $ "inline-text" </> "append-clause"
          ],
        testGroup
          "punctuation"
          [ goldenCase $ "punctuation" </> "append-period",
            goldenCase $ "punctuation" </> "internal-commas",
            goldenCase $ "punctuation" </> "hyphenate",
            goldenCase $ "punctuation" </> "curly-apostrophe",
            goldenCase $ "punctuation" </> "double-quotes",
            goldenCase $ "punctuation" </> "change-quote-style"
          ],
        testGroup
          "whitespace"
          [ goldenCase $ "whitespace" </> "collapse-spaces",
            -- A soft break converts to a plain space, so re-wrapping alone yields no diff.
            goldenCase $ "whitespace" </> "add-soft-break",
            goldenCase $ "whitespace" </> "add-hard-line-break",
            goldenCase $ "whitespace" </> "edit-across-hard-line-break"
          ],
        testGroup
          "marks"
          [ goldenCase $ "marks" </> "add-emphasis",
            goldenCase $ "marks" </> "remove-strong",
            goldenCase $ "marks" </> "link-target",
            goldenCase $ "marks" </> "mark-plus-edit",
            -- Known issue: Strikeout is discarded during conversion.
            goldenCase $ "marks" </> "strikeout-edit",
            goldenCase $ "marks" </> "code-span-edit"
          ],
        testGroup
          "numbers"
          [ goldenCase $ "numbers" </> "change-year",
            goldenCase $ "numbers" </> "change-unit"
          ],
        testGroup
          "math"
          -- Known issue: Math is discarded during conversion.
          [ goldenCase $ "math" </> "inline-math-edit"
          ],
        testGroup
          "unicode"
          [ goldenCase $ "unicode" </> "cjk-one-char",
            goldenCase $ "unicode" </> "change-emoji"
          ],
        -- Block-level
        testGroup
          "blocks"
          [ goldenCase $ "blocks" </> "add-paragraph",
            goldenCase $ "blocks" </> "split-paragraph",
            goldenCase $ "blocks" </> "heading-level"
          ],
        testGroup
          "lists"
          [ goldenCase $ "lists" </> "append-item-to-list",
            goldenCase $ "lists" </> "add-item-in-the-middle-of-list",
            goldenCase $ "lists" </> "delete-list-item-in-the-middle",
            -- Known issue: items align by position, so the unchanged "Item 3" is shown renamed rather than kept.
            goldenCase $ "lists" </> "add-item-in-the-middle-of-list-and-rename-next-item",
            goldenCase $ "lists" </> "add-nested-list",
            goldenCase $ "lists" </> "add-bullet-list"
          ],
        testGroup
          "notes"
          [ goldenCase $ "notes" </> "add-note-in-the-end-of-paragraph",
            goldenCase $ "notes" </> "add-note-in-the-end-of-document",
            goldenCase $ "notes" </> "edit-note-content",
            goldenCase $ "notes" </> "delete-note",
            -- Known issue: deleting a footnote mis-aligns blocks (a paragraph pairs with a
            -- footnote) and re-diffs the surviving note, whose content is unchanged.
            goldenCase $ "notes" </> "footnote-renumber"
          ],
        testGroup
          "images-and-figures"
          [ goldenCase $ "images-and-figures" </> "change-image-src",
            goldenCase $ "images-and-figures" </> "change-caption",
            goldenCase $ "images-and-figures" </> "add-figure",
            goldenCase $ "images-and-figures" </> "add-lone-image"
          ],
        testGroup
          "horizontal-rule"
          [ goldenCase $ "horizontal-rule" </> "add-horizontal-rule",
            goldenCase $ "horizontal-rule" </> "delete-horizontal-rule"
          ],
        -- Cross-cutting
        testGroup
          "moves"
          [ goldenCase $ "moves" </> "swap-paragraphs",
            goldenCase $ "moves" </> "swap-sentences",
            goldenCase $ "moves" </> "swap-list-items"
          ],
        testGroup
          "readability"
          [ goldenCase $ "readability" </> "light-reword",
            goldenCase $ "readability" </> "medium-reword",
            goldenCase $ "readability" </> "heavy-reword",
            goldenCase $ "readability" </> "mostly-reworded",
            goldenCase $ "readability" </> "reword-around-note"
          ],
        -- Mixed (realistic docs combining several edit types)
        testGroup
          "mixed"
          [ goldenCase $ "mixed" </> "guide-section-reword"
          ]
      ]

goldenCase :: FilePath -> TestTree
goldenCase caseSubFolderPath =
  let baseDir = "test/TreeNotation" </> caseSubFolderPath
      md1Input = baseDir </> "doc1.md"
      md2Input = baseDir </> "doc2.md"
      treeGolden = baseDir </> "diff.txt"
   in goldenVsString
        caseSubFolderPath
        treeGolden
        (readFilesAndProduceTreeDiff md1Input md2Input)
