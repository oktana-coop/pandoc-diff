module RichTextDiffOp (RichTextDiffOp (..), RichTextDiffOpType (..), MarkDiff (..), HeadingLevelDiff (..), MetaDiff (..), getDiffOpType, unpackDiffOpValue) where

import DocTree.Common (Mark (..))
import Text.Pandoc.Definition as Pandoc (Meta)

data MetaDiff = MetaDiff Meta Meta deriving (Show, Eq)

data MarkDiff = MarkDiff [Mark] [Mark] deriving (Show, Eq, Ord)

data HeadingLevelDiff = HeadingLevelDiff Int Int deriving (Show, Eq)

data RichTextDiffOp a = Insert a | Delete a | Copy a | UpdateMeta MetaDiff a | UpdateMarks MarkDiff a | UpdateHeadingLevel HeadingLevelDiff a deriving (Show, Eq)

data RichTextDiffOpType
  = InsertType
  | DeleteType
  | CopyType
  | UpdateMetaType
  | UpdateMarksType
  | UpdateHeadingLevelType
  deriving (Show, Eq)

getDiffOpType :: RichTextDiffOp a -> RichTextDiffOpType
getDiffOpType (Insert _) = InsertType
getDiffOpType (Delete _) = DeleteType
getDiffOpType (Copy _) = CopyType
getDiffOpType (UpdateMeta _ _) = UpdateMetaType
getDiffOpType (UpdateMarks _ _) = UpdateMarksType
getDiffOpType (UpdateHeadingLevel _ _) = UpdateHeadingLevelType

unpackDiffOpValue :: RichTextDiffOp a -> a
unpackDiffOpValue (Insert a) = a
unpackDiffOpValue (Delete a) = a
unpackDiffOpValue (Copy a) = a
unpackDiffOpValue (UpdateMeta _ a) = a
unpackDiffOpValue (UpdateMarks _ a) = a
unpackDiffOpValue (UpdateHeadingLevel _ a) = a

instance Functor RichTextDiffOp where
  fmap f (Insert a) = Insert (f a)
  fmap f (Delete a) = Delete (f a)
  fmap f (Copy a) = Copy (f a)
  fmap f (UpdateMeta metaDiff a) = UpdateMeta metaDiff (f a)
  fmap f (UpdateMarks markDiff a) = UpdateMarks markDiff (f a)
  fmap f (UpdateHeadingLevel levelDiff a) = UpdateHeadingLevel levelDiff (f a)
