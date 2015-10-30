-- |
-- Module      :  Sound.HTagLib.Getter
-- Copyright   :  © 2015 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- High-level interface for writing audio meta data. You don't need to
-- import this module directly, import "Sound.HTagLib" instead.

module Sound.HTagLib.Setter
  (
  --   TagSetter
  -- , setTags
  -- , titleSetter
  -- , artistSetter
  -- , albumSetter
  -- , commentSetter
  -- , genreSetter
  -- , yearSetter
  -- , trackNumberSetter
  )
where

import Sound.HTagLib.Internal
import Sound.HTagLib.Type

-- newtype TagSetter a = TagSetter { runGetter :: () -> a}
