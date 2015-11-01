-- -*- Mode: Haskell; -*-
--
-- HTagLib tests, testing of getters.
--
-- Copyright © 2015 Mark Karpov
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

module Getter (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit hiding (Test, path)

import Sound.HTagLib
import Util

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, (<$>))
#endif

tests :: Test
tests = testGroup "Getters" $
        fmap (caseWithFile simpleGetter . fst) fileList ++
        fmap (caseWithFile specializedGetter) fileList

simpleGetter :: FilePath -> Assertion
simpleGetter path = do
  tags <- getTags path (id <$> sampleGetter path)
  tags @?= sampleTags path

specializedGetter :: (FilePath, FileType) -> Assertion
specializedGetter (path, t) = do
  tags <- getTags' path t (id <$> sampleGetter path)
  tags @?= sampleTags path
