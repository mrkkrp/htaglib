--
-- HTagLib tests, testing of getters.
--
-- Copyright © 2015–2017 Mark Karpov
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

module Sound.HTagLib.GetterSpec (spec) where

import Sound.HTagLib
import Sound.HTagLib.Test.Util
import Test.Hspec

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

spec :: Spec
spec =
  describe "getters" $ do
    mapM_ (withFile $ const simpleGetter) fileList
    mapM_ (withFile specializedGetter)    fileList

simpleGetter :: AudioTags -> Expectation
simpleGetter tags = do
  let path = atFileName tags
  extracted <- getTags path (id <$> sampleGetter path)
  extracted `shouldMatchTags` tags

specializedGetter :: FileType -> AudioTags -> Expectation
specializedGetter t tags = do
  let path = atFileName tags
  extracted <- getTags' path t (id <$> sampleGetter path)
  extracted `shouldMatchTags` tags
