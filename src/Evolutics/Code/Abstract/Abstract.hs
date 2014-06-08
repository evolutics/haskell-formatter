module Evolutics.Code.Abstract.Abstract (Abstract, create) where
import qualified Language.Haskell.Exts.Annotated as Exts

data Abstract = Abstract{root :: Exts.Module [AbstractComment]}

data AbstractComment = AbstractComment{displacement ::
                                       Displacement,
                                       isMultiLine :: Bool, content :: String}

data Displacement = Before
                  | After

create :: Exts.Module [AbstractComment] -> Abstract
create root = Abstract{root = root}
