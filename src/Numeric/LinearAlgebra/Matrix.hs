module Numeric.LinearAlgebra.Matrix
( module C
, module M44
, module M33
, Mat44
, Mat33
) where

import Numeric.LinearAlgebra.Matrix.Class as C
import Numeric.LinearAlgebra.Matrix.Mat44 as M44 hiding (Mat44(..))
import Numeric.LinearAlgebra.Matrix.Mat44 (Mat44)
import Numeric.LinearAlgebra.Matrix.Mat33 as M33 hiding (Mat33(..))
import Numeric.LinearAlgebra.Matrix.Mat33 (Mat33)

