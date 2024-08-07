module Sol (
    findSolver,
) where

import qualified Data.Map as M (Map, fromList, lookup)

import qualified Sol.P0001 as P0001 (solve)
import qualified Sol.P0002 as P0002 (solve)
import qualified Sol.P0003 as P0003 (solve)
import qualified Sol.P0004 as P0004 (solve)
import qualified Sol.P0005 as P0005 (solve)
import qualified Sol.P0006 as P0006 (solve)
import qualified Sol.P0007 as P0007 (solve)
import qualified Sol.P0008 as P0008 (solve)
import qualified Sol.P0009 as P0009 (solve)
import qualified Sol.P0010 as P0010 (solve)
import qualified Sol.P0011 as P0011 (solve)
import qualified Sol.P0012 as P0012 (solve)
import qualified Sol.P0013 as P0013 (solve)
import qualified Sol.P0014 as P0014 (solve)
import qualified Sol.P0015 as P0015 (solve)
import qualified Sol.P0016 as P0016 (solve)
import qualified Sol.P0017 as P0017 (solve)
import qualified Sol.P0018 as P0018 (solve)
import qualified Sol.P0019 as P0019 (solve)
import qualified Sol.P0020 as P0020 (solve)
import qualified Sol.P0021 as P0021 (solve)
import qualified Sol.P0022 as P0022 (solve)
import qualified Sol.P0023 as P0023 (solve)
import qualified Sol.P0024 as P0024 (solve)
import qualified Sol.P0025 as P0025 (solve)
import qualified Sol.P0026 as P0026 (solve)
import qualified Sol.P0027 as P0027 (solve)
import qualified Sol.P0028 as P0028 (solve)
import qualified Sol.P0029 as P0029 (solve)
import qualified Sol.P0030 as P0030 (solve)
import qualified Sol.P0031 as P0031 (solve)
import qualified Sol.P0032 as P0032 (solve)
import qualified Sol.P0033 as P0033 (solve)
import qualified Sol.P0034 as P0034 (solve)
import qualified Sol.P0035 as P0035 (solve)
import qualified Sol.P0036 as P0036 (solve)
import qualified Sol.P0037 as P0037 (solve)
import qualified Sol.P0038 as P0038 (solve)
import qualified Sol.P0039 as P0039 (solve)
import qualified Sol.P0040 as P0040 (solve)
import qualified Sol.P0041 as P0041 (solve)
import qualified Sol.P0042 as P0042 (solve)
import qualified Sol.P0043 as P0043 (solve)
import qualified Sol.P0044 as P0044 (solve)
import qualified Sol.P0045 as P0045 (solve)
import qualified Sol.P0046 as P0046 (solve)
import qualified Sol.P0047 as P0047 (solve)
import qualified Sol.P0048 as P0048 (solve)
import qualified Sol.P0049 as P0049 (solve)
import qualified Sol.P0050 as P0050 (solve)
import qualified Sol.P0051 as P0051 (solve)
import qualified Sol.P0052 as P0052 (solve)
import qualified Sol.P0053 as P0053 (solve)
import qualified Sol.P0054 as P0054 (solve)
import qualified Sol.P0055 as P0055 (solve)
import qualified Sol.P0056 as P0056 (solve)
import qualified Sol.P0057 as P0057 (solve)
import qualified Sol.P0058 as P0058 (solve)
import qualified Sol.P0059 as P0059 (solve)
import qualified Sol.P0060 as P0060 (solve)
import qualified Sol.P0061 as P0061 (solve)
import qualified Sol.P0062 as P0062 (solve)
import qualified Sol.P0063 as P0063 (solve)
import qualified Sol.P0064 as P0064 (solve)
import qualified Sol.P0065 as P0065 (solve)
import qualified Sol.P0066 as P0066 (solve)
import qualified Sol.P0067 as P0067 (solve)
import qualified Sol.P0068 as P0068 (solve)
import qualified Sol.P0069 as P0069 (solve)
import qualified Sol.P0070 as P0070 (solve)
import qualified Sol.P0071 as P0071 (solve)
import qualified Sol.P0072 as P0072 (solve)
import qualified Sol.P0073 as P0073 (solve)
import qualified Sol.P0074 as P0074 (solve)
import qualified Sol.P0075 as P0075 (solve)
import qualified Sol.P0076 as P0076 (solve)
import qualified Sol.P0077 as P0077 (solve)
import qualified Sol.P0078 as P0078 (solve)
import qualified Sol.P0079 as P0079 (solve)
import qualified Sol.P0080 as P0080 (solve)
import qualified Sol.P0081 as P0081 (solve)
import qualified Sol.P0082 as P0082 (solve)
import qualified Sol.P0083 as P0083 (solve)
import qualified Sol.P0084 as P0084 (solve)
import qualified Sol.P0085 as P0085 (solve)
import qualified Sol.P0086 as P0086 (solve)
import qualified Sol.P0087 as P0087 (solve)
import qualified Sol.P0088 as P0088 (solve)
import qualified Sol.P0089 as P0089 (solve)
import qualified Sol.P0090 as P0090 (solve)
import qualified Sol.P0091 as P0091 (solve)
import qualified Sol.P0092 as P0092 (solve)
import qualified Sol.P0093 as P0093 (solve)
import qualified Sol.P0094 as P0094 (solve)
import qualified Sol.P0095 as P0095 (solve)
import qualified Sol.P0096 as P0096 (solve)
import qualified Sol.P0097 as P0097 (solve)
import qualified Sol.P0098 as P0098 (solve)
import qualified Sol.P0099 as P0099 (solve)
import qualified Sol.P0100 as P0100 (solve)

{- FOURMOLU_DISABLE -}
funcTbl :: M.Map String String
funcTbl =
    M.fromList
        [ ("1", P0001.solve), ("2", P0002.solve), ("3", P0003.solve), ("4", P0004.solve), ("5", P0005.solve)
        , ("6", P0006.solve), ("7", P0007.solve), ("8", P0008.solve), ("9", P0009.solve), ("10", P0010.solve)
        , ("11", P0011.solve), ("12", P0012.solve), ("13", P0013.solve), ("14", P0014.solve), ("15", P0015.solve)
        , ("16", P0016.solve), ("17", P0017.solve), ("18", P0018.solve), ("19", P0019.solve), ("20", P0020.solve)
        , ("21", P0021.solve), ("22", P0022.solve), ("23", P0023.solve), ("24", P0024.solve), ("25", P0025.solve)
        , ("26", P0026.solve), ("27", P0027.solve), ("28", P0028.solve), ("29", P0029.solve), ("30", P0030.solve)
        , ("31", P0031.solve), ("32", P0032.solve), ("33", P0033.solve), ("34", P0034.solve), ("35", P0035.solve)
        , ("36", P0036.solve), ("37", P0037.solve), ("38", P0038.solve), ("39", P0039.solve), ("40", P0040.solve)
        , ("41", P0041.solve), ("42", P0042.solve), ("43", P0043.solve), ("44", P0044.solve), ("45", P0045.solve)
        , ("46", P0046.solve), ("47", P0047.solve), ("48", P0048.solve), ("49", P0049.solve), ("50", P0050.solve)
        , ("51", P0051.solve), ("52", P0052.solve), ("53", P0053.solve), ("54", P0054.solve), ("55", P0055.solve)
        , ("56", P0056.solve), ("57", P0057.solve), ("58", P0058.solve), ("59", P0059.solve), ("60", P0060.solve)
        , ("61", P0061.solve), ("62", P0062.solve), ("63", P0063.solve), ("64", P0064.solve), ("65", P0065.solve)
        , ("66", P0066.solve), ("67", P0067.solve), ("68", P0068.solve), ("69", P0069.solve), ("70", P0070.solve)
        , ("71", P0071.solve), ("72", P0072.solve), ("73", P0073.solve), ("74", P0074.solve), ("75", P0075.solve)
        , ("76", P0076.solve), ("77", P0077.solve), ("78", P0078.solve), ("79", P0079.solve), ("80", P0080.solve)
        , ("81", P0081.solve), ("82", P0082.solve), ("83", P0083.solve), ("84", P0084.solve), ("85", P0085.solve)
        , ("86", P0086.solve), ("87", P0087.solve), ("88", P0088.solve), ("89", P0089.solve), ("90", P0090.solve)
        , ("91", P0091.solve), ("92", P0092.solve), ("93", P0093.solve), ("94", P0094.solve), ("95", P0095.solve)
        , ("96", P0096.solve), ("97", P0097.solve), ("98", P0098.solve), ("99", P0099.solve), ("100", P0100.solve)
        ]
{- FOURMOLU_ENABLE -}

findSolver :: String -> Maybe String
findSolver = flip M.lookup funcTbl
