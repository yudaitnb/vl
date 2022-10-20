module Translation.RenameExVars where

import Syntax.LambdaVL

-- Exp, UEnv, TEnvが操作対象。
-- exp中の全ての外部モジュール参照の変数のために新しい名前を用意する。
-- 新しく生成された変数は、旧変数の持っている制約を全てコピーする。 
-- <= これをするためには最般型がわかってないといけないので、一回解く必要がある
-- 