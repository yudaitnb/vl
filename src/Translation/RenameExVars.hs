module Translation.RenameExVars where

import Control.Monad.Trans.State
import Control.Monad (forM_, forM)

import Data.List (nub)
import Data.Map (Map, (!))
import qualified Data.Map as M

import Language.LambdaVL

import Syntax.Common
import Syntax.Common as N (Name(..), QName(..), ModuleName(..))

import Syntax.Env
import Syntax.Type as T

import Util
import Data.Maybe (fromMaybe)
import GHC (noSrcLoc)

-- exp中の外部モジュール由来の値を参照する変数(f)の複製に従い,
-- tenv, uenv, cs中の各fに関連する型制約, 型制約, 制約を複製する
-- e.g.) 
-- exp:  f 1 + f 2 -> main = f' 1 f'' 2
-- tenv: f: [!_a1 Int -> Int]_a2 -> f': [!_a1' Int -> Int]_a2', f'': [!_a1'' Int -> Int]_a2''
-- uenv: a1:Label, a2:Label -> a1':Label, a2':Label, a1'':Label, a2'':Label 
-- cs:   (a2 <= Av1 && a2 <= a2_v1 && a1 <= ??) || (a2 <= Av2 && a2 <= a2_v1 && a1 <= ??)
--    -> ((a2'  <= Av1 && a2'  <= a2_v1 && a1'  <= ??) || (a2'  <= Av2 && a2'  <= a2_v1 && a1'  <= ??)) &&
--       ((a2'' <= Av1 && a2'' <= a2_v1 && a1'' <= ??) || (a2'' <= Av2 && a2'' <= a2_v1 && a1'' <= ??))
-- 結局a2_v1/a2_v2のバージョンは1つに決まらなければならないが、これは言語設計の制約の一つなので問題ない

type CounterTable = Map VarKey Int

data RenameEnv' = RenameEnv'
  { counterTable :: CounterTable  -- 各外部モジュール変数の累積数
  , boundVars :: [VarKey]         -- そのスコープにおける束縛変数のリスト
  } deriving (Show)
type RenameEnv a = State RenameEnv' a

mkInitRenameEnv :: ModName -> CounterTable -> [Decl SrcSpanInfo] -> RenameEnv'
mkInitRenameEnv mn ct decls =
  RenameEnv'
    (M.unionWith max ct $ M.fromList $ zip (freeVarsDecls decls) (repeat 0))
    ( map
        (\(PatBind _ p@(PVar _ n) e) -> QVar mn (getName n))
        decls
      ++ map
        UQVar
        ["+", "-", "*", "/", "&&", "||", "<" ,"<=", ">", ">=", "==", "/=" ] )
  where
    freeVarsDecls :: [Decl SrcSpanInfo] -> [VarKey]
    freeVarsDecls [] = []
    freeVarsDecls (d:rst) = (nub . freeVars . getBind $ d) ++ freeVarsDecls rst

getCounterOf :: VarKey -> RenameEnv Int
getCounterOf vn = do
  gets $ (<!> vn) . counterTable

addCounterOf :: VarKey -> RenameEnv ()
addCounterOf vn = do
  oldct <- gets counterTable
  oldc <- getCounterOf vn
  modify $ \env -> env { counterTable = M.insert vn (oldc + 1) oldct }

newNameOfExVar :: String -> Int -> VarName
newNameOfExVar s c = "__" ++ s ++ "_" ++ show c

newNamesOfExVar :: String -> Int -> Int -> [VarName]
newNamesOfExVar s c cstart = take c $ map (newNameOfExVar s) $ iterate (+1) cstart

newNameOfExVarKey :: VarKey -> Int -> VarKey
newNameOfExVarKey vk i = case vk of
  QVar mn vn -> QVar mn (newNameOfExVar vn i)
  UQVar vn   -> UQVar (newNameOfExVar vn i)

newNamesOfExVarKeys :: VarKey -> Int -> Int -> [VarKey]
newNamesOfExVarKeys vk c cstart = take c $ map (newNameOfExVarKey vk) $ iterate (+1) cstart

newNameOfExTyVar :: String -> Int -> VarName
newNameOfExTyVar s c = s ++ "_" ++ show c

newNamesOfExTyVar :: String -> Int -> Int -> [VarName]
newNamesOfExTyVar s c cstart = take c $ map (newNameOfExTyVar s) $ iterate (+1) cstart

-- 変数を一つ受け取り、変数名のみが異なる新しい変数を生成する
genNewVarFrom :: Exp SrcSpanInfo -> RenameEnv (Exp SrcSpanInfo)
genNewVarFrom (Var l1 qn) = case qn of
  N.UnQual l2 (N.Ident l3 s) -> do
    let vk = UQVar s
    c <- getCounterOf vk
    let newVarName = newNameOfExVar s c
        newVar = Var l1 (N.UnQual l2 (N.Ident l3 newVarName))
    addCounterOf vk
    return newVar
  N.Qual l2 mn (N.Ident l3 s) -> do
    let vk = QVar (getName mn) s
    c <- getCounterOf vk
    let newVarName = newNameOfExVar s c
        newVar = Var l1 (N.Qual l2 mn (N.Ident l3 newVarName))
    addCounterOf vk
    return newVar
genNewVarFrom e = error $ "The function genNewVarFrom is not defined for a given expression: " ++ putDocString (ppP e)
  
isFree :: Exp SrcSpanInfo -> RenameEnv Bool
isFree v@(Var _ qn) = gets $ not . (mkVKFromQN qn `elem`) . boundVars
isFree e           = error $ "The function genNewVarFrom is not defined for a given expression: " ++ putDocString (ppP e)

addBoundVar :: VarKey -> RenameEnv ()
addBoundVar vn = do
  bv <- gets boundVars
  modify $ \env -> env { boundVars = vn : bv }

setBoundVar :: [VarKey] -> RenameEnv ()
setBoundVar ss = modify $ \env -> env { boundVars = ss }

addBoundVarFromPat :: Pat SrcSpanInfo -> RenameEnv ()
addBoundVarFromPat p = case p of
  PVar _ name          -> addBoundVar $ UQVar (getName name)
  PBox _ p'            -> addBoundVarFromPat p'
  PTuple _ ps          -> forM_ ps $ \p -> do addBoundVarFromPat p
  PList _ ps           -> forM_ ps $ \p -> do addBoundVarFromPat p
  PInfixApp _ p1 qn p2 -> forM_ [p1,p2] $ \p -> do addBoundVarFromPat p
  PApp _ qn ps         -> forM_ ps $ \p -> do addBoundVarFromPat p
  PWildCard _          -> return ()
  PLit {}              -> return ()

----------------

renameExVarModule :: CounterTable -> Module SrcSpanInfo -> (Module SrcSpanInfo, CounterTable)
renameExVarModule ct mod@(Module l mh pragmas imps decls) =
  let mn = maybe (error "renameExVarModule : No moduleHead") getName mh
      (decls', renv) = runState (forM decls renameExVarDecl) (mkInitRenameEnv mn ct decls)
  in (Module l mh pragmas imps decls', counterTable renv)

renameExVarDecl :: Decl SrcSpanInfo -> RenameEnv (Decl SrcSpanInfo)
renameExVarDecl pb@(PatBind l p@(PVar _ name) e) = PatBind l p <$> renameExVarExp e

renameExVarExp :: Exp SrcSpanInfo -> RenameEnv (Exp SrcSpanInfo)
renameExVarExp exp = case exp of
  Lit _ _ -> return exp
  Var _ _ -> isFree exp >>= \b -> if b
              then genNewVarFrom exp -- 自由変数は外部モジュール由来のはず
              else return exp        -- パターンかλに束縛された変数
  App l e1 e2   -> App l <$> renameExVarExp e1 <*> renameExVarExp e2
  -- If even one pattern that binds the same name exists, terminate the renameExVarExp function,
  Lambda l p e -> do
    oldBoundVars <- gets boundVars   -- 旧テーブルを保存
    addBoundVarFromPat p             -- 束縛変数から新しいテーブルを環境に登録/新しい束縛変数リストを得る
    e' <- renameExVarExp e           -- 更新された環境でrename
    setBoundVar oldBoundVars         -- 旧テーブルに戻す
    return $ Lambda l p e'           -- rename済み式と新しい束縛変数リストを用いてLambdaのrename済み式を構成
  Tuple l elms   -> Tuple l <$> mapM renameExVarExp elms
  List l elms    -> List l <$> mapM renameExVarExp elms
  If l e1 e2 e3  -> If l <$> renameExVarExp e1 <*> renameExVarExp e2 <*> renameExVarExp e3
  Case l e alts  -> Case l <$> renameExVarExp e <*> mapM renameExVarAlt alts
  Pr l e         -> Pr l <$> renameExVarExp e
  VRes l label e -> VRes l label <$> renameExVarExp e
  VExt l e       -> VExt l <$> renameExVarExp e
  
renameExVarAlt :: Alt SrcSpanInfo -> RenameEnv (Alt SrcSpanInfo)
renameExVarAlt (Alt l p e) = do
  oldBoundVars <- gets boundVars   -- 旧テーブルを保存
  addBoundVarFromPat p             -- 束縛変数から新しいテーブルを環境に登録/新しい束縛変数リストを得る
  e' <- renameExVarExp e           -- 更新された環境でrename
  setBoundVar oldBoundVars         -- 旧テーブルに戻す
  return $ Alt l p e'              -- rename済み式と新しい束縛変数リストを用いてLambdaのrename済み式を構成  

-------------------

-- 各外部変数(s)の各環境に対し、ctstart ! sから始まるインデックスを用いてctdiff ! s回の複製を行う
-- duplicateEnvs (["f", 0])　(["f", 2]) ([f : [!_a1 Int -> Int]_a0], [a0:Labels,a1:Labels], [a0 <= C_a0, a1 <= C_a1])
-- -> ( [__f_0 : [!_a1_0 Int -> Int]_a0_0, __f_1 : [!_a1_1 Int -> Int]_a0_1,]
--    , [a0_0:Labels, a1_0:Labels, a0_1:Labels, a1_1:Labels]
--    , [a0_0 <= C_a0, a1_0 <= C_a1, a0_1 <= C_a0, a1_1 <= C_a1])
duplicateEnvs :: CounterTable -> CounterTable -> (TEnv, UEnv, Map VarName Constraints) -> (TEnv, UEnv, Constraints, ExVarResources)
duplicateEnvs ctdiff ctstart (tenv, uenv, csschm) = duplicateEnvs' (M.toList ctdiff) ctstart csschm (tenv, uenv, CTop, mempty)
  where
    duplicateEnvs' :: [(VarKey, Int)] -> CounterTable -> Map VarName Constraints -> (TEnv, UEnv, Constraints, ExVarResources) -> (TEnv, UEnv, Constraints, ExVarResources)
    duplicateEnvs' []           ctstart csschm envs = envs 
    duplicateEnvs' ((vk,i):rst) ctstart csschm (oldTEnv, oldUEnv, oldCs, oldResVarMap) = 
      if i /= 0 then
        let cstart = ctstart <!> vk
            csschmOfS = csschm <!> getVN vk
            newTEnv = duplicateVarsTEnv vk i cstart oldTEnv
            newUEnv = duplicateTyVarsUEnv (map getVN $ freeVars $ oldTEnv <!> vk) i cstart oldUEnv
            newCs   = oldCs `landC` duplicateTyVarsCons csschmOfS i cstart
            newResVarMap = oldResVarMap `M.union` duplicateResVarMap vk i cstart oldTEnv
        in duplicateEnvs' rst ctstart csschm (newTEnv, newUEnv, newCs, newResVarMap)
      else
        duplicateEnvs' rst ctstart csschm (oldTEnv, oldUEnv, oldCs, oldResVarMap)
    duplicateTyVarsUEnv :: [VarName] -> Int -> Int -> UEnv -> UEnv 
    duplicateTyVarsUEnv []       i cstart uenv = uenv
    duplicateTyVarsUEnv (tyn:ss) i cstart uenv = 
      let kind = uenv <!> tyn
          uenvDeleted = M.delete tyn uenv
          newUEnv = foldl (\acc newtyn -> M.insert newtyn kind acc)
            uenvDeleted
            (newNamesOfExTyVar tyn i cstart) -- ["a0_(cstart)", "a0_(cstart+1)"]
      in duplicateTyVarsUEnv ss i cstart newUEnv
    duplicateTyVarsCons :: Constraints -> Int -> Int -> Constraints
    duplicateTyVarsCons csschmOfS i cstart = if i == 0 then csschmOfS else
      foldl1 landC $ map (`renameConsWithIdx` csschmOfS) $ take i $ iterate (+1) cstart
    duplicateVarsTEnv :: VarKey -> Int -> Int -> TEnv -> TEnv
    duplicateVarsTEnv vk i cstart tenv =
      let envty = fromMaybe
            (error $ putDocString $
              ppP "duplicateVarsTEnv: notFound" <> line <> 
              ppP "  vk   : " <> ppP vk <> line <> 
              ppP "  tenv : " <> ppP tenv) $
            lookupEnv vk tenv -- [!_a1 Int -> Int]_a0]
          tenvDeleted = M.delete vk tenv
      in foldl (\acc (i, newvn) -> M.insert newvn (renameEnvTyWithIdx i envty) acc)
          tenvDeleted $
          zip (iterate (+1) cstart) (newNamesOfExVarKeys vk i cstart) --newNamesOfExVar (getVN vk) i cstart) -- [(cstart, "__f_(cstart)"), (cstart+1, "__f_(cstart+1)")]
    duplicateResVarMap :: VarKey -> Int -> Int -> TEnv -> ExVarResources
    duplicateResVarMap vk i cstart tenv =
      case tenv <!> vk of
        NType _ _    -> error ""
        GrType _ _ c -> M.fromList $ zip
                          (newNamesOfExVarKeys vk i cstart)
                          (map ((vk,) . TyVar . T.Ident) $ newNamesOfExTyVar (getName c) i cstart)
          

-- renameConsWithIdx: Resource Schemeの左側の制約だけidxを付ける
-- renameConsWithIdx 2 (CSubset a1 a2) = CSubset a1_2 a2
renameConsWithIdx :: Int -> Constraints -> Constraints
renameConsWithIdx i cs = case cs of
  CSubset t1 t2 -> CSubset (renameTyWithIdx i t1) t2
  CAnd c1 c2    -> CAnd (renameConsWithIdx i c1) (renameConsWithIdx i c2)
  COr c1 c2     -> COr  (renameConsWithIdx i c1) (renameConsWithIdx i c2)
  CTop          -> CTop

-- renameEnvTyWithIdx: 全ての型変数にidxを付ける
-- renameEnvTyWithIdx 2 (GrType t c) = GrType t c
renameEnvTyWithIdx :: Int -> EnvType -> EnvType
renameEnvTyWithIdx i et = case et of
  NType  tag t   -> NType tag (renameTyWithIdx i t)
  GrType tag t c -> GrType tag (renameTyWithIdx i t) (renameTyWithIdx i c)

-- renameTyWithIdx: 全ての型変数にidxをつける
-- renameTyWithIdx 2 (TyVar a1) = TyVar a1_2
renameTyWithIdx :: Int -> Type -> Type
renameTyWithIdx i ty = case ty of
  TyVar (T.Ident n) -> TyVar (T.Ident $ newNameOfExTyVar n i)
  TyCon qn -> ty
  TyFun t1 t2 -> TyFun (renameTyWithIdx i t1) (renameTyWithIdx i t2)
  TyBox c t -> TyBox (renameTyWithIdx i c) (renameTyWithIdx i t)
  TyBottom -> ty
  TyLabels _ -> ty
  -- CAnd c1 c2 -> CAnd (renameTyWithIdx i c1) (renameTyWithIdx i c2)
  -- COr c1 c2 -> COr (renameTyWithIdx i c1) (renameTyWithIdx i c2)