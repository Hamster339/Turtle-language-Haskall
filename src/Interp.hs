module Interp (eval) where
import Ast
import Instruction

-- An environment maps variables to values. This allows you to look up
-- what a variable stands for when you use it (for example, after let-binding,
-- or using a parameter)
type Env = [(Variable, Value)]

-- TurtleState is a Haskell record which contains the state you will need when
-- evaluating. It contains the environment, the definition map, and the list of
-- instructions that you are building up.
data TurtleState = TS {
    env :: Env,
    definitions :: [(DefName, Definition)],
    instructions :: [Instruction]
}

-- Constructs an empty state, given a definition map
emptyState :: [(DefName, Definition)] -> TurtleState
emptyState defs = TS { env = [], definitions = defs, instructions = [] }

{- Exercise 2 -}
evalUnOp :: UnaryOp -> Value -> Value
evalUnOp Not (VBool b1) = (VBool (not b1))
evalUnOp Neg (VInt i1) = (VInt (-i1))
evalUnOp _ _ = error "type error"

evalBinOp :: BinaryOp -> Value -> Value -> Value
evalBinOp Add (VInt i1) (VInt i2) = VInt (i1 + i2)
evalBinOp Sub (VInt i1) (VInt i2) = VInt (i1 - i2)
evalBinOp Mul (VInt i1) (VInt i2) = VInt (i1 * i2)
evalBinOp Div (VInt i1) (VInt i2) = VInt (div i1 i2)

evalBinOp Eq (VInt i1) (VInt i2) = VBool (i1 == i2)
evalBinOp Eq (VBool b1) (VBool b2) = VBool (b1 == b2)
evalBinOp Neq (VInt i1) (VInt i2) = VBool (i1 /= i2)
evalBinOp Neq (VBool b1) (VBool b2) = VBool (b1 /= b2)

evalBinOp Greater (VInt i1) (VInt i2) = VBool (i1 > i2)
evalBinOp Less (VInt i1) (VInt i2) = VBool (i1 < i2)
evalBinOp GreaterEq (VInt i1) (VInt i2) = VBool (i1 >= i2)
evalBinOp LessEq (VInt i1) (VInt i2) = VBool (i1 <= i2)

evalBinOp And (VBool b1) (VBool b2) = VBool (b1 && b2)
evalBinOp Or (VBool b1) (VBool b2) = VBool (b1 || b2)

evalBinOp _ _ _ = error "type error"

{- Exercise 3 -}
addInstruction :: TurtleState -> Instruction -> TurtleState
addInstruction _st _i = _st {instructions = newInsturct} where
    instruct = instructions _st
    newInsturct = instruct ++ [_i]

bind :: TurtleState -> Variable -> Value -> TurtleState
bind st _var _val = st {env = newEnv} where
    envi = env st
    newEnv = envi ++ [(_var,_val)]

lookupVar :: TurtleState -> Variable -> Value
lookupVar _st _var = case mvalue of
                 Just v -> v
                 Nothing -> error "Variable does not exist"
                 where
                    mvalue = lookup _var (env _st)

lookupDef :: TurtleState -> DefName -> Definition
lookupDef _st _defName = definition where
    definition = case mdef of
                 Just v -> v
                 Nothing -> error "Variable does not exist"
    mdef = lookup _defName (definitions _st)

{- Exercise 4 -}
evalExpr :: TurtleState -> Expr -> (Value, TurtleState)
evalExpr st (EInt i) = (VInt i, st)
evalExpr st (EBool b) = (VBool b, st)
evalExpr st (EUnit) = (VUnit, st)

{-Causes infinite loop on recursion-}
evalExpr st (EApp def args) = (vRes, finalSt) where
    (params, _, body) = (lookupDef st def)
    (variables,_) = unzip params
    (values, dSt) = foldl (\(vs,st) x -> let (v,state) = (evalExpr st x) in ((vs ++ [v]),state) ) ([],st) args
    freshEnv = [(var,val) | var <- variables, val <- values]
    defSt = dSt {env = freshEnv}
    (vRes,bodySt) = evalExpr defSt body
    finalSt = st {instructions = (instructions bodySt) }

evalExpr st (EIf cond e1 e2) = (value, state) where
    (value, state) = if (fst (evalExpr st cond)) == VBool True
                        then (evalExpr st e1)
                        else (evalExpr st e2)

evalExpr st (EMove dir e) = (VUnit, state) where
    state = addInstruction st (IMove finalInt)
    (VInt finalInt) = if (dir == Forward) then (VInt i) else (VInt (-i))
    ((VInt i),_) = (evalExpr st e)

evalExpr st (ERotate rDir e) = (VUnit, state) where
    state = addInstruction st (IRotate finalInt)
    (VInt finalInt) = if (rDir == RotateRight) then (VInt i) else (VInt (-i))
    ((VInt i),_) = (evalExpr st e)

evalExpr st (EPenUp) = (VUnit, state) where
    state = addInstruction st IPenUp

evalExpr st (EPenDown) = (VUnit, state) where
    state = addInstruction st IPenDown

evalExpr st (EChangeColor c) = (VUnit, state) where
    state = addInstruction st (IChangeColor c)

evalExpr st (ELet x e1 e2) = evalExpr (bind st x value) e2 where
    (value,_) = evalExpr st e1

evalExpr st (ESeq e1 e2) = evalExpr state e2 where
    (_,state) = evalExpr st e1

evalExpr st (EVar v) = ((lookupVar st v),st)

evalExpr st (EUnOp unop ex) = ((evalUnOp unop value),st) where
    (value,_) = evalExpr st ex

evalExpr st (EBinOp binop ex1 ex2) =((evalBinOp binop value1 value2),st) where
    (value1,_) = evalExpr st ex1
    (value2,_) = evalExpr st ex2

-- External function
eval :: Program -> (Value, [Instruction])
eval (_defs, _e) = (finalValue, instructs) where
    instructs = instructions state
    (finalValue,state) = evalExpr st _e
    st = emptyState _defs

