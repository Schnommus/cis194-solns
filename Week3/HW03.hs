module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | Decr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state s n toFind
  | toFind == s = n
  | otherwise = state toFind
-- ^^Lesson learned: be careful with mindlessly placing arguments

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

toInt :: Bool -> Int
toInt True = 1
toInt False = 0

evalE :: State -> Expression -> Int
evalE _ (Val n) = n
evalE st (Var s) = st s
evalE st (Op l Plus r) = evalE st l + evalE st r
evalE st (Op l Minus r) = evalE st l - evalE st r
evalE st (Op l Times r) = evalE st l * evalE st r
evalE st (Op l Divide r) = evalE st l `div` evalE st r
evalE st (Op l Eql r) = toInt $ evalE st l == evalE st r
evalE st (Op l Gt r) = toInt $ evalE st l > evalE st r
evalE st (Op l Ge r) = toInt $ evalE st l >= evalE st r
evalE st (Op l Lt r) = toInt $ evalE st l < evalE st r
evalE st (Op l Le r) = toInt $ evalE st l <= evalE st r

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s e) = DAssign s e
desugar (Incr s) = DAssign s (Op (Var s) Plus (Val 1))
desugar (Decr s) = DAssign s (Op (Var s) Minus (Val 1))
desugar (If e l r) = DIf e (desugar l) (desugar r)
desugar (While e s) = DWhile e (desugar s)
desugar (Sequence l r) = DSequence (desugar l) (desugar r)
desugar (For pre cond inc code) = DSequence (desugar pre) (DWhile cond (DSequence (desugar code) (desugar inc)))
-- ^^Lesson learned: Read the code specification.
desugar Skip = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign s e) = extend st s (evalE st e)
evalSimple st (DIf e y n)
  | evalE st e == 0 = evalSimple st n
  | otherwise = evalSimple st y
evalSimple st (DSequence l r) = evalSimple (evalSimple st l) r
evalSimple st (DWhile e s)
  | evalE st e == 0 = st
  | otherwise = evalSimple st (DSequence s (DWhile e s))
evalSimple st DSkip = st


run :: State -> Statement -> State
run st program = evalSimple st (desugar program)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]
-- ^^ Lesson learned: Make sure to actually read the test cases!

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
