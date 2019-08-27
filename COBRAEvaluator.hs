module COBRAEvaluator where

import COBRATokens
import COBRAGrammar
import System.IO
import System.Environment
import Control.Exception

-- PureMultiStream describes a collection of PureStreams, e.g. an input file with two Streams is considered a PureMultiStream with two PureStreams
--data PureMultiStream = Empty
--                     | AddedTo PureStream PureMultiStream deriving (Eq, Show)

-- PureStream is a stream of pure integers and nothing else (no expressions within the stream which need to be further evaluated)
--data PureStream = EmptyStream 
--                | PartOf StreamInteger PureStream deriving (Eq, Show)

-- data types
--data Ty = StreamType 
--        | MultiStreamType deriving (Eq,Show)

--data StreamExpr = PureMS PureMultiStream
--                | PlusMStreams StreamExpr StreamExpr
--                | StreamCons StreamExpr StreamExpr
--                | PlusStreams StreamExpr StreamExpr
--                | Variable Var
--                | PureS PureStream
--                | AddToFront StreamInteger StreamExpr
--                | AddToEnd StreamInteger StreamExpr
--                | AccessStream Int StreamExpr
--                | SingleStreamFunction StreamExpr Int Operator
--                | DoubleStreamFunction StreamExpr StreamExpr Int Int Operator
--                | AccF StreamExpr Sequence 
--                | Input
--                | Let Var Ty StreamExpr StreamExpr 
--                | Bracket StreamExpr deriving (Eq, Show)
                

--data Sequence = Constant Int
--              | Accumulation Int Int Operator
--              | MultiplySeq Int Int
--              | AddSeq Int Int deriving (Eq, Show)

--data Operator = Plus
--              | Minus
--              | Multiply
--              | Divide
--              | Modulus
--              | PowerOf deriving (Eq, Show)

--type Var = String

--type StreamInteger = Int

type Control = StreamExpr

type Environment = [(Var,StreamExpr)]

data Frame = HPlusMStreams StreamExpr Environment
           | PlusMStreamsH PureMultiStream
           | HStreamCons StreamExpr Environment
           | StreamConsH PureStream
           | HPlusStreams StreamExpr Environment
           | PlusStreamsH PureStream
           | HLet Var Ty StreamExpr Environment 
           | AddToFrontH StreamInteger
           | AddToEndH StreamInteger
           | AccessStreamH Int
           | HSingleStreamFunction Int Operator
           | LDoubleStreamFunction StreamExpr Int Int Operator Environment
           | RDoubleStreamFunction PureStream Int Int Operator
           | HAccF Sequence deriving (Eq, Show)

type Kontinuation = [Frame]

type CEK = (Control, Environment, Kontinuation, PureMultiStream) -- pass the input MultiStream around until it needs to be read in

unParse :: StreamExpr -> String
unParse (PureS stream) = unParseStream stream
unParse (PureMS multiStream) = unParseIntLists (makeIntListsFromMultiStream multiStream)
unParse exp = error ("The expression " ++ (show exp) ++ " is not a valid pure stream / multi-stream, so has no proper way of unparsing")

unParseStream :: PureStream -> String
unParseStream EmptyStream = ""
unParseStream (PartOf i stream) = (show i) ++ "\n" ++ unParseStream stream

unParseIntLists :: [[Int]] -> String
unParseIntLists xs | checkEmptyLists xs = ""
                   | otherwise = printList tops ++ (unParseIntLists (map tail xs))
     where tops = map head xs

checkEmptyLists :: [[Int]] -> Bool
checkEmptyLists [] = True
checkEmptyLists (xs:xss) = (length xs == 0) && (checkEmptyLists xss)

printList :: [Int] -> String
printList [] = "\n"
printList (x:xs) = (show x) ++ " " ++ (printList xs)

makeIntListsFromMultiStream :: PureMultiStream -> [[Int]]
makeIntListsFromMultiStream Empty = []
makeIntListsFromMultiStream (AddedTo stream multiStream) = (makeIntListFromStream stream) : (makeIntListsFromMultiStream multiStream)

makeIntListFromStream :: PureStream -> [Int]
makeIntListFromStream EmptyStream = []
makeIntListFromStream (PartOf i stream) = i : (makeIntListFromStream stream)

-- function which takes an expression of the language and a PureMultiStream (which is the input to the program) and returns the full evaluated expression
evalFull1 :: StreamExpr -> PureMultiStream -> StreamExpr
evalFull1 exp inp = exp1
   where evals = iterate eval1 (exp,[],[],inp)
         evals1 = zip evals (tail evals)
         evals2 = takeWhile (uncurry (/=)) evals1
         evals3 = snd (evals2 !! ((length evals2) - 1))
         (exp1,env,k,inp1) = evals3


eval1 :: CEK -> CEK
-- check if eval is complete
eval1 ((PureMS ms),env,[],inp) = ((PureMS ms),env,[],inp)
eval1 ((PureS s),env,[],inp) = ((PureS s),env,[],inp)

-- adding two MultiStreams to each other to make a bigger MultiStream
eval1 ((PlusMStreams ms1 ms2),env1,k1, inp) = ((ms1),(env1),((HPlusMStreams ms2 env1):k1),inp)
eval1 ((PureMS ms1),env1,((HPlusMStreams ms2 env2):k1),inp) = ((ms2),env2,((PlusMStreamsH ms1):k1),inp)
eval1 ((PureMS ms2),env2,((PlusMStreamsH ms1):k1),inp) = ((PureMS (addMStreams ms1 ms2)),[],k1,inp)

-- cons a stream to a larger multi stream
eval1 ((StreamCons s ms),env1,k1,inp) = ((s),(env1),((HStreamCons ms env1):k1),inp) 
eval1 ((PureS s),env1,((HStreamCons ms env2):k1),inp) = ((ms),env2,((StreamConsH s):k1),inp)
eval1 ((PureMS ms),env1,((StreamConsH s):k),inp) = ((PureMS (streamCons s ms)),[],k,inp)

-- adding two Streams together to make a MultiStream with those 2 Streams
eval1 ((PlusStreams s1 s2),env1,k1,inp) = ((s1),env1,((HPlusStreams s2 env1):k1),inp)
eval1 ((PureS s1),env1,((HPlusStreams s2 env2):k1),inp) = (s2,env2,((PlusStreamsH s1):k1),inp)
eval1 ((PureS s2),env1,((PlusStreamsH s1):k1),inp) = ((PureMS (addStreams s1 s2)),[],k1,inp)

-- dealing with let blocks
eval1 ((Let x ty exp1 exp2),env1,k1,inp) = ((exp1),(env1),((HLet x ty exp2 env1):k1),inp)
eval1 (v,env1,((HLet x ty exp2 env2):k1),inp) | isPureValue v = (exp2,(update env2 x v),k1,inp)

-- dealing with reading in the input (the input inp being passed around is read into the control as a PureMultiStream
eval1 (Input,env,k1,inp) = (PureMS (inp),env,k1,inp)

-- dealing with variables
eval1 ((Variable x),env1,k1,inp) = ((getValue x env1),[],k1,inp)

-- dealing with adding a StreamInteger to the front of a Stream
eval1 ((AddToFront i exp),env1,k1,inp) = (exp,env1,((AddToFrontH i):k1),inp)
eval1 ((PureS s1),env1,((AddToFrontH i):k1),inp) = ((PureS (addToFront i s1)),[],k1,inp)

-- dealing with adding a StreamInteger to the end of a Stream
eval1 ((AddToEnd i exp),env1,k1,inp) = (exp,env1,((AddToEndH i):k1),inp)
eval1 ((PureS s1),env1,((AddToEndH i):k1),inp) = ((PureS (addToEnd i s1)),[],k1,inp)

-- dealing with accessing a particular Stream in a MultiStream
eval1 ((AccessStream i exp),env1,k1,inp) = ((exp),env1,((AccessStreamH i):k1),inp)
eval1 ((PureMS ms),env1,((AccessStreamH i):k1),inp) = ((PureS (accessStream i ms)),[],k1,inp)

-- dealing with a function acting on one Stream
eval1 ((SingleStreamFunction exp i op),env1,k1,inp) = (exp,env1,((HSingleStreamFunction i op):k1),inp)
eval1 ((PureS s1),env1,((HSingleStreamFunction i op):k1),inp) = ((PureS (singleStreamFunction s1 i op)),[],k1,inp)

-- dealing with a function acting on two Streams
eval1 ((DoubleStreamFunction s1 s2 i1 i2 op),env1,k1,inp) = ((s1),env1,((LDoubleStreamFunction s2 i1 i2 op env1):k1),inp)
eval1 ((PureS s1),env1,((LDoubleStreamFunction s2 i1 i2 op env2):k1),inp) = ((s2),env2,((RDoubleStreamFunction s1 i1 i2 op):k1),inp)
eval1 ((PureS s2),env1,((RDoubleStreamFunction s1 i1 i2 op):k1),inp) = ((PureS (doubleStreamFunction s1 s2 i1 i2 op)),[],k1,inp) 

-- dealing with functions on a stream which involves accumulating the elements of the Stream
eval1 ((AccF s1 seq),env1,k1,inp) = ((s1),env1,((HAccF seq):k1),inp)
eval1 ((PureS s1),env1,((HAccF seq):k1),inp) = ((PureS (accumulatorFunction s1 seq)),[],k1,inp)

-- dealing with bracketed expressions
eval1 ((Bracket exp),env1,k1,inp) = (exp,env1,k1,inp)

addMStreams :: PureMultiStream -> PureMultiStream -> PureMultiStream
addMStreams (Empty) (ms2) = ms2
addMStreams (AddedTo s ms1) (ms2) = AddedTo s (addMStreams ms1 ms2)

addStreams :: PureStream -> PureStream -> PureMultiStream
addStreams s1 s2 = AddedTo s1 (AddedTo s2 Empty)

isPureValue :: StreamExpr -> Bool
isPureValue (PureMS _) = True
isPureValue (PureS _) = True
isPureValue _ = False

update :: Environment -> Var -> StreamExpr -> Environment
update env x exp = (x,exp):env

getValue :: Var -> Environment -> StreamExpr
getValue x [] = error "No Binding"
getValue x ((y,exp):env) | x == y = exp
                         | otherwise = getValue x env

addToFront :: StreamInteger -> PureStream -> PureStream
addToFront i (EmptyStream) = EmptyStream
addToFront i s = PartOf i (removeLastElement s)

removeLastElement :: PureStream -> PureStream
removeLastElement (PartOf i (EmptyStream)) = EmptyStream
removeLastElement (PartOf i s) = PartOf i (removeLastElement s)
removeLastElement EmptyStream = EmptyStream

addToEnd :: StreamInteger -> PureStream -> PureStream
addToEnd i (EmptyStream) = EmptyStream
addToEnd i s = addToEnd' i (removeFirstElement s)

addToEnd' :: StreamInteger -> PureStream -> PureStream
addToEnd' i (EmptyStream) = PartOf i (EmptyStream)
addToEnd' i (PartOf i2 s) = PartOf i2 (addToEnd' i s)

removeFirstElement :: PureStream -> PureStream
removeFirstElement (PartOf i s) = s
removeFirstElement (EmptyStream) = EmptyStream

accessStream :: Int -> PureMultiStream -> PureStream
accessStream _ Empty = EmptyStream
accessStream 0 (AddedTo s ms) = s
accessStream n (AddedTo s ms) = accessStream (n-1) ms

singleStreamFunction :: PureStream -> Int -> Operator -> PureStream
singleStreamFunction (EmptyStream) _ _ = EmptyStream
singleStreamFunction (PartOf i s) n Plus = PartOf (i+n) (singleStreamFunction s n Plus)
singleStreamFunction (PartOf i s) n Minus = PartOf (i-n) (singleStreamFunction s n Minus)
singleStreamFunction (PartOf i s) n Multiply = PartOf (i*n) (singleStreamFunction s n Multiply)
singleStreamFunction (PartOf i s) n Divide = PartOf (i `div` n) (singleStreamFunction s n Divide)
singleStreamFunction (PartOf i s) n Modulus = PartOf (i `mod` n) (singleStreamFunction s n Modulus)
singleStreamFunction (PartOf i s) n PowerOf = PartOf (i^n) (singleStreamFunction s n PowerOf)

doubleStreamFunction :: PureStream -> PureStream -> Int -> Int -> Operator -> PureStream
doubleStreamFunction (EmptyStream) (EmptyStream) _ _ _ = EmptyStream
doubleStreamFunction EmptyStream _ _ _ _ = error "Sequences are not of the same length!"
doubleStreamFunction _ EmptyStream _ _ _ = error "Sequences are not of the same length!"
doubleStreamFunction (PartOf i1 s1) (PartOf i2 s2) n1 n2 Plus = PartOf (n1*i1 + n2*i2) (doubleStreamFunction s1 s2 n1 n2 Plus)
doubleStreamFunction (PartOf i1 s1) (PartOf i2 s2) n1 n2 Minus = PartOf (n1*i1 - n2*i2) (doubleStreamFunction s1 s2 n1 n2 Minus)
doubleStreamFunction (PartOf i1 s1) (PartOf i2 s2) n1 n2 Multiply = PartOf (n1*i1 * n2*i2) (doubleStreamFunction s1 s2 n1 n2 Multiply)
doubleStreamFunction (PartOf i1 s1) (PartOf i2 s2) n1 n2 Divide = PartOf (n1*i1 `div` n2*i2) (doubleStreamFunction s1 s2 n1 n2 Divide)
doubleStreamFunction (PartOf i1 s1) (PartOf i2 s2) n1 n2 Modulus = PartOf (n1*i1 `mod` n2*i2) (doubleStreamFunction s1 s2 n1 n2 Modulus)
doubleStreamFunction (PartOf i1 s1) (PartOf i2 s2) n1 n2 PowerOf = PartOf ((n1*i1)^(n2*i2)) (doubleStreamFunction s1 s2 n1 n2 PowerOf)

accumulatorFunction :: PureStream -> Sequence -> PureStream
accumulatorFunction (EmptyStream) _ = EmptyStream
accumulatorFunction s (Constant i) = accAccumulation s [i,i..] []
accumulatorFunction s (Accumulation n1 n2 op) = accAccumulation s (makeAccumulationList n1 n2 op) []
accumulatorFunction s (MultiplySeq m n) = accAccumulation s (makeSeqList m n) []
accumulatorFunction s (AddSeq m n) = accAccumulation s (makeAddList m n) []

makeAddList :: Int -> Int -> [Int]
makeAddList m n = m:(makeAddList (m+n) n)

makeSeqList :: Int -> Int -> [Int]
makeSeqList m n = m:(makeSeqList (m*n) n)

accAccumulation :: PureStream -> [Int] -> [Int] -> PureStream
accAccumulation EmptyStream _ _ = EmptyStream
accAccumulation (PartOf i s) cs accl = PartOf (sum (mapCoefficients cs (i:accl))) (accAccumulation s cs (i:accl))

makeAccumulationList :: Int -> Int -> Operator -> [Int]
makeAccumulationList n1 n2 Plus = n1:n2:(makeAccumulationList (n1+n2) (n1+n2+n2) Plus)
makeAccumulationList n1 n2 Minus = n1:n2:(makeAccumulationList (n1-n2) (n2-(n1-n2)) Minus)
makeAccumulationList n1 n2 Multiply = n1:n2:(makeAccumulationList (n1*n2) (n2*(n1*n2)) Multiply)
makeAccumulationList n1 n2 Divide = n1:n2:(makeAccumulationList (n1 `div` n2) (n2 `div` (n1 `div` n2)) Divide)
makeAccumulationList n1 n2 Modulus = n1:n2:(makeAccumulationList (n1 `mod` n2) (n2 `mod` (n1 `mod` n2)) Modulus)
makeAccumulationList n1 n2 PowerOf = n1:n2:(makeAccumulationList (n1^n2) (n2^(n1^n2)) PowerOf)

mapCoefficients :: [Int] -> [Int] -> [Int]
mapCoefficients _ [] = []
mapCoefficients (c:cs) (l:ls) = (c*l):(mapCoefficients cs ls)

streamCons :: PureStream -> PureMultiStream -> PureMultiStream
streamCons s ms = AddedTo s ms

type TypeEnvironment = [(Var,Ty)]

lookupTypeBinding :: Var -> TypeEnvironment -> Ty
lookupTypeBinding x [] = error ("No type binding for variable " ++ x)
lookupTypeBinding x ((y,ty):ts) | x == y = ty
                                | otherwise = lookupTypeBinding x ts

addNewBinding :: Var -> Ty -> TypeEnvironment -> TypeEnvironment
addNewBinding x ty env = (x,ty):env

typeOf :: StreamExpr -> TypeEnvironment -> Ty
typeOf (PureMS _) _ = MultiStreamType
typeOf (PlusMStreams ms1 ms2) env | typeMS1 == MultiStreamType && typeMS2 == MultiStreamType = MultiStreamType
                                  | typeMS1 == MultiStreamType = error ("Type error related to the arguments to PlusMStreams\nSpecifically, the second argument " ++ (show ms2) ++ ".\nThis should have the type " ++ (show MultiStreamType) ++ " but instead has type " ++ (show typeMS2) ++ ".")
                                  | typeMS2 == MultiStreamType = error ("Type error related to the arguments to PlusMStreams\nSpecifically, the first argument " ++ (show ms1) ++ ".\nThis should have the type " ++ (show MultiStreamType) ++ " but instead has type " ++ (show typeMS1) ++ ".")
                                  | otherwise = error ("Type error related to the arguments to PlusMStreams\nSpecifically, the first argument " ++ (show ms1) ++ " and the second argument " ++ (show ms2) ++ ".\nThe first argument should have the type " ++ (show MultiStreamType) ++ " but instead has type " ++ (show typeMS1) ++ ".\nThe second argument should have type " ++ (show MultiStreamType) ++ " but instead has type " ++ (show typeMS2) ++ ".")
          where typeMS1 = typeOf ms1 env
                typeMS2 = typeOf ms2 env
typeOf (StreamCons s ms) env | typeS == StreamType && typeMS == MultiStreamType = MultiStreamType
                             | typeS == StreamType = error ("Type error related to the arguments to StreamCons.\nSpecifically, the second argument " ++ (show ms) ++ ".\nThis should have type " ++ (show MultiStreamType) ++ " but instead has type " ++ (show typeMS) ++ ".")
                             | typeMS == MultiStreamType = error ("Type error related to the arguments to StreamCons.\nSpecifically, the first argument " ++ (show s) ++ ".\nThis should have type " ++ (show StreamType) ++ " but instead has type " ++ (show typeS) ++ ".")
                             | otherwise = error ("Type error related to the arguments to StreamCons.\nSpecifically, the first argument " ++ (show s) ++ " and the second argument " ++ (show ms) ++ ".\nThe first argument should have the type " ++ (show StreamType) ++ " but instead has type " ++ (show typeS) ++ ".\nThe second argument should have the type " ++ (show MultiStreamType) ++ " but instead has type " ++ (show typeMS) ++ ".")
          where typeS = typeOf s env
                typeMS = typeOf ms env
typeOf (PlusStreams s1 s2) env | typeS1 == StreamType && typeS2 == StreamType = MultiStreamType
                               | typeS1 == StreamType = error ("Type error related to the arguments to PlusStreams.\nSpecifically, the second argument " ++ (show s2) ++ ".\nThis should have the type " ++ (show StreamType) ++ " but instead has type " ++ (show typeS2) ++ ".")
                               | typeS2 == StreamType = error ("Type error related to the arguments to PlusStreams.\nSpecifically, the first argument " ++ (show s1) ++ ".\nThis should have the type " ++ (show StreamType) ++ " but instead has type " ++ (show typeS1) ++ ".")
                               | otherwise = error ("Type error related to the arguments to PlusStreams.\nSpecifically, the first argument " ++ (show s1) ++ " and the second argument " ++ (show s2) ++ ".\nThe first argument should have the type " ++ (show StreamType) ++ " but instead has type " ++ (show typeS1) ++ ".\nThe second argument should have the type " ++ (show StreamType) ++ " but instead has type " ++ (show typeS2) ++ ".")
          where typeS1 = typeOf s1 env
                typeS2 = typeOf s2 env
typeOf (Variable x) env = lookupTypeBinding x env
typeOf (PureS _) _ = StreamType
typeOf (AddToFront i s) env | typeS == StreamType = StreamType
                            | otherwise = error ("Type error related to the arguments to AddToFront.\nSpecifically, the stream argument " ++ (show s) ++ ".\nThis should have the type " ++ (show StreamType) ++ " but instead has type " ++ (show typeS) ++ ".")
          where typeS = typeOf s env
typeOf (AddToEnd i s) env | typeS == StreamType = StreamType
                          | otherwise = error ("Type error related to the arguments to AddToEnd.\nSpecifically, the stream argument " ++ (show s) ++ ".\nThis should have the type " ++ (show StreamType) ++ " but instead has type " ++ (show typeS) ++ ".")
          where typeS = typeOf s env
typeOf (AccessStream i ms) env | typeMS == MultiStreamType = StreamType
                               | otherwise = error ("Type error related to the arguments to AccessStream.\nSpecifically, the multi-stream argument " ++ (show ms) ++ ".\nThis should have the type " ++ (show MultiStreamType) ++ " but instead has type " ++ (show typeMS) ++ ".")
          where typeMS = typeOf ms env
typeOf (SingleStreamFunction s i op) env | typeS == StreamType = StreamType
                                         | otherwise = error ("Type error related to the arguments to SingleStreamFunction.\nSpecifically, the stream argument " ++ (show s) ++ ".\nThis should have the type " ++ (show StreamType) ++ " but instead has type " ++ (show typeS) ++ ".")
          where typeS = typeOf s env
typeOf (DoubleStreamFunction s1 s2 i1 i2 op) env | typeS1 == StreamType && typeS2 == StreamType = StreamType
                                                 | typeS1 == StreamType = error ("Type error related to the arguments to DoubleStreamFunction.\nSpecifically, the second stream argument " ++ (show s2) ++ ".\nThis should have the type " ++ (show StreamType) ++ " but instead has type " ++ (show typeS2) ++ ".")
                                                 | typeS2 == StreamType = error ("Type error related to the arguments to DoubleStreamFunction.\nSpecifically, the first stream argument " ++ (show s1) ++ ".\nThis should have the type " ++ (show StreamType) ++ " but instead has type " ++ (show typeS1) ++ ".")
                                                 | otherwise = error ("Type error related to the arguments to DoubleStreamFunction.\nSpecifically, the first stream argument " ++ (show s1) ++ " and the second stream argument " ++ (show s2) ++ ".\nThe first argument should have the type " ++ (show StreamType) ++ " but instead has type " ++ (show typeS1) ++ ".\nThe second argument should have the type " ++ (show StreamType) ++ " but instead has type " ++ (show typeS2) ++ ".")
          where typeS1 = typeOf s1 env
                typeS2 = typeOf s2 env
typeOf (AccF s seq) env | typeS == StreamType = StreamType
                        | otherwise = error ("Type error related to the arguments to AccF.\nSpecifically, the stream argument " ++ (show s) ++ ".\nThis should have the type " ++ (show StreamType) ++ " but instead has type " ++ (show typeS) ++ ".") 
          where typeS = typeOf s env
typeOf (Let x ty exp1 exp2) env | typeEXP1 == ty = typeOf exp2 (addNewBinding x ty env)
                                | otherwise = error ("Type error related to Let block.\nSpecifically, the first expression of the Let block : " ++ (show exp1) ++ ".\nThis should have type " ++ (show ty) ++ " but instead has type " ++ (show typeEXP1) ++ ".")
          where typeEXP1 = typeOf exp1 env

typeOf (Input) env = MultiStreamType

typeOf (Bracket exp) env = typeOf exp env