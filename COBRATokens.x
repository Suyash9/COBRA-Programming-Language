{
module COBRATokens where
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters
$eol   = [\n]
-- end of line character

tokens:-

-- Whitespace insensitive
  $eol          ;
  $white+       ;

-- Comments
  "//".*        ;

-- Syntax
  empty                { tok (\p s -> TokenEmpty p) }
  "+++"                { tok (\p s -> TokenAddedTo p) }
  empty_stm            { tok (\p s -> TokenEmptyStream p) }
  "<:"                 { tok (\p s -> TokenPartOf p) }
  StreamType           { tok (\p s -> TokenSType p) }
  MultiStreamType      { tok (\p s -> TokenMSType p) }
  "M+"                 { tok (\p s -> TokenPlusMS p) }
  "S+"                 { tok (\p s -> TokenPlusS p) }
  "::"                 { tok (\p s -> TokenStreamCons p) }
  "+->"                { tok (\p s -> TokenAddToFront p) }
  "<-+"                { tok (\p s -> TokenAddToEnd p) }
  access_stm           { tok (\p s -> TokenAccessStream p) }
  single_func          { tok (\p s -> TokenSingleStreamFunc p) }
  double_func          { tok (\p s -> TokenDoubleStreamFunc p) }
  coeffs               { tok (\p s -> TokenCoeffs p) }
  "<-"                 { tok (\p s -> TokenGetFrom p) }
  accumulate           { tok (\p s -> TokenAccF p) }
  \$                   { tok (\p s -> TokenSeqDef p) }
  "[0]"                { tok (\p s -> TokenBox0 p) }
  "[1]"                { tok (\p s -> TokenBox1 p) }
  "[n]"                { tok (\p s -> TokenSeqExp0 p) }
  "[n-1]"              { tok (\p s -> TokenSeqExp1 p) }
  "[n-2]"              { tok (\p s -> TokenSeqExp2 p) }
  "inp"                { tok (\p s -> TokenInput p) }
  let                  { tok (\p s -> TokenLet p) }
  in                   { tok (\p s -> TokenIn p) }
  $digit+              { tok (\p s -> TokenInt p (read s)) }
  \-[$digit]+          { tok (\p s -> TokenInt p (read s)) }
  \+                   { tok (\p s -> TokenPlus p) }
  \-                   { tok (\p s -> TokenMinus p) }
  \*                   { tok (\p s -> TokenMultiply p) }
  \/                   { tok (\p s -> TokenDivide p) }
  \%                   { tok (\p s -> TokenModulus p) }
  \^                   { tok (\p s -> TokenPowerOf p) }
  \:                   { tok (\p s -> TokenColon p) }
  \=                   { tok (\p s -> TokenEq p) }
  \(                   { tok (\p s -> TokenLParen p) }
  \)                   { tok (\p s -> TokenRParen p) }
  \,                   { tok (\p s -> TokenComma p) }
  $alpha [$alpha $digit \_ \’]*   { tok (\p s -> TokenVar p s) } 

{ 
-- Each action has type :: AlexPosn -> String -> Token 

-- Helper function
tok f p s = f p s

-- The token type: 
data Token = 
  TokenEmpty AlexPosn                      |
  TokenAddedTo AlexPosn                    |
  TokenEmptyStream AlexPosn                |
  TokenPartOf AlexPosn                     |
  TokenSType AlexPosn                      |
  TokenMSType AlexPosn                     |
  TokenPlusMS AlexPosn                     |
  TokenPlusS AlexPosn                      |
  TokenStreamCons AlexPosn                 |
  TokenAddToFront AlexPosn                 |
  TokenAddToEnd AlexPosn                   |
  TokenAccessStream AlexPosn               |
  TokenSingleStreamFunc AlexPosn           |
  TokenDoubleStreamFunc AlexPosn           |
  TokenCoeffs AlexPosn                     |
  TokenGetFrom AlexPosn                    |
  TokenAccF AlexPosn                       |
  TokenSeqDef AlexPosn                     |
  TokenBox0 AlexPosn                       |
  TokenBox1 AlexPosn                       |
  TokenSeqExp0 AlexPosn                    |
  TokenSeqExp1 AlexPosn                    |
  TokenSeqExp2 AlexPosn                    |
  TokenInput AlexPosn                      |
  TokenLet AlexPosn                        | 
  TokenIn  AlexPosn                        |
  TokenPlus AlexPosn                       |
  TokenMinus AlexPosn                      |
  TokenMultiply AlexPosn                   |
  TokenDivide AlexPosn                     |
  TokenModulus AlexPosn                    |
  TokenPowerOf AlexPosn                    | 
  TokenInt AlexPosn Int                    |
  TokenVar AlexPosn String                 |
  TokenColon AlexPosn                      |
  TokenEq AlexPosn                         |
  TokenComma AlexPosn                      |
  TokenLParen AlexPosn                     |
  TokenRParen AlexPosn                     
  deriving (Eq,Show)

tokenPosn :: Token -> String
tokenPosn (TokenEmpty (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAddedTo (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEmptyStream (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPartOf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMSType (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlusMS (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlusS (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenStreamCons (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAddToFront (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAddToEnd (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAccessStream (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSingleStreamFunc (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDoubleStreamFunc (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCoeffs (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGetFrom (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAccF (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSeqDef (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBox0 (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenBox1 (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSeqExp0 (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSeqExp1 (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSeqExp2 (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInput (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLet (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIn (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt  (AlexPn a l c) n) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar  (AlexPn a l c) x) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenColon (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMultiply (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDivide (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenModulus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPowerOf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)

}