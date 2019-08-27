{
module COBRAGrammar where
import COBRATokens
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token
    empty                { TokenEmpty _ }
    addedTo              { TokenAddedTo _ }
    empty_stm            { TokenEmptyStream _ }
    partOf               { TokenPartOf _ }
    StreamType           { TokenSType _ }
    MultiStreamType      { TokenMSType _ }
    plusMS               { TokenPlusMS _ }
    plusS                { TokenPlusS _ }
    streamCons           { TokenStreamCons _ }
    addToFront           { TokenAddToFront _ }
    addToEnd             { TokenAddToEnd _ }
    access_stm           { TokenAccessStream _ }
    single_func          { TokenSingleStreamFunc _ }
    double_func          { TokenDoubleStreamFunc _ }
    coeffs               { TokenCoeffs _ }
    getFrom              { TokenGetFrom _ }
    accumulate           { TokenAccF _ }
    '$'                  { TokenSeqDef _ }
    box0                 { TokenBox0 _ }
    box1                 { TokenBox1 _ }
    seqExp0              { TokenSeqExp0 _ }
    seqExp1              { TokenSeqExp1 _ }
    seqExp2              { TokenSeqExp2 _ }
    inp                  { TokenInput _ }
    let                  { TokenLet _ }
    in                   { TokenIn _ }
    int                  { TokenInt _ $$ }
    '+'                  { TokenPlus _ }
    '-'                  { TokenMinus _ }
    '*'                  { TokenMultiply _ }
    '/'                  { TokenDivide _ }
    '%'                  { TokenModulus _ }
    '^'                  { TokenPowerOf _ }
    ':'                  { TokenColon _ }
    '='                  { TokenEq _ }
    '('                  { TokenLParen _ }
    ')'                  { TokenRParen _ }
    ','                  { TokenComma _ }
    var                  { TokenVar _ $$ }

%right let
%right in
right '+' '-' '*' '/'
%right single_func double_func accumulate
%nonassoc access_stm
%right plusMS
%right addedTo
%right streamCons
%nonassoc plusS
%right partOf
%right addToFront addToEnd
%nonassoc int var '(' ')'

%%
StreamExpr : PureMultiStream                                                  { PureMS $1 }
           | PureStream                                                       { PureS $1 }
           | StreamExpr plusMS StreamExpr                                     { PlusMStreams $1 $3 }
           | StreamExpr streamCons StreamExpr                                 { StreamCons $1 $3 }
           | StreamExpr plusS StreamExpr                                      { PlusStreams $1 $3 }
           | var                                                              { Variable $1 }
           | int addToFront StreamExpr                                        { AddToFront $1 $3 }
           | int addToEnd StreamExpr                                          { AddToEnd $1 $3 }
           | access_stm int StreamExpr                                        { AccessStream $2 $3 }
           | single_func ':' StreamExpr Operator int                          { SingleStreamFunction $3 $5 $4 }
           | double_func ':' int '*' StreamExpr Operator int '*' StreamExpr   { DoubleStreamFunction $5 $9 $3 $7 $6 }
           | accumulate StreamExpr ':' coeffs getFrom Sequence                { AccF $2 $6 }
           | let '(' var ':' Ty ')' '=' StreamExpr in StreamExpr              { Let $3 $5 $8 $10 }
           | inp                                                              { Input }
           | '(' StreamExpr ')'                                               { Bracket $2 }

PureMultiStream : empty                                    { Empty }
                | PureStream addedTo PureMultiStream       { AddedTo $1 $3 }
                | '(' PureMultiStream ')'                  { $2 }

PureStream : empty_stm                          { EmptyStream }
           | int partOf PureStream              { PartOf $1 $3 }
           | '(' PureStream ')'                 { $2 }

Sequence : '$' seqExp0 '=' int                                                                           { Constant $4}
         | '$' box0 '=' int ',' '$' box1 '=' int ',' '$' seqExp0 '=' '$' seqExp2 Operator '$' seqExp1    { Accumulation $4 $9 $16 }
         | '$' box0 '=' int ',' '$' seqExp0 '=' int '*' '$' seqExp1                                      { MultiplySeq $4 $9 }
         | '$' box0 '=' int ',' '$' seqExp0 '=' int '+' '$' seqExp1                                      { AddSeq $4 $9 }
         | '(' Sequence ')'                                                                              { $2 }

Operator : '+'                         { Plus }
         | '-'                         { Minus }
         | '*'                         { Multiply }
         | '/'                         { Divide }
         | '%'                         { Modulus}
         | '^'                         { PowerOf }
         | '(' Operator ')'            { $2 }

Ty : MultiStreamType         { MultiStreamType }
   | StreamType              { StreamType }

{
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error - maybe incomplete expression" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data PureMultiStream = Empty
                     | AddedTo PureStream PureMultiStream deriving (Eq, Show)

data PureStream = EmptyStream 
                | PartOf StreamInteger PureStream deriving (Eq, Show)

data Ty = StreamType 
        | MultiStreamType deriving (Eq,Show)

data StreamExpr = PureMS PureMultiStream
                | PlusMStreams StreamExpr StreamExpr
                | StreamCons StreamExpr StreamExpr
                | PlusStreams StreamExpr StreamExpr
                | Variable Var
                | PureS PureStream
                | AddToFront StreamInteger StreamExpr
                | AddToEnd StreamInteger StreamExpr
                | AccessStream Int StreamExpr
                | SingleStreamFunction StreamExpr Int Operator
                | DoubleStreamFunction StreamExpr StreamExpr Int Int Operator
                | AccF StreamExpr Sequence 
                | Input
                | Let Var Ty StreamExpr StreamExpr 
                | Bracket StreamExpr deriving (Eq, Show)
                

data Sequence = Constant Int
              | Accumulation Int Int Operator
              | MultiplySeq Int Int
              | AddSeq Int Int deriving (Eq, Show)

data Operator = Plus
              | Minus
              | Multiply
              | Divide
              | Modulus
              | PowerOf deriving (Eq, Show)

type Var = String

type StreamInteger = Int


}