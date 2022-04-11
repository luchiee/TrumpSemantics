{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Main where

----------------------------------------------------------- IMPORTS --------------------------------------------------------------
import Data.Attoparsec.Text
    ( sepBy, skipSpace, char, parseOnly, takeWhile1, Parser )
import qualified Data.Text as T
import Control.Applicative ( Alternative((<|>)) )
import Control.Monad ( replicateM )
import Data.Set ( fromList, intersection, toList )
import Data.List ( intersect, union )

----------------------------------------------------------- DATA & TYPE VARIABLES --------------------------------------------------------------


type Relation     = String
type Var          = String
type Value        = Int
type Combination  = [[Value]]

type Inds         = [Var]
type Binary       = [(Relation, (Value, Value))] -- binary relations
type Env          = [Var] -- variables in scope 
type Domain       = [Value]

type Deal          = [(Var, Value)]
type Trump         = [Deal]
type Trumps        = [Trump]  -- [[(x,0), (y,0)]] 
type TrumpCoTrump  = (Trumps, Trumps) -- turn to tuple


data Formula = IndTrue
             | IndFalse
             | Not Formula
             | Reln Relation [Var]
             | And  Formula Formula
             | Or  Formula Formula
             | Exists Var Inds Formula 
             | Forall Var Inds Formula
             deriving (Show)


----------------------------------------------------------- **PARSER** --------------------------------------------------------------

-- Binary Parser
parseB :: T.Text -> Either String Binary
parseB = parseOnly parseBinary

parseBinary :: Parser Binary 
parseBinary = parseBinaryMult <|> parseBinarySingle

parseBinaryMult :: Parser Binary 
parseBinaryMult = do 
      d <- atom <* ss
      char '('
      atoms <- atom `sepBy` char ','
      char ')'
      char ','
      char ' '
      next <- parseBinary
      return $ (d, tupleAtoms atoms) : next


parseBinarySingle :: Parser Binary 
parseBinarySingle = do 
      d <- atom <* ss
      char '('
      atoms <- atom `sepBy` char ','
      char ')'
      return [(d, tupleAtoms atoms )]

tupleAtoms :: [String] -> (Value, Value)
tupleAtoms [a] =  (first, second)
      where first  = read (interim !! 0) :: Int
            second = read (interim !! 1) :: Int
            interim = T.unpack <$> T.splitOn "," (T.pack a)


-- Domain Parser 

parseD :: T.Text -> Either String Domain
parseD = parseOnly parseDomain

parseDomain :: Parser Domain 
parseDomain = do 
      char '['
      atoms <- atom `sepBy` char ','
      char ']'
      return $ valueList atoms

valueList :: [String] -> [Value]
valueList [a] =  [read i :: Int | i <- interim ]
      where interim = T.unpack <$> T.splitOn "," (T.pack a)


-- Formula Parser 

parseF :: T.Text -> Either String Formula
parseF = parseOnly parseFormula

parseFormula :: Parser Formula
parseFormula =  parseConDisjunction <|> parseFormula'

parseFormula' :: Parser Formula
parseFormula' = parseParen
            <|> parseQuantifierInd
            <|> parseQuantifier
            <|> parseConst
            <|> parseReln
            <|> parseNot


parseConst :: Parser Formula
parseConst =   "True"  *> pure (IndTrue)
           <|> "False" *> pure (IndFalse)


parseReln :: Parser Formula
parseReln = do 
      d <- atom <* ss
      char '('
      atoms <- atom `sepBy` char ','
      char ')'
      return $ Reln d (correctBracket atoms)

parseNot :: Parser Formula
parseNot =   do 
      char '~'
      Not <$> parseFormula


parseQuantifierInd :: Parser Formula
parseQuantifierInd = do 
      con <- char 'E' *> pure Exists <|> char 'A' *> pure Forall
      v <- atom <* ss
      char '/'
      atoms <- atom `sepBy` char ','
      char '.'
      con v (correctBracket atoms) <$> parseFormula


parseQuantifier :: Parser Formula
parseQuantifier = do 
      con <- char 'E' *> pure Exists <|> char 'A' *> pure Forall
      v <- atom <* ss
      char '.'
      con v [] <$> parseFormula

parseParen :: Parser Formula
parseParen = do
      char '('
      f <- parseFormula
      char ')'
      return f

parseConDisjunction = parseConDisjunction' -- parseConDisjunctionInd' 
                  <|> parseTerm


parseConDisjunction' :: Parser Formula
parseConDisjunction' = do 
                  t <- parseTerm <* ss
                  con <- char '^' *> pure And <|> char 'v' *> pure Or
                  ss 
                  con t <$> parseFormula

parseTerm =   parseParen
          <|> parseReln
          <|> parseConst
          <|> parseQuantifierInd
          <|> parseQuantifier
          


-- PARSER HELPERS --
correctBracket :: [String] -> [Var]
correctBracket [a] =  T.unpack <$> T.splitOn "," (T.pack a)

ss = skipSpace 

commaSep p = p `sepBy` char ','

atom    = T.unpack <$> takeWhile1 (\c-> c /= ' '  && c /= '"' && c /= '(' && c /= ')' && c /= '.' && c /= '/' && c /= '[' && c /= ']')

----------------------------------------------------------- INTERPRETER --------------------------------------------------------------

eval :: Formula -> Binary -> Env -> Domain -> TrumpCoTrump
eval IndTrue               bin env dom = ([[[]]],[[]])
eval IndFalse              bin env dom = ([[]],[[[]]]) 
eval (Not f)               bin env dom = (cotrump, trump)
                                        where trump      = fst trumpsCos 
                                              cotrump    = snd trumpsCos
                                              trumpsCos  = eval f bin env dom
eval (Reln r vars)         bin env dom = createTrumps trumpCombs dom allVars combinations    
                                        where trumpCombs = [x | x <- combinations, Prelude.take 2 x `elem` binaryVals]
                                              binaryVals = Prelude.map (\(x,y) -> [x,y]) (snd <$> Prelude.filter (\(i',_) -> r == i') bin)
                                              combinations = replicateM (length env) dom
                                              allVars = vars ++ loneVars env vars 
eval (Or f1 f2)        bin env dom = (unionise leftTrump rightTrump, intersectionise leftCoTrump rightCoTrump)
                                        where leftTrump    = filterTrumps (fst leftFormula) env 
                                              leftCoTrump  = filterTrumps (snd leftFormula) env
                                              rightTrump   = filterTrumps (fst rightFormula) env
                                              rightCoTrump = filterTrumps (snd rightFormula) env
                                              leftFormula  = eval f1 bin env dom
                                              rightFormula = eval f2 bin env dom
eval (And f1 f2)      bin env dom = (intersectionise leftTrump rightTrump, unionise leftCoTrump rightCoTrump)
                                        where leftTrump    = filterTrumps (fst leftFormula) env 
                                              leftCoTrump  = filterTrumps (snd leftFormula) env
                                              rightTrump   = filterTrumps (fst rightFormula) env
                                              rightCoTrump = filterTrumps (snd rightFormula) env
                                              leftFormula  = eval f1 bin env dom
                                              rightFormula = eval f2 bin env dom
eval (Exists var inds f)   bin env dom = (maxXtrump, maxXcotrump) 
                                        where maxXcotrump = universalTrump var dom ucotrump
                                              maxXtrump   = existentialTrump var env inds dom utrump 
                                              utrump      = fst trumpsCos 
                                              ucotrump    = snd trumpsCos 
                                              trumpsCos   = eval f bin (var:env) dom 
eval (Forall var inds f)   bin env dom = (maxXtrump, maxXcotrump)
                                        where maxXtrump   = universalTrump var dom utrump
                                              maxXcotrump = existentialTrump var env inds dom ucotrump
                                              utrump      = fst trumpsCos 
                                              ucotrump    = snd trumpsCos 
                                              trumpsCos   = eval f bin (var:env) dom 

-- RELATIONS --

createTrumps :: Combination -> Domain -> Env -> Combination -> TrumpCoTrump
createTrumps trumpCombs dom vars combs = (makeTrumps fullerTrump, makeTrumps fullerCotrump) -- will feed single maximal trumps
                                        where fullerTrump = Prelude.map (zip vars) trumpCombs
                                              fullerCotrump = Prelude.map (zip vars) cotrumpCombs -- this is maximal trump
                                              cotrumpCombs = [ x | x <- combs, x `notElem`  trumpCombs]

makeTrumps :: Trump -> Trumps
makeTrumps p = [p]

-- CONJUNCTION AND DISJUNCTION -- 

intersectionise :: Trumps -> Trumps -> Trumps
intersectionise leftTrumps rightTrumps = toList $ fromList $ reduceSubTrumps $ filter (not . null) $ concat [[toList (Data.Set.intersection (fromList l) (fromList r)) | l <- leftTrumps] | r <- rightTrumps]


unionise :: Trumps -> Trumps -> Trumps
unionise leftTrumps rightTrumps | null leftTrumps       && null rightTrumps        = rightTrumps -- return either null Trumps
                                | null leftTrumps       && not (null rightTrumps)  = rightTrumps
                                | not (null leftTrumps) && null rightTrumps        = leftTrumps
                                | otherwise                                        = reduceSubTrumps $ concat [[l `union` r | l <- leftTrumps] | r <- rightTrumps] --makeUnionList leftTrumps rightTrumps


-- QUANTIFIERS -


-- Universal Trump
universalTrump :: Var -> Domain -> Trumps -> Trumps
universalTrump var dom maxUtrumps | maxUtrumps == [[]] = [[]]
                                  | maxUtrumps == [[[]]] = [[[]]]
                                  | otherwise          =  toList $ fromList $  [universalCanBe (filterTrump maxUtrump var) maxUtrump var dom| maxUtrump <- maxUtrumps]

universalCanBe :: Trump -> Trump -> Var -> Domain -> Trump 
universalCanBe filtered maximalUtrump var dom = [f | f <- filtered, and [(f ++ varMatched d) `elem` maximalUtrump | d <- dom]]
                           where varMatched d = [(var, d)]


-- Existential Trump
existentialTrump :: Var -> Env -> Inds -> Domain -> Trumps -> Trumps -- this will return the maximal trumps
existentialTrump var env inds dom maxUtrumps | maxUtrumps == [[]] = [[]]
                                             | otherwise =  toList $ fromList $ reduceSubTrumps $ cleanify' $  concat [cleanify $ foldr1 (zipWith union) [existentialList (filterTrump wUset var) wUset var dom | wUset <- wEquivalentSet maxUtrump inds env var dom] | maxUtrump <- maxUtrumps]


existentialList :: Trump -> Trump -> Var -> Domain -> Trumps
existentialList xi ui var dom        | xi == [] = [[]]
                               | xi == [[]] = [[[]]]
                               | otherwise = reduceSubTrumps $ foldr1 (zipWith union) [existentialCanBe xAssign ui var dom | xAssign <- xi]

existentialCanBe :: Deal -> Trump -> Var -> Domain -> Trumps -- assumption only one assignment 
existentialCanBe xAssign ui var dom = [if (xAssign ++ [(var,d)]) `elem` ui then [xAssign] else [[]] | d <- dom]


-- W-Equivalent Set

wEquivalentSet :: Trump -> Inds -> Env -> Var -> Domain -> Trumps 
wEquivalentSet utrump inds env var dom | length inds == 0          = [[u] | u <- utrump]
                                        | otherwise        = filter (not . null) [[u | u <- utrump , and [ i `elem` u | i <- nonInds] ] | nonInds <- nonIndsValsMatched ]
                                where nonInds            = [e | e <- env, e `notElem` inds] ++ [var]
                                      nonIndsVals        = replicateM (length nonInds) dom
                                      nonIndsValsMatched = [zip nonInds i | i <- nonIndsVals]



-- Quantifier Helpers
reduceSubTrumps :: Trumps -> Trumps
reduceSubTrumps [[]] = [[]]
reduceSubTrumps trmps = [t | t <- trmps, notIn t trmps] 

notIn :: Trump -> Trumps -> Bool
notIn t1 trmps = not $ or [magic t1 t2 |t2 <- trmps ]
             where magic t1 t2 | t1 == t2  = False
                               | otherwise = and [ small `elem` t2  | small <- t1 ]
cleanify :: Trumps -> Trumps
cleanify [[[]]] = [[[]]]
cleanify trmps = [[t | t <- ts, not (null t)]| ts <- trmps]

cleanify' :: Trumps -> Trumps
cleanify' trmps = [ts | ts <- trmps, not (null ts)] 




-- INTERPRETER HELPERS --

filterTrumps :: Trumps -> Env -> Trumps
filterTrumps trumps env = [toList $ fromList $ [[var | var <- assignment, fst var `elem` env] | assignment <- trump]| trump <- trumps]

filterTrump :: Trump -> Var -> Trump
filterTrump trump var =  toList $ fromList $ [[x | x <- xs, fst x /= var]| xs <- trump]

loneVars :: Env -> Env -> Env 
loneVars env vars = [x | x <- env, x `notElem` vars]

----------------------------------------------------------- MAIN --------------------------------------------------------------

convertF :: Either String Formula -> Formula
convertF (Right f) = f
convertF (Left _) = error "Please input correct Formula Notation, for help, seek ..."

convertB :: Either String Binary -> Binary
convertB (Right b) = b
convertB (Left _) = error "Please input a valid Binary Relation"

convertD :: Either String Domain -> Domain
convertD (Right d) = d
convertD (Left _) = error "Please input a valid Domain"

main :: IO()
main = 
      do
      putStrLn "Please set your formula, ie. Ax.Ey/x.P(x,y)∨(Ez.Q(x,y))"
      formula <- getLine 
      let f = convertF $ parseF (T.pack formula) -- Ax.Ey/x.P(x,y)∨(Ez.Q(x,y))
      putStrLn "Please set your binary relations separated by commas, ie. P(0,0), P(1,1), Q(0,1)"
      relations <- getLine 
      let bin = convertB $ parseB (T.pack relations) 
      putStrLn "Please set your domain, ie. [0,1]"
      formula <- getLine 
      let dom = convertD $ parseD (T.pack formula) -- Ax.Ey/x.P(x,y)∨(Ez.Q(x,y))
      let env = []
      let p = eval f bin env dom
      print p

      --Ax.Ey/x.False