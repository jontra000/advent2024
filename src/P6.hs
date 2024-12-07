module P6 (run1, run2, inputLocation) where
import Lib (textToCoordMap, Coord, Direction(..), move, rotateRight)
import Data.List (nub, find, sort, sortBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Data.Ord (comparing, Down (..))

type Obstructions = S.Set Coord
type MapGrid = M.Map Coord Char
type State = (Direction, Coord)
data Input = Input Int Int Obstructions State
type ObstructionMap = M.Map Direction (M.Map Int [Int])

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input6"

parse :: String -> Input
parse = initialState . textToCoordMap

solve1 :: Input -> Int
solve1 = length . nub . map snd . walk

initialState :: MapGrid -> Input
initialState initMap = Input xMax yMax (obstructions initMap) (DirUp, startLocation initMap)
    where xMax = maximum $ map fst $ M.keys initMap
          yMax = maximum $ map snd $ M.keys initMap

obstructions :: MapGrid -> Obstructions
obstructions = S.fromList . M.keys . M.filter (=='#')

startLocation :: MapGrid -> Coord
startLocation = fst . fromJust . find ((=='^') . snd) . M.toList

walk :: Input -> [State]
walk (Input xMax yMax obstructions_ start) = walk' start
    where walk' state@(dir, loc) =
            let loc' = move dir loc
            in  if outOfBounds xMax yMax loc'
                then [state]
                else if S.member loc' obstructions_
                    then walk' (rotateRight dir, loc)
                    else state : walk' (dir, loc')

walkVertices :: ObstructionMap -> State -> [State]
walkVertices nextObstructionMap = walk'
    where walk' state =
            case findNextObstruction nextObstructionMap state of
                Nothing -> [state]
                Just state' -> state : walk' state'

obstructionMap :: Int -> Int -> Obstructions -> ObstructionMap
obstructionMap xMax yMax obstructions_ =
    let downList = makeDownList xMax obstructions_
        upList = M.map reverse downList
        rightList = makeRightList yMax obstructions_
        leftList = M.map reverse rightList
    in  M.fromList [(DirUp, upList), (DirDown, downList), (DirLeft, leftList), (DirRight, rightList)]

makeDownList :: Int -> Obstructions -> M.Map Int [Int]
makeDownList xMax obstructions_ = M.fromList $ map (ysForX obstructions_) [0..xMax]

ysForX :: Obstructions -> Int -> (Int, [Int])
ysForX coords x = (x, sort (S.toList (S.map snd (S.filter ((==x) . fst) coords))))

makeRightList :: Int -> Obstructions -> M.Map Int [Int]
makeRightList yMax obstructions_ = M.fromList $ map (xsForY obstructions_) [0..yMax]

xsForY :: Obstructions -> Int -> (Int, [Int])
xsForY coords y = (y, sort (S.toList (S.map fst (S.filter ((==y) . snd) coords))))

findNextObstruction :: ObstructionMap -> State -> Maybe State
findNextObstruction nextObstructionMap state = nextLoc state <$> find (nextObstructionCoord state) (obstructionList nextObstructionMap state)

nextObstructionCoord :: (Direction, Coord) -> Int -> Bool
nextObstructionCoord (DirUp, (_,y)) = (< y)
nextObstructionCoord (DirDown, (_,y)) = (> y)
nextObstructionCoord (DirLeft, (x,_)) = (< x)
nextObstructionCoord (DirRight, (x,_)) = (> x)

obstructionList :: ObstructionMap -> State -> [Int]
obstructionList nextObstructionMap (dir, (x,y)) =
    let i = case dir of
            DirUp -> x
            DirDown -> x
            DirLeft -> y
            DirRight -> y
    in  (nextObstructionMap M.! dir) M.! i

nextLoc :: State -> Int -> State
nextLoc (DirUp, (x,_)) y = (DirRight, (x,y+1))
nextLoc (DirRight, (_,y)) x = (DirDown, (x-1,y))
nextLoc (DirDown, (x,_)) y = (DirLeft, (x,y-1))
nextLoc (DirLeft, (_,y)) x = (DirUp, (x+1,y))

outOfBounds :: Int -> Int -> Coord -> Bool
outOfBounds xMax yMax (x,y) = x < 0 || y < 0 || x > xMax || y > yMax

solve2 :: Input -> Int
solve2 input@(Input xMax yMax obstructions' start) = length $ filter (causesLoop initObstructionMap start) potentialLocs
    where potentialLocs = nub $ map snd $ tail $ walk input
          initObstructionMap = obstructionMap xMax yMax obstructions'

causesLoop :: ObstructionMap -> State -> Coord -> Bool
causesLoop initObstructionMap start obstruction = checkLoop S.empty $ walkVertices obstructionMap' start
    where obstructionMap' = updateObstructionMap initObstructionMap obstruction

updateObstructionMap :: ObstructionMap -> Coord -> ObstructionMap
updateObstructionMap initObstructionMap (x,y) = M.mapWithKey go initObstructionMap
    where go DirUp subMap = M.adjust (sortBy (comparing Data.Ord.Down) . (y:)) x subMap
          go DirDown subMap = M.adjust (sort . (y:)) x subMap
          go DirRight subMap = M.adjust (sort . (x:)) y subMap
          go DirLeft subMap = M.adjust (sortBy (comparing Data.Ord.Down) . (x:)) y subMap

checkLoop :: S.Set State -> [State] -> Bool
checkLoop _ [] = False
checkLoop prevStates (x:xs)
    | S.member x prevStates = True
    | otherwise = checkLoop (S.insert x prevStates) xs