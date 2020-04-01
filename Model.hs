

type Cost = Double
type Rate = Double

-- code changes generate some defects, wich requires some code changes
totalCost :: Cost -> Rate -> Cost -> Double -> Cost
totalCost averageCodeChangeCost defectPerCodeChange averageDefectFix changes = changes * averageCodeChangeCost + changes * defectPerCodeChange * averageDefectFix

seriesCAF = map (totalCost 1 0.1 5) [0,100..10000]
seriesTDD = map (+ 350) $ map (totalCost 1 0.01 0.1) [0,100..10000]

showCSV a b = show a ++ "," ++ show b

output = zipWith showCSV seriesCAF seriesTDD

index ls = zipWith addIndex [10,20..1000] ls
    where
        addIndex i s = show i ++ "," ++ s
main = do
    putStr (unlines ("HOURS,CAF,TDD":index output))





