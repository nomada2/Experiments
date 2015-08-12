<<<<<<< HEAD
﻿#load "../../../packages/FsLab.0.2.7/FsLab.fsx"
=======
﻿#load "../../App/packages/FsLab.0.2.7/FsLab.fsx"
>>>>>>> origin/master
open System
open System.IO
open FSharp.Data
open XPlot.GoogleCharts


let [<Literal>] data_NameByState = __SOURCE_DIRECTORY__ + "/data/namesbystate/AK.TXT"
let [<Literal>] data_State = __SOURCE_DIRECTORY__ + "/data/states.csv"


//http://www.ssa.gov/OACT/babynames/limits.html
//http://www.fonz.net/blog/archives/2008/04/06/csv-of-states-and-state-abbreviations/

type censusDataContext = CsvProvider<data_NameByState>
type stateCodeContext = CsvProvider<data_State>
 
let stateCodes =  stateCodeContext.Load(data_State)


//let uri = System.String.Format("https://portalvhdspgzl51prtcpfj.blob.core.windows.net/censuschicken/{0}.TXT",stateCode)
let fetchStateData (stateCode:string)= async {
        let uri = System.String.Format(__SOURCE_DIRECTORY__ + "/data/namesbystate/{0}.TXT", stateCode)
        return! censusDataContext.AsyncLoad(uri) }
 
//put all of the data into 1 array
let sw = System.Diagnostics.Stopwatch.StartNew()

let usaData = stateCodes.Rows 
                |> Seq.map (fun r -> fetchStateData(r.Abbreviation))
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Seq.collect(fun r -> r.Rows)
                |> Seq.toArray

printfn "completed in %d ms" sw.ElapsedMilliseconds

//sum up a name across all states  
let nameSum = usaData 
                |> Seq.groupBy(fun r -> r.Mary)
                |> Seq.map(fun (n,a) -> n,a |> Seq.sumBy(fun (r) -> r.``14``)) 
                |> Seq.toArray
 
//sum up all records in the USA
let totalNames = nameSum |> Seq.sumBy(fun (n,c) -> c) // since 1910
 
//See the most popular names descending
<<<<<<< HEAD

=======
>>>>>>> origin/master
let nameAverage = nameSum 
                    |> Seq.map(fun (n,c) -> n,c,float c/ float totalNames)
                    |> Seq.sortBy(fun (n,c,a) -> -a - 1.)
                    |> Seq.toArray  
                  
<<<<<<< HEAD
nameAverage |> Seq.last
nameAverage |> Seq.head      
=======
                    //|> Seq.last
                    //|> Seq.head      
>>>>>>> origin/master


//make a function to see names, split by gender
let genderSearch name = 
    let nameFilter = usaData
                        |> Seq.filter(fun r -> r.Mary = name)
                        |> Seq.groupBy(fun r -> r.F)
                        |> Seq.map(fun (n,a) -> n,a |> Seq.sumBy(fun (r) -> r.``14``)) 
 
    let nameSum = nameFilter |> Seq.sumBy(fun (n,c) -> c)
    nameFilter 
        |> Seq.map(fun (n,c) -> n, c, float c/float nameSum) 
        |> Seq.toArray

<<<<<<< HEAD
let nameToAnalyze = "Thomas"

=======
>>>>>>> origin/master
genderSearch "Thomas"

//make a function to see names, split by year of birth
let ageSearch name =
    let nameFilter = usaData
                        |> Seq.filter(fun r -> r.Mary = name)
                        |> Seq.groupBy(fun r -> r.``1910``)
                        |> Seq.map(fun (n,a) -> n,a |> Seq.sumBy(fun (r) -> r.``14``)) 
                        |> Seq.toArray
    
    let nameSum = nameFilter |> Seq.sumBy(fun (n,c) -> c)
    nameFilter 
        |> Seq.map(fun (n,c) -> n, c, float c/float nameSum) 
        |> Seq.toArray

ageSearch "Jason" |> Array.sortBy(fun (_,_,p) -> -p) |> Seq.take 5

//Chart the year of birth
<<<<<<< HEAD
let chartData = ageSearch "Jason" // Siri
=======
let chartData = ageSearch "Bryony" // Siri
>>>>>>> origin/master
                    |> Seq.map(fun (y,c,p) -> y, c)
                    |> Seq.sortBy(fun (y,c) -> y)
                    |> Chart.Line

//Chart.Line(chartData)

//basic stats on Name - average, min ,max
ageSearch "Jason"
    |> Seq.map(fun (y,c,p) -> float c)
    |> Seq.average

ageSearch "Jason"
    |> Seq.map(fun (y,c,p) -> float c)
    |> Seq.min

ageSearch "Jason"
    |> Seq.map(fun (y,c,p) -> float c)
    |> Seq.max

//Variance
//http://www.mathsisfun.com/data/standard-deviation.html
let variance (source:float seq) =
    let mean = Seq.average source
    let deltas = Seq.map(fun x -> pown(x-mean) 2) source
    Seq.average deltas

let standardDeviation(values:float seq) =
    sqrt(variance(values))

let standardDeviation' name = ageSearch name
                                |> Seq.map(fun (y,c,p) -> float c)
                                |> standardDeviation
standardDeviation' "Jason"

//Average and Attachment Point
let average name = ageSearch name
                    |> Seq.map(fun (y,c,p) -> float c)
                    |> Seq.average
average "Jason"

let attachmentPoint name = average "Jason" + standardDeviation' "Jason"

attachmentPoint "Jason"

//PopularYears
let popularYears name = 
    let allYears = ageSearch name
    let attachmentPoint' = attachmentPoint name
    let filteredYears = allYears 
                        |> Seq.filter(fun (y,c,p) -> float c > attachmentPoint')
                        |> Seq.sortBy(fun (y,c,p) -> -c)
    filteredYears

popularYears "Jason"

//First and Last Popular Year
let lastPopularYear name = popularYears name |> Seq.last
let firstPopularYear name = popularYears name |> Seq.head

lastPopularYear "Jason"
firstPopularYear "Jason"


//StateSearch
let stateSearch name =
    let nameFilter = usaData
                        |> Seq.filter(fun r -> r.Mary = name)
                        |> Seq.groupBy(fun r -> r.AK)
                        |> Seq.map(fun (n,a) -> n,a |> Seq.sumBy(fun (r) -> r.``14``)) 
                        |> Seq.toArray
    let nameSum = nameFilter |> Seq.sumBy(fun (n,c) -> c)
    nameFilter 
        |> Seq.map(fun (n,c) -> n, c, float c/float nameSum) 
        |> Seq.toArray

//State Chart
let chartData' = stateSearch "Jason"
                    |> Seq.map(fun (s,c,p) -> s,c)
    
Chart.Column(chartData')

//Quartiles
//http://www.mathsisfun.com/data/quartiles.html
let topQuartileStates = stateSearch "Jason"
                            |> Seq.sortBy(fun (s,c,p) -> -c-1)
                            |> Seq.take (50/4)

let topQuartileTotal = topQuartileStates 
                            |> Seq.sumBy(fun (s,c,p) -> c)

let total = stateSearch "Jason"
                |> Seq.sumBy(fun (s,c,p) -> c)

float topQuartileTotal/float total

let genderSearch' name = 
    let nameFilter = usaData
                        |> Seq.filter(fun r -> r.Mary = name)
                        |> Seq.groupBy(fun r -> r.F)
                        |> Seq.map(fun (n,a) -> n,a |> Seq.sumBy(fun (r) -> r.``14``)) 
 
    let nameSum = nameFilter |> Seq.sumBy(fun (n,c) -> c)
    let nameFilter' = nameFilter
                        |> Seq.map(fun (n,c) -> n, c, float c/float nameSum) 
                        |> Seq.filter(fun (g,c,p) -> g = "M")
    if nameFilter' = Seq.empty then
            0.       
        else
        nameFilter'
            |> Seq.map(fun (g,c,p) -> p)
            |> Seq.head

genderSearch' "Jason"

//Last Popular Years -> Including Choice Type
let lastPopularYear' name = 
    let popularYears' = popularYears name 
    if popularYears' = Seq.empty
        then None
    else
        let last = popularYears' 
                    |> Seq.last
        let year, _, _ = last
        Some year

let lpy = lastPopularYear' "Jason"
let gs = genderSearch' "Jason"

//Just doing age and gender b/c state is too inclusive
let nameAssignment (malePercent, lastYearPopular) =
    match malePercent > 0.75, malePercent < 0.75, lastYearPopular < 1945, lastYearPopular > 1980 with
        | true, false, true, false -> "oldMale"
        | true, false, false, false -> "middleAgedMale"
        | true, false, false, true -> "youngMale"
        | false, true, true, false -> "oldFemale"
        | false, true, false, false -> "middleAgedFemale"
        | false, true, false, true -> "youngFemale"
        | _,_,_,_ -> "unknown"

nameAssignment (gs, lpy.Value)