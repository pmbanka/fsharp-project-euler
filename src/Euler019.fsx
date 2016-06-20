open System

let startDate = new DateTime(1901,1,1)
let endDate = new DateTime(2000,12,31)
let numberOfSundays = 
    Seq.unfold (fun d -> if d <= endDate then Some(d, d.AddDays(1.0)) else None) startDate
    |> Seq.where (fun d -> d.DayOfWeek = DayOfWeek.Sunday && d.Day = 1)
    |> Seq.length