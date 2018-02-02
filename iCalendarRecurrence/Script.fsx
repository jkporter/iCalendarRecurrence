// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#if INTERACTIVE
#r "../packages/NodaTime.1.3.2/lib/net35-Client/NodaTime.dll"
#endif

#load "Library1.fs"

open iCalendaRecurrence
open NodaTime

// Define your library scripting code here
type ByDayLimit = 
    | IsoDayOfWeek of IsoDayOfWeek
    | LocalDateTime of LocalDateTime

let occurrences (dateStart : DateTime, zone : DateTimeZone option) recurrenceRule =
    let localDateTime dateTime = new LocalDateTime(dateTime.Date.FullYear, dateTime.Date.Month, dateTime.Date.MonthDay, dateTime.Time.Hour, dateTime.Time.Minute, dateTime.Time.Second)

    let dateTime (localDateTime : LocalDateTime) = { Date = {FullYear = localDateTime.Year; Month = localDateTime.Month; MonthDay = localDateTime.Day  }; Time = { Hour = localDateTime.Hour; Minute = localDateTime.Minute; Second = localDateTime.Second  }}

    let gregorianCalendar = CalendarSystem.GetGregorianCalendar(4)
    let calendarSystem = gregorianCalendar
    // let dateStart = dateStart.WithCalendar(calendarSystem);
    
    let optionalApply (byList : option<list<'T>>) by (occurrences : Set<DateTime>) = 
        match byList with
        | Some(x) -> by x occurrences
        | None -> occurrences
    
    let limit list x = Set.filter (fun (occurrence : DateTime) -> list occurrence |> List.contains (x occurrence))
    
    let expand list generator (occurrences : Set<DateTime>) = 
        let valid value occurrence = 
            try 
                Some(generator value occurrence)
            with :? System.InvalidOperationException -> None
        occurrences
        |> Set.map (fun occurrence -> 
               list occurrence
               |> List.choose (fun value -> valid value occurrence)
               |> Set.ofList)
        |> Set.unionMany
    
    let byMonth byMonthList occurrences = 
        match recurrenceRule.Freqency with
        | Secondly | Minutely | Hourly | Daily | Weekly | Monthly -> 
            occurrences |> limit (fun _ -> byMonthList) (fun occurrence -> occurrence.Date.Month)
        | Yearly -> 
            occurrences 
            |> expand (fun _ -> byMonthList) 
                   (fun monthNumber occurrence -> { occurrence with Date = { occurrence.Date with Month = monthNumber} })
    
    //    let byWeekNo byWeekdayList occurrences = 
    //        match recurrenceRule.Freqency with
    //        | Yearly -> 
    //            (new LocalDateTime()).we
    //            occurrences 
    //            |> expand byWeekdayList 
    //                   (fun monthNumber occurrence -> occurrence.PlusMonths(monthNumber - occurrence.Month))
    //    
    let byYearDay byYearDayList occurrences =
        let dayOfYear occurrence = localDateTime(occurrence).DayOfYear 
        let dayOfYearList (occurrence : DateTime) = 
            let localDateTime = localDateTime(occurrence)
            let maxMonth = localDateTime.Calendar.GetMaxMonth(localDateTime.Year)
            let lastDayOfYear = 
                (new LocalDate(localDateTime.Year, maxMonth, localDateTime.Calendar.GetDaysInMonth(localDateTime.Year, maxMonth))).DayOfYear
            byYearDayList
            |> List.map (fun yearDayNumber -> 
                   match yearDayNumber with
                   | yearDayNumber when yearDayNumber > 0 -> yearDayNumber
                   | yearDayNumber when yearDayNumber < 0 -> lastDayOfYear + yearDayNumber + 1)
            |> List.filter (fun dayOfYear -> dayOfYear >= 1 && dayOfYear <= lastDayOfYear)
            
        match recurrenceRule.Freqency with
        | Secondly | Minutely | Hourly -> occurrences |> limit dayOfYearList (fun occurrence -> dayOfYear(occurrence))
        | Yearly -> 
            occurrences 
            |> expand dayOfYearList (fun dayOfYear occurrence -> dateTime(localDateTime(occurrence).PlusDays(dayOfYear - localDateTime.DayOfYear)))
    
    let byMonthDay byMonthDayList occurrences = 
        let dayOfMonthList (occurrence) = 
            let localDateTime = localDateTime(occurrence)
            let lastDayOfMonth = localDateTime.Calendar.GetDaysInMonth(localDateTime.Year, localDateTime.Month)
            byMonthDayList
            |> List.map (fun monthNumber -> 
                   match monthNumber with
                   | monthNumber when monthNumber > 0 -> monthNumber
                   | monthNumber when monthNumber < 0 -> lastDayOfMonth + monthNumber + 1)
            |> List.filter (fun dayOfMonth -> dayOfMonth >= 1 && dayOfMonth <= lastDayOfMonth)
        match recurrenceRule.Freqency with
        | Secondly | Minutely | Hourly | Daily -> occurrences |> limit dayOfMonthList (fun occurrence -> occurrence.Date.MonthDay)
        | Monthly | Yearly -> 
            occurrences 
            |> expand dayOfMonthList 
                   (fun monthDay occurrence -> { occurrence with Date = { occurrence.Date with MonthDay = monthDay} })
    
    //    let byDay (byWeekDayList : (int option * weekday) list) occurrences = 
    //        
    //
    //        let weekdayToIsoDayOfWeek (weekday) = 
    //            match weekday with
    //            | Sunday -> NodaTime.IsoDayOfWeek.Sunday
    //            | Monday -> NodaTime.IsoDayOfWeek.Monday
    //            | Tuesday -> NodaTime.IsoDayOfWeek.Tuesday
    //            | Wednesday -> NodaTime.IsoDayOfWeek.Wednesday
    //            | Thursday -> NodaTime.IsoDayOfWeek.Thursday
    //            | Friday -> NodaTime.IsoDayOfWeek.Friday
    //            | Saturday -> NodaTime.IsoDayOfWeek.Saturday
    //
    //        let s (startDate:LocalDateTime) endDate nth weekDay =
    //            IsoDayOfWeek.
    //            let startDate = match startDate.IsoDayOfWeek with
    //                            | weekdayToIsoDayOfWeek(weekDay) -> startDate
    //                            | _ -> startDate.
    //            match nth with
    //            | nth when nth > 0 ->  startDate
    //
    //        let daysByWeekday weekday (occurrence: LocalDateTime) = 
    //            Seq.initInfinite (fun _ -> occurrence.Next(weekdayToIsoDayOfWeek(weekday))
    //        
    //        let byWeekDayLists = byWeekDayList |> List.partition (fun (ordWeek, _) -> ordWeek.IsNone)
    //        
    //        let byWeekDayLists = 
    //            (fst (byWeekDayLists) |> List.map (fun (_, weekday) -> weekdayToIsoDayOfWeek (weekday)), 
    //             (snd (byWeekDayLists) |> List.map (fun (ordWeek, weekday) -> 
    //                                          match recurrenceRule.Freqency with
    //                                          | Yearly when recurrenceRule.ByWeekNo.IsSome -> 1
    //                                          | Monthly | Yearly when recurrenceRule.Freqency <> Yearly && recurrenceRule.ByMonth.IsSome -> 2
    //                                          | Yearly -> 3
    //                                          )))
    //        occurrences
    let byHour byHourList occurrences = 
        match recurrenceRule.Freqency with
        | Secondly | Minutely | Hourly -> 
            occurrences |> limit (fun _ -> byHourList) (fun occurrence -> occurrence.Time.Hour)
        | Daily | Weekly | Monthly | Yearly -> 
            occurrences 
            |> expand (fun _ -> byHourList) 
                   (fun hour occurrence -> { occurrence with Time = { occurrence.Time with Hour = hour} })
    
    let byMinute byMinuteList occurrences = 
        match recurrenceRule.Freqency with
        | Secondly | Minutely -> 
            occurrences |> limit (fun _ -> byMinuteList) (fun occurrence -> occurrence.Time.Minute)
        | Hourly | Daily | Weekly | Monthly | Yearly -> 
            occurrences 
            |> expand (fun _ -> byMinuteList) 
                   (fun minute occurrence -> { occurrence with Time = { occurrence.Time with Minute = minute }})
    
    let bySecond bySecondList occurrences = 
        match recurrenceRule.Freqency with
        | Secondly -> occurrences |> limit (fun _ -> bySecondList) (fun occurrence -> occurrence.Time.Second)
        | Minutely | Hourly | Daily | Weekly | Monthly | Yearly -> 
            occurrences 
            |> expand (fun _ -> bySecondList) 
                   (fun second occurrence -> { occurrence with Time = { occurrence.Time with Second = second }})
    
    let bySetPosition bySetPositionList occurrences = 
        let size = Set.count occurrences
        let nthOccurrenceList = 
            bySetPositionList |> List.map (fun setPositionDay -> 
                                     match setPositionDay with
                                     | setPositionDay when setPositionDay > 0 -> setPositionDay
                                     | setPositionDay when setPositionDay < 0 -> size + setPositionDay + 1
                                     | _ -> invalidArg "" "")
        occurrences
        |> Set.toSeq
        |> Seq.indexed
        |> Seq.map (fun (index, occurrence) -> (index + 1, occurrence))
        |> Seq.filter (fun (nth, _) -> nthOccurrenceList |> List.contains (nth))
        |> Seq.map (fun (_, occurrence) -> occurrence)
        |> Set.ofSeq
    
    let interpret occurrence = 
        match zone with
        | Some(x) -> 
            dateTime(zone.Value.ResolveLocal(localDateTime(occurrence), 
                                    (fun mapping -> 
                                    match mapping.Count with
                                    | 0 -> 
                                        new ZonedDateTime(mapping.LocalDateTime.PlusTicks
                                                              (mapping.LateInterval.Savings.Ticks), mapping.Zone, 
                                                          mapping.LateInterval.WallOffset)
                                    | _ -> mapping.First())).LocalDateTime)
        | None -> occurrence
    
    let localDateStart = localDateTime(dateStart)
    Seq.initInfinite (fun index -> 
        let interval = 
            match recurrenceRule.Interval with
            | Some(x) -> x
            | None -> 1
        Set.singleton (dateTime(match recurrenceRule.Freqency with
                                | Secondly -> localDateStart.PlusSeconds(int64 (index * interval))
                                | Minutely -> localDateStart.PlusMinutes(int64 (index * interval))
                                | Hourly -> localDateStart.PlusHours(int64 (index * interval))
                                | Daily -> localDateStart.PlusDays(index * interval)
                                | Weekly -> localDateStart.PlusWeeks(index * interval)
                                | Monthly -> localDateStart.PlusMonths(index * interval)
                                | Yearly -> localDateStart.PlusYears(index * interval))))
    |> Seq.collect (fun occurrences -> 
           occurrences
           |> optionalApply recurrenceRule.ByMonth byMonth
           //|> optionalApply recurrenceRule.ByWeekNo byWeekNo
           |> optionalApply recurrenceRule.ByYearDay byYearDay
           |> optionalApply recurrenceRule.ByMonthDay byMonthDay
           //|> optionalApply recurrenceRule.ByDay byDay
           |> optionalApply recurrenceRule.ByHour byHour
           |> optionalApply recurrenceRule.ByMinute byMinute
           |> optionalApply recurrenceRule.BySecond bySecond
           |> Set.map interpret
           |> optionalApply recurrenceRule.BySetPosition bySetPosition
           |> Set.toSeq
           // |> Seq.map (fun occurrence -> occurrence.WithCalendar(gregorianCalendar)
           )
    // |> Seq.takeWhile (fun occurrence -> recurrenceRule.Until = None || occurrence <= LocalDateTime.FromDateTime(recurrenceRule.Until.Value))
    |> (fun occurrences -> 
    match recurrenceRule.Count with
    | Some(x) -> occurrences |> Seq.take x
    | None -> occurrences)

//let rule = 
//    { Freqency = Yearly
//      Until = None
//      Count = Some(10)
//      Interval = Some(2)
//      BySecond = None
//      ByMinute = Some([ 30 ])
//      ByHour = Some([ 8; 9 ])
//      ByDay = 
//          Some([ { OrderedWeek = None
//                   Weekday = Weekday.Sunday } ])
//      ByMonthDay = None
//      ByYearDay = None
//      ByWeekNo = None
//      ByMonth = Some([ 1 ])
//      BySetPosition = None
//      WeekStart = None
//      RScale = None }
//
//let localDateStart = new LocalDateTime(1997, 1, 5, 8, 30, 0)
//
//occurrences (localDateStart * None) rule |> Seq.iter (fun g -> System.Console.WriteLine(g))
