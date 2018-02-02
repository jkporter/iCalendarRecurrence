namespace iCalendaRecurrence

type Date = { FullYear: int; Month:int; MonthDay:int }
type Time = { Hour: int; Minute:int; Second:int }

type DateTime = { Date: Date; Time: Time }

// type StartDate = { Date: Date; Time: Time option }

type Weekday = 
    | Sunday = 1
    | Monday = 2
    | Tuesday = 3
    | Wednesday = 4
    | Thursday = 5
    | Friday = 6
    | Saturday = 7

type WeekDayNum = {
    OrderedWeek: int option;
    Weekday: Weekday
}

type MonthNum = {
    Num: int;
    Leap: bool option;
}

    

type Freqency =
    | Secondly
    | Minutely
    | Hourly
    | Daily
    | Weekly
    | Monthly
    | Yearly


type RecurrenceRule = 
   {
   Freqency: Freqency;
   Until: StartDate option;
   Count: int option;
   Interval: int option;
   BySecond: int list option;
   ByMinute: int list option;
   ByHour: int list option;
   ByDay: WeekDayNum list option;
   ByMonthDay: int list option;
   ByYearDay: int list option;
   ByWeekNo: int list option;
   ByMonth: int list option;
   BySetPosition: int list option;
   WeekStart: Weekday option;
   RScale: string option
   }