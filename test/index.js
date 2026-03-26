TemporalHelpers.checkToTemporalCalendarFastPath(temporalObject => {
  Temporal.PlainYearMonth.compare({
    year: 2000,
    month: 5,
    calendar: temporalObject
  }, {
    year: 2001,
    month: 6,
    calendar: temporalObject
  });
});
