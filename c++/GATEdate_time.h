#ifndef GATEDATE_TIME_H
#define GATEDATE_TIME_H

#include <vector>

struct DateTime {
    int year;
    int month;
    int day;
    int hour;
    int minute;
    int second;
};

static std::vector<int> month_days = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

void date_converter(float date, int initial_month, int& year, int& month, int& day);
void time_converter(float time, int& hours, int& minutes, int& seconds);
int date_to_days(int year, int month, int day);
bool is_leap_year(int year);
int days_between(int year1, int month1, int day1, int year2, int month2, int day2);

// Converts day of year to month and day
void day_of_year_converter(int year, int julian_day, int& month, int& day);

#endif
