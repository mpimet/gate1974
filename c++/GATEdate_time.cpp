#include <string>
#include <cmath>
#include <iostream>

#include "GATEdate_time.h"

void date_converter(float date, int initial_month, int& year, int& month, int& day) {
    static int ctrl_day = 0;
    static int ctrl_month = 0;

    day = static_cast<int>(date);
    if (ctrl_month == 0) ctrl_month = initial_month;
    if (day < ctrl_day) ctrl_month++;

    ctrl_day = day;
    month = ctrl_month;
    year = 1974;
}

void time_converter(float time, int& hours, int& minutes, int& seconds) {
    hours = static_cast<int>(time);
    minutes = static_cast<int>(std::round(time * 100)) - hours * 100;
    seconds = 0;
}

bool is_leap_year(int year) {
    if (year % 4 == 0) {
        if (year % 100 == 0) {
            if (year % 400 == 0) {
                return true;
            } else {
                return false;
            }
        } else {
            return true;
        }
    } else {
        return false;
    }
}

int date_to_days(int year, int month, int day) {
    int total_days = 0;

    for (int i = 1974; i < year; ++i) {
        total_days += is_leap_year(i) ? 366 : 365;
    }

    for (int i = 1; i < month; ++i) {
        if (i == 2 && is_leap_year(year)) {
            total_days += 29;
        } else {
            total_days += month_days[i - 1];
        }
    }

    total_days += day - 1;
    return total_days;
}

int days_between(int year1, int month1, int day1, int year2, int month2, int day2) {
    return date_to_days(year1, month1, day1) - date_to_days(year2, month2, day2);
}
