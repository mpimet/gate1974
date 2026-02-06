#include <cctype>
#include "GATEutils.h"

// Parse a single format field starting at `pos` in `sv`
FormatField parse_field(std::string_view sv, size_t& pos) {
    if (pos >= sv.size() || sv[pos] != '%') {
        return {0, FormatType::UNKNOWN, false};
    }
    pos++; // skip '%'

    int width = 0;
    while (pos < sv.size() && std::isdigit(sv[pos])) {
        width = width * 10 + (sv[pos] - '0');
        pos++;
    }

    if (pos >= sv.size()) {
        return {width, FormatType::UNKNOWN, false};
    }

    char c = sv[pos];
    pos++;

    FormatType type = FormatType::UNKNOWN;
    bool valid = true;

    if (c == 'f') {
        type = FormatType::FLOAT;
    } else if (c == 'd') {
        type = FormatType::INT;
    } else if (c == 's') {
        type = FormatType::STRING;
    } else if (c == 'l' && pos < sv.size() && sv[pos] == 'f') {
        // %lf
        pos++;
        type = FormatType::FLOAT;
    } else {
        valid = false;
    }

    return {width, type, valid};
}

// Calculate total width and validate format
int calculate_record_width(const char* format) {
    int width = 0;
    while (*format) {
        if (*format == '%') {
            // Skip the % and possibly a number
            ++format;
            while (*format >= '0' && *format <= '9') {
                ++format;
            }
            // Now we expect a format specifier like 'f', 'd', etc.
            if (*format == 'f' || *format == 'd' || *format == 's' || *format == 'c') {
                // We assume the field width is the number before 'f', etc.
                // But we need to extract the width from the format string
                // So we go back and parse the digits
                const char* start = format - 1;
                while (start > format - 10 && *(start - 1) >= '0' && *(start - 1) <= '9') {
                    --start;
                }
                // Now parse the number from start to format
                int field_width = 0;
                while (start < format) {
                    if (*start >= '0' && *start <= '9') {
                        field_width = field_width * 10 + (*start - '0');
                    }
                    ++start;
                }
                width += field_width;
            }
        }
        ++format;
    }
    return width;
}

// Get number of fields
int count_fields(std::string_view format) {
    int count = 0;
    size_t pos = 0;

    while (pos < format.size()) {
        if (format[pos] == ' ') {
            pos++;
            continue;
        }

        auto field = parse_field(format, pos);
        if (field.valid) {
            count++;
        } else {
            return -1; // invalid format
        }
    }

    return count;
}
