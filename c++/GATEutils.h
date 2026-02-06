#ifndef GATE_UTILS_H
#define GATE_UTILS_H

#include <string_view>
#include <cstddef>

enum class FormatType {
    FLOAT,
    INT,
    STRING,
    UNKNOWN
};

struct FormatField {
    int width;
    FormatType type;
    bool valid;
};

// Parse a single format field starting at `pos` in `sv`
FormatField parse_field(std::string_view sv, size_t& pos);

// Calculate total width and validate format
int calculate_record_width(const char* format);

// Get number of fields
int count_fields(std::string_view format);

#endif
