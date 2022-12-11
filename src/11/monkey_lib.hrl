-record(monkey, {
    number,
    items = [],
    mid_items = [],
    next_items = [],
    operation,
    divisible_number,
    if_true,
    if_false,
    common_divisor
}).
-type monkey() :: monkey.
-export_type([monkey/0]).