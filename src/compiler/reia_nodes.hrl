% Terminals
-record(integer,    {line, value}).
-record(float,      {line, value}).
-record(identifier, {line, name}).

% Operators
-record(unary_op,  {line, type, val}).
-record(binary_op, {line, type, val1, val2}).

% Other Expressions
-record(match, {line, left, right}).
-record(cons,  {line, expr, tail}).
-record(empty, {line}).
-record(tuple, {line, elements}).