

(comment) @comment

(integer) @number
(float) @number
(code) @number





(head predicate:      (goal (pred_name ))) @function

 (pred_name) @function.call

[
  ";"
  "."
  ","  
] @punctuation.delimiter


(operator) @operator

[
  "("
  ")"
  "{"
  "}"
]  @punctuation.bracket


(quoted_atom) @string.special

(string) @string
(codes) @string

(eot) @function.builtin

(cut) @function.builtin

(variable) @variable

(operator) @operator

(ERROR) @error


(predicate_indicator) @constant

