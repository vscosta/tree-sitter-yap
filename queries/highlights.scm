

(comment) @comment

(integer) @number
(float) @number
(code) @number



(predicate_definition  head: (term (atom) @function))

(predicate_definition  head: (_ operator: (operator) @function))


(body literal: (term (atom) @function))

(body literal: (_ operator: (operator) @function))



[
  "."
  ","  
] @punctuation.delimiter

(operator) @operator


(quoted_atom) @string.special

(string) @string
(codes) @string

(eot) @function.builtin

(cut) @function.builtin

(variable) @variable

(operator) @operator

(ERROR) @error




