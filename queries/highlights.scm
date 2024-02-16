; highlights.scm

(comment) @comment

(integer) @number
(float) @number
(code) @number



(head  (atom)  @function)
(goal: (atom)  @function.call)
(goal: (builtin)  @function.call)


(functor) @constant

(quoted_atom) @string.special

(string) @string
(codes) @string

(eot) @function.builtin

(curly_bracket) @operator

(cut) @function.builtin

(variable) @variable

(operator) @operator

(ERROR) @error
