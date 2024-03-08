

(comment) @comment

(integer) @number
(float) @number
(code) @number



(head (literal goal_functor: (atom)  @function))
(literal goal_functor: (atom)  @function.call)
(literal goal_functor: (builtin)  @function.call)


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
