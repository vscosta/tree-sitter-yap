


module.exports = grammar({
    name: 'prolog',

    extras: $ => [
	/\s/, '\n', '\r',
	//    $.comment
    ],

    conflicts: $ => [
	[$._infix_operator, $._infix_operator],
	[$._infix_operator, $._prefix_non_associative],
	[$.builtin, $._atomic],
	[$.builtin, $.call_atom],
	[$.builtin, $.head_atom],
	[$.call_atom, $._atomic],
	[$.head_atom,$ ._atomic],
	[$.call_atom, $._compound_term],
	[$.head_atom,$ ._compound_term],
	[$.body,$ ._term],
	[$.goal,$ ._compound_term],
	[$.goal,$ ._operator_notation],
	[$.goal,$ ._functional_notation],
	//[$.__infix_non_associative, $.__infix_non_associative]
    ],

    superTypes: $ => [
	$._atomic,
	$.variable,
	$._compound_term,
	$.bracketed_term
    ],


    rules: {
	// TODO: add the actual grammar rules
	source_file: $ =>
	seq(
	    repeat(
		choice(
		    $.comment,
		    seq(
			choice(
			    $.directive,
			    $.predicate_definition
			),
			$.eot
		    )
		)
	    )
	),
	

	directive: $ => 
      	$._directive,

	predicate_definition: $ =>
	seq(
	    $.head,
	    optional($._body)),

	_body: $ =>
	choice(

	    prec.right(-1100,
		       seq($._body,';',$._body)),
	    prec.right(-1000,
		       seq($._body,',',$._body)),
	    prec.right(-1050,
		       seq($._body,'->',$._body)),
	    prec.right(-1050,
		       seq($._body,'*->',$._body)),
	    seq('(',  field("inner", $._body), ')')) ,
	    seq('{', $._body, '}'),
	    $.goal
    ),
    
	goal: $=>
	choice(
	    $._compound_goal,
	    $._functional_notation,
	    $._operator_notation,
	    field("goal",
		  $.call_atom
		 )     ),
	
head: $=>
seq(
    choice(
	$._compound_head,
	$._operator_notation,
	$.head_atom
    ),
    ,optional(seq(choice(':-','-->')
		 )
	     )
),
	call_atom: $  =>
	choice($.builtin,$._atom),
	head_atom: $  =>
	choice($.builtin,$._atom),
	builtin: $ =>
	choice(

	    "!",
	    "|",
	    "*->",
	    "<",
	    "=",
	    "=..",
	    "=:=",
	    "=<",
	    "==",
	    "=@=",
	    "=\\=",
	    ">",
	    ">=",
	    "?-",
	    "@<",
	    "@=<",
	    "@>",
	    "@>=",
	    "[]",
	    "\\+",
	    "\\=",
	    "\\==",
	    "^"
	),
	/*
	    "{}",
	    "abolish",
	    "abolish_all_tables",
	    "abolish_frozen_choice_points",
	    "abolish_module",
	    "abolish_table",
	    "abort",
	    "absf_trace",
	    "absf_trace_compogtnent",
	    "absolute_file_name",
	    "absolute_file_system_path",
	    "access",
	    "access_file",
	    "acyclic_term",
	    "add_import_module",
	    "add_to_array_element",
	    "add_to_path",
	    "alarm",
	    "all",
	    "all_attvars",
	    "always_prompt_user",
	    "append",
	    "append_args",
	    "arena_size",
	    "arg",
	    "array",
	    "array_element",
	    "assert",
	    "asserta",
	    "asserta_static",
	    "assert_in_program",
	    "assert_static",
	    "assertz",
	    "assertz_static",
	    "at_end_of_line",
	    "at_end_of_stream",
	    "at_end_of_stream_0",
	    "at_halt",
	    "atom",
	    "atom_chars",
	    "atom_codes",
	    "atom_concat",
	    "_atomic",
	    "_atomic_concat",
	    "_atomic_length",
	    "_atomic_list_concat",
	    "_atomics_to_string",
	    "_atomic_to_string",
	    "_atomic_to_term",
	    "atom_length",
	    "atom_number",
	    "atom_string",
	    "atom_to_string",
	    "atom_to_term",
	    "attvar",
	    "aux_preds",
	    "bagof",
	    "bb_delete",
	    "bb_get",
	    "bb_put",
	    "bb_update",
	    "between",
	    "b_getval",
	    "bind_attvar",
	    "break",
	    "b_setval",
	    "build_body",
	    "'C'",
	    "call",
	    "callable",
	    "call_cleanup",
	    "call_count",
	    "call_count_data",
	    "call_count_reset",
	    "call_in_mod",
	    "call_residue",
	    "call_residue_vars",
	    "call_shared_object_function",
	    "call_with_args",
	    "call_wo_mod",
	    "catch",
	    "cd",
	    "char_code",
	    "char_conversion",
	    "chars",
	    "char_type",
	    "char_type_alnum",
	    "char_type_alpha",
	    "char_type_ascii",
	    "char_type_cntrl",
	    "char_type_csym",
	    "char_type_csymf",
	    "char_type_digit",
	    "char_type_end_of_file",
	    "char_type_end_of_line",
	    "char_type_graph",
	    "char_type_lower",
	    "char_type_newline",
	    "char_type_paren",
	    "char_type_period",
	    "char_type_prolog_atom_start",
	    "char_type_prolog_identifier_continue",
	    "char_type_prolog_prolog_symbol",
	    "char_type_prolog_var_start",
	    "char_type_punct",
	    "char_type_quote",
	    "char_type_space",
	    "char_type_upper",
	    "char_type_white",
	    "char_type_xdigit",
	    "check_dbload_stream",
	    "checklist",
	    "checknodes",
	    "checknodes_list",
	    "clause",
	    "clause_property",
	    "close",
	    "close_shared_object",
	    "close_static_array",
	    "codes",
	    "code_type",
	    "code_type_alnum",
	    "code_type_alpha",
	    "code_type_ascii",
	    "code_type_cntrl",
	    "code_type_csym",
	    "code_type_csymf",
	    "code_type_digit",
	    "code_type_end_of_file",
	    "code_type_end_of_line",
	    "code_type_graph",
	    "code_type_lower",
	    "code_type_newline",
	    "code_type_paren",
	    "code_type_period",
	    "code_type_prolog_atom_start",
	    "code_type_prolog_identifier_continue",
	    "code_type_prolog_prolog_symbol",
	    "code_type_prolog_var_start",
	    "code_type_punct",
	    "code_type_quote",
	    "code_type_space",
	    "code_type_upper",
	    "code_type_white",
	    "code_type_xdigit",
	    "comma",
	    "comment_hook",
	    "commons_library",
	    "compare",
	    "compile",
	    "compile_clause",
	    "compile_clauses",
	    "compile_expressions",
	    "compile_predicates",
	    "compound",
	    "constraining_variables",
	    "consult",
	    "consult_depth",
	    "context_module",
	    "convlist",
	    "copy_term",
	    "copy_term_nat",
	    "create_mutable",
	    "create_prolog_flag",
	    "creep",
	    "creep_allowed",
	    "ctrace",
	    "current_atom",
	    "current_char_conversion",
	    "current_choice_point",
	    "current_dollar_var",
	    "current_error",
	    "current_host",
	    "current_input",
	    "current_key",
	    "current_line_number",
	    "current_module",
	    "current_mutex",
	    "current_op",
	    "current_output",
	    "current_predicate",
	    "current_prolog_flag",
	    "current_reference_count",
	    "current_source_module",
	    "current_stream",
	    "current_thread",
	    "cut_at",
	    "'_cut_by'",
	    "cut_by",
	    "cut_to",
	    "cyclic_term",
	    "db_files",
	    "dbload",
	    "dbload_add_fact",
	    "dbload_add_facts",
	    "dbload_count",
	    "db_reference",
	    "dcg_extend",
	    "debug",
	    "debug_action_hook",
	    "debugging",
	    "decrease_reference_count",
	    "del_attr",
	    "del_attrs",
	    "delete_file",
	    "delete_import_module",
	    "depth_bound_call",
	    "det_atom_concat",
	    "dif",
	    "diff_list",
	    "digit_weight",
	    "directory_files",
	    "discontiguous",
	    "display",
	    "do_dbload",
	    "domain_error",
	    "do_not_compile_expressions",
	    "downcase_atom",
	    "downcase_text_to_atom",
	    "downcase_text_to_chars",
	    "downcase_text_to_codes",
	    "downcase_text_to_string",
	    "dum",
	    "dump_active_goals",
	    "duplicate_term",
	    "dynamic",
	    "dynamic_predicate",
	    "dynamic_update_array",
	    "element_types",
	    "ensure_loaded",
	    "enter_compiler",
	    "erase",
	    "eraseall",
	    "erased",
	    "error_handler",
	    "exclude",
	    "existence_error",
	    "exists",
	    "exists_directory",
	    "exists_file",
	    "exists_source",
	    "exodb_add_fact",
	    "exodb_add_facts",
	    "exo_files",
	    "expand_clause",
	    "expand_expr",
	    "expand_exprs",
	    "expand_file_name",
	    "expand_goal",
	    "expand_term",
	    "expects_dialect",
	    "export",
	    "export_from_prolog",
	    "export_list",
	    "export_resource",
	    "extensions_to_present_answer",
	    "fail",
	    "false",
	    "fast_get_for_int_vector",
	    "fast_set_for_int_vector",
	    "file_base_name",
	    "file_directory_name",
	    "fileerrors",
	    "file_exists",
	    "file_name_extension",
	    "file_size",
	    "findall",
	    "float",
	    "flush_output",
	    "foldl",
	    "foldl_",
	    "foldl2",
	    "foldl2_",
	    "foldl3",
	    "foldl3_",
	    "foldl4",
	    "foldl4_",
	    "forall",
	    "format",
	    "format_to_chars",
	    "free_att",
	    "free_variables_in_term",
	    "freeze",
	    "freeze_choice_point",
	    "frozen",
	    "fully_strip_module",
	    "functor",
	    "garbage_collect",
	    "garbage_collect_atoms",
	    "gated_call",
	    "gc",
	    "get",
	    "get0",
	    "get_all_swi_atts",
	    "get_att",
	    "get_attr",
	    "get_attrs",
	    "get_byte",
	    "get_char",
	    "get_code",
	    "getcwd",
	    "get_depth_limit",
	    "getenv",
	    "get_module",
	    "get_mutable",
	    "get_string_code",
	    "get_time",
	    "get_value",
	    "global_trie_statistics",
	    "goal_expansion",
	    "'_ground'",
	    "ground",
	    "ground_type",
	    "grow_heap",
	    "grow_stack",
	    "halt",
	    "handle_port",
	    "handle_priv_port",
	    "has_type",
	    "heap_space_info",
	    "hide",
	    "hide_atom",
	    "hide_predicate",
	    "hostname_address",
	    "if",
	    "ignore",
	    "import_module",
	    "include",
	    "increase_reference_count",
	    "initialization",
	    "initialize_prolog",
	    "init_prolog",
	    "init_random_state",
	    "inline",
	    "instance",
	    "instance_property",
	    "instantiation_error",
	    "int_action",
	    "integer",
	    "is",
	    "is_absolute_file_name",
	    "is_atom",
	    "is_boolean",
	    "is_bound",
	    "is_callable",
	    "is_cyclic_term",
	    "isinf",
	    "is_list",
	    "is_list_or_partial_list",
	    "is_mutable",
	    "isnan",
	    "is_nonvar",
	    "is_not",
	    "is_of_type",
	    "is_tabled",
	    "key_erased_statistics",
	    "keysort",
	    "key_statistics",
	    "leash",
	    "length",
	    "libraries_directories",
	    "line_count",
	    "list_directory",
	    "listing",
	    "live",
	    "live__",
	    "load_absolute_foreign_files",
	    "load_db",
	    "load_exofacts",
	    "load_facts",
	    "load_files",
	    "load_foreign_files",
	    "load_mega_clause",
	    "log_event",
	    "logsum",
	    "ls",
	    "ls_imports",
	    "make",
	    "make_directory",
	    "make_library_index",
	    "maplist",
	    "mapnodes",
	    "mapnodes_list",
	    "may_bind_to_type",
	    "message",
	    "message_queue_create",
	    "message_queue_destroy",
	    "message_queue_property",
	    "message_to_string",
	    "meta_predicate",
	    "mksys",
	    "mmapped_array",
	    "module",
	    "module_property",
	    "module_state",
	    "msort",
	    "mtrace",
	    "multifile",
	    "must_be",
	    "must_be_atom",
	    "must_be_boolean",
	    "must_be_bound",
	    "must_be_callable",
	    "must_be_ground",
	    "must_be_instantiated",
	    "must_be_list",
	    "must_be_of_type",
	    "must_be_predicate_indicator",
	    "must_bind_to_type",
	    "mutex_create",
	    "mutex_property",
	    "mutex_unlock_all",
	    "name",
	    "nb_create",
	    "nb_current",
	    "nb_delete",
	    "nb_getval",
	    "nb_linkarg",
	    "nb_linkval",
	    "nb_setarg",
	    "nb_set_bit",
	    "nb_set_shared_arg",
	    "nb_set_shared_val",
	    "nb_setval",
	    "new_system_module",
	    "nl",
	    "nodebug",
	    "nofileerrors",
	    "nogc",
	    "non_det_atom_concat",
	    "non_ground",
	    "nonvar",
	    "no_source",
	    "nospy",
	    "nospyall",
	    "no_style_check",
	    "not",
	    "not_a_list",
	    "not_a_rational",
	    "notrace",
	    "nth_clause",
	    "nth_instance",
	    "number",
	    "number_atom",
	    "number_chars",
	    "number_codes",
	    "number_string",
	    "numbervars",
	    "once",
	    "on_exception",
	    "on_signal",
	    "op",
	    "opaque",
	    "op_cases",
	    "opdec",
	    "open",
	    "open_mem_read_stream",
	    "open_mem_write_stream",
	    "open_pipe_stream",
	    "open_shared_object",
	    "opt_statistics",
	    "or_statistics",
	    "otherwise",
	    "parallel",
	    "parallel_findall",
	    "parallel_findfirst",
	    "parallel_once",
	    "partition",
	    "path",
	    "path_concat",
	    "peek",
	    "peek_byte",
	    "peek_char",
	    "peek_code",
	    "peek_mem_write_stream",
	    "permission_error",
	    "phrase",
	    "phrase_",
	    "plus",
	    "portray_clause",
	    "predicate_erased_statistics",
	    "predicate_property",
	    "predicate_statistics",
	    "predmerge",
	    "pred_name",
	    "predsort",
	    "primitive",
	    "print",
	    "print_exception",
	    "print_message",
	    "print_message_lines",
	    "private",
	    "procedure",
	    "profalt",
	    "profend",
	    "profile_data",
	    "profile_reset",
	    "profinit",
	    "profoff",
	    "profon",
	    "prolog",
	    "prolog_current_frame",
	    "prolog_file_name",
	    "prolog_file_type",
	    "prolog_flag",
	    "prolog_flag_property",
	    "prolog_initialization",
	    "prolog_library",
	    "prolog_load_context",
	    "prolog_to_os_filename",
	    "prompt",
	    "prompt1",
	    "protected_exodb_add_fact",
	    "put",
	    "put_att",
	    "put_attr",
	    "put_attrs",
	    "put_byte",
	    "put_char",
	    "put_char1",
	    "put_code",
	    "putenv",
	    "pwd",
	    "qend_program",
	    "qload_file",
	    "qload_module",
	    "qpack_clean_up_to_disjunction",
	    "qsave_file",
	    "qsave_module",
	    "qsave_program",
	    "query_to_answer",
	    "raise_exception",
	    "rational",
	    "rational_term_to_forest",
	    "rational_term_to_tree",
	    "read",
	    "read_clause",
	    "read_from_chars",
	    "read_sig",
	    "read_term",
	    "read_term_from_atom",
	    "read_term_from__atomic",
	    "read_term_from_chars",
	    "read_term_from_codes",
	    "read_term_from_string",
	    "reconsult",
	    "recorda",
	    "recorda_at",
	    "recordaifnot",
	    "recorded",
	    "recordz",
	    "recordz_at",
	    "recordzifnot",
	    "release_random_state",
	    "remove_from_path",
	    "rename",
	    "repeat",
	    "representation_error",
	    "reset_static_array",
	    "reset_total_choicepoints",
	    "resize_static_array",
	    "restore",
	    "retract",
	    "retractall",
	    "rm_att",
	    "rmdir",
	    "s",
	    "same_file",
	    "save_program",
	    "scanl",
	    "scanl_",
	    "scan_to_list",
	    "see",
	    "seeing",
	    "seen",
	    "selectlist",
	    "selectlists",
	    "setarg",
	    "set_base_module",
	    "setenv",
	    "set_error",
	    "set_input",
	    "set_module_property",
	    "setof",
	    "set_output",
	    "set_prolog_flag",
	    "set_random_state",
	    "set_source_module",
	    "set_stream",
	    "set_stream_position",
	    "setup_call_catcher_cleanup",
	    "setup_call_cleanup",
	    "set_value",
	    "sformat",
	    "sh",
	    "show_all_local_tables",
	    "show_all_tables",
	    "show_global_trie",
	    "show_low_level_trace",
	    "showprofres",
	    "show_table",
	    "show_tabled_predicates",
	    "simple",
	    "skip",
	    "skip_list",
	    "sleep",
	    "socket",
	    "socket_accept",
	    "socket_bind",
	    "socket_close",
	    "socket_connect",
	    "socket_listen",
	    "sort",
	    "sort2",
	    "source",
	    "source_file",
	    "source_location",
	    "source_mode",
	    "source_module",
	    "split_path_file",
	    "spy",
	    "srandom",
	    "start_low_level_trace",
	    "stash_predicate",
	    "static_array",
	    "static_array_location",
	    "static_array_properties",
	    "static_array_to_term",
	    "statistics",
	    "stop_low_level_trace",
	    "stream_position",
	    "stream_position_data",
	    "stream_property",
	    "stream_select",
	    "string",
	    "string_atom",
	    "string_chars",
	    "string_code",
	    "string_codes",
	    "string_concat",
	    "string_length",
	    "string_number",
	    "string_to_atom",
	    "string_to__atomic",
	    "string_to_list",
	    "string_to_term",
	    "strip_module",
	    "style_check",
	    "sub_atom",
	    "sub_string",
	    "subsumes_term",
	    "sub_term",
	    "sub_text",
	    "succ",
	    "sumlist",
	    "sumnodes",
	    "sumnodes_body",
	    "sys_debug",
	    "system",
	    "system_library",
	    "system_module",
	    "system_module_",
	    "system_predicate",
	    "tab",
	    "table",
	    "table_statistics",
	    "tabling_mode",
	    "tabling_statistics",
	    "t_body",
	    "tell",
	    "telling",
	    "term_attterms",
	    "term_attvars",
	    "term_factorized",
	    "term_hash",
	    "term_string",
	    "term_to_atom",
	    "term_to_codes",
	    "term_to_string",
	    "term_variable_occurrences",
	    "term_variables",
	    "text",
	    "t_head",
	    "t_hgoal",
	    "t_hlist",
	    "thread_at_exit",
	    "thread_cancel",
	    "thread_create",
	    "thread_default",
	    "thread_defaults",
	    "thread_detach",
	    "thread_exit",
	    "thread_get_message",
	    "thread_join",
	    "thread_local",
	    "thread_peek_message",
	    "thread_property",
	    "threads",
	    "thread_self",
	    "thread_send_message",
	    "thread_set_default",
	    "thread_set_defaults",
	    "thread_signal",
	    "thread_sleep",
	    "thread_statistics",
	    "throw",
	    "throw_error",
	    "throw_file_error",
	    "time",
	    "time_file",
	    "time_file64",
	    "told",
	    "tolower",
	    "total_choicepoints",
	    "total_erased",
	    "toupper",
	    "trace",
	    "trace_error",
	    "trace_goal",
	    "trace_goal_",
	    "true",
	    "true_file_name",
	    "tthread_peek_message",
	    "t_tidy",
	    "ttyget",
	    "ttyget0",
	    "ttynl",
	    "ttyput",
	    "ttyskip",
	    "type_error",
	    "udi",
	    "unhide_atom",
	    "unify_with_occurs_check",
	    "unix",
	    "unknown",
	    "unload_file",
	    "unload_module",
	    "unnumbervars",
	    "upcase_atom",
	    "upcase_text_to_atom",
	    "upcase_text_to_chars",
	    "upcase_text_to_codes",
	    "upcase_text_to_string",
	    "update_array",
	    "update_mutable",
	    "update_whole_array",
	    "use_module",
	    "user_defined_directive",
	    "use_system_module",
	    "var",
	    "variables_in_any_term",
	    "variables_in_both_terms",
	    "variables_in_term",
	    "varnumbers",
	    "version",
	    "virtual_alarm",
	    "volatile",
	    "vsc_go",
	    "vsc_wait",
	    "wait",
	    "wake_choice_point",
	    "watch_goal",
	    "when",
	    "with_mutex",
	    "with_output_to",
	    "working_directory",
	    "write",
	    "write_canonical",
	    "write_depth",
	    "writeln",
	    "writeq",
	    "write_term",
	    "yap_flag",
	),*/


	_term: $ => prec(1, choice(
	    // Define Term
	    $._atomic,
	    $.variable,
	    $._compound_term,
	    $.bracketed_term,
	    $.dict
	)),

	bracketed_term: $ =>
	     seq('(', $._scoped_term, ')')
	     		    ,
	_scoped_term: $=>
	prec.right(
	    choice(
		prec(-1199, seq($._scoped_term,':-',$._scoped_term)),
		prec(-1199, seq($._scoped_term,'-->',$._scoped_term)),
		prec.right(-1100, seq($._scoped_term,';',$._scoped_term)),
		prec.right(-1000, seq($._scoped_term,",",$._scoped_term)),
		$._term
	    )
	),

	eot: $ =>
	    /\.[\s\n]/,

	_atomic: $ => prec(0,choice(
	    $.number,
	    //$.negaltive_number,
	    $.codes,
	    $.string,
	    $._atom,
	    alias('!', $.cut)
	)),

	number: $ =>
	choice(
	    $.code,
	    $.integer,
	    $.float
	),
	
	method: $ => seq(token.immediate('.'),/[A-Za-z_][A-Za-z0-9_]*/),
	

	variable: $ =>
	// Define Veriable
	    /[A-Z_][a-zA-Z0-9_]*/,

	_compound_term: $ =>  choice(
	    seq($._atom,
		token.immediate('('),
		$.args,
		')'
	       ),
	    $._functional_notation,
	    $._operator_notation,
	    $._list_notation,
	    $._curly_bracketed_notation,
	),

	_compound_head: $ => 
	seq(   field("predicate",
	  $.head_atom
		    ),
	       
	    token.immediate('('),
	    $.args,
	    ')'
	   ),

	_compound_goal: $ => 
	seq(   field("predicate",
	  $.call_atom
		    ),
	       
	    token.immediate('('),
	    $.args,
	    ')'
	   ),

	predicate: $ => $._atom,
	
	

	_directive: $ => prec(-1200,
			      seq(
				  ':-',$._body)),
	
	
	_functional_notation: $ => seq(

	    
	    optional(  choice($._atom,$.variable)),
	    repeat1( $.method),
	    optional(
		seq('(',//$.open_ct,
		    $.args,
		    ')'//$.close_ct
		   ))),
	
	_operator_notation: $ => choice(
//	    $._prefixqq_notation,
	    $._infix_notation,
	    $._postfix_notation
	    //'thing that should not be matched'
	),

	_list_notation: $ => $.list,

	_curly_bracketed_notation: $ => $.curly_bracket_term,

	_prefix_notation: $ => field('prefix', alias(choice(
	    // Define prefx notation`
	    $._prefix_non_associative,          // fx
	    $._prefix_right_associative         // fy
	), $.prefix_operator)),

	_infix_notation: $ => (
	    // Define infix notation
	    $._infix_operator
	),

	_postfix_notation: $ => field('postfix', alias(choice(
	    // Define postfix notation - Non in SWI spec I can see
	    // Need dynamic defintion of operators
	    //$._postfix_non_associative,         // xf
	    //$._postfix_left_associative         // yf
	), $.postfix_operator)),


	_infix_operator: $ => 
	{ // xfx
	    const table = [
		[prec,800,"<=="],
		[prec,800,"-=="],
		[prec,800,"+=="],
		[prec,780,"of"],
		[prec,700,"within"],
		[prec,700,"ins"],
		[prec,700,"in"],
		[prec,450,".."],
		[prec,50,"same"],
		[prec,200,"**"],
		[prec,600,"as"],
		[prec,700,">="],
		[prec,700,"=<"],
		[prec,700,">"],
		[prec,700,"<"],
		[prec,700,"=\\="],
		[prec,700,"=:="],
		[prec,700,"\\=@="],
		[prec,700,"=@="],
		[prec,700,"@>="],
		[prec,700,"@=<"],
		[prec,700,"@>"],
		[prec,700,"@<"],
		[prec,700,"\\=="],
		[prec,700,"=="],
		[prec,700,"=.."],
		[prec,700,"is"],
		[prec,700,"\\="],
		[prec,700,"="],
		//  [prec,1200,"-->"],
		//  [prec,1200,":-"],
		[prec.right,200,"^"],
		[prec.right,600,":"],
		//	    [prec.right,1000,","],
		//[prec.right,1050,"*->"],
		//[prec.right,1050,"->"],
		//  [prec.right,1100,";"],
		[prec.right,1105,"|"],
		[prec.left,950,":="],
		[prec.left,400,"rem"],
		[prec.left,400,"mod"],
		[prec.left,400,">>"],
		[prec.left,400,"<<"],
		[prec.left,400,"//"],
		[prec.left,400,"/"],
		[prec.left,400,"*"],
		[prec.left,400,"xor"],
		[prec.left,400,"div"],
		[prec.left,400,"rdiv"],
		[prec.left,500,"#"],
		[prec.left,500,"><"],
		[prec.left,500,"\\/"],
		[prec.left,500,"/\\"],
		[prec.left,500,"-"],
		[prec.left,500,"+"]
	    ];

	    return choice(...table.map(([fn, precedence, operator]) => fn(-precedence, seq(
		$._term,
		field('operator', alias(operator, $.operator)),
		$._term
	    ))))
	},


	// Prefix operaters seporated.. Unsure if I should colapes them at this stage.
	_prefix_non_associative: $ => {
	    const table = [
		[790,"matrix"],
		[790,"array"],
		[1150,"block"],
		[900,"nospy"],
		[900,"spy"],
		[1150,"table"],
		[1150,"discontiguous"],
		[1150,"module_transparent"],
		[1150,"meta_predicate"],
		[1150,"multifile"],
		[1150,"public"],
		[1150,"mode"],
		[1150,"volatile"],
		[1150,"initialization"],
		[1150,"thread_local"],
		[1150,"dynamic"],
		[1201,"?-"],
		//  [1201,":-"]
	    ];

	    return choice(...table.map(([precedence, operator]) => prec(-precedence, seq(
		field('operator', alias(operator, $.operator)),
		$._term
	    ))))
	},

	_prefix_right_associative: $=> {
	    const table = [
		//          [950,":="],
		[200,"\\"],
		[200,"-"],
		[200,"+"],
		[900,"\\+"],
		[900,"not"]
	    ];

	    return choice(...table.map(([precidence, operator]) => prec.right(-precidence, seq(
		field('operator', alias(operator, $.operator)),
		field('right', $._term)
	    ))))
	},

	dict: $ => seq(
	    choice(
		$._atom,
		$.variable
	    ),
	    '{',
	    seq($.dict_value,
		repeat(
		    seq(
			',',
			$.dict_value
		    )
		)
	       ),
	    '}'
	),

	dict_value: $ => seq(
	    $._atom,
	    token.immediate(':'),
	    $._term
	),
	integer: $ => token(choice(
	    seq(
		choice('0x', '0X'),
		/[A-Fa-f0-9]+/,
		optional(/[Ll]/)
	    ),
	    seq(
		choice('0o', '0O'),
		/_?[0-7]+/,
		optional(/[Ll]/)
	    ),
	    seq(
		choice('0b', '0B'),
		/_?[0-1]+/,
		optional(/[Ll]/)
	    ),
            seq(optional("-"),/[0-9]+/)
	)),

	float: $ => {
	    const digits = repeat1(/[0-9]+_?/);
	    const exponent = seq(/[eE][\+-]?/, digits)

	    return token(seq(
		choice(
		    seq(digits, '.', repeat1(digits), optional(exponent)),
		    seq(optional(digits), '.', repeat1(digits), optional(exponent)),
		    seq(digits, exponent)
		),
		optional(choice(/[Ll]/, /[jJ]/))
	    ))
	}, 

	code: $ =>
	seq(token("0\'"),
	    token.immediate(/./)
	   ),
	
	
	_atom: $ => 
	choice(
	    /[a-z][a-zA-Z0-9_]*/ ,
	    $.quoted_atom
	),


	



	quoted_atom: $ => token(seq('\'', /.*/, '\'')),

	string: $ => token(seq('`', /.*/, '`')),
	codes: $ => token(seq('\"', /.*/, '\"')),

	comment: $ => token(choice(
	    seq('%', /.*/),
	    seq(
		'/*',
		/[^*]*\*+([^/*][^*]*\*+)*/,
		'/'
	    )
	)),

	list: $ => prec.right(seq(
	    '[',
	    optional(	    $.args ),
	    ']'
	)),

	curly_bracket_term: $ => prec.right(seq(
	    field('open_cb', alias('{', $.curly_bracket)),
	    optional(seq($._term,
			 repeat(
			     prec.right(seq( // TODO: Work out if correct
				 ',',
				 $._term
			     ))
			 ))),
	    field('close_cb', alias('}', $.curly_bracket))
	)),

	_arg: $ => prec.left(choice(
	    $._term
	)),

	args: $ => prec.right(seq(
	    $._arg,
	    repeat(
		prec.left(seq( // TODO: Work out if correct
		    ',',
		    $._arg        ))
	    ),
	)),
	
    },
    user_defined_operators: $ => {

    }

});


