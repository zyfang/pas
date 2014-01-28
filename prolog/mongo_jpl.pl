:- module(mongo_jpl,
	[
		get_models/1,
		get_links/2,
		add_link_clauses/2,
		add_model_clauses/1,
		add_clauses/0	
	]).
	
:- use_module(library('jpl')).



get_models(Model_Arr) :-
	jpl_new('mongojpl.MongoPrologInterface', [], DB),
	jpl_call(DB, 'getModelNames', [], Models),
	jpl_array_to_list(Models, Model_Arr).
	
get_links(Model, Link_Arr) :-
	jpl_new('mongojpl.MongoPrologInterface', [], DB),
	jpl_call(DB, 'getLinkNames', [Model], Links),
	jpl_array_to_list(Links, Link_Arr).
			
	
	
add_link_clauses(_,[]).
	
add_link_clauses(Model, [H_Link|T_Link]) :-
	assert(link(H_Link)),
	assert(parent(Model, H_Link)),
	assert(child(H_Link, Model)),
	add_link_clauses(Model, T_Link).
	
	
add_model_clauses([]).

add_model_clauses([H_Model|T_Model]) :-
	assert(model(H_Model)),
	get_links(H_Model, Link_Arr),
	add_link_clauses(H_Model, Link_Arr),
	add_model_clauses(T_Model).

add_clauses :-
	get_models(Model_Arr),
	add_model_clauses(Model_Arr).
	
	
	
	
	
	
	
	
	
