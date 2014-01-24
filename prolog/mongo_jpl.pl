:- module(mongo_jpl,
	[
		get_models/0,
		get_links/1		
	]).
	
:- use_module(library('jpl')).



get_models :-
	jpl_new('mongojpl.MongoPrologInterface', [], DB),
	jpl_call(DB, 'getModelNames', [], Models),
	jpl_array_to_list(Models, Model_Arr),
	write(Model_Arr).

get_links(Model) :-
	jpl_new('mongojpl.MongoPrologInterface', [], DB),
	jpl_call(DB, 'getLinkNames', [Model], Links),
	jpl_array_to_list(Links, Link_Arr),
	write(Link_Arr).

get_models_and_links :-
	jpl_new('mongojpl.MongoPrologInterface', [], DB),
	jpl_call(DB, 'getModelNames', [], Models),
	jpl_array_to_list(Models, Model_Arr),
	
	
