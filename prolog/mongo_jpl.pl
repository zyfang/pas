:- module(mongo_jpl,
	[
		tests_mongo/0,
		get_models_links/0
	]).
	
:- use_module(library('jpl')).

tests_mongo :-
	jpl_new( 'mongojpl.MongoJpl', [], db),
	jpl_call( db, tests, [], _).	
	
get_models_links :-
	jpl_new( 'mongojpl.MongoJpl', [], DB),
	jpl_call(DB, getModels, [], Models),
	write(Models).
	

