% Author: Andrei Haidu
% Email: a.haidu@gmail.com


:- module(mongo_prolog,
	[
		create_interface/1,
		get_models/2,
		get_links/3,
		get_collisions/4,
		get_model_pose/1,	
		add_collision_clauses/2,
		add_link_clauses/3,
		add_model_clauses/2,
		add_clauses/0,
		add_world_clauses/0
	]).
	
:- use_module(library('jpl')).



% create_interface(-IJavaDB).
%% instantiates the mongo - java - prolog interface object
create_interface(IJavaDB) :-		
	jpl_new('mongo_prolog.MongoPrologInterface', [], IJavaDB).
	

% get_models(+IJavaDB, -Model_Arr).
%% calls 'getModelNames' java function, returns array of model names 
get_models(IJavaDB, Model_Arr) :-
	jpl_call(IJavaDB, 'getModelNames',[], Models),
	jpl_array_to_list(Models, Model_Arr).


% get_links(+IJavaDB, +Model, -Link_Arr).
%% calls 'getLinkNames' java function of the given model,
%% returns array of link names 
get_links(IJavaDB, Model, Link_Arr) :-
	jpl_call(IJavaDB, 'getLinkNames', [Model], Links),
	jpl_array_to_list(Links, Link_Arr).


% get_collisions(+IJavaDB, +Model, +Link, -Collision_Arr).
%% calls 'getCollisionNames' of the given Model::Link,
%% returns array of collision names 
get_collisions(IJavaDB, Model, Link, Collision_Arr) :-
	jpl_call(IJavaDB, 'getCollisionNames', [Model, Link], Collisions),
	jpl_array_to_list(Collisions, Collision_Arr).


get_model_pose(Model) :-
	jpl_new('mongo_prolog.MongoPrologInterface', [], DB),
	jpl_call(DB, 'getModelPose', [Model], Pose),
	write(Pose).



%% make sure when the array is empty the return is 'true'	
add_collision_clauses(_,[]).

% add_collision_clauses(+Link, +Collision_Arr).
%% assert the collision clauses, with parent and children
add_collision_clauses(Link, [H_Collision|T_Collision]) :-
	assert(collision(H_Collision)),
	assert(parent_of(Link, H_Collision)),
	assert(child_of(H_Collision, Link)),
	add_collision_clauses(Link, T_Collision).


%% make sure when the array is empty the return is 'true'	
add_link_clauses(_,_,[]).

%add_link_clauses(+IJavaDB, +Model, +Link_Arr).
%% assert the link clauses, with parent and children
%% get collision for every link and assert them
add_link_clauses(IJavaDB, Model, [H_Link|T_Link]) :-
	assert(link(H_Link)),
	assert(parent_of(Model, H_Link)),
	assert(child_of(H_Link, Model)),
	get_collisions(IJavaDB, Model, H_Link, Collision_Arr),
	add_collision_clauses(H_Link, Collision_Arr),
	add_link_clauses(IJavaDB, Model, T_Link).


%% make sure when the array is empty the return is 'true'	
add_model_clauses(_,[]).

%add_model_clauses(+IJavaDB, +Model_Arr).
%% assert the model clauses, with parent and children
%% get links for every model and assert them
add_model_clauses(IJavaDB, [H_Model|T_Model]) :-
	assert(model(H_Model)),
	get_links(IJavaDB, H_Model, Link_Arr),
	add_link_clauses(IJavaDB, H_Model, Link_Arr),
	add_model_clauses(IJavaDB, T_Model).
	 
	
%% set world clauses by initializing java interface object
%% getting all models, links, collisions and asserting them
add_world_clauses :-
	create_interface(IJavaDB),
	get_models(IJavaDB, Model_Arr),
	add_model_clauses(IJavaDB, Model_Arr).

		
	
