% Author: Andrei Haidu
% Email: a.haidu@gmail.com


:- module(mongo_prolog,
	[
		create_interface/1,
		set_world_state/2,
		get_models/2,
		get_links/3,
		get_collisions/4,
		add_collision_clauses/2,
		add_link_clauses/3,
		add_model_clauses/2,
		add_world_clauses/0,
		get_contacts/5,
		get_model_pose/3	
	]).
	
:- use_module(library('jpl')).

:- use_module(library(pce)).

% create_interface(-IJavaDB).
%% instantiates the mongo - java - prolog interface object
create_interface(IJavaDB) :-		
	jpl_new('mongo_prolog.MongoPrologInterface', [], IJavaDB).
	
% set_world_state(+IJavaDB, +Timestamp).
%% set all entities at given timestamp
set_world_state(IJavaDB, Timestamp) :-
	jpl_call(IJavaDB, 'setWorldState',[Timestamp],[]).


% get_models(+IJavaDB, -Model_Arr).
%% calls 'getModelNames' java function, returns array of model names 
get_models(IJavaDB, Model_Arr) :-
	jpl_call(IJavaDB, 'getModelNames',[], Models),
	jpl_array_to_list(Models, Model_Arr).


% get_links(+IJavaDB, +Model, -Link_Arr).
%% calls 'getLinkNames' java function of the given model,
%% returns an array of link names 
get_links(IJavaDB, Model, Link_Arr) :-
	jpl_call(IJavaDB, 'getLinkNames', [Model], Links),
	jpl_array_to_list(Links, Link_Arr).


% get_collisions(+IJavaDB, +Model, +Link, -Collision_Arr).
%% calls 'getCollisionNames' of the given Model::Link,
%% returns an array of collision names 
get_collisions(IJavaDB, Model, Link, Collision_Arr) :-
	jpl_call(IJavaDB, 'getCollisionNames', [Model, Link], Collisions),
	jpl_array_to_list(Collisions, Collision_Arr).


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
	 

%% set world clauses by initializing java interface object,
%% setting the world entities in the given timestamp
%% assert all mdoels, links, collisions and their relations
add_world_clauses :-
	create_interface(IJavaDB),
%	set_world_state(IJavaDB, Timestamp),
	get_models(IJavaDB, Model_Arr),
	add_model_clauses(IJavaDB, Model_Arr).

		
	
% get_contacts(+IJavaDB, +Model, +Link, +Collision, -Contact_Arr).
%% calls 'getContactNames' of the given Model::Link::Collision,
%% returns an array of contact names
get_contacts(IJavaDB, Model, Link, Collision, Contact_Arr) :-
	jpl_call(IJavaDB, 'getContactNames', [Model, Link, Collision], Contacts),
	jpl_array_to_list(Contacts, Contact_Arr).


% get_model_pose(+Model, +Timestamp, -Pose_Arr).
%% returns the pose of the given model as an array of double [X,Y,Z,R,P,Y]
get_model_pose(Model, Timestamp,  Pose_Arr) :-
	jpl_new('mongo_prolog.MongoPrologInterface', [], DB),
	jpl_call(DB, 'getModelPose', [Model, Timestamp], Pose),
	jpl_array_to_list(Pose, Pose_Arr).


% get_model_bb(+Model, +Timestamp, -BB_Arr).
%% returns the bounding box of the given model as an array of double [minX,minY,minZ,maxX,maxY,maxZ]
get_model_bb(Model, Timestamp, BB_Arr) :-
	jpl_new('mongo_prolog.MongoPrologInterface', [], DB),
	jpl_call(DB, 'getModelBoundingBox', [Model, Timestamp], BB),
	jpl_array_to_list(BB, BB_Arr).


% get_model_contacts(+Model, +Timestamp, -Contacts)
%% returns the contacts as collisions of the given model
get_model_contacts(Model, Timestamp, Contacts_Arr) :-	
	jpl_new('mongo_prolog.MongoPrologInterface', [], DB),
	jpl_call(DB, 'getModelContactNames', [Model, Timestamp], Contacts),
	jpl_array_to_list(Contacts, Contacts_Arr).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TESTING..


get_model_of_collision(Model,Collision) :-
	parent_of(X, Collision),
	parent_of(Model,X). 


%gen_model_arr([]).

%gen_model_arr([H_Arr|T_Arr]) :-
	


iterate_coll_arr([]).

iterate_coll_arr([H_Coll|T_Coll]) :-
	get_model_of_collision(Model,H_Coll),
%	append(Model,Models_Arr,Models_Arr),
	write(Model),
	iterate_coll_arr(T_Coll).

get_contacts_models(Model, Timestamp, Models_Arr) :-
	get_model_contacts(Model, Timestamp, Contacts_Arr),
	iterate_coll_arr(Contacts_Arr, Models_Arr).
	












































	
