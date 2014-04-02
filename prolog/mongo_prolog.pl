% Author: Andrei Haidu
% Email: a.haidu@gmail.com


:- module(mongo_prolog,
	[
		create_interface/2,
		set_world_state/2,
		get_models/2,
		get_links/3,
		get_collisions/4,
		add_collision_clauses/2,
		add_link_clauses/3,
		add_model_clauses/2,
		add_world_clauses/1,
		get_contacts/5,
		get_model_pose/4,
		get_model_pose/3,
		get_model_bb/4,
		get_link_pose/4,
		get_manipulation_event_timestamps/3
	]).
	

:- use_module(library('jpl')).


% create_interface(-IJavaDB).
%% instantiates the mongo - java - prolog interface object
create_interface(CollectionNr, IJavaDB) :-		
	jpl_new('mongo_prolog.MongoPrologInterface', [CollectionNr], IJavaDB).
	
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
add_world_clauses(CollectionNr) :-
	create_interface(CollectionNr, IJavaDB),
%	set_world_state(IJavaDB, Timestamp),
	get_models(IJavaDB, Model_Arr),
	add_model_clauses(IJavaDB, Model_Arr),
	!.

		
	
% get_contacts(+IJavaDB, +Model, +Link, +Collision, -Contact_Arr).
%% calls 'getContactNames' of the given Model::Link::Collision,
%% returns an array of contact names
get_contacts(IJavaDB, Model, Link, Collision, Contact_Arr) :-
	jpl_call(IJavaDB, 'getContactNames', [Model, Link, Collision], Contacts),
	jpl_array_to_list(Contacts, Contact_Arr).


% get_model_pose(+CollectionNr, +Model, +Timestamp, -Pose_Arr).
%% returns the pose of the given model as an array of double [X,Y,Z,R,P,Y]
get_model_pose(CollectionNr, Model, Timestamp, Pose_Arr) :-
	jpl_new('mongo_prolog.MongoPrologInterface', [CollectionNr], DB),
	jpl_call(DB, 'getModelPose', [Model, Timestamp], Pose),
	jpl_array_to_list(Pose, Pose_Arr).


% get_model_bb(+CollectionNr, +Model, +Timestamp, -BB_Arr).
%% returns the bounding box of the given model as an array of double [minX,minY,minZ,maxX,maxY,maxZ]
get_model_bb(CollectionNr, Model, Timestamp, BB_Arr) :-
	jpl_new('mongo_prolog.MongoPrologInterface', [CollectionNr], DB),
	jpl_call(DB, 'getModelBoundingBox', [Model, Timestamp], BB),
	jpl_array_to_list(BB, BB_Arr).


% get_link_pose(+CollectionNr, +Link, +Timestamp, -Pose_Arr).
%% returns the pose of the given link as an array of double [X,Y,Z,R,P,Y]
get_link_pose(CollectionNr, Link, Timestamp, Pose_Arr) :-
	jpl_new('mongo_prolog.MongoPrologInterface', [CollectionNr], DB),
	jpl_call(DB, 'getLinkPose', [Link, Timestamp], Pose),
	jpl_array_to_list(Pose, Pose_Arr).


%%%%%%%%%%%%%%%%%%%% HARDCODED STUFF IN JAVA %%%%%%%%%%%%%%%%%%%%%

%%%% flip
spatula_contact_pancake(CollectionNr, T) :-	
	jpl_new('mongo_prolog.MongoPrologInterface',[CollectionNr], DB),
	jpl_call(DB, 'getFlipStart', [CollectionNr], T).

lost_spatula_contact_pancake(CollectionNr, T) :-
	jpl_new('mongo_prolog.MongoPrologInterface', [CollectionNr], DB),
	jpl_call(DB, 'getFlipEnd', [CollectionNr], T).


%%%%%%%%%%%%%%%%% Robohow %%%%%%%%%%%%%%%%%%

% from the DB with events
% returns the pose of the given model as an array of double [X,Y,Z,R,P,Y]
get_model_pose(Model, Timestamp, Pose_Arr) :-
	jpl_new('mongo_prolog.MongoPrologInterface', [], DB),
	jpl_call(DB, 'getModelPose', [Model, Timestamp], Pose),
	jpl_array_to_list(Pose, Pose_Arr).


object_event_type(mug, pour).
object_event_type(spatula, flip).

object_type(mug, container).



get_manipulation_event_timestamps(Event, StT, EndT) :-
	jpl_new('mongo_prolog.MongoPrologInterface', [], DB),
	jpl_call(DB, 'getManipulationEventTimestamps', [Event], Timestamps),
	jpl_array_to_list(Timestamps, Timestamps_Arr),
	[StT,EndT] = Timestamps_Arr.


hand_manipulates_object(Obj, StT, EndT) :-
	object_event_type(Obj, Event),
	get_manipulation_event_timestamps(Event, StT, EndT).



get_particles_leaving_container(PT_Arr) :-
	jpl_new('mongo_prolog.MongoPrologInterface', [], DB),
	jpl_call(DB, 'getParticlesLeavingContainer', [], PT_HashMap),
	findall(PT_KeyValuePair, jpl_map_element(PT_HashMap, PT_KeyValuePair), PT_Arr).

	
pour(Stuff, Cont, ActSt, ActEnd, EvSt, EvEnd) :-
	object_type(Cont, container),
	hand_manipulates_object(Cont, ActSt, ActEnd),
	get_particles_leaving_container(Stuff),		% Stuff key-value pair particle-timestamp(string)
	pairs_values(Stuff, Times),			% get the timestamps(strings)	
	maplist(atom_number, Times, TimesInt), 		% change all thimestamps from string to number
	min_list(TimesInt, EvSt),
	max_list(TimesInt, EvEnd).


pouringPose(Pose) :-
	pour(_, Cont, _, _, EventStart, _),
	get_model_pose(Cont, EventStart, Pose).


draw_trajectory(Object, StartT, EndT) :-
        jpl_new('mongo_prolog.MongoPrologInterface', [], DB),
	jpl_call(DB, 'createTrajectory',[Object, StartT, EndT],[]).

pour_action_trajectory :-
	pour(_,Cont, ActSt, ActEnd,_,_),
	draw_trajectory(Cont, ActSt, ActEnd).



	
%%% KnowRob

sim_to_knowrob('mug', knowrob:'Cup').
sim_to_knowrob('mix', knowrob:'PancakeMix').

create_pouring_owl :-
	% read data from DB
	pour(Stuff, Cont, ActSt, ActEnd, EvSt, EvEnd),

	% create action instance, set start and end times
	cram_start_action(knowrob:'PouringSomething', '', ActSt, _, ActionInst),
	cram_finish_action(ActionInst, ActEnd),

	% create object instances
	sim_to_knowrob(Cont, ContCls),
	rdf_instance_from_class(ContCls, ContInst),

	pairs_keys(Stuff, StuffIDs),
	rdf_instance_from_class(knowrob:'PancakeMix', StuffInst),

	findall(Particle, (member(Particle, StuffIDs),
			rdf_assert(Particle, rdf:type, knowrob:'PancakeMix'),
			rdf_assert(StuffInst, knowrob:physicalParts, Particle)), _),

	% linst instances to the action
	rdf_assert(ActionInst, knowrob:fromLocation, ContInst),
	rdf_assert(ActionInst, knowrob:objectActedOn, StuffInst).










































	
