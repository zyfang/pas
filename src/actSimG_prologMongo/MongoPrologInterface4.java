/* Author: Andrei Haidu
 * Email: a.haidu@gmail.com
 */

package actSimG_prologMongo;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

import Jama.Matrix;
import Jama.EigenvalueDecomposition;

import com.mongodb.BasicDBObject;
import com.mongodb.DB;
import com.mongodb.DBCollection;
import com.mongodb.DBCursor;
import com.mongodb.DBObject;
import com.mongodb.MongoClient;
import com.mongodb.util.JSON;
import com.mxgraph.swing.util.mxGraphActions.RemoveFromParentAction;

import org.javatuples.Pair;
import org.jgrapht.ext.*;
import org.jgrapht.*;
import org.jgrapht.graph.*;
import org.jgrapht.util.*;

public class MongoPrologInterface4 {

	private static final long A_TIMESTAMP = 0L;

	private MongoClient mongoClient;
	private DB db;
	private DBCollection coll;
	private World world;

	//TODO this should be unnecessary once the logging is fixed
	private static final Map<String, String> nameMap  = new HashMap<String, String>();
	static{
		nameMap.put("kitchen_link_geom_counter_side_island_back_link", "kitchen_counter");
		nameMap.put("kitchen_link_geom_counter_side_island_left_link", "kitchen_counter");
		nameMap.put("kitchen_link_geom_counter_side_island_right_link", "kitchen_counter");
		nameMap.put("kitchen_link_geom_counter_top_island_link", "kitchen_counter");
		nameMap.put("kitchen_link_geom_skirting_island_link", "kitchen_counter");
		nameMap.put("kitchen_link_geom_stove_link", "kitchen_counter");
		nameMap.put("mug_body_geom_mug_max_x", "mug");
		nameMap.put("mug_body_geom_mug_max_x_help", "mug");
		nameMap.put("mug_body_geom_mug_max_y", "mug");
		nameMap.put("mug_body_geom_mug_min_x", "mug");
		nameMap.put("mug_body_geom_mug_min_y", "mug");
		nameMap.put("mug_body_geom_mug_min_x_help", "mug");
		nameMap.put("mug_body_geom_mug_bottom", "mug");
		//		nameMap.put("right_hand_fore_finger_base_link_collision", "right_forefinger");
		//		nameMap.put("right_hand_fore_finger_proximal_link_collision", "right_forefinger");
		//		nameMap.put("right_hand_fore_finger_middle_link_collision", "right_forefinger");
		//		nameMap.put("right_hand_fore_finger_distal_link_collision", "right_forefinger");
		//		nameMap.put("right_hand_middle_finger_base_link_collision", "right_middlefinger");
		//		nameMap.put("right_hand_middle_finger_proximal_link_collision", "right_middlefinger");
		//		nameMap.put("right_hand_middle_finger_middle_link_collision", "right_middlefinger");
		//		nameMap.put("right_hand_middle_finger_distal_link_collision", "right_middlefinger");
		//		nameMap.put("right_hand_ring_finger_base_link_collision", "right_ringfinger");
		//		nameMap.put("right_hand_ring_finger_proximal_link_collision", "right_ringfinger");
		//		nameMap.put("right_hand_ring_finger_middle_link_collision", "right_ringfinger");
		//		nameMap.put("right_hand_ring_finger_distal_link_collision", "right_ringfinger");
		//		nameMap.put("right_hand_thumb_base_collision", "right_thumb");
		//		nameMap.put("right_hand_thumb_base_link_collision", "right_thumb");
		//		nameMap.put("right_hand_thumb_proximal_link_collision", "right_thumb");
		//		nameMap.put("right_hand_thumb_middle_link_collision", "right_thumb");
		//		nameMap.put("right_hand_thumb_distal_link_collision", "right_thumb");
		nameMap.put("right_hand_fore_finger_base_link_collision", "right_hand");
		nameMap.put("right_hand_fore_finger_proximal_link_collision", "right_hand");
		nameMap.put("right_hand_fore_finger_middle_link_collision", "right_hand");
		nameMap.put("right_hand_fore_finger_distal_link_collision", "right_hand");
		nameMap.put("right_hand_middle_finger_base_link_collision", "right_hand");
		nameMap.put("right_hand_middle_finger_proximal_link_collision", "right_hand");
		nameMap.put("right_hand_middle_finger_middle_link_collision", "right_hand");
		nameMap.put("right_hand_middle_finger_distal_link_collision", "right_hand");
		nameMap.put("right_hand_ring_finger_base_link_collision", "right_hand");
		nameMap.put("right_hand_ring_finger_proximal_link_collision", "right_hand");
		nameMap.put("right_hand_ring_finger_middle_link_collision", "right_hand");
		nameMap.put("right_hand_ring_finger_distal_link_collision", "right_hand");
		nameMap.put("right_hand_thumb_base_collision", "right_hand");
		nameMap.put("right_hand_thumb_base_link_collision", "right_hand");
		nameMap.put("right_hand_thumb_proximal_link_collision", "right_hand");
		nameMap.put("right_hand_thumb_middle_link_collision", "right_hand");
		nameMap.put("right_hand_thumb_distal_link_collision", "right_hand");
		nameMap.put("spatula_extension_collision", "spatula_rest");
		nameMap.put("spatula_head_collision", "spatula_rest");
	}

	/**
	 * MongoPrologInterface constructor
	 * @param collection 
	 */
	public MongoPrologInterface4(int collection) {		
		// echo for prolog
		System.out.println("IJavaDB: " + "calling MongoPrologInterface constructor, setting up connection to database..");

		try {
			// create a new DB client
			this.mongoClient = new MongoClient( "localhost" , 27017 );

			// get the given DB
			this.db = mongoClient.getDB("sim_db");

			// get the given collection from the DB
			//this.coll = this.db.getCollection("collection_X" + collection);
			this.coll = this.db.getCollection("event_coll_X_copy");

		} catch (UnknownHostException e) {
			e.printStackTrace();
		}
	}

	public Map<String, String> parseJSONAllObjects(JSONObject jsonobj, Map<String,String> output) throws JSONException
	{
		Iterator<?> keys = jsonobj.keys();
		int i = 0;
		while(keys.hasNext()){
			System.out.println("i: " + i);
			String key = (String)keys.next();
			String val = null;
			try{
				JSONObject value = jsonobj.getJSONObject(key);
				parseJSONAllObjects(value,output);
			}catch(Exception e){
				val = jsonobj.getString(key);
			}
			if(val != null){
				output.put(key,val);
				System.out.println("Key: " + key + ", Val: " + val);
			}
			i++;
		}
		return output;
	}

	/**
	 * Gets all objects in the current world and creates classes of objects to save them to 
	 * E.g. all models are saved in the world, the models are a class which contain links, links are a class which contain
	 * collisions, collisions are a class which contain contacts. I'm not sure this is the most efficient way to do this
	 * but let's work with it and see where it gets us.
	 * 
	 * @param timestamp
	 */
	public void setWorldState(long timestamp){

		// echo for prolog
		System.out.println("IJavaDB: " + "Asserting world state from timestamp: " + timestamp + " :");

		// local models map
		World local_world = new World();		
		// query for getting the document at the given closest greater or equal than the timestamp
		BasicDBObject query = new BasicDBObject("timestamp", new BasicDBObject("$gte", timestamp));
		// get document at given timestamp
		DBObject doc = coll.findOne(query);

		//System.out.println(_doc);

		// extracts characters and tokens from given string
		JSONTokener tokener = new JSONTokener(doc.toString());
		try{
			// get the JSON root object
			JSONObject root_obj = new JSONObject(tokener);
			///Map<String,String> allkeys = new HashMap<String,String>();
			///parseJSONAllObjects(root_obj, allkeys);
			// get the models JSON array from the JSON root object
			JSONArray models_array = root_obj.getJSONArray("models");

			//loop through all the models array
			for(int i = 0; i < models_array.length(); i++)
			{
				// get the given JSON object from the array
				JSONObject curr_model_obj = models_array.getJSONObject(i);

				// create a local model with the given name from the JSON obj
				Model curr_model = new Model(curr_model_obj.getString("name"));				

				//				// echo for prolog
				//				System.out.println("\t" + curr_model.getName());

				// get the links JSON array from the current JSON obj 
				JSONArray links_array = models_array.getJSONObject(i).getJSONArray("links");
				// loop through all the links for the current model
				for(int j = 0; j < links_array.length(); j++)
				{
					// get the given JSON object from the array
					JSONObject curr_link_obj = links_array.getJSONObject(j);

					// create a local link with the given name from the JSON obj
					Link curr_link = new Link( curr_link_obj.getString("name"));
					// get the collision JSON array from the current JSON obj 
					JSONArray collisions_array = links_array.getJSONObject(j).getJSONArray("collisions");
					// loop through all the collisions for the current link
					for(int k = 0; k < collisions_array.length(); k++)
					{											
						// get the given JSON object from the array
						JSONObject curr_collision_obj = collisions_array.getJSONObject(k);

						// create a local model with the given name from the JSON obj
						Collision curr_collision = new Collision(curr_collision_obj.getString("name"));
						// get the contacts JSON array
						JSONArray contacts_array = collisions_array.getJSONObject(k).getJSONArray("contacts");

						// loop through all the contacts for the current collision
						for(int l = 0; l < contacts_array.length(); l++)
						{
							// get the given JSON object from the array
							JSONObject curr_contact_obj = contacts_array.getJSONObject(l);

							// create a local model with the given name fron the JSON obj
							Contact curr_contact = new Contact(curr_contact_obj.getString("name"));

							// echo for prolog
							System.out.println("\t\t\t\t" + curr_contact.getName());

							// add the current contact to the map, using the name as key value
							curr_collision.addContact(curr_contact.getName(), curr_contact);
						}
						// add the current collision to the map, using the name as key value
						curr_link.addCollision(curr_collision.getName(), curr_collision);					
					}
					// add the current link to the map, using the name as key value
					curr_model.addLink(curr_link.getName(), curr_link);	
				}
				// add current model to the world
				local_world.addModel(curr_model.getName(), curr_model);
			}
		} catch (JSONException e) {
			e.printStackTrace();
		}
		this.world = local_world;
	}

	/**
	 * This is a temporary function to cope with names until logging contact has been expanded. 
	 * I'm trying to get the event chains working and otherwise there are simply to many different collisions to get a sensible sparse matrix I think 
	 * 
	 * CALLED BY: 	addNewNode
	 * 				addNewEdge
	 * CALLS:		-
	 * 
	 * @param oldname
	 * @return
	 */
	public String replaceNames(String oldname)
	{
		if(nameMap.containsKey(oldname))
		{
			return nameMap.get(oldname);
		}
		return oldname;
	}

	/**
	 * Function to safely add a new vertex to the current graph: will do so if it does not yet exist, otherwise prints to let know it already exists.
	 * 
	 * CALLED BY:	constructGraph
	 * CALLS:		replaceNames
	 * 
	 * @param graph
	 * @param nodename
	 * @param time
	 * @return
	 */
	public void addNewNode(UndirectedGraph<String, DefaultEdge> graph, String nodename, long time)
	{
		//TODO this should not be necessary later? convert sphere_collisions. contact info on separate spheres is just too much, should be nicer workaround though
		String newname;
		if(nodename.contains("sphere_collision_")) 
			newname = "liquid_spheres";
		else
			newname = replaceNames(nodename);

		//add node to graph if the current collision is not already present in the graph.
		if(!graph.containsVertex(newname))
		{
			graph.addVertex(newname);
			//System.out.println(time + ": Added " + newname);
		}
	}

	/**
	 * Function to safely add a new edge to the current graph: will do so if it does not yet exist, otherwise prints to let know it already exists.
	 * 
	 * CALLED BY:	constructGraph
	 * CALLS:		replaceNames
	 * 
	 * @param graph
	 * @param node1
	 * @param node2
	 * @param time
	 */
	public void addNewEdge (UndirectedGraph<String, DefaultEdge> graph, String node1, String node2, long time)
	{
		//TODO this should not be necessary later? convert sphere_collisions. contact info on separate spheres is just too much, should be nicer workaround though
		String new1;
		if(node1.contains("sphere_collision_"))
			new1 = "liquid_spheres";
		else
			new1 = replaceNames(node1);
		String new2;
		if(node2.contains("sphere_collision_"))
			new2 = "liquid_spheres";
		else
			new2 = replaceNames(node2);

		//add edge to graph if it doesn't exist yet
		if(!graph.containsVertex(new1) || !graph.containsVertex(new2))
		{
			System.out.println("WARNING: one of the nodes doesn't exist yet. This should never occur in the current implementation!");
		}
		else if (!new1.equals(new2)) //cannot code for object(parts) touching themselves. With liquid spheres this will give problems because the undirectedGraph class doesn't allow selfconnections
		{
			graph.addEdge(new1, new2);
			//System.out.println(time + ": Added edge between " + new1 + " and " + new2);
		}
	}

	/**
	 * Function (recursive) for traversing two nested JSONArrays to the required level while keeping track of parents and 
	 * constructing an appropriate graph for each timestamp.
	 * 
	 * CALLED BY: 	contactEventsGraphs
	 * CALLS: 		addNewNode
	 * 				addNewEdge
	 * 
	 * @param graph_rep
	 * @param levelnames
	 * @param curar
	 * @param parents
	 * @param time
	 * @throws JSONException
	 */
	public void constructGraph(UndirectedGraph<String, DefaultEdge> graph_rep, String[] levelnames, JSONArray curar, Map<String,String> parents, long time) throws JSONException
	{
		if(levelnames.length==1) //we've traversed all the levels we needed
		{
			String collname = parents.get("collisions"); //TODO make summarize option so will actually just use modelnames, only possible when the model of a collision can be tracked down (Andrei has to change logging system)
			addNewNode(graph_rep, collname, time);

			for(int i=0; i<curar.length();i++)//go through all the contacts of the current collision
			{
				JSONObject contact_obj = curar.getJSONObject(i);
				String contactname = contact_obj.getString("name");
				addNewNode(graph_rep, contactname, time);
				addNewEdge(graph_rep, collname, contactname, time);
			}
		}
		else //keep going down levels. Know where to go because levelnames are given.
		{
			for(int i=0;i<curar.length();i++) 
			{
				JSONObject curobj = curar.getJSONObject(i);
				JSONArray nextar = curar.getJSONObject(i).getJSONArray(levelnames[1]); //get the field with the name that equals the second in the levelnames list (the first is the current level)
				//remove the first levelname in the array to pass to the next recursive call
				String[] updatedlevelnames = new String[levelnames.length-1];
				System.arraycopy(levelnames, 1, updatedlevelnames, 0, levelnames.length-1);
				//store current object in the parents list
				parents.put(levelnames[0], curobj.getString("name"));
				constructGraph(graph_rep, updatedlevelnames, nextar, parents, time);
			}
		}
	}

	/**
	 * Make a list of event graphs, one graph for each timestamp.
	 * 
	 * CALLED BY:	-
	 * CALLS:		constructGraph
	 * 
	 * @param summarize
	 * @throws JSONException
	 * @throws IOException
	 */
	public List<UndirectedGraph<String,DefaultEdge>> contactEventsGraphs(boolean summarize, List<Long> graphtimes) throws JSONException, IOException
	{
		//storing graphs
		List<UndirectedGraph<String,DefaultEdge>> allgraphs = new ArrayList<UndirectedGraph<String,DefaultEdge>>();
		//know which levels to traverse
		String[] levelnames = {"models","links","collisions","contacts"};

		BasicDBObject query = new BasicDBObject();
		BasicDBObject fields = new BasicDBObject("models.links.collisions.contacts.name", 1);
		fields.append("models.name", 1);
		fields.append("_id", 0);
		fields.append("timestamp", 1);
		fields.append("models.links.name", 1);
		fields.append("models.links.collisions.name", 1);

		DBCursor doccursor = coll.find(query,fields).limit(100);
		//DBCursor doccursor = coll.find(query,fields);
		//		int i = 0;
		try {
			while(doccursor.hasNext()) { //for all timestamps
				//get the next entry
				DBObject dbdoc = doccursor.next();
				//convert to JSON
				JSONTokener tokener = new JSONTokener(dbdoc.toString());
				JSONObject db_jsonobj = new JSONObject(tokener);
				JSONArray cur_allmods = db_jsonobj.getJSONArray("models");
				long currenttime = db_jsonobj.getLong("timestamp")/1000000;

				//setup variables to construct graph with
				UndirectedGraph<String, DefaultEdge> i_graph =
						new SimpleGraph<String, DefaultEdge>(DefaultEdge.class); //each timestamp will be represented by an undirected graph
				Map<String, String> parents = new HashMap<String, String>(); //keeps a list whose child the current collision is
				constructGraph(i_graph, levelnames, cur_allmods, parents, currenttime);
				//System.out.println(i_graph.toString());
				//store graph
				allgraphs.add(i_graph);
				graphtimes.add(currenttime);

				//				//setup variables for dotexporter
				//				StringNameProvider<String> p1=new StringNameProvider<String>();
				//				IntegerNameProvider<String> p2=new IntegerNameProvider<String>();
				//				StringEdgeNameProvider<DefaultEdge> p3 = new StringEdgeNameProvider<DefaultEdge>();
				//				//export graphs as dotfiles for visualization in graphviz, each file is approx. 0.5kB.
				//				DOTExporter<String, DefaultEdge> exporter = new DOTExporter<String,DefaultEdge>(p1, p2, p3);
				//				String targetDirectory = "/home/yfang/project_actSimG/printedgraphs/";
				//				new File(targetDirectory).mkdirs();
				//				exporter.export(new FileWriter(targetDirectory + "graph"+ i + ".dot"), i_graph);
				//				i++;
			}
		} finally {
			doccursor.close();
		}
		return allgraphs;
	}

	/**
	 * Makes a list of event graphs that signify a change in the graph structure. The first graph is stored, and so are
	 * graphs that are structurally different compared to the one from the previous timestamp.
	 * 
	 * CALLED BY:	-
	 * CALLS:		graphToEigenvalue
	 * 
	 * @param allgraphs
	 * @return
	 */
	public List<UndirectedGraph<String,DefaultEdge>> extractMainGraphs(List<UndirectedGraph<String,DefaultEdge>> allgraphs, List<Long> graphtimes, List<Long> importanttimes)
	{
		List<UndirectedGraph<String,DefaultEdge>> mainGraphs = new ArrayList<UndirectedGraph<String,DefaultEdge>>();

		UndirectedGraph<String,DefaultEdge> prev_graph = null;
		//convert graph to adjacency matrix, them compute eigenvalues and check with previous eigenvalues whether graphstructure has changed
		//if it changed, store graph. If it didn't, go to next one.
		int i = 0;
		for(UndirectedGraph<String,DefaultEdge> igraph : allgraphs)
		{
			if (prev_graph == null) //always add the first graph
			{
				mainGraphs.add(igraph);
				prev_graph = igraph;
				//				System.out.println("Storing " + i);
			}
			else
			{
				//TODO don't have to calculate previous eigenvalue per se since this should be already calculated at the last step. Can save some time here. Can't be bothered to do it right now.
				double[] prev_eig = graphToEigenvalue(prev_graph);
				double[] cur_eig = graphToEigenvalue(igraph);
				if(!Arrays.equals(prev_eig, cur_eig))
				{
					mainGraphs.add(igraph);
					importanttimes.add(graphtimes.get(i));
					//					System.out.println("Storing " + i);
				}
				prev_graph = igraph;
			}
			i++;
		}		
		return mainGraphs;
	}

	/**
	 * Construct semantic event chains, in which the relations between "objects" are coded for each important timestamp.
	 * Relations are keyed using an imported class Pair<A,B>, which is kind of equivalent to tuple (which don't exist in Java). I wanted to use an immutable type for the key, so Sets or Lists don't fit. Could've used ImmutableSet I found out later, but this is fine too I think.
	 * The value is then a double[] array which contains values for all timesteps. I chose to key by row because in the 
	 * compressed SEC, there are unequal amount of columns in each row, and a column is not meaningful anymore while a row is.
	 * 
	 * There are n(n-1)/2 number of relationships possible (because they are undirected). TODO: think about how to do this 
	 * when add spatial relations, since these ARE directed.
	 * 
	 * TODO NOTE: this function doesn't work properly yet if objects are created or destroyed!
	 * 
	 * CALLED BY:	-
	 * CALLS:		initializeSEC
	 * 
	 * @param main_graphs
	 * @return
	 */
	public Map<Pair<Integer, Integer>, List<Integer>> originalSEC(List<UndirectedGraph<String,DefaultEdge>> main_graphs)
	{
		//can't use int or double here because primitives are not supported by Generics.
		Map<Pair<Integer,Integer>, List<Integer>> SEC = initializeSEC(main_graphs);
		VertexNameProvider<String> nameProvider = new IntegerNameProvider<String>();
		//for every important graph/timestep
		for(int i = 0; i< main_graphs.size(); i++)
		{
			UndirectedGraph<String,DefaultEdge> igraph = main_graphs.get(i);
			// assign ids in vertex set iteration order. Checked and the order of numbers will be WRONG if don't do this first.
			for (String node : igraph.vertexSet()) 
			{
				nameProvider.getVertexName(node);
			}

			//these are the only non-zero elements in this column
			Set<DefaultEdge> edges = igraph.edgeSet(); 
			//Collection<String> test = Collections.unmodifiableSet(new Set<String>());
			for(DefaultEdge iedge : edges)
			{
				String from = igraph.getEdgeSource(iedge);
				String to = igraph.getEdgeTarget(iedge);
				//convert to numbers
				Integer fromNumber = Integer.parseInt(nameProvider.getVertexName(from));
				Integer toNumber = Integer.parseInt(nameProvider.getVertexName(to));

				//because Pairs are ordered, need to have the right order for the key to work. The way the map is initialized, the smaller number always comes first
				Pair<Integer, Integer> cur_row_index;
				if(fromNumber>toNumber)
				{
					cur_row_index = new Pair<Integer,Integer>(toNumber,fromNumber); 
				}
				else
				{
					cur_row_index = new Pair<Integer,Integer>(fromNumber, toNumber); 
				}
				Integer rel_value = new Integer(1); //TODO can later take this value from a variable to accommodate more types of relationships
				SEC.get(cur_row_index).set(i, rel_value);
			}
		}
		return SEC;
	}
	

	/**
	 * Construct derived semantic event chains. Instead of the current relation, it codes the change in relation.
	 * Since this is done by combining the numbers that code for the relation, I'm converting the Integer to a String to merge the two.
	 * Rows in which no change occurs over all timesteps are discarded. Note that instead of a regular hashmap a linked hashmap is used so that items are ordered according to insertion.
	 * 
	 * CALLED BY:	-
	 * CALLS:		-
	 * 
	 * @param originalSEC
	 * @return
	 */
	public Map<Pair<Integer, Integer>, List<String>> derivedSEC(Map<Pair<Integer, Integer>, List<Integer>> originalSEC)
	{
		Map<Pair<Integer,Integer>, List<String>> newSEC = new LinkedHashMap<Pair<Integer,Integer>, List<String>>();
		//make new change encoding per row (since that's one list)
		for(Map.Entry<Pair<Integer, Integer>, List<Integer>> irow_entry : originalSEC.entrySet())
		{
			//get the list of the current row
			List<Integer> curlist = irow_entry.getValue();
			//only need to add new list if there is more than 1 value, otherwise it means that nothing happens in this row and it should be skipped
			HashSet<Integer> uniqueSet = new HashSet<Integer>();
			uniqueSet.addAll(curlist);
			if(uniqueSet.size() > 1)
			{
				List<String> newlist = new ArrayList<String>();
				for(int i=1; i<curlist.size(); i++) //start with the second item, since need to know the change
				{
					Integer last = curlist.get(i-1);
					Integer cur  = curlist.get(i);
					String change = last.toString() + cur.toString();
					//add the new value to the new list
					newlist.add(change);
				}
				//add new list to the new map
				newSEC.put(irow_entry.getKey(), newlist);	
			}
		}
		return newSEC;
	}

	/**
	 * Construct compressed semantic event chains. All elements in which no change occurs are discarded.
	 * This results in loss of temporal information. Note that instead of a regular hashmap a linked hashmap is used so that items are ordered according to insertion.
	 * 
	 * CALLED BY:	-
	 * CALLS:		-
	 * 
	 * @param derivedSEC
	 * @return
	 */
	public Map<Pair<Integer, Integer>, List<String>> compressedSEC(Map<Pair<Integer, Integer>, List<String>> derivedSEC)
	{
		Map<Pair<Integer,Integer>, List<String>> newSEC = new LinkedHashMap<Pair<Integer,Integer>, List<String>>();
		//delete values from each list that are the same double digits (e.g. 00 or 11) to make compressedSEC
		for(Map.Entry<Pair<Integer, Integer>, List<String>> irow_entry : derivedSEC.entrySet())
		{
			//get the list of the current row
			List<String> curlist = irow_entry.getValue();
			List<String> newlist = new ArrayList<String>();
			for(String el : curlist) //start with the second item, since need to know the change
			{
				if(el.charAt(0)!= el.charAt(1)) //not same value
				{
					newlist.add(el);
				}
			}
			//add new list to the new map
			newSEC.put(irow_entry.getKey(), newlist);	
		}
		return newSEC;
	}

	public Double [][] spatialSimilarityMatrix(Map<Pair<Integer, Integer>, List<String>> compressed_SEC1, Map<Pair<Integer, Integer>, List<String>> compressed_SEC2)
	{
		Double[][] similarity_matrix = new Double[compressed_SEC1.size()][compressed_SEC2.size()];
		//Every row of SEC1 is compared to every row of SEC2 to find the highest similarity.
		int irow =0; //to keep track of where to store the similarity value in the matrix. 
		for(Map.Entry<Pair<Integer, Integer>, List<String>> irow_SEC1 : compressed_SEC1.entrySet())
		{
			int icolumn =0;
			for(Map.Entry<Pair<Integer, Integer>, List<String>> irow_SEC2 : compressed_SEC2.entrySet())
			{
				List<String> currow1 = irow_SEC1.getValue();
				List<String> currow2 = irow_SEC2.getValue();
				double max_similarity = 0; //maximum similarity for the comparison of the two rows
				if(currow1.size() > currow2.size())
				{					
					max_similarity = max_spatial_row_similarity(currow1, currow2);
				}
				else //currow2 is bigger or of equal size as currow1
				{
					max_similarity = max_spatial_row_similarity(currow2, currow1);
				}
				similarity_matrix[irow][icolumn]=max_similarity;
				icolumn++;
			}
			irow++;
		}
		return similarity_matrix;
	}

	/**
	 * Loops over rows recursively to find row/column correspondence. Splits off to multiple if more than one possibility in a row.
	 * Computes final spatial similarity values for all the possible correspondence choices if two columns have the same big value for one row. 
	 * The value is the average value of the highest similarities across rows of the similarity matrix.
	 * 
	 * WARNING: SEC1 (rows) should always be equal to or smaller than SEC2 in size! Otherwise the way to compute similarity doesn't really make sense.
	 * TODO decide what to do when number of rows not equal --> 1) ignore inequality and take the smaller one as the row for the correspondence, so the extra columns 
	 * will just be left over. 2) Take the largest one as row for correspondence and fill up remaining similarity values with 0 (equivalent to filling up
	 * with dummies. May make sense that actions that involve more objects changing are different and this should be reflected in the similarity value,
	 * but maybe unrelated things are going on at the same time --> how do we know? + a BIGGER issue is that just by chance the last object(s) will never be
	 * matched even if they actually correspond much better)
	 * For now implemented option 1 because that first came into mind. Can implement 2) later too and compare results if desirable.
	 * TODO this algorithm is not guaranteed to find the optimal solution for the correspondence problem: if an earlier row Did have a single corresponding
	 * highest column, a later row will never be able to associate with that column because it will never be available, even if the overall resulting similarity would be higher.
	 * TODO right now the whole process only works if the same SEC has equal or smaller number of rows AND equal or smaller number of columns compared to the other. Need
	 * the row constraint for this function and the column constraint for the temporal similarity matrix. If they are not the same, the order in the permutationlist will be false.
	 * I can solve this by changing the order in which I input the correspondence pairs (it's easy to check which SEC has fewer columns. I should do this in the wrap function and
	 * give it here as a boolean.
	 * 
	 * @param permutations
	 * @param similarityvalue
	 * @param spatial_sim_matrix_left
	 * @param SEC1labels_left
	 * @param SEC2labels_left
	 */
	public void similarityCorrespondence(List<List<Pair<String, String>>> permutations, List<Double> similarityvalues, List<Pair<String,String>> oneperm, Double tempsimsum, int nrows, Double[][] spatial_sim_matrix_left, List<String> SEC1labels_left, List<String> SEC2labels_left)
	{
		//stop condition
		if(SEC1labels_left.size()==0)//I don't know which is smaller, there may be objects left 
		{
			permutations.add(oneperm);
			similarityvalues.add(tempsimsum/nrows);
			System.out.println("Adding:\n"+oneperm.toString());
			System.out.println("Similarity value: " +tempsimsum/nrows);
		}
		else //there are still labels left to correspond
		{
			//Determine max value by sorting a copy of the row and looking at the end TODO Collections.sort(temprow); Double largestval = temprow[temprow.length-1];
			//need to first determine what the largest val is, and then go through row again to see which ones have the largest val
			Double[] temprow = spatial_sim_matrix_left[0];
			Double largestval = new Double(0);
			for(int icolumn=0; icolumn<temprow.length;icolumn++)
			{
				if(temprow[icolumn] > largestval)
				{
					largestval = temprow[icolumn];
				}
			}

			Double[] currow = spatial_sim_matrix_left[0];
			String rowind = SEC1labels_left.get(0); //we know there has to be Some correspondent to this row
			for(int icolumn=0; icolumn<currow.length;icolumn++)
			{
				Double curval =currow[icolumn];
				if(curval.equals(largestval))
				{
					System.out.println("current tempsum=" + tempsimsum);
					//which association does this largest val have?
					printArray(currow);
					System.out.println(SEC1labels_left.toString());
					System.out.println(SEC2labels_left.toString());
					String colind= SEC2labels_left.get(icolumn);
					Pair<String,String> curassoc = new Pair<String,String>(rowind, colind);
					//remove affected row and column, and then go on to the rest of the matrix
					Double[][] next_simmatrix = removeFromMatrix(spatial_sim_matrix_left, 0, icolumn);
					System.out.println("Mat:");
					printMatrix(next_simmatrix);
					System.out.println();
					List<String> newSEC1 = new ArrayList<String>(SEC1labels_left);
					newSEC1.remove(0);
					System.out.println("newSEC1: " + newSEC1.toString());
					List<String> newSEC2 = new ArrayList<String>(SEC2labels_left);
					newSEC2.remove(icolumn);
					System.out.println("newSEC2: " + newSEC2.toString());
					List<Pair<String,String>> addedPerm = new ArrayList<Pair<String,String>>(oneperm);
					addedPerm.add(curassoc);
					similarityCorrespondence(permutations, similarityvalues, addedPerm, tempsimsum+largestval, nrows, next_simmatrix, newSEC1, newSEC2);
				}				
			}
		}
	}

	/**
	 * Wrap for similarityCorrespondence function. Initializes variables and prints out results.
	 * In addition it will check whether the final similarity value for the different permutations are also the same and get rid of the permutations
	 * that in the end turn out to be suboptimal. (it may occur that for a specific row there are multiple, equally valid options, but then
	 * for later rows it turns out that a certain pick for the earlier row would have been better). TODO right now just returns all the permutations. 
	 * 
	 * TODO the authors of the SEC paper neglect the case where two rows may have the highest fit with the same column. If we don't check
	 * which assignments have already been taken, this would lead to a higher similarity than is actually the case. 
	 * Unless we want to determine the similarity of actions using different objects (cup vs bowl or something like that), we don't have a problem
	 * matching it. But that might be nice to be able to do so I'll implement it here so that if there are equally largest values in one row, we
	 * will compute the similarity for both possibilities. If there is more than one permutation with equal final similarity value, will return
	 * multiple permutation assignment lists.
	 * 
	 * @param spatial_sim_matrix
	 * @return 
	 * @return
	 */
	public SimTotalResults spatialSimilarityValue(Double[][] spatial_sim_matrix, Set<Pair<Integer, Integer>> SEC1labels, Set<Pair<Integer, Integer>> SEC2labels)
	{
		//nrows needs to be given as a constant because the recursive function will lose the rows in recursion
		int nrows = spatial_sim_matrix.length;
		//variables that need to be given to recurse on
		List<String> SEC1labels_string = setToString(SEC1labels);
		List<String> SEC2labels_string = setToString(SEC2labels);
		List<Pair<String,String>> oneperm = new ArrayList<Pair<String,String>>();

		//variables for storing results
		List<List<Pair<String, String>>> permutations_res = new ArrayList<List<Pair<String,String>>>();
		List<Double> similarityval_res = new ArrayList<Double>();
		similarityCorrespondence(permutations_res, similarityval_res, oneperm, 0.0, nrows, spatial_sim_matrix, SEC1labels_string, SEC2labels_string);

		System.out.println("permutationres: " +permutations_res.toString());
		System.out.println("similarityres: " + similarityval_res.toString());
		//it may be that some of the permutations eventually lead to a lower similarity value, so take these out
		Double maxval = new Double(0);
		for(int i=0; i<similarityval_res.size(); i++)
		{
			if(similarityval_res.get(i)>maxval)
			{
				maxval = similarityval_res.get(i);
			}
		}
		for(int i=similarityval_res.size()-1; i>=0; i--)//doing it the other way around to not mess up the indexes when removing something
		{
			if(similarityval_res.get(i) < maxval)
			{
				System.out.println("Removing value " + similarityval_res.get(i) + "\nPermutation " + permutations_res.get(i));
				permutations_res.remove(i);
				similarityval_res.remove(i);
			}
		}
		return new SimTotalResults(permutations_res, similarityval_res);
	}



	public Double[][] temporalSimilarityMatrix(Map<Long, List<String>> tSEC1, Map<Long, List<String>> tSEC2, int ncols1, int ncols2)
	{
		//compare all columns with all columns to construct similarity matrix. No shuffling needed.
		Double[][] similarity_matrix = new Double[ncols1][ncols2];

		int irow =0; //to keep track of where to store the similarity value in the matrix. 
		for(Map.Entry<Long, List<String>> icol_SEC1 : tSEC1.entrySet()) //a of theta
		{
			int icolumn =0;
			for(Map.Entry<Long, List<String>> icol_SEC2 : tSEC2.entrySet()) //b of theta
			{
				//take the two current SEC columns and calculate the similarity value for the current cell of the matrix for that
				similarity_matrix[irow][icolumn] = single_temporal_similarity(icol_SEC1.getValue(), icol_SEC2.getValue());
				icolumn++;
			}
			irow++;
		}
		return similarity_matrix;
	}

	/**
	 * TODO NOT DONE. meant to write a simpler function that actually does what the authors of the paper do, but for now I'll use the similarityCorrespondence,
	 * we can discuss the details at a later point.
	 * 
	 * @param temporalSimMatrix
	 * @return
	 */
	public double temporalSimilaritySum(Double[][] temporalSimMatrix)
	{
		double sum=0;
		//TODO unclear in paper what it conceptually means 
		for(int i=0; i<temporalSimMatrix.length; i++)
		{

		}
		return sum/temporalSimMatrix.length;
	}

	/**
	 * NOTE: the input to this function should be such that SEC1 has less columns than SEC2. 
	 * 
	 * If number of rows are not equal, fill up with dummy rows. (use value 0, because in a derived matrix all numbers will have at least two digits, so it won't be similar to anything).
	 * 
	 * @param derived_SEC1
	 * @param derived_SEC2
	 * @param permutations
	 */
	public void temporalSimilarityValue(Map<Pair<Integer, Integer>, List<String>> derived_SEC1, Map<Pair<Integer, Integer>, List<String>> derived_SEC2, SimTotalResults perm_res, List<Long> importanttimes1, List<Long> importanttimes2)
	{
		//get the possible permutations from spatial similarity perspective
		List<List<Pair<String, String>>> permutations = perm_res.getPerm(); 
		
		//initialize variables for storing results
		Double max_sim = new Double(0);
		List<List<Pair<String, String>>> max_spat_permutation = new ArrayList<List<Pair<String,String>>>(); //for storing exactly which permutations lead to the given similarity value
		List<List<Pair<String, String>>> max_temp_permutation = new ArrayList<List<Pair<String,String>>>();

		//how many rows is the max? need to fill smaller matrix with dummies
		int max_rows = 0;
		if (derived_SEC1.size()>derived_SEC2.size())
			max_rows = derived_SEC1.size();
		else
			max_rows = derived_SEC2.size();

		//produce similarity matrices for all permutations
		for(int iperm=0; iperm < permutations.size(); iperm++)
		{
			//First create a SEC2 that is ordered such that the rows correspond according to the current permutation
			Map<Pair<Integer, Integer>, List<String>> ordered_SEC2 = new LinkedHashMap<Pair<Integer, Integer>, List<String>>(); //will need to create a new LinkedHashMap for different permutations because LinkedHashMaps like SEC2 cannot be reordered
			//Assume SEC1 will remain unchanged while we swap around rows in SEC2 to match
			int ncols1 = 0;
			int ncols2 = 0;
			for(Map.Entry<Pair<Integer, Integer>, List<String>> irow_SEC1 : derived_SEC1.entrySet())
			{
				Pair<Integer,Integer> correspondingkey = correspondingRelation(permutations.get(iperm), irow_SEC1.getKey()); //which objectrelation in SEC2 corresponds to this key?
				//put the right key and value into the new SEC2. Given that we're going through the ordered SEC1 one by one, the order of SEC2 is also automatically right.
				ordered_SEC2.put(correspondingkey, derived_SEC2.get(correspondingkey));
				//get the number of columns. unfortunately doing it here because I can only get a list by key...
				ncols1 = irow_SEC1.getValue().size();
				ncols2 = derived_SEC2.get(correspondingkey).size();
			}
			//Because need to go through all the columns instead of rows now, need to have columns in lists, transpose.
			Map<Long, List<String>> tSEC1 = transposeSEC(derived_SEC1, max_rows, importanttimes1);
			Map<Long, List<String>> tSEC2 = transposeSEC(ordered_SEC2, max_rows, importanttimes2);

			//compare all columns with all columns to construct similarity matrix. No shuffling needed.
			Double[][] similarity_matrix = temporalSimilarityMatrix(tSEC1, tSEC2, ncols1, ncols2);

			//compute final similarity value and correspondences
			//nrows needs to be given as a constant because the recursive function will lose the rows in recursion
			int nrows = similarity_matrix.length;
			//variables that need to be given to recurse on
			List<String> SEC1labels_string = setToString(tSEC1.keySet());
			List<String> SEC2labels_string = setToString(tSEC2.keySet());
			List<Pair<String,String>> oneperm = new ArrayList<Pair<String,String>>();

			//variables for storing results
			List<List<Pair<String, String>>> permutations_res = new ArrayList<List<Pair<String,String>>>();
			List<Double> similarityval_res = new ArrayList<Double>();
			similarityCorrespondence(permutations_res, similarityval_res, oneperm, 0.0, nrows, similarity_matrix, SEC1labels_string, SEC2labels_string);
			System.out.println("all permutationres temporal: " +permutations_res.toString());
			System.out.println("all similarityres temporal: " + similarityval_res.toString());
			//compare with previous stored value: if it's greater, throw out all the other ones, if it's the same, add to the list, if it's smaller, ignore
			for(int j=0; j<similarityval_res.size(); j++) //the values in similarityval_res are not perse all equally large, so can't test them as a group, need to loop over
			{
				int compareval = similarityval_res.get(j).compareTo(max_sim);
				if(compareval == 0) //if no previously stored value yet, will only be equal if the similarity is actually 0. If the highest similarity IS 0, maybe still want the permutation list no? so that's why it's not a special case here
				{
					max_temp_permutation.add(permutations_res.get(j)); //add current permutation, max_sim stays the same
					if(!max_spat_permutation.contains(permutations.get(iperm))) //has the current permutation been added to this? TODO probably more efficient using Set
					{
						max_spat_permutation.add(permutations.get(iperm));
					}
				}
				else if(compareval > 0) //similarityval_res(j) is larger than max_sim
				{
					//replace max_sim
					max_sim = similarityval_res.get(j);
					//replace permutation list
					max_temp_permutation = new ArrayList<List<Pair<String,String>>>();
					max_temp_permutation.add(permutations_res.get(j));
					max_spat_permutation = new ArrayList<List<Pair<String,String>>>();
					max_spat_permutation.add(permutations.get(iperm));
				}
				//if similarityval_res(j) smaller than max_sim, nothing happens
			}
		}
		//print out results
		System.out.println("maximum similarity: " + max_sim);
		System.out.println("for spatial permutations: " + max_spat_permutation.toString());
		System.out.println("with temporal permutations: " + max_temp_permutation.toString());
	}
	
	/**
	 * Return a map with columns as lists and the key will be the timestamp
	 * In addition it fills the SEC up to a number of rows with dummies if there are not enough rows
	 * 
	 * TESTED
	 * 
	 * @param SEC
	 * @param min_nrows
	 * @param importanttimes
	 * @return
	 */
	public Map<Long, List<String>> transposeSEC (Map<Pair<Integer, Integer>, List<String>> SEC, int min_nrows, List<Long> importanttimes)
	{
		//make transposed SEC
		Map<Long, List<String>> newSEC = new LinkedHashMap<Long, List<String>>();
		//go through all the lists in the map
		for(Map.Entry<Pair<Integer, Integer>, List<String>> irow_SEC : SEC.entrySet())
		{
			//go through each element in the list
			List<String> currow = irow_SEC.getValue();
			for(int i =0; i< currow.size(); i++)
			{
				//if the key doesn't exist yet (this is the first row you go through), create one, otherwise only add to the list associated with that key
				if(newSEC.containsKey(importanttimes.get(i)))
				{
					newSEC.get(importanttimes.get(i)).add(currow.get(i));
				}
				else
				{
					List<String> newcolumn = new ArrayList<String>();
					newcolumn.add(currow.get(i));
					newSEC.put(importanttimes.get(i), newcolumn);
				}
			}
		}
		//determine whether need to add dummy rows
		int ndummies = min_nrows-SEC.size();
		//add dummies to rows (note that the rows of this matrix used to be the columns of the other one)
		for(Map.Entry<Long, List<String>> icol : newSEC.entrySet())
		{
			for(int i=0; i<ndummies; i++)
			{
				icol.getValue().add("0");
			}
		}
		return newSEC;
	}


	/**
	 * Given a specific key and permutation, it will return the corresponding relation to the key as indicated within the permutation.
	 * In addition the permutation contains keys as Strings, while the given key is a Pair. Converts latter to String to match.
	 * 
	 * CALLED BY: 	temporalSimilarityValue
	 * CALLS:		-
	 * 
	 * @param permutation
	 * @param key
	 * @return
	 */
	public Pair<Integer,Integer> correspondingRelation(List<Pair<String, String>> permutation, Pair<Integer, Integer> key)
	{
		Pattern p = Pattern.compile("\\d+");
		String keystr = key.toString();
		for(Pair<String,String> ipair: permutation)
		{
			if(ipair.getValue0().equals(keystr)) //if the given key equals the current first value of the pair
			{
				String valstr = ipair.getValue1(); //this is the string we're looking to convert to pair
				List<String> valstrpairs = new ArrayList<String>();
				//use regex to find the numbers
				Matcher m = p.matcher(valstr); 
				while (m.find()) {
				   valstrpairs.add(m.group());
				}
				if(valstrpairs.size() != 2)
				{
					System.out.println("ERROR: number of integers found in the valstr of correspondingRelation is not 2!");
				}
				Integer val1 = Integer.parseInt(valstrpairs.get(0));
				Integer val2 = Integer.parseInt(valstrpairs.get(1));
				//create a pair from the string
				Pair<Integer,Integer> result = new Pair<Integer,Integer>(val1,val2);
				return result;
			}
		}
		System.out.println("WARNING: no match was found for " + keystr);
		return null; //this means no match was found, this means that there will be a dummy row inserted 
	}

	/**
	 * Converts a list of Pairs to a list of Strings
	 * 
	 * CALLED BY:	spatialSimilarityValue (with Pair set)
	 * 				temporalSimilarityValue (with Long set)
	 * CALLS:		-
	 * 
	 * @param pair
	 * @return
	 */
	public <T> List<String> setToString(Set<T> pair)
	{
		List<String> result= new ArrayList<String>();
		for(T item : pair)
		{
			result.add(item.toString());
		}
		return result;
	}

	/**
	 * Help function for spatialSimilarityMatrix. Computes the maximum similarity between two rows (of different SECs),
	 * Assumes currow1 >= currow2.
	 * 
	 * @param currow1
	 * @param currow2
	 * @return
	 */
	private double max_spatial_row_similarity(List<String> currow1, List<String> currow2)
	{
		double max_similarity = 0;
		for(int i=0; i<currow1.size()-currow2.size()+1; i++) //this is how many times we have to compare. If they're the same length, only i=0 will be compared
		{
			int sum = 0; //keeps track of the sum for the similarity of the current shift
			for(int j=0; j<currow2.size(); j++)
			{
				String el2 = currow2.get(j);
				String el1 = currow1.get(j+i);
				if(el2.equals(el1)) //add 1 to the sum if they're the same
				{
					sum++;
				}
			}
			double ft = (100.0/currow1.size()) * sum; //similarity for the current shift
			if(ft > max_similarity)
			{
				max_similarity = ft;
			}
		}
		return max_similarity;
	}

	/**
	 * Help function for temporalSimilarityMatrix. Computes the similarity of one column (of different SECs).
	 * Assumes curcol1.size == curcol2.size (because was filled with dummies if necessary)
	 * 
	 * @param curcol1
	 * @param curcol2
	 * @return
	 */
	private double single_temporal_similarity(List<String> curcol1, List<String> curcol2)
	{
		int sum = 0;
		for(int i=0; i<curcol1.size(); i++) //both columns should be of the same size
		{
			String el1 = curcol1.get(i);
			String el2 = curcol2.get(i);
			if(el2.equals(el1))
			{
				sum++;
			}
		}
		double theta_ab = (100.0/curcol1.size()) * sum;
		return theta_ab;
	}

	/**
	 * Takes a list of main_graphs to get the number of rows and columns required and returns an originalSEC map with values set to 0.
	 * 
	 * TODO what to do if an object didn't exist yet or ceases to exist?! --> need to fill rows with 9's once encountered, or with 9 once not present anymore.
	 * 
	 * CALLED BY:	originalSEC
	 * CALLS:		-
	 * 
	 * @param main_graphs
	 * @return
	 */
	public Map<Pair<Integer,Integer>, List<Integer>> initializeSEC (List<UndirectedGraph<String,DefaultEdge>> main_graphs)
	{
		Map<Pair<Integer,Integer>, List<Integer>> SEC = new LinkedHashMap<Pair<Integer,Integer>, List<Integer>>();
		VertexNameProvider<String> nameProvider = new IntegerNameProvider<String>();
		//for every column
		for(UndirectedGraph<String,DefaultEdge> igraph : main_graphs)
		{
			int nobjects = igraph.vertexSet().size();
			System.out.println("nobjects: " + nobjects);
			for (String node : igraph.vertexSet()) 
			{
				nameProvider.getVertexName(node);
			}
			//for every row
			for(String node : igraph.vertexSet())
			{
				Integer current = Integer.parseInt(nameProvider.getVertexName(node));
				for(int i = current+1; i <= nobjects; i++)
				{
					Pair<Integer, Integer> cur_row_index = new Pair<Integer,Integer>(current, i);
					//if this is the first column, need to initialize the lists
					if(!SEC.containsKey(cur_row_index))
					{
						SEC.put(cur_row_index, new ArrayList<Integer>());

					}
					//add 0 to the current row
					SEC.get(cur_row_index).add(new Integer(0)); 
				}
			}
		}
		return SEC;
	}

	/**
	 * Takes a matrix and returns it with a specific row and column removed
	 * If no row or column should be removed, call with that set to -1
	 *  
	 * @param matrix
	 * @param rm_row
	 * @param rm_column
	 * @return 
	 */
	public Double[][] removeFromMatrix(Double[][] matrix, int rm_row, int rm_column)
	{
		int nrows = matrix.length;
		int ncolumns = matrix[0].length;
		//determine dimensions of the new matrix
		int newnr;
		int newnc;
		if(rm_row < 0)
			newnr=nrows;
		else
			newnr=nrows-1;
		if(rm_column < 0)
			newnc=ncolumns;
		else
			newnc=ncolumns-1;

		Double[][] newmatrix = new Double[newnr][newnc];
		int newi=0; //keep track of indexes of new matrix
		for(int i=0; i<nrows; i++)
		{
			if(i==rm_row) //skip this row in copying
			{
				continue;
			}
			int newj=0;
			for(int j=0; j<ncolumns;j++)
			{
				if(j==rm_column)//skip this column in copying
				{
					continue;
				}
				newmatrix[newi][newj]=matrix[i][j];
				newj++;
			}
			newi++;
		}
		return newmatrix;
	}

	/**
	 * Compute Eigenvalues of adjacency matrix of a given even graph
	 * 
	 * CALLED BY:	extractMainGraphs
	 * CALLS:		-
	 * 
	 * @param graph
	 * @return
	 */
	public double[] graphToEigenvalue(UndirectedGraph<String,DefaultEdge> graph)
	{
		double[][] matrix = adjacencyMatrix(graph);
		Matrix i_mat = new Matrix(matrix);
		EigenvalueDecomposition eig_mat = i_mat.eig();
		//printMatrix(matrix);
		double[] eigenvalues = eig_mat.getRealEigenvalues();
		//printArray(eigenvalues);
		return eigenvalues;
	}

	/**
	 * Adapted function from MatrixExporter of the jgrapht library to output full adjacency matrix to a double[][] array
	 * 
	 * @param graph
	 * @return
	 */
	public double[][] adjacencyMatrix(UndirectedGraph<String, DefaultEdge> graph)
	{
		VertexNameProvider<String> nameProvider = new IntegerNameProvider<String>();
		for (String from : graph.vertexSet()) {
			// assign ids in vertex set iteration order --> same as printed in .dot file.
			nameProvider.getVertexName(from);
		}
		int matrix_size = graph.vertexSet().size();
		double[][] matrix = new double[matrix_size][matrix_size];
		for (String from : graph.vertexSet()) {
			//for each vertex in the set, get the adjacency values
			adjacencyMatrixVertex(nameProvider, from, Graphs.neighborListOf(graph, from), matrix, graph);
		}
		return matrix;
		//printMatrix(matrix);
	}

	/**
	 * Adapted function from MatrixExporter of the jgrapht library to output full adjacency matrix to a double[][] array
	 * 
	 * @param nameProvider
	 * @param from
	 * @param neighbors
	 * @param matrix
	 * @param graph
	 */
	private void adjacencyMatrixVertex(VertexNameProvider<String> nameProvider, String from, List<String> neighbors, double[][] matrix, UndirectedGraph<String, DefaultEdge> graph)
	{
		int fromName = Integer.parseInt(nameProvider.getVertexName(from));
		//System.out.println(from + ": " + fromName); //check that the numbers and names here are the same as in the .dot file
		for (String to : neighbors) //for each neighbor of the node "from"
		{
			//TODO once we have functions for determining spacial relations like above, rightof and things like that, can insert as a different number into the matrix
			int toName = Integer.parseInt(nameProvider.getVertexName(to));
			matrix[fromName-1][toName-1] = 1;
			matrix[toName-1][fromName-1] = 1;
		}
	}

	/**
	 * For printing SECs in a matrix like form
	 * 
	 * @param SEC
	 */
	public <S,T,V> void printSEC(Map<S, List<T>> SEC, V times) 
	{
		if(times!=null)
		{
			System.out.println("\t" + times.toString());
		}
		for(Map.Entry<S, List<T>> irow_entry : SEC.entrySet())
		{
			System.out.print(irow_entry.getKey().toString() + "\t");
			System.out.println(irow_entry.getValue().toString());
		}
	}

	/**
	 * print a primitive two-dimensional matrix
	 * 
	 * @param m
	 */
	public <T> void printMatrix(T[][] m)
	{
		//In Java, 2D arrays are really arrays of arrays with possibly different lengths (there are no guarantees that in 2D arrays 
		//that the 2nd dimension arrays all be the same length)
		//You can get the length of any 2nd dimension array as z[n].length where 0 <= n < z.length.
		//If you're treating your 2D array as a matrix, you can simply get z.length and z[0].length, but note that you might be 
		//making an assumption that for each array in the 2nd dimension that the length is the same.
		for(int i=0; i<m.length;i++)
		{
			for(int j=0; j<m[0].length;j++)
			{
				System.out.print(m[i][j]+ " ");
			}
			System.out.println();
		}
	}

	/**
	 * print a primitive array
	 * 
	 * @param a
	 */
	public <T> void printArray(T[] a)
	{
		for(int i=0; i<a.length;i++)
			System.out.print(a[i]+ " ");
		System.out.println();
	}




	/**
	 * 
	 */
	public String[] getModelNames()
	{	
		//prolog shell output
		System.out.println("getModelNames called.");

		// check if world is initialized, if not, get the world at the initial timestamp (this will populate the world with models, links, collisions and contacts)
		if (this.world == null)
		{
			this.setWorldState(A_TIMESTAMP);
		}
		// get the models map
		HashMap<String, Model> models = (HashMap<String, Model>) this.world.getModels();
		// a string array with all the model names
		String[] model_names = models.keySet().toArray(new String[models.size()]);
		return model_names;
	}

	/**
	 * 
	 * @param model_name
	 * @return
	 */
	public String[] getLinkNames(String model_name)
	{
		// check if world is initialized, if not, get the world at the initial timestamp
		if (this.world == null)
		{
			this.setWorldState(A_TIMESTAMP);
		}
		//get the links map
		HashMap<String, Link> links = (HashMap<String, Link>) this.world.getModels().get(model_name).getLinks();
		// a string array with all the link names
		String[] link_names = links.keySet().toArray(new String[links.size()]);
		return link_names;
	}

	/**
	 * 
	 * @param model_name
	 * @param link_name
	 * @return
	 */
	public String[] getCollisionNames(String model_name, String link_name)
	{
		// check if world is initialized, if not, get the world at the initial timestamp
		if (this.world == null)
		{
			System.out.println("settin world to " + A_TIMESTAMP);
			this.setWorldState(A_TIMESTAMP);
		}

		HashMap<String, Collision> collisions =	(HashMap<String, Collision>) 
				this.world.getModels().get(model_name).getLinks().get(link_name).getCollisions();

		// a string array with all the collision names
		String[] collision_names = collisions.keySet().toArray(new String[collisions.size()]);
		return collision_names;
	}

	/**
	 * 
	 * @param model_name
	 * @param link_name
	 * @param collision_name
	 * @return
	 */
	public String[] getContactNames(String model_name, String link_name, String collision_name)
	{
		// check if world is initialized, if not, get the world at the initial timestamp
		if (this.world == null)
		{
			System.out.println("settin world to " + A_TIMESTAMP);
			this.setWorldState(A_TIMESTAMP);
		}

		HashMap<String, Contact> contacts = (HashMap<String, Contact>)
				this.world.getModels().get(model_name).getLinks().get(link_name).getCollisions().get(collision_name).getContacts();

		// a string array with all the contact names
		String[] contact_names = contacts.keySet().toArray(new String[contacts.size()]);
		return contact_names;
	}

	/**
	 * Get the names of the contacts of a specific collision of a specific link of a specific model at a specific time
	 * @param model_name
	 * @param link_name
	 * @param collision_name
	 * @return
	 */
	public String[] getContactNames(String model_name, String link_name, String collision_name, double timestamp)
	{
		// check if world is initialized, if not, get the world at the initial timestamp
		if (this.world == null)
		{
			System.out.println("settin world to " + A_TIMESTAMP);
			this.setWorldState(A_TIMESTAMP);
		}

		HashMap<String, Contact> contacts = (HashMap<String, Contact>)
				this.world.getModels().get(model_name).getLinks().get(link_name).getCollisions().get(collision_name).getContacts();

		// a string array with all the contact names
		String[] contact_names = contacts.keySet().toArray(new String[contacts.size()]);
		return contact_names;
	}

	/**
	 * TODO NOW there are no contact duplicates, even if from different collisions
	 * TODO it returns the collisions regarding the model as a whole
	 * @param model_name
	 * @param timestamp
	 * @return
	 */
	public String[] getModelContactNames(String model_name, long timestamp){
		List<String> contacts_list = new ArrayList<String>(); 

		// query for getting the document at the given closest greater or equal than the timestamp
		BasicDBObject query = new BasicDBObject("timestamp", new BasicDBObject("$gte", timestamp));	

		// fields for projecting only the contact names
		BasicDBObject fields = new BasicDBObject("_id", 0);
		fields.append("models", new BasicDBObject("$elemMatch", new BasicDBObject("name", model_name)));
		fields.append("models.links.collisions.contacts.name", 1);

		// find the first document for the query (it should only be one)
		DBObject first_doc = coll.findOne(query, fields);

		// check that the query returned a document
		if(first_doc == null)
		{
			return null;
		}
		// extracts characters and tokens from given string
		JSONTokener tokener = new JSONTokener(first_doc.toString());
		try {
			// get the JSON root object
			JSONObject root_obj = new JSONObject(tokener);
			// get the models array (is only one value but of type array)
			JSONArray models_array = root_obj.getJSONArray("models");
			// get the links array (need to check all links for collisions)
			JSONArray links_array = models_array.getJSONObject(0).getJSONArray("links");
			// loop through all the links
			for (int i = 0; i < links_array.length(); i++){
				// get collision array from the current link
				JSONArray collisions_array = links_array.getJSONObject(i).getJSONArray("collisions");
				// loop through all collisions
				for (int j = 0; j < collisions_array.length(); j++){
					// get contacts of the current collision
					JSONArray contacts_array = collisions_array.getJSONObject(j).getJSONArray("contacts");
					// loop through contacts and add then to the list (if not already there, no duplicates)
					for (int k = 0; k < contacts_array.length(); k++){
						// get the contact name
						String contact_name = contacts_array.getJSONObject(k).getString("name");
						// add the contact to the list only if it is not already present
						if (!contacts_list.contains(contact_name)){
							contacts_list.add(contact_name);
						}
					}
				}
			}
		} catch (JSONException e) {
			e.printStackTrace();
		}	
		// return the list as an array of strings
		return contacts_list.toArray(new String[contacts_list.size()]);
	}

	/**
	 * 
	 * 
	 * @param model_name
	 * @param timestamp
	 * @return
	 */
	public double[] getModelPose(String model_name, long timestamp)
	{
		// echo for prolog
		//		System.out.println("IJavaDB: getting models '" + model_name + "' pose at timestamp: " + timestamp);

		// pose, X Y Z R P Y
		double[] pose = new double[6];

		//long timestamp = A_TIMESTAMP;

		// query for getting the document at the given closest greater or equal than the timestamp
		BasicDBObject query = new BasicDBObject("timestamp", new BasicDBObject("$gte", timestamp));
		//BasicDBObject query = new BasicDBObject("timestamp", timestamp).append("models", new BasicDBObject("$exists","true"));

		// fields for projecting only the pose of the given model name
		BasicDBObject fields = new BasicDBObject("_id", 0);
		fields.append("models", new BasicDBObject("$elemMatch", new BasicDBObject("name", model_name)));
		fields.append("models.pos", 1);
		fields.append("models.rot", 1);

		// find the first document for the query (it should only be one)
		DBObject first_doc = coll.findOne(query, fields);
		// check that the query returned a document
		if(first_doc == null)
		{
			System.out.println("getModelPose: could not find position of " + model_name + " for the specified timestamp " + timestamp);
			return null;
		}

		// extracts characters and tokens from given string
		JSONTokener tokener = new JSONTokener(first_doc.toString());
		try {
			// get the JSON root object
			JSONObject root_obj = new JSONObject(tokener);
			// get the models array (is only one value but of type array)
			JSONArray models_array = root_obj.getJSONArray("models");
			// get the position from the array
			JSONObject json_pos = models_array.getJSONObject(0).getJSONObject("pos");
			// get the orientation from the array
			JSONObject json_rot = models_array.getJSONObject(0).getJSONObject("rot");

			// set the pose
			pose[0] = json_pos.getDouble("x");
			System.out.println("real value: " + json_pos.getDouble("x"));
			pose[1] = json_pos.getDouble("y");
			pose[2] = json_pos.getDouble("z");

			pose[3] = json_rot.getDouble("x");
			pose[4] = json_rot.getDouble("y");
			pose[5] = json_rot.getDouble("z");
		} catch (JSONException e) {
			e.printStackTrace();
		}
		return pose;
	}

	/**
	 * 
	 * @param link_name
	 * @param timestamp
	 * @return
	 */
	public double[] getLinkPose(String link_name, long timestamp)
	{
		// echo for prolog
		//		System.out.println("IJavaDB: " + "getting link '" + link_name + "' pose at timestamp: " + timestamp);

		// pose, X Y Z R P Y
		double[] pose = new double[6];

		//long timestamp = A_TIMESTAMP;

		// query for getting the document at the given timestamp
		BasicDBObject query = new BasicDBObject("timestamp", new BasicDBObject("$gte", timestamp));		

		// fields for projecting all the links from the model sice there is no nested elemMatch function
		// we need to loop then through all the returned links until we find the proper one
		BasicDBObject fields = new BasicDBObject("_id", 0);
		fields.append("models.links", 1);
		fields.append("models", new BasicDBObject("$elemMatch", new BasicDBObject("links.name", link_name)));

		// find the first document for the query (it should only be one)
		DBObject first_doc = coll.findOne(query, fields);

		// check that the query returned a document
		if(first_doc == null)
		{
			// echo for prolog
			//			System.out.println("IJavaDB: timestamp out of bounds");
			return null;
		}

		// extracts characters and tokens from given string
		JSONTokener tokener = new JSONTokener(first_doc.toString());

		try {
			// get the JSON root object
			JSONObject root_obj = new JSONObject(tokener);

			// get the models array (is only one value but of type array)
			JSONArray models_array = root_obj.getJSONArray("models");			

			// get the links array from the result
			JSONArray links_array = models_array.getJSONObject(0).getJSONArray("links");

			//////////////////////////////////////////////////////////
			// loop through all the links to check for the given link name
			for(int i = 0; i < links_array.length(); i++)
			{
				// get the given JSON object from the array
				JSONObject curr_link_obj = links_array.getJSONObject(i);

				// check if the current links is the one we are looking for
				if(curr_link_obj.getString("name").equals(link_name))
				{
					// get the position from the array
					JSONObject json_pos = curr_link_obj.getJSONObject("pos");

					// get the orientation from the array
					JSONObject json_rot = curr_link_obj.getJSONObject("rot");

					// set the pose
					pose[0] = json_pos.getDouble("x");
					pose[1] = json_pos.getDouble("y");
					pose[2] = json_pos.getDouble("z");

					pose[3] = json_rot.getDouble("x");
					pose[4] = json_rot.getDouble("y");
					pose[5] = json_rot.getDouble("z");

					return pose;
				};
			}
		} catch (JSONException e) {
			e.printStackTrace();
		}	
		return null;
	}

	/**
	 * 
	 * @param model_name
	 * @param timestamp
	 * @return
	 */
	public double[] getModelBoundingBox(String model_name, long timestamp)
	{
		// echo for prolog
		//		System.out.println("IJavaDB: " + "calling  '" + model_name + "' boundingbox at timestamp: " + timestamp);

		// BB min XYZ, max XYZ
		double[] bounding_box = new double[6];

		// query for getting the document at the given closest greater or equal than the timestamp
		BasicDBObject query = new BasicDBObject("timestamp", new BasicDBObject("$gte", timestamp));	

		// fields for projecting only the pose of the given model name
		BasicDBObject fields = new BasicDBObject("_id", 0);
		fields.append("models", new BasicDBObject("$elemMatch", new BasicDBObject("name", model_name)));
		fields.append("models.bbox.min", 1);
		fields.append("models.bbox.max", 1);

		// find the first document for the query (it should only be one)
		DBObject first_doc = coll.findOne(query, fields);

		// check that the query returned a document
		if(first_doc == null)
		{
			// echo for prolog
			//			System.out.println("IJavaDB: timestamp out of bounds");
			return null;
		}
		// extracts characters and tokens from given string
		JSONTokener tokener = new JSONTokener(first_doc.toString());

		try {
			// get the JSON root object
			JSONObject root_obj = new JSONObject(tokener);

			// get the models array (is only one value but of type array)
			JSONArray models_array = root_obj.getJSONArray("models");

			// get the bbox from the array
			JSONObject json_bbox = models_array.getJSONObject(0).getJSONObject("bbox");

			// get the min value from the bbox
			JSONObject json_min = json_bbox.getJSONObject("min");

			// get the max value from the bbox
			JSONObject json_max = json_bbox.getJSONObject("max");

			// set the pose
			bounding_box[0] = json_min.getDouble("x");
			bounding_box[1] = json_min.getDouble("y");
			bounding_box[2] = json_min.getDouble("z");

			bounding_box[3] = json_max.getDouble("x");
			bounding_box[4] = json_max.getDouble("y");
			bounding_box[5] = json_max.getDouble("z");

		} catch (JSONException e) {
			e.printStackTrace();
		}	
		return bounding_box;
	}	

	/**
	 * 
	 * @param args
	 */
	public static void main(String[] args) 
	{				
		MongoPrologInterface4 mpi2 = new MongoPrologInterface4(1);

		//		mpi2.drawTrajectory("mug", 0, 1234253235);

		//System.out.println(mpi2.getModelPose("mug", 30629000000L)[0]);
		//		String[] test = mpi2.getModelContactNames("mug", 30629000000L);
		//		for(int i=0; i<test.length; i++)
		//		{
		//			System.out.println(test[i]);
		//		}
		try {
			File outputfile = new File("/home/yfang/javamongotestout2.txt");
			if(!outputfile.exists())
			{
				outputfile.createNewFile();
			}
			boolean summarize = false;
			for(int i=0; i<1; i++)
			{
				//clock how long the calls take
				long startTime = System.nanoTime();

				List<Long> graphtimes = new ArrayList<Long>(); //for storing at which times graphs occurred
				List<UndirectedGraph<String, DefaultEdge>> all_graphs = mpi2.contactEventsGraphs(summarize, graphtimes);
				List<Long> importanttimes = new ArrayList<Long>(); //for storing at which times important graphs occurred. This is nice to keep track of for printing later, So I actually know what timepoints the columns in the SECs correspond to
				List<UndirectedGraph<String, DefaultEdge>> important_graphs = mpi2.extractMainGraphs(all_graphs, graphtimes, importanttimes);
				//				for(UndirectedGraph<String, DefaultEdge> igraph : important_graphs)
				//				{
				//					System.out.println(igraph);
				//				}			
				Map<Pair<Integer, Integer>, List<Integer>> original_sec = mpi2.originalSEC(important_graphs);
				Map<Pair<Integer, Integer>, List<String>> derived_sec = mpi2.derivedSEC(original_sec);
				Map<Pair<Integer, Integer>, List<String>> compressed_sec = mpi2.compressedSEC(derived_sec);
				Double[][] spatial_sim_m = mpi2.spatialSimilarityMatrix(compressed_sec, compressed_sec);

				System.out.println("Orginal SEC: ");
				mpi2.printSEC(original_sec,importanttimes);
				System.out.println("Derived SEC: ");
				mpi2.printSEC(derived_sec,importanttimes);
				System.out.println("Compressed SEC: ");
				mpi2.printSEC(compressed_sec, null);
				System.out.println("Similarity matrix:");
				System.out.println(compressed_sec.keySet().toString());
				mpi2.printMatrix(spatial_sim_m);
				SimTotalResults permutations = mpi2.spatialSimilarityValue(spatial_sim_m, compressed_sec.keySet(), compressed_sec.keySet());
				mpi2.temporalSimilarityValue(derived_sec, derived_sec, permutations, importanttimes, importanttimes);
				
				//test transpose
//				Map<Long, List<String>> t = mpi2.transposeSEC(derived_sec, 4, importanttimes);
//				mpi2.printSEC(t, derived_sec.keySet());
				long endTime = System.nanoTime();
				long duration =endTime-startTime;
				System.out.println(duration);
			}
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
