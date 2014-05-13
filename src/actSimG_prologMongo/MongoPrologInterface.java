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

import sun.security.util.DerEncoder;

/**
 * 
 * @author yfang
 * 
 * TODO: right now the temporal similarity is implemented as in the paper, but it might have undesirable characteristics.
 * In particular, it will ignore dissimilarities between SECs if one SEC is longer than the other (but the other is a subset of it).
 * They only report the temporal similarity outcome, which is according to the authors more restrictive, but it is actually not always the lower value.
 * For example if we have two SECs that are identical except that SEC2 has 1 more timestamp with some changes, the spatial similarity value will pick
 * up on this and be significantly lower than 100%, while temporal similarity ignores this completely (because of the way max. similarity is looked for
 * across rows; if you have more columns than rows, some columns can be completely ignored if they have low values) and computes a 100% similarity.
 * For now I'll just report both.
 * 
 * TODO: the same problem as described above will happen with A compared to B and B has more objects (=more rows) than A: in the similarity values,
 * for which the max. among the rows is taken, a column of B can be completely ignored if A has less objects. Therefore, a similarity between two 
 * sequences in which one contains more objects (that also change relationships with other objects) can still be 100%. NOTE that they discuss the
 * possibility of Arow > Brow, in which case dummy rows are added to B. But they also require that Acol <= Bcol. So even if I wanted to switch around
 * A and B for the rows (so making sure that Brow never has more objects than Arow), I'd get in trouble if one has more objects and the other has more columns.
 * For now I'm leaving it like this and still reporting both spatial and temporal similarity. 
 *
 */
public class MongoPrologInterface {

	private static final long A_TIMESTAMP = 0L;

	private MongoClient mongoClient;
	private DB db;
	private DBCollection coll;
	private World world;

	//TODO this should be unnecessary once the logging is fixed
	private static final Map<String, String> NAMEMAP  = new HashMap<String, String>();
	static{
		//		nameMap.put("kitchen_link_geom_counter_side_island_back_link", "kitchen_counter");
		//		nameMap.put("kitchen_link_geom_counter_side_island_left_link", "kitchen_counter");
		//		nameMap.put("kitchen_link_geom_counter_side_island_right_link", "kitchen_counter");
		//		nameMap.put("kitchen_link_geom_counter_top_island_link", "kitchen_counter");
		//		nameMap.put("kitchen_link_geom_skirting_island_link", "kitchen_counter");
		//		nameMap.put("kitchen_link_geom_stove_link", "kitchen_counter");
		//		nameMap.put("mug_body_geom_mug_max_x", "mug");
		//		nameMap.put("mug_body_geom_mug_max_x_help", "mug");
		//		nameMap.put("mug_body_geom_mug_max_y", "mug");
		//		nameMap.put("mug_body_geom_mug_min_x", "mug");
		//		nameMap.put("mug_body_geom_mug_min_y", "mug");
		//		nameMap.put("mug_body_geom_mug_min_x_help", "mug");
		//		nameMap.put("mug_body_geom_mug_bottom", "mug");

		//		//fingers separately
		//		nameMap.put("fore_finger_base_collision", "forefinger");
		//		nameMap.put("fore_finger_proximal_collision", "forefinger");
		//		nameMap.put("fore_finger_middle_collision", "forefinger");
		//		nameMap.put("fore_finger_distal_collision", "forefinger");
		//		nameMap.put("middle_finger_base_collision", "middlefinger");
		//		nameMap.put("middle_finger_proximal_collision", "middlefinger");
		//		nameMap.put("middle_finger_middle_collision", "middlefinger");
		//		nameMap.put("middle_finger_distal_collision", "middlefinger");
		//		nameMap.put("ring_finger_base_collision", "ringfinger");
		//		nameMap.put("ring_finger_proximal_collision", "ringfinger");
		//		nameMap.put("ring_finger_middle_collision", "ringfinger");
		//		nameMap.put("ring_finger_distal_collision", "ringfinger");
		//		nameMap.put("thumb_base_collision", "thumb");
		//		nameMap.put("thumb_base_collision", "thumb");
		//		nameMap.put("thumb_proximal_collision", "thumb");
		//		nameMap.put("thumb_middle_collision", "thumb");
		//		nameMap.put("thumb_distal_collision", "thumb");

		//fingers collapsed into hand only
		NAMEMAP.put("fore_finger_base_collision", "hand");
		NAMEMAP.put("fore_finger_proximal_collision", "hand");
		NAMEMAP.put("fore_finger_middle_collision", "hand");
		NAMEMAP.put("fore_finger_distal_collision", "hand");
		NAMEMAP.put("middle_finger_base_collision", "hand");
		NAMEMAP.put("middle_finger_proximal_collision", "hand");
		NAMEMAP.put("middle_finger_middle_collision", "hand");
		NAMEMAP.put("middle_finger_distal_collision", "hand");
		NAMEMAP.put("ring_finger_base_collision", "hand");
		NAMEMAP.put("ring_finger_proximal_collision", "hand");
		NAMEMAP.put("ring_finger_middle_collision", "hand");
		NAMEMAP.put("ring_finger_distal_collision", "hand");
		NAMEMAP.put("thumb_base_collision", "hand");
		NAMEMAP.put("thumb_base_collision", "hand");
		NAMEMAP.put("thumb_proximal_collision", "hand");
		NAMEMAP.put("thumb_middle_collision", "hand");
		NAMEMAP.put("thumb_distal_collision", "hand");

		//		//OLD WAY
		//		nameMap.put("right_hand_fore_finger_base_link_collision", "right_hand");
		//		nameMap.put("right_hand_fore_finger_proximal_link_collision", "right_hand");
		//		nameMap.put("right_hand_fore_finger_middle_link_collision", "right_hand");
		//		nameMap.put("right_hand_fore_finger_distal_link_collision", "right_hand");
		//		nameMap.put("right_hand_middle_finger_base_link_collision", "right_hand");
		//		nameMap.put("right_hand_middle_finger_proximal_link_collision", "right_hand");
		//		nameMap.put("right_hand_middle_finger_middle_link_collision", "right_hand");
		//		nameMap.put("right_hand_middle_finger_distal_link_collision", "right_hand");
		//		nameMap.put("right_hand_ring_finger_base_link_collision", "right_hand");
		//		nameMap.put("right_hand_ring_finger_proximal_link_collision", "right_hand");
		//		nameMap.put("right_hand_ring_finger_middle_link_collision", "right_hand");
		//		nameMap.put("right_hand_ring_finger_distal_link_collision", "right_hand");
		//		nameMap.put("right_hand_thumb_base_collision", "right_hand");
		//		nameMap.put("right_hand_thumb_base_link_collision", "right_hand");
		//		nameMap.put("right_hand_thumb_proximal_link_collision", "right_hand");
		//		nameMap.put("right_hand_thumb_middle_link_collision", "right_hand");
		//		nameMap.put("right_hand_thumb_distal_link_collision", "right_hand");

		NAMEMAP.put("spatula_extension_collision", "spatula_rest");
		NAMEMAP.put("spatula_head_collision", "spatula_rest");
	}

	//which episodes should be compared to each other
	private static final List<String> EPISODES = new ArrayList<String>();
	static{
		EPISODES.add("acat_move1");
		EPISODES.add("acat_move2");
		EPISODES.add("acat_move3");
		EPISODES.add("acat_move4");
		EPISODES.add("acat_pour1");
		EPISODES.add("acat_pour2");
		EPISODES.add("acat_pour3");
		EPISODES.add("acat_pour4");
		EPISODES.add("acat_flip1");
		EPISODES.add("acat_flip2");
		EPISODES.add("acat_flip3");
		EPISODES.add("acat_flip4");
	}
	
	/**
	 * MongoPrologInterface constructor
	 * @param collection 
	 */
	public MongoPrologInterface(String name) {		
		// echo for prolog
//		System.out.println("IJavaDB: " + "calling MongoPrologInterface constructor, setting up connection to database..");

		try {
			// create a new DB client
			this.mongoClient = new MongoClient( "localhost" , 27017 );

			// get the given DB
			this.db = mongoClient.getDB("sim_db");

			// get the given collection from the DB
			this.coll = this.db.getCollection(name);

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
		if(NAMEMAP.containsKey(oldname))
		{
			return NAMEMAP.get(oldname);
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
	 * @param graphtimes
	 * @throws JSONException
	 * @throws IOException
	 */
	public List<UndirectedGraph<String,DefaultEdge>> contactEventsGraphs(List<Long> graphtimes) throws JSONException, IOException
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

		//DBCursor doccursor = coll.find(query,fields).limit(100);
		DBCursor doccursor = coll.find(query,fields);
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
				//TODO: this needs to be uncommented again (?) --> don't store the very first graph because nothing is touching anything in it and it's only the very first timestamp. The way things are inititialized a lot of stuff will show up that's not actually important to the action.
				//				mainGraphs.add(igraph);
				//				importanttimes.add(graphtimes.get(i)); //TODO
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
	 * Searches which row-column correspondences lead to the highest overall similarity in a greedy fashion, without assuming which dimension is smaller
	 * If there are equal values, it returns the results when selecting each of these values (recursive)
	 * 
	 * @param permutations
	 * @param similarityvalues
	 * @param oneperm
	 * @param tempsimsum
	 * @param nrows
	 * @param spatial_sim_matrix_left
	 * @param SEC1labels_left
	 * @param SEC2labels_left
	 */
	public void greedySimCor(List<List<Pair<String, String>>> permutations, List<Double> similarityvalues, List<Pair<String,String>> oneperm, Double tempsimsum, int nrows, Double[][] spatial_sim_matrix_left, List<String> SEC1labels_left, List<String> SEC2labels_left)
	{
//		//print matrix to inspect progression on search
//		printMatrix(spatial_sim_matrix_left, SEC1labels_left, SEC2labels_left);
		//stop condition
		System.out.println(tempsimsum);
		if(SEC1labels_left.size()==0)
		{
			permutations.add(oneperm);
			similarityvalues.add(tempsimsum/nrows);
			System.out.println("tempsimsum/nrows: " + tempsimsum + "/" + nrows + "=" + tempsimsum/nrows);
			if(SEC2labels_left.size() != 0)
			{
				System.out.println("WARNING: No correspondences where possible for the following SEC2:\n"+SEC2labels_left.toString());
			}
		}
		else if(SEC2labels_left.size()==0)
		{
			permutations.add(oneperm);
			similarityvalues.add(tempsimsum/nrows);
			System.out.println("tempsimsum/nrows: " + tempsimsum + "/" + nrows + "=" + tempsimsum/nrows);
			//don't need to check whether SEC1labels is 0, because if it was, it would have never gotten in this condition
			System.out.println("WARNING: No correspondences where possible for the following SEC1:\n"+SEC1labels_left.toString());
		}
		else //there are still labels left to correspond
		{
			//need to first determine what the largest val is, and then go through row again to see which ones have the largest val (because there could be more than 1)
			Double largestval = maxValue(spatial_sim_matrix_left);

			for(int irow = 0; irow<spatial_sim_matrix_left.length; irow++)
			{
				for(int icol = 0; icol<spatial_sim_matrix_left[0].length; icol++)
				{
					Double curval = spatial_sim_matrix_left[irow][icol];
					if(curval.equals(largestval))
					{
						//which association does this largest val have?
						String rowind= SEC1labels_left.get(irow);
						String colind= SEC2labels_left.get(icol);
						Pair<String,String> curassoc = new Pair<String,String>(rowind, colind);

						//remove affected row and column, and then go on to the rest of the matrix
						Double[][] next_simmatrix = removeFromMatrix(spatial_sim_matrix_left, irow, icol);
						List<String> newSEC1 = new ArrayList<String>(SEC1labels_left);
						newSEC1.remove(irow);
						//					System.out.println("newSEC1: " + newSEC1.toString());
						List<String> newSEC2 = new ArrayList<String>(SEC2labels_left);
						newSEC2.remove(icol);
						//					System.out.println("newSEC2: " + newSEC2.toString());
						List<Pair<String,String>> addedPerm = new ArrayList<Pair<String,String>>(oneperm);
						addedPerm.add(curassoc);

						greedySimCor(permutations, similarityvalues, addedPerm, tempsimsum+largestval, nrows, next_simmatrix, newSEC1, newSEC2);
					}	
				}
			}
		}
	}

	/**
	 * Alternative function for finding correspondences from a similarity matrix. Instead of checking all child nodes with the same value
	 * (e.g. if there is more than one of the largest value), simply takes the first one it finds and goes with that.
	 * 
	 * @param permutations
	 * @param similarityval
	 * @param oneperm
	 * @param tempsimsum
	 * @param nrows
	 * @param spatial_sim_matrix
	 * @param SEC1labels
	 * @param SEC2labels
	 * @return
	 */
	public List<Double> fastSimCor(List<List<Pair<String, String>>> permutations, List<Pair<String,String>> oneperm, Double tempsimsum, int nrows, Double[][] spatial_sim_matrix, List<String> SEC1labels, List<String> SEC2labels)
	{
		List<Double> similarityval = new ArrayList<Double>(); //for storing result
		//stop condition
		if(SEC1labels.size()==0)
		{
			permutations.add(oneperm);
			similarityval.add(tempsimsum/nrows);
			if(SEC2labels.size() != 0)
			{
				System.out.println("WARNING: No correspondences where possible for the following SEC2:\n"+SEC2labels.toString());
			}
			return similarityval;
		}
		else if(SEC2labels.size()==0)
		{
			permutations.add(oneperm);
			similarityval.add(tempsimsum/nrows);
			//don't need to check whether SEC1labels is 0, because if it was, it would have never gotten in this condition
			System.out.println("WARNING: No correspondences where possible for the following SEC1:\n"+SEC1labels.toString());
			return similarityval;
		}
		else //there are still rows to go through
		{
			//need to first determine what the largest val is, and then go through row again to see which ones have the largest val
			Double largestval = maxValue(spatial_sim_matrix);
			
			for(int irow = 0; irow<spatial_sim_matrix.length; irow++)
			{
				for(int icolumn=0; icolumn<spatial_sim_matrix[0].length;icolumn++)
				{
					if(spatial_sim_matrix[irow][icolumn].equals(largestval))
					{
						//which association does this largest val have?
						Pair<String,String> curassoc = new Pair<String,String>(SEC1labels.get(irow), SEC2labels.get(icolumn));

						//remove affected row and column, and then go on to the rest of the matrix
						Double[][] next_simmatrix = removeFromMatrix(spatial_sim_matrix, irow, icolumn);
						List<String> newSEC1 = new ArrayList<String>(SEC1labels);
						newSEC1.remove(irow);
						//					System.out.println("newSEC1: " + newSEC1.toString());
						List<String> newSEC2 = new ArrayList<String>(SEC2labels);
						newSEC2.remove(icolumn);
						//					System.out.println("newSEC2: " + newSEC2.toString());
						List<Pair<String,String>> addedPerm = new ArrayList<Pair<String,String>>(oneperm);
						addedPerm.add(curassoc);

						return fastSimCor(permutations, addedPerm, tempsimsum+largestval, nrows, next_simmatrix, newSEC1, newSEC2);
					}				
				}
			}
		}
		return similarityval;
	}
	
	public Double [][] spatialSimilarityMatrix(CompressedSEC SEC1, CompressedSEC SEC2)
	{
		Double[][] similarity_matrix = new Double[SEC1.SECmatrix.size()][SEC2.SECmatrix.size()];
		System.out.println("Comparing: ");
		SEC1.printSEC(SEC1.getRelationStrings(), null);
		System.out.println("To: ");
		SEC2.printSEC(SEC2.getRelationStrings(), null);

		//Every row of SEC1 is compared to every row of SEC2 to find the highest similarity.
		int irow =0; //to keep track of where to store the similarity value in the matrix. 
		for(List<String> irow_SEC1 : SEC1.SECmatrix)
		{
			int icolumn =0;
			for(List<String> irow_SEC2 : SEC2.SECmatrix)
			{
				double max_similarity = 0; //maximum similarity for the comparison of the two rows
				if(irow_SEC1.size() > irow_SEC2.size())
				{					
					max_similarity = max_spatial_row_similarity(irow_SEC1, irow_SEC2);
				}
				else //currow2 is bigger or of equal size as currow1
				{
					max_similarity = max_spatial_row_similarity(irow_SEC2, irow_SEC1);
				}
				similarity_matrix[irow][icolumn]=max_similarity;
				icolumn++;
			}
			irow++;
		}
		return similarity_matrix;
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
	public PermResults spatialSimilarityValue(Double[][] spatial_sim_matrix, List<String> SEC1labels, List<String> SEC2labels)
	{
		//nrows needs to be given as a constant because the recursive function will lose the rows in recursion. Changed to ndimension because if the input is not matched according to dimension, there might be more columns than rows and then we want to divide by that.
		int nmax_dimension = Math.max(spatial_sim_matrix.length, spatial_sim_matrix[0].length);
		//variables that need to be given to recurse on
		List<Pair<String,String>> oneperm = new ArrayList<Pair<String,String>>();

		//variables for storing results
		List<List<Pair<String, String>>> permutations_res = new ArrayList<List<Pair<String,String>>>();
		List<Double> similarityval_res = new ArrayList<Double>();
		//hackTemporalSimilaritySimpleCorrespondence(permutations_res, similarityval_res, oneperm, 0.0, nrows,spatial_sim_matrix, SEC1labels, SEC2labels);
		greedySimCor(permutations_res, similarityval_res, oneperm, 0.0, nmax_dimension, spatial_sim_matrix, SEC1labels, SEC2labels);

		PermResults result = new PermResults(permutations_res, similarityval_res);
		//it may be that some of the permutations eventually lead to a lower similarity value, so take these out
		//determine maximum value of row
		Double maxval = maxValue(similarityval_res);
		//throw out items that are not equal to the maximum value
		for(int i=similarityval_res.size()-1; i>=0; i--)//doing it the other way around to not mess up the indexes when removing something
		{
			if(similarityval_res.get(i) < maxval)
			{
				System.out.println("Removing value " + similarityval_res.get(i) + "\nPermutation " + permutations_res.get(i));
				result.removeItem(i);
			}
		}
//		System.out.println("all permres spatial:" + result.getPerm().toString());
//		System.out.println("all similarity spatial: " + result.getSim().toString());
		return result;
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
	 * TODO: right now the smaller matrix derived matrix (irrespective of whether it's 1 or 2) is filled up with dummy rows. The authors do not
	 * describe this in the paper (they only say in case 2 is smaller than 1), but that doesn't make sense because it would mean that the similarities
	 * between actions are not symmetrical. Going from their similarity matrix (which is symmetrical), something else must have happened.  Should look
	 * into this later. Like the function does it now, it will be symmetrical.
	 * 
	 * NOTE: the ncols2 and SEC2 are here the rows and SEC1 and ncols1 the columns because the SEC has been transposed prior to calculating the
	 * temporal similarity. (this may not be necessary, was quickly doing it so can use SimilarityCorrespondence and because old representation didn't
	 * let me check columns easily)
	 * 
	 * @param tSEC1
	 * @param tSEC2
	 * @param ncols1
	 * @param ncols2
	 * @return
	 */
	public Double[][] temporalSimilarityMatrix(DerivedSEC tSEC1, DerivedSEC tSEC2)
	{
		System.out.println("Comparing: ");
		tSEC1.printSEC(tSEC1.getRelationStrings(), tSEC1.getTimeStrings());
		System.out.println("To: ");
		tSEC2.printSEC(tSEC2.getRelationStrings(), tSEC2.getTimeStrings());
		
		//compare all columns with all columns to construct similarity matrix. No shuffling needed.
		Double[][] similarity_matrix = new Double[tSEC1.SECmatrix.get(0).size()][tSEC2.SECmatrix.get(0).size()];
		int irow =0; //to keep track of where to store the similarity value in the matrix. 
		for(int icol1=0; icol1<tSEC1.SECmatrix.get(0).size(); icol1++) //a of theta
		{
			int icolumn =0;//to keep track of where to store the similarity value in the matrix. 
			for(int icol2=0; icol2<tSEC2.SECmatrix.get(0).size(); icol2++) //b of theta
			{
				List<String> icol_SEC1 = getColumnFrom2DList(tSEC1.SECmatrix, icol1);
				List<String> icol_SEC2 = getColumnFrom2DList(tSEC2.SECmatrix, icol2);
				//take the two current SEC columns and calculate the similarity value for the current cell of the matrix for that
				similarity_matrix[irow][icolumn] = single_temporal_similarity(icol_SEC1, icol_SEC2);
				icolumn++;
			}
			irow++;
		}
		return similarity_matrix;
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
	public SimTotalResults temporalSimilarityValue(DerivedSEC derived_SEC1, DerivedSEC derived_SEC2, PermResults perm_res)
	{
		//get the possible permutations from spatial similarity perspective
		List<List<Pair<String, String>>> permutations = perm_res.getPerm(); 

		//initialize variables for storing results
		Double max_temp_sim = new Double(0);
		Double max_spat_sim = new Double(0);
		List<List<Pair<String, String>>> max_spat_permutation = new ArrayList<List<Pair<String,String>>>(); //for storing exactly which permutations lead to the given similarity value
		List<List<Pair<String, String>>> max_temp_permutation = new ArrayList<List<Pair<String,String>>>();

		//how many rows is the max? need to fill smaller matrix with dummies
		int max_rows = 0;
		if (derived_SEC1.SECmatrix.size()>derived_SEC2.SECmatrix.size())
			max_rows = derived_SEC1.SECmatrix.size();
		else
			max_rows = derived_SEC2.SECmatrix.size();

		//produce similarity matrices for all permutations
		for(int iperm=0; iperm < permutations.size(); iperm++)
		{
			//Create a SEC2 that is ordered such that the rows correspond to SEC1 according to the current permutation
			DerivedSEC ordered_SEC2 = SemanticEventChains.reorderDerivedSEC(derived_SEC1,derived_SEC2, permutations.get(iperm));

			//Because need to go through all the columns instead of rows now, need to have columns in lists, transpose.
			DerivedSEC tSEC1 = derived_SEC1.extendWithDummySEC(max_rows);
			DerivedSEC tSEC2 = ordered_SEC2.extendWithDummySEC(max_rows);

			//compare all columns with all columns to construct similarity matrix. No shuffling needed.
			Double[][] similarity_matrix = temporalSimilarityMatrix(tSEC1, tSEC2);
			System.out.println("temporal similarity matrix:");
			printMatrix(similarity_matrix, tSEC1.getTimeStrings(), tSEC2.getTimeStrings()); //SEC2 first because of transposition

			//compute final similarity value and correspondences
			int nmax_dimension = Math.max(similarity_matrix.length, similarity_matrix[0].length);
			List<Pair<String,String>> oneperm = new ArrayList<Pair<String,String>>(); //variable to recurse on
			//variables for storing results
			List<List<Pair<String, String>>> permutations_res = new ArrayList<List<Pair<String,String>>>();
			List<Double> similarityval_res = fastSimCor(permutations_res, oneperm, 0.0, nmax_dimension, similarity_matrix, tSEC1.getTimeStrings(), tSEC2.getTimeStrings());
//			//this finds better solutions but is just too slow: TODO maybe reimplementation can make it much faster
//			List<Double> similarityval_res = new ArrayList<Double>();
//			greedySimCor(permutations_res, similarityval_res, oneperm, 0.0, nrows, r_similarity_matrix, tSEC1.getTimeStrings(), tSEC2.getTimeStrings());
			
//			System.out.println("all permutationres temporal: " +permutations_res.toString());
			System.out.println("all similarityres temporal: " + similarityval_res.toString());
			//compare with previous stored value: if it's greater, throw out all the other ones, if it's the same, add to the list, if it's smaller, ignore
			for(int j=0; j<similarityval_res.size(); j++) //the values in similarityval_res are not perse all equally large, so can't test them as a group, need to loop over
			{
				int compareval = similarityval_res.get(j).compareTo(max_temp_sim);
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
					max_temp_sim = similarityval_res.get(j);
					max_spat_sim = perm_res.getSim().get(iperm);
					//replace permutation list
					max_temp_permutation = new ArrayList<List<Pair<String,String>>>();
					max_temp_permutation.add(permutations_res.get(j));
					max_spat_permutation = new ArrayList<List<Pair<String,String>>>();
					max_spat_permutation.add(permutations.get(iperm));
				}
				//if similarityval_res(j) smaller than max_sim, nothing happens
			}
		}
		System.out.println("Max. spatial similarity:" + max_spat_sim);
		System.out.println("Max. temporal similarity:" + max_temp_sim);
		System.out.println("for spatial permutations: " + max_spat_permutation.toString());
		System.out.println("and temporal permutations: " + max_temp_permutation.toString());
		
		SimTotalResults results = new SimTotalResults(max_spat_permutation, max_temp_permutation, max_spat_sim, max_temp_sim);
		return results;
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

	public SemanticEventChains processEpisodeToSEC() throws JSONException, IOException
	{
		//OBTAINING EVENT AND MAIN GRAPHS1
		List<Long> graphtimes = new ArrayList<Long>(); //for storing at which times graphs occurred
		List<UndirectedGraph<String, DefaultEdge>> all_graphs = this.contactEventsGraphs(graphtimes);
		List<Long> importanttimes = new ArrayList<Long>(); //for storing at which times important graphs occurred. This is nice to keep track of for printing later, So I actually know what timepoints the columns in the SECs correspond to
		List<UndirectedGraph<String, DefaultEdge>> important_graphs = this.extractMainGraphs(all_graphs, graphtimes, importanttimes);

		//SEC PROCESSING, initialize SEC
		SemanticEventChains newSEC = new SemanticEventChains(important_graphs, importanttimes);
		newSEC.constructOriginalSEC(important_graphs);
		
		//FILL SEC AND MAKE DERIVED AND COMPRESSED VERSIONS
//		newSEC.getOSEC().printNodeMap();
		DerivedSEC dsec = newSEC.constructDerivedSEC(newSEC.getOSEC());
		newSEC.constructCompressedSEC(dsec);
		newSEC.getDSEC().printSEC(newSEC.getDSEC().getRelationStrings(), newSEC.getDSEC().getTimeStrings());
		newSEC.getCSEC().printSEC(newSEC.getCSEC().getRelationStrings(), null);
		return newSEC;
	
	}
	
	public static List<Double> compareEpisodes(String episode1, String episode2) throws JSONException, IOException
	{
		System.out.println("Comparing " + episode1 + " and " + episode2);
		MongoPrologInterface mpi1 = new MongoPrologInterface(episode1);
		MongoPrologInterface mpi2 = new MongoPrologInterface(episode2);
		
		//CONVERT EPISODE TO EVENT GRAPHS AND MAKE SEC
		SemanticEventChains SEC1 = mpi1.processEpisodeToSEC();
		SemanticEventChains SEC2 = mpi2.processEpisodeToSEC();
				
		//SPATIAL SIMILARITY MATRIX
		Double[][] spatial_sim_m = mpi1.spatialSimilarityMatrix(SEC1.getCSEC(), SEC2.getCSEC());
		System.out.println("Spatial similarity matrix:");
		printMatrix(spatial_sim_m, SEC1.getCSEC().getRelationStrings(), SEC2.getCSEC().getRelationStrings());
		//TEMPORAL SIMILARITY MATRIX
		PermResults permutations = mpi1.spatialSimilarityValue(spatial_sim_m, SEC1.getCSEC().getRelationStrings(), SEC2.getCSEC().getRelationStrings());
		SimTotalResults allresults = mpi1.temporalSimilarityValue(SEC1.getDSEC(), SEC2.getDSEC(), permutations);
		List<Double> simvals = new ArrayList<Double>();
		simvals.add(allresults.max_spat_sim);
		simvals.add(allresults.max_temp_sim);
		
		return simvals;
	}

	/**
	 * 
	 * @param args
	 */
	public static void main(String[] args) 
	{				
		try {
			File outputfile = new File("/home/yfang/javamongotestout2.txt");
			if(!outputfile.exists())
			{
				outputfile.createNewFile();
			}
			//Compare all episodes that have been defined in EPISODES with each other
			List<Pair<String,String>> uniqueComparisons = allUniquePairs(EPISODES);
			//make matrix for similarities between episodes
			Double[][] totalsim_spat = new Double[EPISODES.size()][EPISODES.size()];
			Double[][] totalsim_temp = new Double[EPISODES.size()][EPISODES.size()];
			
//			List<Double> simvals = compareEpisodes("acat_move2", "acat_move3"); //1=spatial, 2=temporal
			
			int row = 0; //to keep track of at which row you are
			int column = 1;
			for(int i = 0; i<uniqueComparisons.size(); i++)
			{
				Pair<String,String> comp = uniqueComparisons.get(i);
				List<Double> simvals = compareEpisodes(comp.getValue0(), comp.getValue1()); //1=spatial, 2=temporal
				totalsim_spat[row][column] = simvals.get(0);
				totalsim_temp[row][column] = simvals.get(1);
				if(EPISODES.size()==column+1) //have to start filling the next row
				{
					row++;
					column = row+1;
				}
				else
				{
					column++;
				}
			}
			System.out.println("Spatial similarities:");
			printMatrix(totalsim_spat, EPISODES, EPISODES);
			System.out.println("Temporal similarities:");
			printMatrix(totalsim_temp, EPISODES, EPISODES);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	
	//########################################################################
	//## GENERIC HELP FUNCTIONS 											##
	//########################################################################
	
	/**
	 * Returns all possible unique pairs of elements of the given list, without self pairs (this would be pointless for our purposes since the similarity between these would always be 100)
	 * 
	 * @param list
	 * @return
	 */
	public static List<Pair<String,String>> allUniquePairs(List<String> list)
	{
		List<Pair<String,String>> result = new ArrayList<Pair<String,String>>();
		for(int i=0; i<list.size(); i++) //go through each element of the list
		{
			for(int j=i+1; j<list.size();j++) //pair the element with each element that follows it in the list (to get unique pairs)
			{
//				System.out.println("<" + list.get(i) + "," + list.get(j) + ">");
				result.add(new Pair<String,String>(list.get(i), list.get(j)));
			}
		}
		return result;
	}

	/**
	 * Converts a set of Pairs to a list of Strings
	 * 
	 * CALLED BY:	spatialSimilarityValue (with Pair set)
	 * 				temporalSimilarityValue (with Long set)
	 * CALLS:		-
	 * 
	 * @param pair
	 * @return
	 */
	public <T> List<String> listToStringList(List<T> pair)
	{
		List<String> result= new ArrayList<String>();
		for(T item : pair)
		{
			result.add(item.toString());
		}
		return result;
	}

	
	public Double maxValue(List<Double> list)
	{
		Double maxval = list.get(0);
		for(int i=1; i<list.size(); i++)
		{
			if(list.get(i) >= maxval)
			{
				maxval = list.get(i);
			}
		}
		return maxval;
	}
	public Double maxValue(Double[] array)
	{
		Double maxval = array[0];
		for(int i=1; i<array.length; i++)
		{
			if(array[i] >= maxval)
			{
				maxval = array[i];
				//				System.out.println("Pos: " + i + "; Maxval: " + maxval);
			}
		}
		return maxval;
	}
	
	public Double maxValue(Double[][] matrix)
	{
		Double maxval = matrix[0][0]; //not initializing to 0 but to a value of the parameters so behavior is OK also for negative valued input
		for(int irow=0; irow<matrix.length; irow++)
		{
			Double maxirow = maxValue(matrix[irow]);
			if(maxirow > maxval)
			{
				maxval = maxirow;
			}
		}
		return maxval;
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
	 * For printing SECs in a matrix like form
	 * 
	 * @param SEC
	 */
	public static <S,T,V> void printSEC(Map<S, List<T>> SEC, V times) 
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
	 * print a primitive two-dimensional matrix with row and column labels
	 * 
	 * @param m
	 */
	public static <T> void printMatrix(T[][] m, List<String> rlabels, List<String> clabels)
	{
		//In Java, 2D arrays are really arrays of arrays with possibly different lengths (there are no guarantees that in 2D arrays 
		//that the 2nd dimension arrays all be the same length)
		//You can get the length of any 2nd dimension array as z[n].length where 0 <= n < z.length.
		//If you're treating your 2D array as a matrix, you can simply get z.length and z[0].length, but note that you might be 
		//making an assumption that for each array in the 2nd dimension that the length is the same.
		System.out.println("\t" + clabels.toString());

		for(int i=0; i<m.length;i++)
		{
			System.out.print(rlabels.get(i).toString() + "\t");
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
	public static <T> void printArray(T[] a)
	{
		for(int i=0; i<a.length;i++)
			System.out.print(a[i]+ " ");
		System.out.println();
	}

	public static Double[][] transposeMatrix(Double[][] matrix)
	{
		Double[][] newmatrix = new Double[matrix[0].length][matrix.length]; //column and row length are reversed compared to old matrix
		//loop through rows
		for(int i=0; i<matrix.length; i++)
		{
			//loop through columns
			for(int j=0; j<matrix[0].length; j++)
			{
				newmatrix[j][i] = matrix[i][j];
			}
		}
		return newmatrix;
	}
	
	/**
	 * Returns the column (2nd dimension of the nested arraylist) at index i
	 * @param arraylist
	 * @param index
	 * @return
	 */
	public static <T> List<T> getColumnFrom2DList(List<List<T>> arraylist, int index)
	{
		List<T> column = new ArrayList<T>();
		for(List<T> irow : arraylist)
		{
			column.add(irow.get(index));
		}
		return column;
	}
}
	//########################################################################
	//## NO LONGER USED FUNCTIONS 											##
	//########################################################################
//
//	/**
//	 * This function has been replaced by greedySimCor, which checks the overall max. value in the remaining matrix instead of row by row.
//	 * That has the effect that it no longer matters whether there are more rows than columns (wouldn't work with this one) or not. It is
//	 * less efficient though, since it takes an extra for-loop to compute the max. of the matrix instead of just one vector.
//	 * 
//	 * Loops over rows recursively to find row/column correspondence. Splits off to multiple if more than one possibility in a row.
//	 * Computes final spatial similarity values for all the possible correspondence choices if two columns have the same big value for one row. 
//	 * The value is the average value of the highest similarities across rows of the similarity matrix.
//	 * 
//	 * WARNING: SEC1 (rows) should always be equal to or smaller than SEC2 in size! Otherwise the way to compute similarity doesn't really make sense.
//	 * Decide what to do when number of rows not equal --> 1) ignore inequality and take the smaller one as the row for the correspondence, so the extra columns 
//	 * will just be left over. 2) Take the largest one as row for correspondence and fill up remaining similarity values with 0 (equivalent to filling up
//	 * with dummies. May make sense that actions that involve more objects changing are different and this should be reflected in the similarity value,
//	 * but maybe unrelated things are going on at the same time --> how do we know? + a BIGGER issue is that just by chance the last object(s) will never be
//	 * matched even if they actually correspond much better)
//	 * For now implemented option 1 because that first came into mind. Can implement 2) later too and compare results if desirable.
//	 * TODO this algorithm is not guaranteed to find the optimal solution for the correspondence problem: if an earlier row Did have a single corresponding
//	 * highest column, a later row will never be able to associate with that column because it will never be available, even if the overall resulting similarity would be higher.
//	 * TODO right now the whole process only works if the same SEC has equal or smaller number of rows AND equal or smaller number of columns compared to the other. Need
//	 * the row constraint for this function and the column constraint for the temporal similarity matrix. If they are not the same, the order in the permutationlist will be false.
//	 * I can solve this by changing the order in which I input the correspondence pairs (it's easy to check which SEC has fewer columns. I should do this in the wrap function and
//	 * give it here as a boolean.
//	 * 
//	 * @param permutations
//	 * @param similarityvalue
//	 * @param spatial_sim_matrix_left
//	 * @param SEC1labels_left
//	 * @param SEC2labels_left
//	 */
//	public void similarityCorrespondence(List<List<Pair<String, String>>> permutations, List<Double> similarityvalues, List<Pair<String,String>> oneperm, Double tempsimsum, int nrows, Double[][] spatial_sim_matrix_left, List<String> SEC1labels_left, List<String> SEC2labels_left)
//	{
//		//print matrix to inspect progression on search
//		//		printMatrix(spatial_sim_matrix_left, SEC1labels_left, SEC2labels_left);
//		//stop condition
//		if(SEC1labels_left.size()==0)//I don't know which is smaller, there may be objects left 
//		{
//			permutations.add(oneperm);
//			similarityvalues.add(tempsimsum/nrows);
////			System.out.println("Adding:\n"+oneperm.toString());
////			System.out.println("Similarity value: " +tempsimsum/nrows);
//		}
//		else if(SEC2labels_left.size()==0)
//		{
//			System.err.println("WARNING: SEC2 is smaller than SEC1, with the size checks in the comparison function this should never happen.");
//		}
//		else //there are still labels left to correspond
//		{
//			//Determine max value by sorting a copy of the row and looking at the end TODO Collections.sort(temprow); Double largestval = temprow[temprow.length-1];
//			//need to first determine what the largest val is, and then go through row again to see which ones have the largest val
//			Double[] currow = spatial_sim_matrix_left[0];
//			Double largestval = maxValue(currow);
//
//			String rowind = SEC1labels_left.get(0); //we know there has to be Some correspondent to this row
//			for(int icolumn=0; icolumn<currow.length;icolumn++)
//			{
//				Double curval =currow[icolumn];
//				if(curval.equals(largestval))
//				{
//					//					System.out.println("current tempsum=" + tempsimsum);
//					//which association does this largest val have?
//					String colind= SEC2labels_left.get(icolumn);
//					Pair<String,String> curassoc = new Pair<String,String>(rowind, colind);
//
//					//remove affected row and column, and then go on to the rest of the matrix
//					Double[][] next_simmatrix = removeFromMatrix(spatial_sim_matrix_left, 0, icolumn);
//					List<String> newSEC1 = new ArrayList<String>(SEC1labels_left);
//					newSEC1.remove(0);
//					//					System.out.println("newSEC1: " + newSEC1.toString());
//					List<String> newSEC2 = new ArrayList<String>(SEC2labels_left);
//					newSEC2.remove(icolumn);
//					//					System.out.println("newSEC2: " + newSEC2.toString());
//					List<Pair<String,String>> addedPerm = new ArrayList<Pair<String,String>>(oneperm);
//					addedPerm.add(curassoc);
//
//					similarityCorrespondence(permutations, similarityvalues, addedPerm, tempsimsum+largestval, nrows, next_simmatrix, newSEC1, newSEC2);
//				}				
//			}
//		}
//	}
//
