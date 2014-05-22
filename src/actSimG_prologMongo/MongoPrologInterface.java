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
import java.util.Random;
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
 * @author Zhou Fang, 05-2014, University of Bremen
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
	
	//edgeweights for the different relations, relations with higher weights have priority over those with lower weights
	private static final Map<String, Double> RELATIONWEIGHTS = new HashMap<String, Double>();
	static{
		RELATIONWEIGHTS.put("contact", 1.0);
		RELATIONWEIGHTS.put("supportedby", 2.0);
		RELATIONWEIGHTS.put("insideof", 3.0);
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
	 * 
	 * 
	 * @param model_name
	 * @param timestamp
	 * @return
	 */
	public Pose getModelPose2(String model_name, long timestamp)
	{
		// echo for prolog
		//		System.out.println("IJavaDB: getting models '" + model_name + "' pose at timestamp: " + timestamp);

		// pose, X Y Z R P Y
		double[] pose = new double[6];
		Vector3 pos = null;
		Vector3 rot = null;

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
			pose[1] = json_pos.getDouble("y");
			pose[2] = json_pos.getDouble("z");

			pose[3] = json_rot.getDouble("x");
			pose[4] = json_rot.getDouble("y");
			pose[5] = json_rot.getDouble("z");
			pos = new Vector3(pose[0], pose[1], pose[2]);
			rot = new Vector3(pose[3], pose[4], pose[5]);
		} catch (JSONException e) {
			e.printStackTrace();
		}
		return new Pose(pos,rot);
	}
	
	/**
	 * TODO used for inferring "inside of" relation. Need to consider whether look at model bounding box only or also links bounding box (?!)
	 * 
	 * @param model_name
	 * @param timestamp
	 * @return
	 */
	public double[] getModelBoundingBox(String model_name, long timestamp)
	{
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
	 * Checks whether the center of model2 is inside model1. Returns true if so and false otherwise. //TODO this means that the center for objects (especially spheres and other content things) should be defined correctly in Gazebo
	 * Defined one boundingbox to be inside the other if the absolute of bbox_min AND bbox_max of model2 are inside the absolute
	 * of bbox_min and bbox_max of model1.
	 * 
	 * TODO need to test and debug this function, but first need to get data again where the boundingbox exists
	 * 
	 * @param modelname1
	 * @param modelname2
	 * @param timestamp
	 * @return
	 */
	public boolean insideOfModelBoundingbox(String modelname1, String modelname2, long timestamp)
	{
		double[] bbox1 = getModelBoundingBox(modelname1, timestamp); //first 3 coordinates are the box_min and other 3 are the box_max
		Pose pose1 = getModelPose2(modelname1, timestamp); //need to get pose of model1 for the rotation parameters
		Pose pose2 = getModelPose2(modelname2, timestamp); //get pose of model2 so can do same transform as with bbox1
		//bbox content is like this: min_x, min_y, min_z, max_x, max_y, max_z
		double[][] box1_resize = {{bbox1[0], bbox1[1], bbox1[2]}, {bbox1[3], bbox1[4], bbox1[5]}}; //resizing boundingbox dimensions for multiplication with rotation matrix
		double[] pose2_pos = {pose2.pos.x(), pose2.pos.y(), pose2.pos.z()};
		Matrix bbox1_mat = new Matrix(box1_resize);
		Matrix pose2_mat = new Matrix(pose2_pos, 1);
		bbox1_mat.print(10, 4);
		pose2_mat.print(10, 4);
		
		Matrix rotation_mat = rotationMatrix(pose1.rot.x(), pose1.rot.y(), pose1.rot.z()); //get rotation matrix for the container object
		rotation_mat.transpose(); //this gives you the inverse of the rotationMatrix, which needs to be applied to the current object to get it back to align with the world axes
		//rotate container object so that it's aligned with the world axis. Rotate the content object the same (if I would rotate it according to it's own axis, it might not be correct whether it's still in the container or not..?).
		Matrix aligned_bbox1_mat = bbox1_mat.times(rotation_mat);
		Matrix aligned_pose2_mat = pose2_mat.times(rotation_mat);
		aligned_bbox1_mat.print(10, 4);
		aligned_pose2_mat.print(10, 4);
		
		//check whether point is within box by checking for each axis whether minbox <= point <= maxbox
		System.out.println(aligned_bbox1_mat.get(0, 0) <= aligned_pose2_mat.get(0, 0));
		System.out.println(aligned_pose2_mat.get(0, 0) <= aligned_bbox1_mat.get(1, 0));
		System.out.println(aligned_bbox1_mat.get(0, 1) <= aligned_pose2_mat.get(0, 1)); 
		System.out.println(aligned_pose2_mat.get(0, 1) <= aligned_bbox1_mat.get(1, 1)); 
		System.out.println(aligned_bbox1_mat.get(0, 2) <= aligned_pose2_mat.get(0, 2)); 
		System.out.println(aligned_pose2_mat.get(0, 2) <= aligned_bbox1_mat.get(1, 2));
		// box_minx <= obj_x && obj_x <= box_maxx && box_miny <= obj_y && obj_y <= box_maxy && box_minz <= obj_z && obj_z <= box_maxz  
		return (aligned_bbox1_mat.get(0, 0) <= aligned_pose2_mat.get(0,0) && aligned_pose2_mat.get(0, 0) <= aligned_bbox1_mat.get(1, 0) && aligned_bbox1_mat.get(0,1) <= aligned_pose2_mat.get(0,1) && aligned_pose2_mat.get(0, 1) <= aligned_bbox1_mat.get(1, 1) && aligned_bbox1_mat.get(0, 2) <= aligned_pose2_mat.get(0, 2) && aligned_pose2_mat.get(0, 2) <= aligned_bbox1_mat.get(1, 2));
	}
	
	/**
	 * Checks whether model2 is supported by model1. Returns true if so and false otherwise. 
	 * Supported by is defined as in contact and above (like in CRAM)
	 * 
	 * @param modelname1
	 * @param modelname2
	 * @param timestamp
	 * @return
	 */
	public boolean supportedBy(String modelname1, String modelname2, long timestamp)
	{
		//get bounding box model 1
		double[] bbox1 = getModelBoundingBox(modelname1, timestamp);
		//get location of the centerpoint model 2
		Pose pose2 = getModelPose2(modelname2, timestamp);
//		System.out.println(modelname1 + " has max bounding box z " + bbox1[5]);
//		System.out.println(modelname2 + " has z pose " + pose2.pos.z());
		
		return bbox1[5] <= pose2.pos.z();
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
	public void addNewNode(SimpleWeightedGraph<String,DefaultWeightedEdge> graph, String nodename, long time)
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
	public void addNewEdge (SimpleWeightedGraph<String,DefaultWeightedEdge> graph, String node1, String node2, String node1modelname, String node2modelname, long time)
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

		double edgeweight;
		//TODO: insert checking for whether boundingbox is inside model and change graph label accordingly. Placed a random function here 
		//because the current episodes don't Have boundingboxes so I can only test this later.
		
//		Random rand=new Random(); //temporarily random
//		edgeweight = (double)rand.nextInt(3); //randomly the weight will be 0 or 1 or 2 //TODO replace random assignment with actual function
//		if(edgeweight ==2)//if(insideOfModelBoundingbox(node1modelname, node2modelname, time)) //Andrei said that the boundingbox for the spheres model is totally off (way too big), so they won't be recognized as within the mug. I'm taking the center now and hope that works. Otherwise I'll have to check whether the link is inside the model1 boundingbox rather than the whole model2. TODO: think about whether want to check this on link level or on model level. How can I make this as general as possible?
//		{
//			edgeweight = RELATIONWEIGHTS.get("insideof").doubleValue();
//		}
//		if(edgeweight ==1)//if(supportedBy(node1modelname, node2modelname, time) || supportedBy(node2modelname, node1modelname, time)) //TODO need to throw around graph representation sos it will be a directed graph. Right now even the supported by and inside relations can't distinguish who is supported and who is doing the supporting.
//		{
//			
//			edgeweight = RELATIONWEIGHTS.get("supportedby").doubleValue();
//		}
//		else if(edgeweight ==0)
//		{
			edgeweight = RELATIONWEIGHTS.get("contact").doubleValue();
//		}

		//add edge to graph if it doesn't exist yet
		if(!graph.containsVertex(new1) || !graph.containsVertex(new2))
		{
			System.out.println("WARNING: one of the nodes doesn't exist yet. This should never occur in the current implementation!");
		}
		else if (!new1.equals(new2)) //cannot code for object(parts) touching themselves. With liquid spheres this will give problems because the SimpleWeightedGraph class doesn't allow selfconnections. 
		{
			if (!graph.containsEdge(new1, new2)) // checks whether the current edge already exists (which happens because of remapping and a bug in the logs which causes multiple contacts with the same object to exist)
			{
				DefaultWeightedEdge newedge = graph.addEdge(new1, new2);
				graph.setEdgeWeight(newedge, edgeweight);
//				System.out.println(time + ": Add edge " + new1 + "-" + new2 + ", weight " + edgeweight);
			}
			else //if edge exists but a relation with higher priority is detected, replace
			{
				DefaultWeightedEdge curedge = graph.getEdge(new1, new2);
				if(graph.getEdgeWeight(curedge) < edgeweight)
				{
					graph.setEdgeWeight(curedge, edgeweight);
//					System.out.println(time + ": Updating edge " + new1 + "-" + new2 + ", new weight " + edgeweight);
				}
				
			}

		}
	}

	/**
	 * Function (recursive) for traversing two nested JSONArrays to the required level while keeping track of parents and 
	 * constructing an appropriate graph for each timestamp.
	 * TODO: for now relations are only checked as a subtype of contact: the function loops through the contact collisions and checks what kind of relationship exists.
	 * This might give problems for insideOf if somehow the contacts are not OK. (In principle one would expect the content to somehow contact the container).
	 * SupportedBy and Holding are also subtypes of contact.
	 * However, if we would like to have spatial relations etc., we should cycle through each model (regardless of contact) to see whether this relation exists. This will have to
	 * be in another function since this one is recursive and specifically meant to get contacts.
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
	public void constructGraph(SimpleWeightedGraph<String,DefaultWeightedEdge> graph_rep, String[] levelnames, JSONArray curar, Map<String,String> parents, long time) throws JSONException
	{
		if(levelnames.length==1) //we've traversed all the levels we needed
		{
			String collname = parents.get("collisions"); //TODO make summarize option so will actually just use modelnames, only possible when the model of a collision can be tracked down (Andrei has to change logging system)
			addNewNode(graph_rep, collname, time);
//			System.out.println("Checking node " + collname);
			String collmodelname = parents.get("models");
			for(int i=0; i<curar.length();i++)//go through all the contacts of the current collision
			{
				JSONObject contact_obj = curar.getJSONObject(i);
				String contactname = contact_obj.getString("name");
//				System.out.println("Adding contact: " + contact_obj.toString());
				String contactmodelname = contact_obj.getString("coll_model_name");
				addNewNode(graph_rep, contactname, time);
				addNewEdge(graph_rep, collname, contactname, collmodelname, contactmodelname,  time); //TODO to add new relations, must label edges for tracking relations later
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
	public List<SimpleWeightedGraph<String,DefaultWeightedEdge>> contactEventsGraphs(List<Long> graphtimes) throws JSONException, IOException
	{
		//storing graphs
		List<SimpleWeightedGraph<String,DefaultWeightedEdge>> allgraphs = new ArrayList<SimpleWeightedGraph<String,DefaultWeightedEdge>>();
		//know which levels to traverse
		String[] levelnames = {"models","links","collisions","contacts"};

		BasicDBObject query = new BasicDBObject();
		BasicDBObject fields = new BasicDBObject("models.links.collisions.contacts.name", 1);
		fields.append("models.name", 1);
		fields.append("_id", 0);
		fields.append("timestamp", 1);
		fields.append("models.links.name", 1);
		fields.append("models.links.collisions.name", 1);
		fields.append("models.links.collisions.contacts.coll_model_name", 1);

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
				SimpleWeightedGraph<String,DefaultWeightedEdge> i_graph =
						new SimpleWeightedGraph<String, DefaultWeightedEdge>(DefaultWeightedEdge.class); //each timestamp will be represented by an undirected graph
				Map<String, String> parents = new HashMap<String, String>(); //keeps a list whose child the current collision is
				constructGraph(i_graph, levelnames, cur_allmods, parents, currenttime);
//				System.out.println("Graph " + currenttime + ": " + i_graph.toString());
				//System.out.println(i_graph.toString());
				//store graph
				allgraphs.add(i_graph);
				graphtimes.add(currenttime);

				//				//setup variables for dotexporter
				//				StringNameProvider<String> p1=new StringNameProvider<String>();
				//				IntegerNameProvider<String> p2=new IntegerNameProvider<String>();
				//				StringEdgeNameProvider<String> p3 = new StringEdgeNameProvider<String>();
				//				//export graphs as dotfiles for visualization in graphviz, each file is approx. 0.5kB.
				//				DOTExporter<String, String> exporter = new DOTExporter<String,String>(p1, p2, p3);
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
	public List<SimpleWeightedGraph<String,DefaultWeightedEdge>> extractMainGraphs(List<SimpleWeightedGraph<String,DefaultWeightedEdge>> allgraphs, List<Long> graphtimes, List<Long> importanttimes)
	{
		List<SimpleWeightedGraph<String,DefaultWeightedEdge>> mainGraphs = new ArrayList<SimpleWeightedGraph<String,DefaultWeightedEdge>>();

		SimpleWeightedGraph<String,DefaultWeightedEdge> prev_graph = null;
		//convert graph to adjacency matrix, them compute eigenvalues and check with previous eigenvalues whether graphstructure has changed
		//if it changed, store graph. If it didn't, go to next one.
		int i = 0;
		double[] prev_eig =null;
		for(SimpleWeightedGraph<String,DefaultWeightedEdge> igraph : allgraphs)
		{
			if (prev_graph == null) //always add the first graph
			{
				//TODO: this needs to be uncommented again (?) --> don't store the very first graph because nothing is touching anything in it and it's only the very first timestamp. The way things are inititialized a lot of stuff will show up that's not actually important to the action.
				//				mainGraphs.add(igraph);
				//				importanttimes.add(graphtimes.get(i));
				prev_graph = igraph;
				prev_eig = graphToEigenvalue(prev_graph); //TODO add other relations + check whether eigenvalues change if the values change (e.g. whether it detects all changes)
				//				System.out.println("Storing " + i);
			}
			else
			{
				if(prev_eig == null)
				{
					System.err.println("ERROR: prev_eigen is null when prev_graph is not.");
				}
				double[] cur_eig = graphToEigenvalue(igraph);
				if(!Arrays.equals(prev_eig, cur_eig))
				{
					mainGraphs.add(igraph);
					importanttimes.add(graphtimes.get(i));
					//					System.out.println("Storing " + i);
				}
				prev_graph = igraph;
				prev_eig = cur_eig;
			}
			i++;
		}		
		return mainGraphs;
	}

	/**
	 * Compute Eigenvalues of adjacency matrix of a given event graph
	 * 
	 * CALLED BY:	extractMainGraphs
	 * CALLS:		-
	 * 
	 * @param graph
	 * @return
	 */
	public double[] graphToEigenvalue(SimpleWeightedGraph<String,DefaultWeightedEdge> graph)
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
	public double[][] adjacencyMatrix(SimpleWeightedGraph<String,DefaultWeightedEdge> graph)
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
			adjacencyMatrixVertex(nameProvider, from, Graphs.neighborListOf(graph, from), matrix, graph); //TODO add other relations
		}
		return matrix;
	}
	
	/**
	 * Adapted function from MatrixExporter of the jgrapht library to output full adjacency matrix to a double[][] array
	 * Note that results are given by changing the arguments rather than return value
	 * 
	 * @param nameProvider
	 * @param from
	 * @param neighbors
	 * @param matrix
	 * @param graph
	 */
	private void adjacencyMatrixVertex(VertexNameProvider<String> nameProvider, String from, List<String> neighbors, double[][] matrix, SimpleWeightedGraph<String,DefaultWeightedEdge> graph)
	{
		int fromName = Integer.parseInt(nameProvider.getVertexName(from));
//		System.out.println(from + ": " + fromName); //check that the numbers and names here are the same as in the .dot file
		for (String to : neighbors) //for each neighbor of the node "from"
		{
			//TODO once we have functions for determining spacial relations like above, rightof and things like that, can insert as a different number into the matrix
			int toName = Integer.parseInt(nameProvider.getVertexName(to));
			matrix[fromName-1][toName-1] = graph.getEdgeWeight(graph.getEdge(from, to)); //TODO code depends on the relation in the gaph (keeping todo here so I know where critical parts of this implementation are)
			matrix[toName-1][fromName-1] = graph.getEdgeWeight(graph.getEdge(from, to));
		}
	}

	/**
	 * Wraps processing pipeline from event graphs to SemanticEventChains (all SEC types) together for one episode. 
	 * 
	 * CALLS:		contactEventGraphs
	 * 				extractMainGraphs
	 * 				SemanticEventChains.constructAllSEC
	 * 
	 * @return
	 * @throws JSONException
	 * @throws IOException
	 */
	public SemanticEventChains processEpisodeToSEC() throws JSONException, IOException
	{
		//OBTAINING EVENT AND MAIN GRAPHS1
		List<Long> graphtimes = new ArrayList<Long>(); //for storing at which times graphs occurred
		List<SimpleWeightedGraph<String,DefaultWeightedEdge>> all_graphs = this.contactEventsGraphs(graphtimes);
		List<Long> importanttimes = new ArrayList<Long>(); //for storing at which times important graphs occurred. This is nice to keep track of for printing later, So I actually know what timepoints the columns in the SECs correspond to
		List<SimpleWeightedGraph<String,DefaultWeightedEdge>> important_graphs = this.extractMainGraphs(all_graphs, graphtimes, importanttimes);

		//SEC PROCESSING, initialize SEC
		SemanticEventChains newSEC = new SemanticEventChains(important_graphs, importanttimes);
		//FILL SEC AND MAKE DERIVED AND COMPRESSED VERSIONS
		newSEC.constructAllSEC(important_graphs);
		newSEC.getOSEC().printNodeMap();
		newSEC.getDSEC().printSEC();
		newSEC.getCSEC().printSEC();
		return newSEC;
	
	}
	
	/**
	 * Build a ModelSEC from the given episodes.
	 * Next function will use the ModelSEC to classify a new episode
	 * 
	 * @param episodes
	 * @throws IOException 
	 * @throws JSONException 
	 */
	public static void buildSECModel(List<String> episodes) throws JSONException, IOException
	{
		//get SECs for all the episodes in the list
		List<SemanticEventChains> semeventchains = new ArrayList<SemanticEventChains>();
		for(String ep : episodes)
		{
			MongoPrologInterface mpi = new MongoPrologInterface(ep);
			SemanticEventChains chain = mpi.processEpisodeToSEC();
			semeventchains.add(chain);
		}
		//choose the SEC with the largest number of columns to start with
		int base_chain = mostColumns(semeventchains);
		//make ModelSEC
		ModelSEC actionModel = new ModelSEC(semeventchains, base_chain);
		
		//compare all the other matrices with the SECmodel
		for(int i=0; i<semeventchains.size(); i++)
		{
			if(i!=base_chain) //skip if we're at the starting matrix since don't need to compare that
			{
				SemanticEventChains ichain = semeventchains.get(i);
				DerivedSEC modeldsec = actionModel.getdSECModel();
				CompressedSEC modelcsec = actionModel.getcSECModel();
				//compute similarity between the current sec and the model
				boolean exactMatch = false;
				PermResults permutations = semeventchains.get(i).spatialSimilarityValue(modelcsec, ichain.getCSEC(), exactMatch);
				System.out.println("WARNING: right now take the first permutation for building model. Update this later. With exactMatch there should only be one permutation.");
				//temporalSimilarityValue also reorders te second derived sec
				SimTotalResults allresults = semeventchains.get(i).temporalSimilarityValueWith(modeldsec, ichain.getDSEC(), permutations);
				//compare current matrix with the existing SECmodel
				actionModel.updateModel(ichain);
			}
		}
	}
	
	
	/**
	 * Compares two episodes with each other
	 * 
	 * CALLS: 		processEpisodeToSEC
	 * 				SemanticEventChains.spatialSimilarityValueWith
	 * 				SemanticEventChains.temporalSimilarityValueWith
	 * CALLED BY:	-
	 * 
	 * @param episode1
	 * @param episode2
	 * @return
	 * @throws JSONException
	 * @throws IOException
	 */
	public static List<Double> compareEpisodes(String episode1, String episode2) throws JSONException, IOException
	{
		System.out.println("Comparing " + episode1 + " and " + episode2);
		MongoPrologInterface mpi1 = new MongoPrologInterface(episode1);
		MongoPrologInterface mpi2 = new MongoPrologInterface(episode2);
		
		//CONVERT EPISODE TO EVENT GRAPHS AND MAKE SEC
		SemanticEventChains SEC1 = mpi1.processEpisodeToSEC();
		SemanticEventChains SEC2 = mpi2.processEpisodeToSEC();
				
		//SPATIAL SIMILARITY MATRIX
		boolean exactMatch = false;
		PermResults permutations = SEC1.spatialSimilarityValue(SEC1.getCSEC(), SEC2.getCSEC(), exactMatch);
		
		//TEMPORAL SIMILARITY MATRIX
		SEC2.getDSEC().printSEC();
		SimTotalResults allresults = SEC1.temporalSimilarityValueWith(SEC1.getDSEC(), SEC2.getDSEC(), permutations);
		SEC2.getDSEC().printSEC();
		List<Double> simvals = new ArrayList<Double>();
		simvals.add(allresults.max_spat_sim);
		simvals.add(allresults.max_temp_sim);
		
		return simvals;
	}
	
	/**
	 * Make comparisons between all episodes in EPISODES and prints out a similarity table at the end
	 * 
	 * @throws JSONException
	 * @throws IOException
	 */
	public static void makeFullComparisonTable() throws JSONException, IOException
	{
		//Compare all episodes that have been defined in EPISODES with each other
		List<Pair<String,String>> uniqueComparisons = allUniquePairs(EPISODES);
		//make matrix for similarities between episodes
		Double[][] totalsim_spat = new Double[EPISODES.size()][EPISODES.size()];
		Double[][] totalsim_temp = new Double[EPISODES.size()][EPISODES.size()];
		
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
		MyUtil.printMatrix(totalsim_spat, EPISODES, EPISODES, "Spatial similarities:");
		MyUtil.printMatrix(totalsim_temp, EPISODES, EPISODES, "Temporal similarities:");
	}

	/**
	 * 
	 * @param args
	 */
	public static void main(String[] args) 
	{				
		try {
			
			List<Double> simvals = compareEpisodes("acat_move2", "acat_move3"); //1=spatial, 2=temporal
//			makeFullComparisonTable();
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
	
	public static Matrix rotationMatrix(double a, double b, double c)
	{
		double[][] rotmatrix =  new double[3][3];
		rotmatrix[1][1] = Math.cos(a) * Math.cos(b);
		rotmatrix[1][2] = Math.cos(a) * Math.sin(b) * Math.sin(c) - Math.sin(a) * Math.cos(c);
		rotmatrix[1][3] = Math.cos(a) * Math.sin(b) * Math.cos(c) + Math.sin(a) * Math.sin(c);
		rotmatrix[2][1] = Math.sin(a) * Math.cos(b);
		rotmatrix[2][2] = Math.sin(a) * Math.sin(b) * Math.sin(c) + Math.cos(a) * Math.cos(c);
		rotmatrix[2][3] = Math.sin(a) * Math.sin(b) * Math.cos(c) - Math.cos(a) * Math.sin(c);
		rotmatrix[3][1] = -Math.sin(b);
		rotmatrix[3][2] = Math.cos(b) * Math.sin(c);
		rotmatrix[3][3] = Math.cos(b) * Math.cos(c);
		Matrix result = new Matrix(rotmatrix);
		return result;
	}
	
	/**
	 * Returns the index of the semantic chain whose derived matrix has the most columns of the SEC in the list
	 * 
	 * @param _matrices
	 * @return
	 */
	private static int mostColumns(List<SemanticEventChains> secs)
	{
		int largest = 0;
		int largest_index = 0;
		//all the DSECs have the same number of columns in each row, I can just take the column labels (time) to see how many columns it has
		for(int i=0; i<secs.size(); i++)
		{
			int ncolumns = secs.get(i).getDSEC().getTimeStrings().size();
			if(ncolumns > largest)
			{
				largest = ncolumns;
				largest_index = i;
			}
		}
		return largest_index;
	}

}