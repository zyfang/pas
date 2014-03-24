/* Author: Andrei Haidu
 * Email: a.haidu@gmail.com
 */

package mongo_prolog;

import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

import com.mongodb.BasicDBObject;
import com.mongodb.DB;
import com.mongodb.DBCollection;
import com.mongodb.DBObject;
import com.mongodb.MongoClient;

public class MongoPrologInterface {

	private static final long A_TIMESTAMP = 0L;
	
	private MongoClient mongoClient;
	private DB db;
	private DBCollection coll;
	private World world;

	/**
	 * MongoPrologInterface constructor
	 * @param collection 
	 */
	//////////////////////////////////////////////////////////
	public MongoPrologInterface(int collection) {		
		// echo for prolog
		System.out.println("IJavaDB: " + "calling MongoPrologInterface constructor, setting up connection to database..");
		
		try {
			// create a new DB client
			this.mongoClient = new MongoClient( "localhost" , 27017 );

			// get the given DB
			this.db = mongoClient.getDB("sim_db");

			// get the given collection from the DB
			this.coll = this.db.getCollection("collection_X" + collection);

		} catch (UnknownHostException e) {
			e.printStackTrace();
		}
	}

	/**
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
			
			// get the models JSON array from the JSON root object
			JSONArray models_array = root_obj.getJSONArray("models");
			
			
			//////////////////////////////////////////////////////////
			// loop through all the models array
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
				
				//////////////////////////////////////////////////////////
				// loop through all the links for the current model
				for(int j = 0; j < links_array.length(); j++)
				{
					// get the given JSON object from the array
					JSONObject curr_link_obj = links_array.getJSONObject(j);

					// create a local link with the given name from the JSON obj
					Link curr_link = new Link( curr_link_obj.getString("name"));
					
//					// echo for prolog
//					System.out.println("\t\t" + curr_link.getName());
					
					// get the collision JSON array from the current JSON obj 
					JSONArray collisions_array = links_array.getJSONObject(j).getJSONArray("collisions");
					
					
					//////////////////////////////////////////////////////////
					// loop through all the collisions for the current link
					for(int k = 0; k < collisions_array.length(); k++)
					{											
						// get the given JSON object from the array
						JSONObject curr_collision_obj = collisions_array.getJSONObject(k);

						// create a local model with the given name from the JSON obj
						Collision curr_collision = new Collision(curr_collision_obj.getString("name"));
						
//						// echo for prolog
//						System.out.println("\t\t\t" + curr_collision.getName());
						
//						// get the contacts JSON array
//						JSONArray contacts_array = collisions_array.getJSONObject(k).getJSONArray("contacts");
//						
//						TODO, not needed since we get the contacts at given timestamps in the future
//						//////////////////////////////////////////////////////////
//						// loop through all the contacts for the current collision
//						for(int l = 0; l < contacts_array.length(); l++)
//						{
//							// get the given JSON object from the array
//							JSONObject curr_contact_obj = contacts_array.getJSONObject(l);
//							
//							// create a local model with the given name fron the JSON obj
//							Contact curr_contact = new Contact(curr_contact_obj.getString("name"));
//							
//							// echo for prolog
//							System.out.println("\t\t\t\t" + curr_contact.getName());
//							
//							// add the current contact to the map, using the name as key value
//							curr_collision.addContact(curr_contact.getName(), curr_contact);
//						}
			
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
	 */
	public String[] getModelNames()
	{	
		// check if world is initialized, if not, get the world at the initial timestamp
		if (this.world == null)
		{
			this.setWorldState(A_TIMESTAMP);
		}
				
		// get the models map
		HashMap<String, Model> models = (HashMap<String, Model>) this.world.getModels();
		
		// a string array with all the model names
		String[] model_names = new String[models.size()];
		
		int i = 0;
		// loop through all the models and add their names to the string array
		for(String key_name : models.keySet())
		{
			model_names[i] = key_name;
			i++;
		}

		// return the string array with the model names
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
		
		HashMap<String, Link> links = (HashMap<String, Link>) this.world.getModels().get(model_name).getLinks();
		
		// a string array with all the model names
		String[] link_names = new String[links.size()];
		
		int i = 0;
		// loop through all the links and add their names to the string array
		for(String key_name : links.keySet())
		{
			link_names[i] = key_name;
			i++;
		}
		
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
		String[] collision_names = new String[collisions.size()];
		
		int i = 0;
		// loop through all the links and add their names to the string array
		for(String key_name : collisions.keySet())
		{
			collision_names[i] = key_name;
			i++;
		}
		
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
		String[] contact_names = new String[contacts.size()];
		
		int i = 0;
		// loop through all the links and add their names to the string array
		for (String key_name : contacts.keySet())
		{
			contact_names[i] = key_name;
			i++;
		}
		
//		contact_names = (String[]) contacts.keySet().toArray();		
		
		return contact_names;
	}
	
	
	/**
	 * 
	 * @param model_name
	 * @param timestamp
	 * @return
	 */
	public double[] getModelPose(String model_name, long timestamp)
	{
		// echo for prolog
		System.out.println("IJavaDB: getting models '" + model_name + "' pose at timestamp: " + timestamp);
		
		// pose, X Y Z R P Y
		double[] pose = new double[6];
		
		//long timestamp = A_TIMESTAMP;
		
		// query for getting the document at the given closest greater or equal than the timestamp
		BasicDBObject query = new BasicDBObject("timestamp", new BasicDBObject("$gte", timestamp));
	    
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
			System.out.println("IJavaDB: timestamp out of bounds");
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
	//////////////////////////////////////////////////////////
	public double[] getLinkPose(String link_name, long timestamp)
	{
		// echo for prolog
		System.out.println("IJavaDB: " + "getting link '" + link_name + "' pose at timestamp: " + timestamp);
		
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
			System.out.println("IJavaDB: timestamp out of bounds");
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
		System.out.println("IJavaDB: " + "calling  '" + model_name + "' boundingbox at timestamp: " + timestamp);
		
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
			System.out.println("IJavaDB: timestamp out of bounds");
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
	 * TODO NOW there are no contact duplicates, even if from different collisions
	 * TODO it returns the collisions regarding the model as a whole
	 * @param model_name
	 * @param timestamp
	 * @return
	 */
	public String[] getModelContactNames(String model_name, long timestamp){
		// echo for prolog
		System.out.println("IJavaDB: " + "getting contacts for  '" + model_name +"' .." );
		
		// needs a dynamic list since we do not no the exact nr of contacts
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
			System.out.println("IJavaDB: timestamp out of bounds");
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
	 * @param args
	 */

	public static void main(String[] args) 
	{				
		MongoPrologInterface mpi = new MongoPrologInterface(3);
		
//		System.out.println(mpi.getModelBoundingBox("mug", 15205000000L)[5]);
		
		mpi.getLinkPose("spatula_head_link", 15205000000L);
		
//		System.out.println(mpi.getModelBoundingBox("mug", 25205000000L)[5]);
//		System.out.println(mpi.getModelPose("mug", 25411001000L)[5]);
		
		mpi.getModelContactNames("spatula", 26411000000L);
		
//		World world = mpi.getWorldState(25205000000L);
		
//		System.out.println(world.getModels().get("mug").getLinks().
//				get("mug_link").getCollisions().get("mug_bottom_collision").
//				getContacts().keySet().toString());
		
//		
//		System.out.println(world.getModels().toString());
//		
//		System.out.println(world.getModels().get("spatula").getLinks().toString());
//		
//		System.out.println(world.getModels().get("spatula").getLinks().get("spatula_link").getCollisions().toString());
//		
//		System.out.println(world.getModels().get("spatula").
//				getLinks().get("spatula_link").getCollisions().get("spatula_handle_collision").getContacts().toString());
//		
//		String[] model_names = mpi.getModelNames();
//		for (int i = 0; i< model_names.length; i++)
//		{
//			System.out.println(model_names[i]);
//		}
		
//		String[] link_names = mpi.getLinkNames("spatula");
//		for (int i = 0; i< link_names.length; i++)
//		{
//			System.out.println(link_names[i]);
//		}
		
//		String[] collision_names = mpi.getCollisionNames("spatula", "spatula_link");
//		for (int i = 0; i< collision_names.length; i++)
//		{
//			System.out.println(collision_names[i]);
//		}
		
//		String[] contact_names = mpi.getContactNames("mug","mug_link","mug_backside_collision");
//		for (int i = 0; i< contact_names.length; i++)
//		{
//			System.out.println(contact_names[i]);
//		}

//		System.out.println(mpi.getModelPose("mug",25205000000L)[0]);
		
	}

}
