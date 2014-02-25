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

	private MongoClient mongoClient;
	private DB db;
	private DBCollection coll;
	private World world;

	
	//////////////////////////////////////////////////////////
	public MongoPrologInterface() {
		try {
			// create a new DB client
			this.mongoClient = new MongoClient( "localhost" , 27017 );

			// get the given DB
			this.db = mongoClient.getDB("sim_db");

			// get the given collection from the DB
			this.coll = this.db.getCollection("collection_X");

		} catch (UnknownHostException e) {
			e.printStackTrace();
		}
	}

	//////////////////////////////////////////////////////////	
	public World getWorldState(long _timestamp){
		
		// local models map
		World world = new World();		
		
		// query for getting the document at the given timestamp
		BasicDBObject _query = new BasicDBObject("timestamp", _timestamp);
		
		// get document at given timestamp
		DBObject _doc = coll.findOne(_query);
		
//		System.out.println(_doc);
		
		// extracts characters and tokens from given string
		JSONTokener tokener = new JSONTokener(_doc.toString());
		
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
						
						// get the contacts JSON array
						JSONArray contacts_array = collisions_array.getJSONObject(k).getJSONArray("contacts");
						
						
						//////////////////////////////////////////////////////////
						// loop through all the contacts for the current collision
						for(int l = 0; l < contacts_array.length(); l++)
						{
							// get the given JSON object from the array
							JSONObject curr_contact_obj = contacts_array.getJSONObject(l);
							
							// create a local model with the given name fron the JSON obj
							Contact curr_contact = new Contact(curr_contact_obj.getString("name"));
							
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
				world.addModel(curr_model.getName(), curr_model);
				
			}
			
		} catch (JSONException e) {
			e.printStackTrace();
		}
		
		return world;
	}
	
	//////////////////////////////////////////////////////////
	public String[] getModelNames()
	{	
		// get the models in the initial timestamp
		long _timestamp = 0L;
		
		// get the world at the given timestamp
		this.world = this.getWorldState(_timestamp);
				
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
		
	
	//////////////////////////////////////////////////////////
	public String[] getLinkNames(String model_name)
	{
		// check if world is initialized
		if (this.world == null)
		{
			// get the world at the initial timestamp
			this.world = this.getWorldState(0);
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
	
	
	//////////////////////////////////////////////////////////
	public String[] getCollisionNames(String model_name, String link_name)
	{
		// check if world is initialized
		if (this.world == null)
		{
			// get the world at the initial timestamp
			this.world = this.getWorldState(0);
		}
		
		HashMap<String, Collision> collisions =	(HashMap<String, Collision>) 
				this.world.getModels().get(model_name).getLinks().get(link_name).getCollisions();
		
		// a string array with all the model names
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
	
	
	//////////////////////////////////////////////////////////
	public String getModelPose(String model_name/*, long timestamp*/)
	{
		Pose _pose = new Pose();
		long timestamp = 3784000000L;
		
		// query for getting the document at the given timestamp
		BasicDBObject query = new BasicDBObject("time.timestamp", timestamp);		
	    
		// fields for projecting only the pose of the given model name
		BasicDBObject fields = new BasicDBObject("_id", 0);
		fields.append("models", new BasicDBObject("$elemMatch", new BasicDBObject("name", model_name)));
		fields.append("models.pose", 1);
		
		// find the first document for the query (it should only be one)
		DBObject first_doc = coll.findOne(query, fields);	
		
		//System.out.println(first_doc);
		
		// extracts characters and tokens from given string
		JSONTokener tokener = new JSONTokener(first_doc.toString());
		
		try {
			// get the JSON root object
			JSONObject root_obj = new JSONObject(tokener);
			
			// get the models array (is only one value but of type array)
			JSONArray models_array = root_obj.getJSONArray("models");

			// get the pose from the array
			JSONObject pose = models_array.getJSONObject(0).getJSONObject("pose");
			
			// get the values from the pose
			double x 		= pose.getDouble("x");
			double y 		= pose.getDouble("y");
			double z 		= pose.getDouble("z");
			double roll 	= pose.getDouble("roll");
			double pitch 	= pose.getDouble("pitch");
			double yaw 		= pose.getDouble("yaw");

			_pose.setPose(x, y, z, roll, pitch, yaw);
					
		} catch (JSONException e) {
			e.printStackTrace();
		}
		
		return _pose.toString();
	}
	

	//////////////////////////////////////////////////////////
	public static void main(String[] args) 
	{				
		MongoPrologInterface mpi = new MongoPrologInterface();
		
//		World world = mpi.getWorldState(25227000000L);
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
		
		String[] link_names = mpi.getLinkNames("spatula");
		for (int i = 0; i< link_names.length; i++)
		{
			System.out.println(link_names[i]);
		}
		
//		String[] collision_names = mpi.getCollisionNames("spatula", "spatula_link");
//		for (int i = 0; i< collision_names.length; i++)
//		{
//			System.out.println(collision_names[i]);
//		}

	}

}
