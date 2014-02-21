/* Author: Andrei Haidu
 * Email: a.haidu@gmail.com
 */

package mongojpl;

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
		
		System.out.println(_doc);
		
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
		// create a local model list
		List<Model> _models = new ArrayList<Model>();
		
		// set the model list
		this.setUpEntities(_models);
		
		// a string array with all the model names
		String[] model_names = new String[_models.size()];

		// loop through all the models and add their names to the string array
		for(int i = 0; i < _models.size(); i++)
		{
			model_names[i] = _models.get(i).getName();
		}

		// return the string array with the model names
		return model_names;
	}
		
	
	//////////////////////////////////////////////////////////
	public String[] getLinkNames(String model_name)
	{
		// create a local model list
		List<Model> _models = new ArrayList<Model>();
		
		// set the model list
		this.setUpEntities(_models);		
		
		// loop through all the models until the given name is found
		for(int i = 0; i < _models.size(); i++)
		{
			// compare model names 
			if (model_name.equals(_models.get(i).getName()))
			{
				// if names correspond create a local link array
				List<Link> _links = new ArrayList<Link>();
				
				// set the link array with the models links
				_links = _models.get(i).getALinks();

				// a string array with all link names of the given model
				String[] link_names = new String[_links.size()];
				
				// loop through all the links and add their name to the string array
				for(int j = 0; j < _links.size(); j++)
				{
					link_names[j] = _links.get(j).getName();
				}
				
				// break loop and return the string array
				return link_names;
			}
		}

		// return null if the given name does not match anything
		return null;
	}
	
	
	//////////////////////////////////////////////////////////
	public String[] getCollisionNames(String link_name)
	{
		// create a local model list
		List<Model> _models = new ArrayList<Model>();

		// set the model list
		this.setUpEntities(_models);		

		// loop through all the models until the given name is found
		for(int i = 0; i < _models.size(); i++)
		{
			// if names correspond create a local link array
			List<Link> _links = new ArrayList<Link>();

			// set the link array with the models links
			_links = _models.get(i).getALinks();

			// loop through all the links until the given name is found
			for(int j = 0; j < _links.size(); j++)
			{

				// compare link names 
				if (link_name.equals(_links.get(j).getName()))
				{
					// if link names correspond create a local collision array
					List<Collision> _collisions = new ArrayList<Collision>();
					
					// set collision array with the collisions of the current link
					_collisions = _links.get(j).getACollisions();

					// a string array with all the collisions of the link
					String[] collision_names = new String[_collisions.size()];

					// loop through all the links and add their name to the string array
					for(int k = 0; k < _collisions.size(); k++)
					{
						collision_names[k] = _collisions.get(k).getName();
					}

					// break loop and return the string array

					return collision_names;
				}
			}
		}

		// return null if the given name does not match anything
		return null;
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
		
		World world = mpi.getWorldState(25227000000L);
		
		System.out.println(world.getModels().toString());
		
		System.out.println(world.getModels().get("spatula").getLinks().toString());
		
		System.out.println(world.getModels().get("spatula").getLinks().get("spatula_link").getCollisions().toString());
		
		System.out.println(world.getModels().get("spatula").
				getLinks().get("spatula_link").getCollisions().get("spatula_handle_collision").getContacts().toString());

	}

}
