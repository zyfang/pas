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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

import com.mongodb.BasicDBObject;
import com.mongodb.DB;
import com.mongodb.DBCollection;
import com.mongodb.DBCursor;
import com.mongodb.DBObject;
import com.mongodb.MongoClient;
import com.mongodb.util.JSON;

public class MongoPrologInterface_andrei {

	private static final long A_TIMESTAMP = 0L;

	private MongoClient mongoClient;
	private DB db;
	private DBCollection coll;
	private World world;


	/**
	 * MongoPrologInterface constructor
	 * @param collection 
	 */
	public MongoPrologInterface_andrei(int collection) {		
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
	 * Takes two JSONArrays and looks for each element in "addition" whether it is already present in "original".
	 * Returns a list of strings with values from the "field" field of the JSONObjects in "addition" that were not in the "original" array.
	 * 
	 * Called by: 	getAllContacts(File outputfile)
	 * Calls: 		-
	 * 
	 * @param original
	 * @param addition
	 * @return
	 * @throws JSONException 
	 */
	public List<String> intersectionJSONLS(JSONArray original, JSONArray addition, String field) throws JSONException
	{
		List<String> result = new ArrayList<String>();
		for(int i=0; i<addition.length();i++)//check which contacts are in the addition that were not in the original
		{
			JSONObject adobj = addition.getJSONObject(i);
			boolean contains = false;
			for(int j=0; j<original.length(); j++)
			{
				if(original.getJSONObject(j).getString(field).equals(adobj.getString(field)))
				{
					contains = true;
					break;
				}
			}
			if(!contains)
			{
				result.add(adobj.getString(field));
			}
		}
		return result;
	}

	/**
	 * Helper function for printing out contactchanges to the console and to a textfile (will later be the PROLOG database).
	 * 
	 * Called by:	getAllContacts(File outputfile)
	 * Calls: 		-
	 * 
	 * @param changes
	 * @param model
	 * @param link
	 * @param collision
	 * @param time
	 * @param bw
	 * @param sformat
	 * @throws JSONException 
	 * @throws IOException 
	 */
	public void printChanges(List<String> changes, JSONObject model, JSONObject link, JSONObject collision, long time, BufferedWriter bw, String[] sformat) throws JSONException, IOException
	{
		if(changes.size()> 0)
		{
			System.out.print(model.getString("name") + "->");
			System.out.print(link.getString("name") + "->");
			System.out.print(collision.getString("name") + " has "+ sformat[0] +"contact with: ");
			for (String ichange : changes)
			{
				System.out.print(ichange + "; ");
				String outstring = String.format(sformat[1],time,model.getString("name"),ichange);
				bw.write(outstring);
			}
			System.out.println();
		}
	}

	/**
	 * 
	 * Get a list of all documents where the contact has changed compared to the previous one
	 * 
	 * TODO: I don't know whether JSONarrays return objects always in the same order, therefore it might be dangerous to compare
	 * The models of previous and current the way I'm doing now. Look into this later.
	 * 
	 * TODO: for now assumed that everything except contacts stays the same (so always the same models are present, with the same links and same collisions)
	 * 
	 * TODO: write a recursive help function to get to the right level of the JSONobjects (you can input the name of the level and it will keep going until it gets there)
	 * 
	 * @param out_fw
	 * @throws JSONException
	 * @throws IOException
	 */
	public void getAllContacts(File outputfile) throws JSONException, IOException
	{
		//for writing contact changes to file
		FileWriter fw = new FileWriter(outputfile.getAbsoluteFile());
		BufferedWriter bw = new BufferedWriter(fw);

		List<DBObject> contactChanges = new ArrayList<DBObject>();
		BasicDBObject query = new BasicDBObject();
		BasicDBObject fields = new BasicDBObject("models.links.collisions.contacts.name", 1);
		fields.append("models.links.name", 1);
		fields.append("models.links.collisions.name", 1);
		fields.append("models.name", 1);
		fields.append("timestamp", 1);
		fields.append("_id", 0);
		DBCursor doccursor = coll.find(query,fields).limit(30);
		JSONArray prev_mods = null;
		try {
			while(doccursor.hasNext()) {
				//get the next entry
				DBObject dbdoc = doccursor.next();
				//convert to JSON
				JSONTokener tokener = new JSONTokener(dbdoc.toString());
				JSONObject db_jsonobj = new JSONObject(tokener);
				JSONArray cur_allmods = db_jsonobj.getJSONArray("models");
				long currenttime = db_jsonobj.getLong("timestamp")/1000000;

				//look through all models
				if(prev_mods == null) //if this is the first iteration, always print out
				{
					System.out.println("Timestamp: " + currenttime);
					for(int i=0; i< cur_allmods.length();i++) {
						JSONObject curmod = cur_allmods.getJSONObject(i);
						System.out.println(curmod);
					}
				}
				else //otherwise check for each model whether the contacts changed
				{
					//There are a few options what to return: 
					// 1) Return all model names for which the contacts have changed (but this doesn't provide us with a lot of detailed info
					// 2) Return all contacts that that have changed
					for(int i=0; i<cur_allmods.length();i++) {
						JSONObject curmod = cur_allmods.getJSONObject(i);
						JSONObject prevmod = prev_mods.getJSONObject(i);
						//TODO: because the orders can be different, sometimes it gets into the if clause, but
						//doesn't find any contacts that have changed (the latter is the right behavior). Upon inspection it does
						//recognize that nothing has changed because we check the names instead of the strings.
						//For robustness it would be better to solve this issue however.
						if (!curmod.toString().equals(prevmod.toString())) //we know something has changed in the contacts the model has
						{
							System.out.println("Timestamp: " + currenttime);
							//System.out.println(curmod); //for manually checking whether the same; sometimes the entries are the same but the orders different and it still gets here
							//System.out.println(prevmod);
							JSONArray cur_alllink = curmod.getJSONArray("links"); //check which contacts have changed
							JSONArray prev_alllink = prevmod.getJSONArray("links");
							for(int j=0; j<cur_alllink.length();j++)
							{
								JSONObject curlink = cur_alllink.getJSONObject(j);

								JSONArray cur_allcollis = curlink.getJSONArray("collisions");
								JSONArray prev_allcollis = prev_alllink.getJSONObject(j).getJSONArray("collisions");
								for(int k=0; k<cur_allcollis.length();k++) 
								{
									JSONObject curcollis = cur_allcollis.getJSONObject(k);

									JSONArray cur_allcont = cur_allcollis.getJSONObject(k).getJSONArray("contacts");
									JSONArray prev_allcont = prev_allcollis.getJSONObject(k).getJSONArray("contacts");

									List<String> added_contacts = intersectionJSONLS(prev_allcont, cur_allcont, "name");
									List<String> removed_contacts = intersectionJSONLS(cur_allcont, prev_allcont, "name");

									//given that added and removed contacts have different string outputs, send a stringformat to the function
									//the first string is for the console output, the second for the file output. 
									String[] stringdescr = {"initiated", "occurs(d1,%d,contact(%s,%s)))\n"}; 
									printChanges(added_contacts,curmod,curlink,curcollis,currenttime, bw, stringdescr);
									stringdescr[0] = "broken"; 
									stringdescr[1]= "occurs(d1,%d,not(contact(%s,%s)))\n";
									printChanges(removed_contacts,curmod,curlink,curcollis,currenttime,bw,stringdescr);																
								}//for all collisions
							}//for all links
						}//if there might be a difference
					}//for all models
				}//if not the first one
				prev_mods = cur_allmods;
				//String test = dbobj.getString("models");
				contactChanges.add(dbdoc);
				//System.out.println(dbdoc);
			}
		} finally {
			doccursor.close();
			bw.close();
		}
		//System.out.println("Found this many changes:" + contactChanges.size());
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
	 * TODO NOW there are no contact duplicates, even if from different collisions
	 * TODO it returns the collisions regarding the model as a whole
	 * @param model_name
	 * @param timestamp
	 * @return
	 */
	public String[] getModelContactNames(String model_name, long timestamp){
		// echo for prolog
		//		System.out.println("IJavaDB: " + "getting contacts for  '" + model_name +"' .." );

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
	 * @param _event
	 * @return
	 */
	public long[] getManipulationEventTimestamps(String _event){
		// echo for prolog
		//		System.out.println("IJavaDB: getting timestamps for " + _event + " event");

		// start and end timestamp
		long[] timestamps = new long[2];

		// query used for MongoDB
		BasicDBObject query;

		// TODO make it nicer
		if (_event.equals("pour"))
		{
			// set the START timestamp
			query = new BasicDBObject("events.pour","StartPouring");	

			// find the first document for the query (it should only be one)
			DBObject start_doc = coll.findOne(query);

			// extracts characters and tokens from given string
			JSONTokener tokener = new JSONTokener(start_doc.toString());

			try {
				// get the JSON root object
				JSONObject root_obj = new JSONObject(tokener);	

				// set the start timestamp
				timestamps[0] = root_obj.getLong("timestamp");

			}catch (JSONException e) {
				e.printStackTrace();
			}	


			// set the END timestamp
			query = new BasicDBObject("events.pour","EndPouring");	

			// find the first document for the query (it should only be one)
			DBObject end_doc = coll.findOne(query);

			// extracts characters and tokens from given string
			tokener = new JSONTokener(end_doc.toString());

			try {
				// get the JSON root object
				JSONObject root_obj = new JSONObject(tokener);	

				// set the end timestamp
				timestamps[1] = root_obj.getLong("timestamp");

			}catch (JSONException e) {
				e.printStackTrace();
			}	

		}
		else if (_event.equals("flip"))
		{
			// set the START timestamp
			query = new BasicDBObject("events.flip","StartFlipping");	

			// find the first document for the query (it should only be one)
			DBObject start_doc = coll.findOne(query);

			// extracts characters and tokens from given string
			JSONTokener tokener = new JSONTokener(start_doc.toString());

			try {
				// get the JSON root object
				JSONObject root_obj = new JSONObject(tokener);	

				// set the start timestamp
				timestamps[0] = root_obj.getLong("timestamp");

			}catch (JSONException e) {
				e.printStackTrace();
			}	


			// set the END timestamp
			query = new BasicDBObject("events.flip","EndFlipping");	

			// find the first document for the query (it should only be one)
			DBObject end_doc = coll.findOne(query);

			// extracts characters and tokens from given string
			tokener = new JSONTokener(end_doc.toString());

			try {
				// get the JSON root object
				JSONObject root_obj = new JSONObject(tokener);	

				// set the end timestamp
				timestamps[1] = root_obj.getLong("timestamp");

			}catch (JSONException e) {
				e.printStackTrace();
			}	

		}		
		return timestamps;
	}


	/**
	 * TODO it returns string, since hashmap can only have objects as keys and values, and prolog only recognizes .String as primitive
	 * @return
	 */
	public HashMap<String, String> getParticlesLeavingContainer()
	{
		// echo for prolog
		//		System.out.println("IJavaDB: getting timestamps for particle leaves container event..");

		// store the particle - timestamp
		HashMap<String, String> particle_map = new HashMap<String, String>();

		// query used for MongoDB
		BasicDBObject query = new BasicDBObject("events.leaves_container", new BasicDBObject("$exists", "true"));

		BasicDBObject fields = new BasicDBObject("events", 1);
		fields.append("timestamp", 1);

		// cursor for the returned values
		DBCursor cursor = coll.find(query, fields);

		// loop through the cursor to get all values
		try {
			while (cursor.hasNext()){				

				// extracts characters and tokens from given string
				JSONTokener tokener = new JSONTokener(cursor.next().toString());

				// get the JSON root object
				JSONObject root_obj = new JSONObject(tokener);

				// add values to HashMap, particle collision name - timestamp 
				particle_map.put( root_obj.getJSONObject("events").getString("leaves_container"),String.valueOf(root_obj.getLong("timestamp")));

			}
		} catch (JSONException e) {
			e.printStackTrace();
		} finally{
			cursor.close();
		}

		return particle_map;
	}


	/**
	 * 
	 * @param obj_name
	 * @param start
	 * @param end
	 * @return
	 */
	public JSONArray getObjectTracjectory(String model_name, long start, long end)
	{				
		// query for getting the document between the given timestamp
		BasicDBObject query = new BasicDBObject("timestamp", new BasicDBObject("$gte", start).append("$lte", end));

		// fields for projecting only the pose of the given model name
		BasicDBObject fields = new BasicDBObject("_id", 0);
		fields.append("models", new BasicDBObject("$elemMatch", new BasicDBObject("name", model_name)));
		fields.append("models.pos", 1);
		fields.append("models.rot", 1);
		fields.append("timestamp", 1);

		// find all the matching documents
		DBCursor cursor = coll.find(query, fields);

		// tf trajectory array
		JSONArray tf_traj_arr = new JSONArray();

		// loop through the found results
		while (cursor.hasNext()){

			// tf json object
			JSONObject tf_js = new JSONObject();			
			// tf header
			JSONObject tf_header_js = new JSONObject();
			// the tf transformation
			JSONObject tf_transform_js = new JSONObject();

			// get the current document
			JSONTokener tokener = new JSONTokener(cursor.next().toString());

			try {				
				// get the JSON root object
				JSONObject root_obj = new JSONObject(tokener);

				// get the current timestamp
				long timestamp = root_obj.getLong("timestamp");

				// get the models array (it's only one value but of type array)
				JSONArray models_array = root_obj.getJSONArray("models");

				// get the position from the array
				JSONObject json_pos = models_array.getJSONObject(0).getJSONObject("pos");

				// get the orientation from the array
				JSONObject json_rot = models_array.getJSONObject(0).getJSONObject("rot");

				// set the tf transf pose
				tf_transform_js.put("translation", json_pos);
				tf_transform_js.put("rotation", json_rot);

				tf_header_js.put("stamp", timestamp);
				tf_header_js.put("frame_id", "/map");


				tf_js.put("transform",tf_transform_js);
				tf_js.put("header",tf_header_js);
				tf_js.put("child_frame_id","/" + model_name);


				tf_traj_arr.put(tf_js);

			} catch (JSONException e) {
				e.printStackTrace();
			}

		}

		return tf_traj_arr;
	}	

	/**
	 * TODO! HARDCODED INTERVALS 
	 */	
	public long getFlipStart(int CollectionNr)
	{
		if (CollectionNr == 1)
			return 26267000000L;
		else if(CollectionNr == 2)
			return 19643000000L;
		else if(CollectionNr == 3)
			return 31046000000L;
		else if(CollectionNr == 4)
			return 27713000000L;
		else if(CollectionNr == 5)
			return 17235000000L;

		else return 0;
	}

	public long getFlipEnd(int CollectionNr)
	{
		if (CollectionNr == 1)
			return 33370000000L;
		else if(CollectionNr == 2)
			return 23338000000L;
		else if(CollectionNr == 3)
			return 38742000000L;
		else if(CollectionNr == 4)
			return 35873000000L;
		else if(CollectionNr == 5)
			return 21598000000L;

		else return 0;
	}

	public long getMixLeavingContainer(int CollectionNr)
	{
		if (CollectionNr == 1)
			return 26267000000L;
		else if(CollectionNr == 2)
			return 19643000000L;
		else if(CollectionNr == 3)
			return 31046000000L;
		else if(CollectionNr == 4)
			return 27713000000L;
		else if(CollectionNr == 5)
			return 17235000000L;

		else return 0;
	}

	public long getMixOnOven(int CollectionNr)
	{
		if (CollectionNr == 1)
			return 33370000000L;
		else if(CollectionNr == 2)
			return 23338000000L;
		else if(CollectionNr == 3)
			return 38742000000L;
		else if(CollectionNr == 4)
			return 35873000000L;
		else if(CollectionNr == 5)
			return 21598000000L;

		else return 0;
	}



	/**
	 * 
	 * @param args
	 */
	public static void main(String[] args) 
	{				
		//		MongoPrologInterface mpi = new MongoPrologInterface(3);
		MongoPrologInterface_andrei mpi2 = new MongoPrologInterface_andrei(1);

		//		mpi2.drawTrajectory("mug", 0, 1234253235);

		System.out.println(mpi2.getModelPose("mug", 30629000000L)[0]);
		try {
			File outputfile = new File("/home/yfang/javamongotestout2.txt");
			if(!outputfile.exists())
			{
				outputfile.createNewFile();
			}
			long startTime = System.nanoTime();
			mpi2.getAllContacts(outputfile);
			long endTime = System.nanoTime();
			long duration =endTime-startTime;
			System.out.println(duration);
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

			//		System.out.println(mpi2.getEventTimestamps("flip")[0]);

			//		System.out.println(mpi2.getParticlesLeavingContainer());

			//		System.out.println(mpi2.getObjectTracjectory("mug",0,300L));

			//		System.out.println(mpi.getModelBoundingBox("mug", 15205000000L)[5]);

			//		mpi.getLinkPose("spatula_head_link", 15205001000L);

			//		System.out.println(mpi.getModelBoundingBox("mug", 25205000000L)[5]);
			//		System.out.println(mpi.getModelPose("mug", 25411001000L)[5]);

			//		mpi.getModelContactNames("spatula", 26411000000L);

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
