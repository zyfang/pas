package mongojpl;

import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

import com.mongodb.DB;
import com.mongodb.DBCollection;
import com.mongodb.DBObject;
import com.mongodb.MongoClient;

public class MongoPrologInterface {
	
	private MongoClient mongoClient;
	private DB db;
	private DBCollection coll;
	
	/** 
	 * Constructor
	 * 
	 * Initialize DB client and connect to the mongo database.
	 * 
	 */
	public MongoPrologInterface() {
		try {
			mongoClient = new MongoClient( "localhost" , 27017 );

			db = mongoClient.getDB("sim_db");

			coll = db.getCollection("collection_13417_copy");

		} catch (UnknownHostException e) {
			e.printStackTrace();
		}
	}
	
	//////////////////////////////////////////////////////////
	public void setModels(List<Model> models)
	{
		// get the whole first document
		DBObject first_doc = coll.findOne();	
		
		//System.out.println(first_doc);
		
		JSONTokener tokener = new JSONTokener(first_doc.toString());
		
		try {
			// get the JSON root object
			JSONObject root_obj = new JSONObject(tokener);
			
			// get the models JSON array from the JSON root object
			JSONArray models_array = root_obj.getJSONArray("models");
			
			// loop through all the models array
			for(int i = 0; i < models_array.length(); i++)
			{
				// get the given JSON object from the array
				JSONObject curr_model_obj = models_array.getJSONObject(i);
				
				// create a local model with the given name from the JSON obj
				Model curr_model = new Model(curr_model_obj.getString("name"));
				
				// get the links JSON array from the current JSON obj 
				JSONArray links_array = models_array.getJSONObject(i).getJSONArray("links");
				
				// loop through all the links for the current model
				for(int j = 0; j < links_array.length(); j++)
				{
					// get the given JSON object from the array
					JSONObject curr_link_obj = links_array.getJSONObject(j);
				
					// create a local link with the given name from the JSON obj
					Link curr_link = new Link( curr_link_obj.getString("name"));
					
					// add the current link to the models link list
					curr_model.addLink(curr_link);					
				}
				
				// add current model to the models list
				models.add(curr_model);
			}		
			
		} catch (JSONException e) {
			e.printStackTrace();
		}
		
	}	
	
	
	public static void main(String[] args) 
	{
		List<Model> models = new ArrayList<Model>();
				
		MongoPrologInterface mpi = new MongoPrologInterface();
		
		mpi.setModels(models);
		
		for(int i = 0; i < models.size(); i++)
		{
			System.out.println(models.get(i).getName());
			List<Link> links = models.get(i).getLinks();
			
			for(int j = 0; j < links.size(); j++)
			{
				System.out.println("\t" + links.get(j).getName());
			}
			
		}
	}

}
