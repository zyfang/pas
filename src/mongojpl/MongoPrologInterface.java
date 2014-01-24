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

	//////////////////////////////////////////////////////////
	public MongoPrologInterface() {
		try {
			// create a new DB client
			this.mongoClient = new MongoClient( "localhost" , 27017 );

			// get the given DB
			this.db = mongoClient.getDB("sim_db");

			// get the given collection from the DB
			this.coll = this.db.getCollection("collection_13417_copy");

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

	//////////////////////////////////////////////////////////
	public String[] getModelNames()
	{
		// create a local model list
		List<Model> models = new ArrayList<Model>();
		
		// set the model list
		this.setModels(models);
		
		// a string array with all the model names
		String[] model_names = new String[models.size()];

		// loop through all the models and add their names to the string array
		for(int i = 0; i < models.size(); i++)
		{
			model_names[i] = models.get(i).getName();
		}

		// return the string array with the model names
		return model_names;
	}
	
	//////////////////////////////////////////////////////////
	public String[] getLinkNames(String model_name)
	{
		// create a local model list
		List<Model> models = new ArrayList<Model>();
		
		// set the model list
		this.setModels(models);		
		
		// loop through all the models until the given name is found
		for(int i = 0; i < models.size(); i++)
		{
			// compare model names 
			if (model_name.equals(models.get(i).getName()))
			{
				// if names correspond create a local link array
				List<Link> links = new ArrayList<Link>();
				
				// set the link array with the models links
				links = models.get(i).getLinks();

				// a string array with the models all link names
				String[] link_names = new String[links.size()];
				
				// loop through all the links and add their name to the string array
				for(int j = 0; j < links.size(); j++)
				{
					link_names[j] = links.get(j).getName();
				}
				
				// break loop and return the string array
				return link_names;
			}
		}

		// return null if the given name does not match anything
		return null;
	}

	public String returnAString()
	{
		return "hit_hand_mock";
	}
	
	public String returnGivenString(String string)
	{
		return string;
	}
	
	//////////////////////////////////////////////////////////
//	public static void main(String[] args) 
//	{				
//		MongoPrologInterface mpi = new MongoPrologInterface();
//
//		//		List<Model> models = new ArrayList<Model>();
//		//		
//		//		mpi.setModels(models);
//		//		
//		//		for(int i = 0; i < models.size(); i++)
//		//		{
//		//			System.out.println(models.get(i).getName());
//		//			List<Link> links = models.get(i).getLinks();
//		//			
//		//			for(int j = 0; j < links.size(); j++)
//		//			{
//		//				System.out.println("\t" + links.get(j).getName());
//		//			}
//		//			
//		//		}
//		
//		String[] model_names = mpi.getModelNames();
//		for(int i = 0; i < model_names.length; i++)
//		{
//			System.out.println(model_names[i]);
//		}
//		
//		String[] link_names = mpi.getLinkNames("hit_hand");
//		
//		for(int i = 0; i < link_names.length; i++)
//		{
//			System.out.println(link_names[i]);
//		}
//		
//	}

}
