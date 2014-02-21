package mongojpl;

import java.util.HashMap;
import java.util.Map;

public class World {
	
	private Map<String, Model> models;
	
	/**
	 * World constructor
	 */
	public World(){
		this.models = new HashMap<String, Model>();
	}

	/**
	 * Get models map
	 * @return
	 */
	public Map<String, Model> getModels() {
		return models;
	}

	/**
	 * Set new models map
	 * @param _models
	 */
	public void setModels(Map<String, Model> _models) {
		this.models.clear();
		this.models.putAll(_models);
	}
	
	/**
	 * Add new model to the map
	 * @param _name
	 * @param _model
	 */
	public void addModel(String _name, Model _model){
		this.models.put(_name, _model);
	}

}
