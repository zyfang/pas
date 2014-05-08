/* Author: Andrei Haidu
 * Email: a.haidu@gmail.com
 */

package mongo_prolog;

import java.util.HashMap;
import java.util.Map;

public class Model extends Entity{
	
	private Map<String, Link> links;
	
	/**
	 * Model constructor
	 * @param 	_name	name of the model
	 */
	public Model(String _name){
		super(_name);
		
		this.links = new HashMap<String, Link>();
	}	

	/**
	 * Get links
	 * @return links of the model
	 */
	public Map<String, Link> getLinks() {
		return this.links;
	}

	/**
	 * Set new links
	 * @param 	_links set links map
	 */
	public void setLinks(Map<String, Link> _links) {
		this.links.clear();
		this.links.putAll(_links);
	}
	
	/**
	 * Add a link to the map
	 * @param 	_name key of the map
	 * @param 	_link link to add to the map
	 */
	public void addLink(String _name, Link _link)
	{
		this.links.put(_name, _link);
	}
}
