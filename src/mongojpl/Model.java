/* Author: Andrei Haidu
 * Email: a.haidu@gmail.com
 */

package mongojpl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Model extends Entity{
	
	private Map<String, Link> links;
	
	//TODO
	private List<Link> a_links;	
	
	/**
	 * Model constructor
	 * @param 	_name	name of the model
	 */
	public Model(String _name){
		super(_name);
		
		this.links = new HashMap<String, Link>();
		
		this.a_links = new ArrayList<Link>();
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
	
	
	
	///
	public List<Link> getALinks() {
		return a_links;
	}

	public void setALinks(List<Link> links) {
		this.a_links = links;
	}
	
	public void addALink(Link link) {
		this.a_links.add(link);
	}
	///
}
