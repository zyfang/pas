/* Author: Andrei Haidu
 * Email: a.haidu@gmail.com
 */

package mongo_prolog;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Link extends Entity{

	private Map<String, Collision> collisions;
	
	private List<Collision> a_collisions; 
	
	/**
	 * Link constructor
	 * @param 	_name name of the link
	 */
	public Link(String _name){		
		super(_name);
		
		this.collisions = new HashMap<String, Collision>();
		
		this.setACollisions(new ArrayList<Collision>());
	}

	/**
	 * Return collisions map
	 * @return	collisions map
	 */
	public Map<String, Collision> getCollisions() {
		return collisions;
	}
	
	/**
	 * Set new collision map
	 * @param	_collisions set collisions map
	 */
	public void setCollisions(Map<String, Collision> _collisions) {
		this.collisions.clear();
		this.collisions.putAll(_collisions);
	}
	
	/**
	 * Add a new collision to the current link	 * 
	 * @param	_name the key of the map
	 * @param	_collision the collision object 
	 */
	public void addCollision(String _name, Collision _collision){
		this.collisions.put(_name, _collision);
	}


	
	///
	public List<Collision> getACollisions() {
		return a_collisions;
	}

	public void setACollisions(List<Collision> collisions) {
		this.a_collisions = collisions;
	}
	
	public void addACollision(Collision collision)
	{
		this.a_collisions.add(collision);
	}
	///
	
}
