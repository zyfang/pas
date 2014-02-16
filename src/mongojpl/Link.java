/* Author: Andrei Haidu
 * Email: a.haidu@gmail.com
 */

package mongojpl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Link extends Entity{
	
	private Map<String, Collision> collisions;
	
	private List<Collision> a_collisions; 
	
	public Link(String name){
		super(name);
		this.setCollisions(new HashMap<String, Collision>());
		
		this.setACollisions(new ArrayList<Collision>());
	}

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

	public Map<String, Collision> getCollisions() {
		return collisions;
	}

	public void setCollisions(Map<String, Collision> collisions) {
		this.collisions = collisions;
	}

	
}
