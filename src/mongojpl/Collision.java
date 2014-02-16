/* Author: Andrei Haidu
 * Email: a.haidu@gmail.com
 */

package mongojpl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Collision extends Entity{
	
	private Map<String, Contact> collisions;

	private List<Contact> a_contacts;
	
	public Collision(String name) {
		super(name);
		
		this.setCollisions(new HashMap<String, Contact>());
		
		this.a_contacts = new ArrayList<Contact>();
	}

	public List<Contact> getAContacts() {
		return a_contacts;
	}

	public void setAContacts(List<Contact> contacts) {
		this.a_contacts = contacts;
	}

	public Map<String, Contact> getCollisions() {
		return collisions;
	}

	public void setCollisions(Map<String, Contact> collisions) {
		this.collisions = collisions;
	}	
}
