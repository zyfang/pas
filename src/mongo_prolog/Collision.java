/* Author: Andrei Haidu
 * Email: a.haidu@gmail.com
 */

package mongo_prolog;

import java.util.HashMap;
import java.util.Map;

public class Collision extends Entity{

	private Map<String, Contact> contacts;
	
	/**
	 * Collision constructor
	 * @param _name
	 */
	public Collision(String _name) {
		super(_name);
		
		this.contacts = new HashMap<String, Contact>();
	}

	/**
	 * get the contacts map
	 * @return
	 */
	public Map<String, Contact> getContacts() {
		return contacts;
	}

	/**
	 * Set new contacts map
	 * @param _contacts
	 */
	public void setContacts(Map<String, Contact> _contacts) {
		this.contacts.clear();
		this.contacts.putAll(_contacts);
	}
	
	/**
	 * Add new contact to the map
	 * @param _name
	 * @param _contact
	 */
	public void addContact(String _name, Contact _contact){
		this.contacts.put(_name, _contact);
	}
}
