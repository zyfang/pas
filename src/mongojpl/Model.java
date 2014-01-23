package mongojpl;

import java.util.ArrayList;
import java.util.List;

public class Model extends Entity{
	
	private List<Link> links;	
	private List<Contact> contacts;
	
	public Model(String name)
	{
		super(name);
		this.links = new ArrayList<Link>();
		this.contacts = new ArrayList<Contact>();
	}
	
	
	public List<Link> getLinks() {
		return links;
	}

	public void setLinks(List<Link> links) {
		this.links = links;
	}
	
	public void addLink(Link link) {
		this.links.add(link);
	}

	public List<Contact> getCollisions() {
		return contacts;
	}

	public void setCollisions(List<Contact> collisions) {
		this.contacts = collisions;
	}
		
}
