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
	
	private List<Link> a_links;	
	
	public Model(String name)
	{
		super(name);
		this.setLinks(new HashMap<String, Link>());
		
		this.a_links = new ArrayList<Link>();
	}	
	
	public List<Link> getALinks() {
		return a_links;
	}

	public void setALinks(List<Link> links) {
		this.a_links = links;
	}
	
	public void addALink(Link link) {
		this.a_links.add(link);
	}

	public Map<String, Link> getLinks() {
		return links;
	}

	public void setLinks(Map<String, Link> links) {
		this.links = links;
	}
}
