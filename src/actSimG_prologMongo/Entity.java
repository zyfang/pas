/* Author: Andrei Haidu
 * Email: a.haidu@gmail.com
 */

package mongo_prolog;

public class Entity {
	
	private String name;
	private Pose pose;

	/**
	 * Entity constructor
	 * @param _name
	 */
	public Entity(String _name){
		this.name = _name;		
	}

	/**
	 * Get the name of the entity
	 */
	public String getName() {
		return this.name;
	}
	
	/**
	 * Set the name of the entity
	 * @param _name
	 */
	public void setName(String _name) {
		this.name = _name;
	}
	
	/**
	 * get the pose of the entity
	 * @return
	 */
	public Pose getPose() {
		return this.pose;
	}
	
	/**
	 * set the pose of the entity
	 * @param pose
	 */
	public void setPose(Pose pose) {
		this.pose = pose;
	}
	

}
