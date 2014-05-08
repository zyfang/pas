/* Author: Andrei Haidu
 * Email: a.haidu@gmail.com
 */

package actSimG_prologMongo;

public class Pose {	
	
	public Vector3 pos;
	public Vector3 rot;
	
	/**
	 * Constructor
	 */
	public Pose(){	
		this.pos = new Vector3();
		this.rot = new Vector3();
	}
	
	/**
	 * Constructor
	 * @param _pos
	 * @param _rot
	 */
	public Pose(Vector3 _pos, Vector3 _rot){
		this.pos = _pos;
		this.rot = _rot;
	}
	
	@Override 
	public String toString() {
		return pos.toString() + " " + rot.toString(); 
	}
	
}
