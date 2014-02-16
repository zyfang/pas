/* Author: Andrei Haidu
 * Email: a.haidu@gmail.com
 */

package mongojpl;

public class Contact{

	private Collision collision1;
	private Collision collision2;
	
	// constructor
	public Contact( Collision _col1, Collision _col2){	
		this.collision1 = _col1;
		this.collision2 = _col2;
	}
	
	public Collision getCollision1() {
		return collision1;
	}
	public void setCollision1(Collision collision1) {
		this.collision1 = collision1;
	}
	public Collision getCollision2() {
		return collision2;
	}
	public void setCollision2(Collision collision2) {
		this.collision2 = collision2;
	}
	
}
