/* Author: Andrei Haidu
 * Email: a.haidu@gmail.com
 */

package mongo_prolog;

public class Pose {	
	
	private double x;
	private double y;
	private double z;
	private double roll;
	private double pitch;
	private double yaw;
	
	public Pose(){	
	}
	
	public Pose(double x, double y, double z, double roll, double pitch, double yaw){
		this.x 		= x;
		this.y 		= y;
		this.z 		= z;
		this.roll 	= roll;
		this.pitch 	= pitch;
		this.yaw 	= yaw;
	}	
	
	public void setPose(double x, double y, double z, double roll, double pitch, double yaw){
		this.x 		= x;
		this.y 		= y;
		this.z 		= z;
		this.roll 	= roll;
		this.pitch 	= pitch;
		this.yaw 	= yaw;
	}	
	
	public double getX() {
		return x;
	}
	
	public void setX(double x) {
		this.x = x;
	}
	
	public double getY() {
		return y;
	}
	
	public void setY(double y) {
		this.y = y;
	}
	
	public double getZ() {
		return z;
	}
	
	public void setZ(double z) {
		this.z = z;
	}
	
	public double getRoll() {
		return roll;
	}
	
	public void setRoll(double roll) {
		this.roll = roll;
	}
	
	public double getPitch() {
		return pitch;
	}
	
	public void setPitch(double pitch) {
		this.pitch = pitch;
	}
	
	public double getYaw() {
		return yaw;
	}
	
	public void setYaw(double yaw) {
		this.yaw = yaw;
	}	
	
	@Override 
	public String toString() {
		return String.valueOf(this.x) + " " 
				+ String.valueOf(this.y) + " "
				+ String.valueOf(this.z) + " "
				+ String.valueOf(this.roll) + " "
				+ String.valueOf(this.pitch) + " "
				+ String.valueOf(this.yaw); 
	}
	
}
