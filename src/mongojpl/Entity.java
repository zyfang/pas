package mongojpl;

public class Entity {
	
	private String name;
	private Pose pose;

	public Entity(String name){
		this.name = name;		
	}

	
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public Pose getPose() {
		return pose;
	}
	public void setPose(Pose pose) {
		this.pose = pose;
	}
	

}
