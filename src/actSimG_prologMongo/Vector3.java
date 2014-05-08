package mongo_prolog;

public class Vector3 {
	
    private double[] data = new double[3];
    
    public Vector3(){}
 
    public Vector3(double x, double y, double z){
        data[0] = x;
        data[1] = y;
        data[2] = z;
    }
    public Vector3(Vector3 v){
        data[0] = v.x();
        data[1] = v.y();
        data[2] = v.z();
    }    

    public double x()
    {
        return data[0];
    }
    public double y()
    {
        return data[1];
    }
    public double z()
    {
        return data[2];
    }
    
	@Override 
	public String toString() {
		return String.valueOf(data[0]) + " " 
				+ String.valueOf(data[1]) + " "
				+ String.valueOf(data[2]); 
	}	
	

}
