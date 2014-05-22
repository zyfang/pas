/**
 * 
 */
package actSimG_prologMongo;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.javatuples.Pair;

/**
 * @author Zhou Fang, 04-2014, University of Bremen
 *
 * For storing Semantic Event Chains (original, derived or compressed)
 */
public abstract class SEC {

	public List<Pair<Integer, Integer>> relationlabels;
	public final Map<Integer,String> nodenamemap;
	public Object SECmatrix;
	
	public SEC(List<Pair<Integer, Integer>> _relations, Map<Integer,String> _nodenamemap)
	{
		this.relationlabels = _relations;
		this.nodenamemap = _nodenamemap;
	}	
	
	abstract public void printSEC();
	
	public List<String> getRelationStrings()
	{
		List<String> result= new ArrayList<String>();
		for(Pair<Integer, Integer> item : relationlabels)
		{
			result.add(item.toString());
		}
		return result;
	}
	
	public List<String> getRelationNameStrings()
	{
		List<String> result = new ArrayList<String>();
		for(Pair<Integer, Integer> item : relationlabels)
		{
			String name1 = nodenamemap.get(item.getValue0());
			String name2 = nodenamemap.get(item.getValue1());
			String namepair = new String("[" + name1 + "," + name2 + "]");
			result.add(namepair);
		}
		return result;
	}
	
//	public List<Pair<Integer,Integer>> getRowLabels()
//	{
//		return this.relationlabels;
//	}
//
//	public List<Long> getColLabels()
//	{
//		return this.timelabels;
//	}
//
//	public List<List<Double>> getSECmatrix()
//	{
//		return this.SECmatrix;
//	}
//	
//	public List<List<Double>> setSECmatrix()
//	{
//		
//	}
}
