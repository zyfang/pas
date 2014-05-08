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
 * For storing Original Semantic Event Chains
 */
public class OriginalSEC extends SEC{

	public List<Long> timelabels;
	public List<List<Integer>> SECmatrix;
	public Map<Integer,String> nodenamemap;
	
	public OriginalSEC(List<List<Integer>> _matrix, List<Pair<Integer, Integer>> _relations, List<Long> _times, Map<Integer,String> _nodenamemap)
	{
		super(_relations);
		this.timelabels = _times;
		this.nodenamemap = _nodenamemap;
		this.SECmatrix = _matrix;
	}
	
	public List<String> getTimeStrings()
	{
		List<String> result = new ArrayList<String>();
		for(Long item : timelabels)
		{
			result.add(item.toString());
		}
		return result;
	}
	
	public void printNodeMap()
	{
		for(Map.Entry<Integer,String> it : nodenamemap.entrySet())
		{
			System.out.println(it.getKey().toString() + ":" + it.getValue().toString());
		}
	}

	/**
	 * For printing SECs in a matrix like form
	 * 
	 * @param SEC
	 */
	@Override
	public void printSEC(List<String> rlabels, List<String> clabels) 
	{
		System.out.println("Original SEC:");
		//print column labels
		System.out.println("\t" + clabels);
		
		//print row labels + data
		for(int irow = 0; irow < rlabels.size(); irow++)
		{
			System.out.print(rlabels.get(irow) + "\t");
			System.out.println(this.SECmatrix.get(irow).toString());
		}
	}
}
