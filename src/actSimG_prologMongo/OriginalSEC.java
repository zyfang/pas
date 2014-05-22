/**
 * 
 */
package actSimG_prologMongo;

import java.util.ArrayList;
import java.util.HashSet;
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
	
	public OriginalSEC(List<List<Integer>> _matrix, List<Pair<Integer, Integer>> _relations, List<Long> _times, Map<Integer,String> _nodenamemap)
	{
		super(_relations, _nodenamemap);
		this.timelabels = _times;
		this.SECmatrix = _matrix;
	}
	
	/**
	 * Makes a DerivedSEC based on the given OriginalSEC. Combines info from two timepoints to code for change.
	 * For example, if t1 = 1 and t2 = 0, then now t1-t2 will be "10".
	 *  
	 * Discards all rows in which no change ever happens, i.e. the relations that never change
	 * 
	 * CALLS:		-
	 * CALLED BY:	constructAllSEC
	 * 
	 */
	public DerivedSEC constructDerivedSEC()
	{
		List<List<String>> dsec_matrix = new ArrayList<List<String>>();
		List<Pair<Integer, Integer>> rlabels = new ArrayList<Pair<Integer,Integer>>();
		List<String> timelabels = new ArrayList<String>();

		//convert originalSEC coding to code for changes, do this per row 
		for(int i =0; i< this.SECmatrix.size(); i++)
		{
			List<Integer> irowlist = this.SECmatrix.get(i);
			//only need to add new list if there is more than 1 value, otherwise it means that nothing happens in this row and it should be skipped
			HashSet<Integer> uniqueSet = new HashSet<Integer>();
			uniqueSet.addAll(irowlist);
			if(uniqueSet.size() > 1) //this row will be added to dsec
			{
				//add rowlabel
				rlabels.add(this.relationlabels.get(i));

				List<String> newlist = new ArrayList<String>();
				//go through list. Start with the second item, since need to know the change
				for(int j=1; j<irowlist.size(); j++) 
				{
					Integer last = irowlist.get(j-1);
					Integer cur  = irowlist.get(j);
					String change = last.toString() + cur.toString();
					//add the new value to the new list
					newlist.add(change);
				}
				//add new list to the matrix
				dsec_matrix.add(newlist);
			}
		}
		//make new time labels by combining two timestrings together
		for(int i=1; i<this.timelabels.size(); i++)
		{
			String newlabel = this.timelabels.get(i-1).toString() + "-" + this.timelabels.get(i).toString();
			timelabels.add(newlabel);
		}
		DerivedSEC newdsec = new DerivedSEC(dsec_matrix, rlabels, timelabels, this.nodenamemap);
		return newdsec;
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
	public void printSEC() 
	{
		System.out.println("Original SEC:");
		//print column labels
		System.out.println("\t" + this.getTimeStrings());
		
		//print row labels + data
		for(int irow = 0; irow < this.getRelationStrings().size(); irow++)
		{
			System.out.print(this.getRelationStrings().get(irow) + "\t");
			System.out.println(this.SECmatrix.get(irow).toString());
		}
	}
}
