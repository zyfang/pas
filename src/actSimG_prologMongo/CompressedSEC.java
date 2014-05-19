/**
 * 
 */
package actSimG_prologMongo;

import java.util.List;
import java.util.Map;

import org.javatuples.Pair;

/**
 * @author Zhou Fang, 04-2014, University of Bremen
 *
 * For storing Original Semantic Event Chains
 */
public class CompressedSEC extends SEC{

	public List<List<String>> SECmatrix;
	
	public CompressedSEC(List<List<String>> _matrix, List<Pair<Integer, Integer>> _relations, Map<Integer,String> _nodenamemap)
	{
		super(_relations, _nodenamemap);
		this.SECmatrix = _matrix;
	}

	/**
	 * For printing SECs in a matrix like form
	 * 
	 * @param SEC
	 */
	@Override
	public void printSEC() 
	{
		System.out.println("Compressed SEC");
		//print row labels + data
		for(int irow = 0; irow < this.getRelationStrings().size(); irow++)
		{
			System.out.print(this.getRelationStrings().get(irow) + "\t");
			System.out.println(this.SECmatrix.get(irow).toString());
		}
	}
}
