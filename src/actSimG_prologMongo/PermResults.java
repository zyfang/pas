/**
 * 
 */
package actSimG_prologMongo;

import java.util.List;

import org.javatuples.Pair;

/**
 * @author Zhou Fang, 04-2014, University of Bremen
 *
 * Class for storing permutation lists and associated similarity values
 * 
 */
public class PermResults {
	private List<List<Pair<String, String>>> permutations;
	private List<Double> similarities;
	
	public PermResults(List<List<Pair<String, String>>> permutations, List<Double> similarities)
	{
		this.permutations = permutations;
		this.similarities = similarities;
	}
	
	/**
	 * removes item at index "it" both from permutations and similarities lists.
	 * @param item
	 */
	public void removeItem(int it)
	{
		this.permutations.remove(it);
		this.similarities.remove(it);
	}
	
	public List<List<Pair<String, String>>> getPerm()
	{
		return this.permutations;
	}
	
	public List<Double> getSim()
	{
		return this.similarities;
	}
}
