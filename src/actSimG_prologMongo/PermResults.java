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
 * TODO this class needs to be changed so the representation is one permresult and then the functions using it can make a list of it.
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
	
	public List<List<Pair<String, String>>> getPerms()
	{
		return this.permutations;
	}
	
	public List<Double> getSim()
	{
		return this.similarities;
	}
	
	/**
	 * Returns the permutation and similarity at the given index
	 * @param index
	 * @return
	 */
	public Pair<List<Pair<String,String>>, Double> getOnePerm(int index)
	{
		Pair<List<Pair<String, String>>, Double> result = new Pair<List<Pair<String,String>>, Double>(permutations.get(index), similarities.get(index));
		return result;
	}
}
