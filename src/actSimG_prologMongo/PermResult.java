/**
 * 
 */
package actSimG_prologMongo;

import java.util.List;

import org.javatuples.Pair;

/**
 * @author Zhou Fang, 04-2014, University of Bremen
 *
 * Class for storing a single permutation and its associated similarity
 * 
 */
public class PermResult {
	private List<Pair<String, String>> permutation;
	private Double similarity;
	
	public PermResult(List<Pair<String, String>> _permutation, Double _similarity)
	{
		this.permutation = _permutation;
		this.similarity = _similarity;
	}
	
	
	public List<Pair<String, String>> getPerm()
	{
		return this.permutation;
	}
	
	public Double getSim()
	{
		return this.similarity;
	}
}
