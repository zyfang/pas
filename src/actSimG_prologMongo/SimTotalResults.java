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
public class SimTotalResults {
	public final List<List<Pair<String, String>>> spatial_permutations;
	public final List<List<Pair<String, String>>> temp_permutations;
	public final Double max_spat_sim;
	public final Double max_temp_sim;
	
	public SimTotalResults(List<List<Pair<String, String>>> _spat_perm, List<List<Pair<String, String>>> _temp_perm, Double _spat_sim, Double _temp_sim)
	{
		this.spatial_permutations = _spat_perm;
		this.temp_permutations = _temp_perm;
		this.max_spat_sim = _spat_sim;
		this.max_temp_sim = _temp_sim;
	}
}
