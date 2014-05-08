/**
 * 
 */
package actSimG_prologMongo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.javatuples.Pair;
import org.jgrapht.UndirectedGraph;
import org.jgrapht.ext.IntegerNameProvider;
import org.jgrapht.ext.VertexNameProvider;
import org.jgrapht.graph.DefaultEdge;

/**
 * @author Zhou Fang, 04-2014, University of Bremen
 *
 * For storing semantic event chain matrices with row and column labels
 */
public class HackSemanticEventChains {

	List<UndirectedGraph<String,DefaultEdge>> original_main_graphs;
	private OriginalSEC oSEC;
	private DerivedSEC dSEC;
	private CompressedSEC cSEC;

	/**
	 * Initializes an original SEC from a list of mainGraphs (the event graphs of timestamps where something changes). Takes a list of main_graphs to get the number of rows and columns required and returns an originalSEC map with values set to 0.
	 * 
	 * TODO what to do if an object didn't exist yet or ceases to exist?! --> need to fill rows with 9's once encountered, or with 9 once not present anymore.
	 * 
	 * 
	 * 
	 * @param main_graphs
	 * @param rlabels
	 * @param clabels
	 */
	public HackSemanticEventChains(List<List<Integer>> SECmatrix, List<Pair<Integer, Integer>> rlabels, List<Long> clabels) 
	{
		Map<Integer,String> nodenamemap = new HashMap<Integer, String>();
		nodenamemap.put(1, "dummy");
		//Store graph and obtained SEC
		OriginalSEC original = new OriginalSEC(SECmatrix, rlabels, clabels, nodenamemap);
		this.oSEC = original;
	}
	
	/**
	 * Changes the original SEC such that only first and last contact changes per relationship are stored
	 * (first I thought only needs this if it's hand only, but turns out things like pancake_maker and spheres, and spatula and kitchen
	 * counter also tend to jitter).
	 */
	public void hackTimestampsCSEC(DerivedSEC dsec, CompressedSEC csec)
	{
		//it's a list of relationships, and for every entry there's a map where the relation name points to another map. That map is keyed on timestamp and gives you the change
		List<Map<Pair<Integer,Integer>, Map<Integer, String>>> mostImportantTimes = new ArrayList<Map<Pair<Integer,Integer>, Map<Integer, String>>>();
		
		//list of which are the first and last changes for each relation
		List<Map<Pair<Integer,Integer>, List<String>>> importantChanges = new ArrayList<Map<Pair<Integer,Integer>, List<String>>>();
		
		List<List<String>> csec_matrix1 = csec.SECmatrix;
		for(int i=0; i< csec_matrix1.size(); i++) //for every row in the compressed matrix, check what the first and last change looks like
		{
			List<String> currow = csec_matrix1.get(i); //current row of changes
			Pair<Integer,Integer> relationlabel = csec.relationlabels.get(i); //label of current row
			String change1 = currow.get(0); //get the first change
			//String change_final = 
			
		}
		
		
//		Pattern p = Pattern.compile("\\d+");
//		String keystr = key.toString();
//		for(Pair<String,String> ipair: permutation)
//		{
//			if(ipair.getValue0().equals(keystr)) //if the given key equals the current first value of the pair
//			{
//				String valstr = ipair.getValue1(); //this is the string we're looking to convert to pair
//				List<String> valstrpairs = new ArrayList<String>();
//				//use regex to find the numbers
//				Matcher m = p.matcher(valstr); 
//				while (m.find()) {
//					valstrpairs.add(m.group());
//				}
//				if(valstrpairs.size() != 2)
//				{
//					System.out.println("ERROR: number of integers found in the valstr of correspondingRelation is not 2!");
//				}
//		
//
//		//delete values from each list that are the same double digits (e.g. 00 or 11) to make compressedSEC
//		for(List<String> irowlist : dsec.SECmatrix)
//		{
//			List<String> newlist = new ArrayList<String>();
//			for(String el : irowlist) //for every element in the list, check whether it's double digit same 
//			{
//				if(el.charAt(0)!= el.charAt(1)) //not same value
//				{
//					newlist.add(el);
//				}
//			}
//			//add new list to the matrix
//			csec_matrix1.add(newlist);
//		}
//		CompressedSEC newcsec = new CompressedSEC(csec_matrix1, dsec.relationlabels);
//		this.cSEC = newcsec;
	}

	/**
	 * 
	 * @param osec
	 */
	public void constructDerivedSEC(OriginalSEC osec)
	{
		List<List<String>> dsec_matrix = new ArrayList<List<String>>();
		List<Pair<Integer, Integer>> rlabels = new ArrayList<Pair<Integer,Integer>>();
		List<String> timelabels = new ArrayList<String>();

		//convert originalSEC coding to code for changes, do this per row 
		for(int i =0; i< osec.SECmatrix.size(); i++)
		{
			List<Integer> irowlist = osec.SECmatrix.get(i);
			//only need to add new list if there is more than 1 value, otherwise it means that nothing happens in this row and it should be skipped
			HashSet<Integer> uniqueSet = new HashSet<Integer>();
			uniqueSet.addAll(irowlist);
			if(uniqueSet.size() > 1) //this row will be added to dsec
			{
				//add rowlabel
				rlabels.add(osec.relationlabels.get(i));

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
		for(int i=1; i<osec.timelabels.size(); i++)
		{
			String newlabel = osec.timelabels.get(i-1).toString() + "-" + osec.timelabels.get(i).toString();
			timelabels.add(newlabel);
		}
		DerivedSEC newdsec = new DerivedSEC(dsec_matrix, rlabels, timelabels);
		this.dSEC = newdsec;
	}

	/**
	 * Construct compressed semantic event chains. All elements in which no change occurs are discarded.
	 * This results in loss of temporal information. Note that instead of a regular hashmap a linked hashmap is used so that items are ordered according to insertion.
	 * 
	 * @param dsec
	 */
	public void constructCompressedSEC(DerivedSEC dsec)
	{
		List<List<String>> csec_matrix = new ArrayList<List<String>>();

		//delete values from each list that are the same double digits (e.g. 00 or 11) to make compressedSEC
		for(List<String> irowlist : dsec.SECmatrix)
		{
			List<String> newlist = new ArrayList<String>();
			for(String el : irowlist) //for every element in the list, check whether it's double digit same 
			{
				if(el.charAt(0)!= el.charAt(1)) //not same value
				{
					newlist.add(el);
				}
			}
			//add new list to the matrix
			csec_matrix.add(newlist);
		}
		CompressedSEC newcsec = new CompressedSEC(csec_matrix, dsec.relationlabels);
		this.cSEC = newcsec;
	}

	/**
	 * Create a SEC2 that is ordered such that the rows correspond to SEC1 according to the current permutation
	 * 
	 * @param sec1
	 * @param sec2
	 * @param cur_perm
	 * @return
	 */
	public static DerivedSEC reorderDerivedSEC(DerivedSEC sec1, DerivedSEC sec2, List<Pair<String, String>> cur_perm)
	{
		List<List<String>> reordered_matrix = new ArrayList<List<String>>();
		List<Pair<Integer,Integer>> reorder_rellabels = new ArrayList<Pair<Integer,Integer>>();
		//Assume SEC1 will remain unchanged while we swap around rows in SEC2 to match
		for(int i=0; i< sec1.SECmatrix.size(); i++)
		{
			Pair<Integer,Integer> correspondingkey = correspondingRelation(cur_perm, sec1.relationlabels.get(i)); //which objectrelation in SEC2 corresponds to this key?
			//find at which position that key is
			int corresponding_index = labelIndex(correspondingkey, sec2.relationlabels);
			//put the right key and value into the new SEC2. Given that we're going through the ordered SEC1 one by one, the order of SEC2 is also automatically right.
			reordered_matrix.add(sec2.SECmatrix.get(corresponding_index));
			reorder_rellabels.add(sec2.relationlabels.get(corresponding_index));
		}
		if(sec2.relationlabels.size() > sec1.relationlabels.size()) //if there are more objects in sec1 than sec 2
		{
			for(int i=0; i< sec2.relationlabels.size(); i++) //for every row in sec2, see whether it's already in the new matrix and if not, add it
			{
				if(!reorder_rellabels.contains(sec2.relationlabels.get(i)))
				{
					reordered_matrix.add(sec2.SECmatrix.get(i));
					reorder_rellabels.add(sec2.relationlabels.get(i));
				}
			}
		}
		else if(sec1.relationlabels.size() > sec2.relationlabels.size()) //if there are more objects in sec1 than sec 2, can't handle this right now //TODO
		{
			System.exit(1);
		}
		return new DerivedSEC(reordered_matrix, reorder_rellabels, sec2.timelabels);
	}

	/**
	 * Given a specific key and permutation, it will return the corresponding relation to the key as indicated within the permutation.
	 * In addition the permutation contains keys as Strings, while the given key is a Pair. Converts latter to String to match.
	 * 
	 * CALLED BY: 	temporalSimilarityValue
	 * CALLS:		-
	 * 
	 * @param permutation
	 * @param key
	 * @return
	 */
	public static Pair<Integer,Integer> correspondingRelation(List<Pair<String, String>> permutation, Pair<Integer, Integer> key)
	{
		Pattern p = Pattern.compile("\\d+");
		String keystr = key.toString();
		for(Pair<String,String> ipair: permutation)
		{
			if(ipair.getValue0().equals(keystr)) //if the given key equals the current first value of the pair
			{
				String valstr = ipair.getValue1(); //this is the string we're looking to convert to pair
				List<String> valstrpairs = new ArrayList<String>();
				//use regex to find the numbers
				Matcher m = p.matcher(valstr); 
				while (m.find()) {
					valstrpairs.add(m.group());
				}
				if(valstrpairs.size() != 2)
				{
					System.out.println("ERROR: number of integers found in the valstr of correspondingRelation is not 2!");
				}
				Integer val1 = Integer.parseInt(valstrpairs.get(0));
				Integer val2 = Integer.parseInt(valstrpairs.get(1));
				//create a pair from the string
				Pair<Integer,Integer> result = new Pair<Integer,Integer>(val1,val2);
				return result;
			}
		}
		System.out.println("WARNING: no match was found for " + keystr);
		return null; //this means no match was found, this means that there will be a dummy row inserted 
	}

	/**
	 * Finds the index of the given relation in the label list (this is how we can retrace which row and column a value belongs to)
	 * @param index
	 * @return
	 */
	private static <T> int labelIndex(T label, List<T> labelslist)
	{
		int result = labelslist.indexOf(label);
		if(result < 0)
		{
			System.out.println("WARNING: label " + label.toString() + " was not found in the list.");
		}
		return result;
	}

	public OriginalSEC getOSEC()
	{
		return this.oSEC;
	}

	public DerivedSEC getDSEC()
	{
		return this.dSEC;
	}

	public CompressedSEC getCSEC()
	{
		return this.cSEC;
	}
}
