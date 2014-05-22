/**
 * 
 */
package actSimG_prologMongo;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.javatuples.Pair;

/**
 * @author Zhou Fang, 04-2014, University of Bremen
 *
 * For storing Original Semantic Event Chains
 */
public class DerivedSEC extends SEC{

	public final List<String> timelabels;
	public List<List<String>> SECmatrix;

	public DerivedSEC(List<List<String>> _matrix, List<Pair<Integer, Integer>> _relations, List<String> _times, Map<Integer,String> _nodenamemap)
	{
		super(_relations, _nodenamemap);
		this.SECmatrix = _matrix;
		this.timelabels = _times;
	}
	
	/**
	 * Reorder the current DerivedSEC object (SEC2), such that the rows correspond to SEC1 according to the current permutation
	 * If SEC2 has more rows than SEC 1, will add the non-corresponding rows to the end. 
	 * 
	 * @param sec2
	 * @param cur_perm
	 * @return
	 */
	public void reorderDerivedSEC(DerivedSEC sec1, List<Pair<String, String>> cur_perm)
	{
		List<List<String>> reordered_matrix = new ArrayList<List<String>>();
		int sec2_ncols = this.SECmatrix.get(0).size(); // how many columns does this sec have? need to know for creating dummy rows
		List<Pair<Integer,Integer>> reorder_rellabels = new ArrayList<Pair<Integer,Integer>>();
		//Assume SEC1 will remain unchanged while we swap around rows in SEC2 to match
		for(int i=0; i< sec1.SECmatrix.size(); i++)
		{
			System.out.println(cur_perm.toString());
			Pair<Integer,Integer> correspondingkey = correspondingRelation(cur_perm, sec1.relationlabels.get(i)); //which objectrelation in SEC2 corresponds to this key?
			if(correspondingkey != null)
			{
				//find at which position that key is
				int corresponding_index = labelIndex(correspondingkey, this.relationlabels);
				//put the right key and value into the new SEC2. Given that we're going through the ordered SEC1 one by one, the order of SEC2 is also automatically right.
				reordered_matrix.add(this.SECmatrix.get(corresponding_index));
				reorder_rellabels.add(this.relationlabels.get(corresponding_index));
			}
			else //if the key is 0, it means no correspondence was found for the current row of SEC1 (because SEC1 is larger ?)
			{
				System.out.println(sec1.getRelationStrings().get(i) + "corresponding key null");
				//add dummy row
				Pair<Integer,Integer> dummy_rellabel = new Pair<Integer,Integer>(999,999);
				List<String> dummyrow = new ArrayList<String>();
				for(int j=0; j<sec2_ncols; j++) //how long the row should be
				{
					dummyrow.add("0");
				}
				reordered_matrix.add(dummyrow);
				reorder_rellabels.add(dummy_rellabel);
			}
		}
		//if there are more objects in sec2 than sec1
		for(int i=0; i< this.relationlabels.size(); i++) //for every row in sec2, see whether it's already in the new matrix and if not, add it
		{
			if(!reorder_rellabels.contains(this.relationlabels.get(i)))
			{
				reordered_matrix.add(this.SECmatrix.get(i));
				reorder_rellabels.add(this.relationlabels.get(i));
			}
		}
		this.SECmatrix = reordered_matrix;
		this.relationlabels = reorder_rellabels;
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
	private static Pair<Integer,Integer> correspondingRelation(List<Pair<String, String>> permutation, Pair<Integer, Integer> key)
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
	 * 
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
	
	/**
	 * Construct compressed semantic event chains. All elements in which no change occurs are discarded.
	 * This results in loss of temporal information. Note that instead of a regular hashmap a linked hashmap is used so that items are ordered according to insertion.
	 * 
	 * CALLS:		-
	 * CALLED BY:	constructAllSEC
	 *  
	 */
	public CompressedSEC constructCompressedSEC()
	{
		List<List<String>> csec_matrix = new ArrayList<List<String>>();

		//delete values from each list that are the same double digits (e.g. 00 or 11) to make compressedSEC
		for(List<String> irowlist : this.SECmatrix)
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
		CompressedSEC newcsec = new CompressedSEC(csec_matrix, this.relationlabels, this.nodenamemap);
		return newcsec;
	}
	
	/**
	 * Fills up the SEC with dummy rows up to min_nrows if the SEC has less rows than min_nrows
	 * 
	 * CALLS: 		-
	 * CALLED BY: 	SemanticEventChains.temporalSimilarityValueWith
	 * @param min_nrows
	 * @return
	 */
	public DerivedSEC extendWithDummySEC (int min_nrows)
	{
		//determine whether need to add dummy rows to the end (transposed these will be dummy columns)
		int ndummyrow = min_nrows-SECmatrix.size();
		int ncols = this.SECmatrix.get(0).size();
		//add dummy rows
		Pair<Integer,Integer> dummy_rellabel = new Pair<Integer,Integer>(999,999);
		for(int i=0; i<ndummyrow; i++)
		{
			List<String> dummyrow = new ArrayList<String>();
			for(int j=0; j<ncols; j++) //how long the row should be
			{
				dummyrow.add("0");
			}
			this.SECmatrix.add(dummyrow);
			this.relationlabels.add(dummy_rellabel);
			
		}
		return this;
	}
	
	/**
	 * Adds nrow dummy rows to the SEC. 
	 * This function is for helping with debugging spatial and temporal similarity code (by artificially changing the dimensions of the SEC for testing comparisons)
	 * 
	 * @param nrow
	 * @return
	 */
	public DerivedSEC addDummyRows (int nrow)
	{
		int ncols = this.SECmatrix.get(0).size();
		//add dummy rows
		Pair<Integer,Integer> dummy_rellabel = new Pair<Integer,Integer>(999,999);
		for(int i=0; i<nrow; i++)
		{
			List<String> dummyrow = new ArrayList<String>();
			for(int j=0; j<ncols; j++) //how long the row should be
			{
				dummyrow.add("12");
			}
			this.SECmatrix.add(dummyrow);
			this.relationlabels.add(dummy_rellabel);
		}
		return this;
	}
	
	/**
	 * Adds ncol dummy columns to the SEC.
	 * This function is for helping with debugging spatial and temporal similarity code (by artificially changing the dimensions of the SEC for testing comparisons)
	 * @param ncol
	 * @return
	 */
	public DerivedSEC addDummyColumns (int ncol)
	{
		//determine whether need to add dummy rows to the end (transposed these will be dummy columns)
		int nrows = this.SECmatrix.size();
		//add 00's at end of each row (one for each dummy column)
		String dummy_timelabel = "xxxxxxx";
		for(int i=0; i<nrows; i++)
		{
			for(int j=0; j<ncol; j++) //how long the row should be
			{
				this.SECmatrix.get(i).add("12");
				if(i==0)//this is the first row, so also need to add dummy column labels
				{
					this.timelabels.add(dummy_timelabel);
				}
			}
		}
		return this;
	}
	
	public List<String> getTimeStrings()
	{
		return timelabels;
	}

	/**
	 * For printing SECs in a matrix like form
	 * 
	 * @param SEC
	 */
	@Override
	public void printSEC() 
	{
		System.out.println("Derived SEC:");
		//print column labels
		System.out.println("\t" + this.getTimeStrings());

		//print row labels + data
		for(int irow = 0; irow < this.getRelationStrings().size(); irow++)
		{
			System.out.print(this.getRelationStrings().get(irow).toString() + "\t");
			System.out.println(this.SECmatrix.get(irow).toString());
		}
	}
}
