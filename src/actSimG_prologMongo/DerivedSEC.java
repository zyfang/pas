/**
 * 
 */
package actSimG_prologMongo;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.javatuples.Pair;

/**
 * @author Zhou Fang, 04-2014, University of Bremen
 *
 * For storing Original Semantic Event Chains
 */
public class DerivedSEC extends SEC{

	public final List<String> timelabels;
	public List<List<String>> SECmatrix;

	public DerivedSEC(List<List<String>> _matrix, List<Pair<Integer, Integer>> _relations, List<String> _times)
	{
		super(_relations);
		this.SECmatrix = _matrix;
		this.timelabels = _times;
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
	public void printSEC(List<String> rlabels, List<String> clabels) 
	{
		System.out.println("Derived SEC:");
		//print column labels
		System.out.println("\t" + clabels.toString());

		//print row labels + data
		for(int irow = 0; irow < rlabels.size(); irow++)
		{
			System.out.print(rlabels.get(irow).toString() + "\t");
			System.out.println(this.SECmatrix.get(irow).toString());
		}
	}
}
