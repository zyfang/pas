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
	 * Return a map with columns as lists and the key will be the timestamp. Used for transposing columns of SEC to rows.
	 * In addition it fills the SEC up to a number of rows with dummies if there are not enough rows and the same for columns
	 * Pass min_nrows = 0 if don't want dummies
	 * 
	 * TODO: when filling row with dummies, no label for the row is added, will this be a problem?!
	 * 
	 * @param min_nrows
	 * @return
	 */
	public DerivedSEC transposeSEC (int min_nrows, int min_ncols)
	{
		//make transposed SEC
		List<List<String>> newmatrix = new ArrayList<List<String>>();
		List<Pair<Integer, Integer>> newrellabels = new ArrayList<Pair<Integer,Integer>>(relationlabels);
		List<String> newtimelabels = new ArrayList<String>(timelabels);
		//how many columns are there. Matrix should have same # columns in each row
		int n_col = SECmatrix.get(0).size();
		//determine whether need to add dummy rows to the end (transposed these will be dummy columns)
		int ndummyrow = min_nrows-SECmatrix.size();
		//build new matrix column by column
		for(int i=0; i< n_col; i++)
		{
			List<String> newcolumn = new ArrayList<String>();
			Pair<Integer,Integer> dummy_rellabel = new Pair<Integer,Integer>(999,999);
			for(List<String> irow : SECmatrix) //for every row, add the current column value to the column list
			{
				newcolumn.add(irow.get(i));
			}
			for(int j=0; j<ndummyrow; j++) //for every dummy, add one 0 to the end
			{
				newcolumn.add("0");
				if(i==0)//if this is the first column, so the dummylabels haven't been added before for the current row(s)
				{
					newrellabels.add(dummy_rellabel);
				}
			}
			newmatrix.add(newcolumn);
		}
		//add dummycols (here they are rows because the matrix is transposed)
		int ndummycol = min_ncols-n_col;
		for(int i=0; i<ndummycol; i++)
		{
			List<String> newcolumn = new ArrayList<String>();
			String dummy_timelabel = "xxx";
			for(int j=0; j<min_nrows; j++)
			{
				newcolumn.add("0");
			}
			newmatrix.add(newcolumn);
			newtimelabels.add(dummy_timelabel);
		}
		DerivedSEC newSEC = new DerivedSEC(newmatrix, newrellabels, newtimelabels);
//		newSEC.printSEC(newSEC.getTimeStrings(), newSEC.getRelationStrings());
		return newSEC;
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
