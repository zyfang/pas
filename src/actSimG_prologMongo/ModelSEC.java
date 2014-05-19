/**
 * 
 */
package actSimG_prologMongo;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.javatuples.Pair;

/**
 * @author Zhou Fang, 04-2014, University of Bremen
 *
 * For storing Original Semantic Event Chains
 */
public class ModelSEC extends SEC{

	private List<DerivedSEC> SECs; //list of SECmatrices
	private List<List<String>> SECmodel;
	private List<Double> column_weights;
	private List<Double> row_weights;
	//how much the weight is increased with each occurence
	private final double wi = 0.5;
	
	public ModelSEC(List<DerivedSEC> _matrices, List<Pair<Integer, Integer>> _relations, Map<Integer,String> _nodenamemap)
	{
		super(_relations, _nodenamemap);
		this.SECs = new ArrayList<DerivedSEC>(_matrices);
		
		//make SECmodel
		
		//choose the SEC with the largest number of columns to start with
		int starting_matrix = mostColumns();
		SECmodel = SECs.get(starting_matrix).SECmatrix; //initialize SECmodel to be this
		//compare all the other matrices with the SECmodel and remove the compared matrix from the list
	}
	
	public void printNodeMap()
	{
		for(Map.Entry<Integer,String> it : nodenamemap.entrySet())
		{
			System.out.println(it.getKey().toString() + ":" + it.getValue().toString());
		}
	}

	/**
	 * Returns the index of the SEC whose matrix has the most columns of the SEC in the list
	 * 
	 * @param _matrices
	 * @return
	 */
	public int mostColumns()
	{
		int largest = 0;
		int largest_index = 0;
		//all the DSECs have the same number of columns in each row, I can just take the column labels (time) to see how many columns it has
		for(int i=0; i<SECs.size(); i++)
		{
			if(SECs.get(i).getTimeStrings().size() > largest)
			{
				largest = SECs.get(i).getTimeStrings().size();
				largest_index = i;
			}
		}
		return largest_index;
	}
	
	public void addNewRow(List<String> newRow)
	{
		SECmodel.add(newRow);
		for(int i=0; i< newRow.size(); i++)
		{
			row_weights.add(wi);
		}		
		System.out.println("Added new row to the Model matrix: " + newRow.toString());
	}
	
	/**
	 * Adds new elements to the back of the rows (new columns). Because now we've added more columns, need to expand all other rows too. 
	 * Given that nothing has changed in the other rows, at the end of the whole process we will fill in the missing elements with non-changing codes.
	 * For example, if a row ends on "21" and we need more columns but don't have additional info on that row, will fill up the row with "11"s.
	 * This can ONLY be done at the end because the matrix that caused the new column elements to be added may also have column elements at other rows,
	 * so shouldn't fill up rows until the end (otherwise it will seem like there is a mismatch).
	 * 
	 * @param newItems
	 */
	public void addNewColumnsToEnd(List<String> newItems, int row)
	{
		SECmodel.get(row).addAll(newItems);
	}
	
	/**
	 * Similar to addNewColumnsToEnd, but now to the front of the array.
	 * The problem is that we can't check the row length at the end, because we won't know what will have been added to the front and what to the back.
	 * TODO
	 * 
	 * @param newItems
	 * @param row
	 */
	public void addNewColumnsToBeginning(List<String> newItems, int row)
	{
		List<String> newRow = new ArrayList<String>(newItems);
		newRow.addAll(SECmodel.get(row));
		SECmodel.set(row, newRow);
	}
	
	/**
	 * adds wi weight to the given row
	 * @param index
	 */
	public void incrementRowWeight(int index)
	{
		row_weights.set(index, row_weights.get(index) + wi);
	}
	
	/**
	 * adds wi weight to the given column
	 * @param index
	 */
	public void incrementColWeight(int index)
	{
		column_weights.set(index, column_weights.get(index) + wi);
	}
	
	public List<List<String>> getSECModel()
	{
		return SECmodel;
	}
	
	public List<Double> getRowWeights()
	{
		return row_weights;
	}
	
	public List<Double> getColumnWeights()
	{
		return column_weights;
	}

	/**
	 * Printing model SEC in matrix form. Note that modelSEC has the same for as DSEC
	 */
	@Override
	public void printSEC() {
		System.out.println("Model SEC:");
		//print column labels
		System.out.println("\t" + column_weights.toString());

		//print row labels + data
		for(int irow = 0; irow < row_weights.size(); irow++)
		{
			System.out.print(row_weights.get(irow).toString() + "\t");
			System.out.println(this.SECmodel.get(irow).toString());
		}
		
	}
	
	/**
	 * For printing a single (given) DSEC in a matrix like form
	 * 
	 * @param SEC
	 */
	public void printSEC(int isec) 
	{
		System.out.println("Derived matrix " + isec + ":");
		
		DerivedSEC cursec = SECs.get(isec);
		cursec.printSEC();
	}
}
