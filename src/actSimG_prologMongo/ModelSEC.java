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
public class ModelSEC{

	private List<SemanticEventChains> all_chains;
	private List<DerivedSEC> dSECs; //list of SECmatrices\
	private PermResults perm_sim_results;
	private DerivedSEC dSECmodel;
	private CompressedSEC cSECmodel;
	private List<Double> column_weights;
	private List<Double> row_weights;
	//how much the weight is increased with each occurrence
	private final double ws = 0.5; //starting weight
	private final double wi = 0.5; //increasing weight
	
	public ModelSEC(List<SemanticEventChains> _chains, int startingmatrix)
	{
		this.all_chains = _chains;
		List<DerivedSEC> alldsec = new ArrayList<DerivedSEC>();
		for(SemanticEventChains ichain : all_chains)
		{
			alldsec.add(ichain.getDSEC());
		}
		this.dSECs = alldsec;
		
		//////////////////////make SECmodel
		
		//Start with the SEC with the most columns
		dSECmodel = dSECs.get(startingmatrix); //initialize SECmodel to be this
		cSECmodel = dSECmodel.constructCompressedSEC();
		//fill initial weight arrays with wd
		for(int i=0; i< dSECmodel.SECmatrix.size(); i++)
		{
			row_weights.add(ws);
		}
		for(int j=0; j< dSECmodel.SECmatrix.size(); j++)
		{
			column_weights.add(ws);
		}
	}
	
	/**
	 * Think about this function, should it be static?
	 * 
	 * TODO I'm not sure but how I read the paper, weights for rows and columns are increased every time an element in that row or column is matched (so not one increase for matching the whole row or column, but for each element)
	 * 
	 * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	 * TODO I have to rewrite the whole thing, because actually you don't want to look row for row which index matches the best; then you'd ignore
	 * the information whether certain events happened at the same time for both matrices. Rather, if the curdsec has less columns than the big matrix, move the whole curdsec matrix and find 1 index to start at and go for that.
	 * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	 * 
	 * @param action_model
	 * @param chain
	 */
	public void updateModel(SemanticEventChains chain)
	{
		DerivedSEC cur_dsec_ordered = chain.getDSEC(); //ordering was done by calling temporalValue in the MPI class
		DerivedSEC modeldsec = dSECmodel;
		//check that the actionModel has at least as many columns as the new chain
		if(dSECmodel.timelabels.size() < cur_dsec_ordered.relationlabels.size())
		{
			System.err.println("ERROR: The given Semantic Event Chain has more columns than the action model. Currently this function is not supported");
		}
		//for every row in the new dsec, see whether it has to be shifted around (if number of columns of the given SEC is smaller)
		List<Integer> rowmatch_indexes = new ArrayList<Integer>();
		
		//is the model shorter than curdsec
//		int min_nrows = 
		for(int i=0; i<cur_dsec_ordered.SECmatrix.size(); i++)
		{
			int res_ind = max_row_similarity_index(modeldsec.SECmatrix.get(i), cur_dsec_ordered.SECmatrix.get(i));
			rowmatch_indexes.add(new Integer(res_ind));
		}
		//if the curdsec has less columns than modeldsec, look how big the difference is and try up to that difference at which position the similarity (both row and columns) would be maximal; BASICALLY, we're looking for a match that would increase the most weights
		for(int i=0; i<(modeldsec.getTimeStrings().size()-cur_dsec_ordered.getTimeStrings().size()+1); i++)
		{
			//go from 0 to the length of the model matrix (#columns). If the current rowmatchindex==i, we know where to start counting to add weights, if the rowmatchindex !=i, know that can safely add one dummy to the front
			//TODO this is very inefficient now, but I think this should be adaptable to being able to incorporate new columns
			for(int j=0; j<rowmatch_indexes.size(); j++)
			{
				if(rowmatch_indexes.get(j) == i)
				{
					updateRow(j, i, cur_dsec_ordered.SECmatrix.get(j));
				}
			}
		}
	}
	
	/**
	 * Takes one row and updates the model according to the matches of that row with the row in the model, starting at starting_col_ind (dSEC_row may be shorter and not start matching at col_ind=0)
	 * @param row_ind
	 * @param starting_col_ind
	 * @param dSEC_row
	 */
	private void updateRow(int row_ind, int starting_col_ind, List<String> dSEC_row)
	{
		//for each element, see whether it matches the element in the model
		for(int i=0; i<dSEC_row.size(); i++)
		{
			//see whether the element matches the element in the model that it should match
			if(dSEC_row.get(i) == this.dSECmodel.SECmatrix.get(row_ind).get(i+starting_col_ind))
			{
				incrementColWeight(i+starting_col_ind);
				incrementRowWeight(row_ind);
				System.out.println("Increasing weight of row " + row_ind + " and column " + i+starting_col_ind);
			}
		}
	}
	
	/**
	 * Help function for spatialSimilarityMatrix. Computes the maximum similarity between two rows (of different SECs),
	 * Assumes currow1 >= currow2.
	 * 
	 * @param currow1
	 * @param currow2
	 * @return
	 */
	private int max_row_similarity_index(List<String> currow1, List<String> currow2)
	{
		double max_similarity = 0;
		int max_similarity_index = 0; //keeps track of where currow2 should start being compared to currow1
		for(int i=0; i<currow1.size()-currow2.size()+1; i++) //this is how many times we have to compare. If they're the same length, only i=0 will be compared
		{
			int sum = 0; //keeps track of the sum for the similarity of the current shift
			for(int j=0; j<currow2.size(); j++)
			{
				String el2 = currow2.get(j);
				String el1 = currow1.get(j+i);
				if(el2.equals(el1)) //add 1 to the sum if they're the same
				{
					sum++;
				}
			}
			double ft = (100.0/currow1.size()) * sum; //similarity for the current shift
			if(ft > max_similarity)
			{
				max_similarity = ft;
				max_similarity_index = i;
			}
		}
		return max_similarity_index;
	}

	/**
	 * Print which nodenumbers correspond to which object names
	 */
	public void printNodeMap()
	{
		for(Map.Entry<Integer,String> it : dSECmodel.nodenamemap.entrySet())
		{
			System.out.println(it.getKey().toString() + ":" + it.getValue().toString());
		}
	}

	private void addNewRow(List<String> newRow)
	{
		dSECmodel.SECmatrix.add(newRow);
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
	private void addNewColumnsToEnd(List<String> newItems, int row)
	{
		dSECmodel.SECmatrix.get(row).addAll(newItems);
	}
	
	/**
	 * Similar to addNewColumnsToEnd, but now to the front of the array.
	 * The problem is that we can't check the row length at the end, because we won't know what will have been added to the front and what to the back.
	 * TODO
	 * 
	 * @param newItems
	 * @param row
	 */
	private void addNewColumnsToBeginning(List<String> newItems, int row)
	{
		List<String> newRow = new ArrayList<String>(newItems);
		newRow.addAll(dSECmodel.SECmatrix.get(row));
		dSECmodel.SECmatrix.set(row, newRow);
	}
	
	/**
	 * adds wi weight to the given row
	 * @param index
	 */
	private void incrementRowWeight(int index)
	{
		row_weights.set(index, row_weights.get(index) + wi);
	}
	
	/**
	 * adds wi weight to the given column
	 * @param index
	 */
	private void incrementColWeight(int index)
	{
		column_weights.set(index, column_weights.get(index) + wi);
	}
	
	public DerivedSEC getdSECModel()
	{
		return dSECmodel;
	}
	
	public CompressedSEC getcSECModel()
	{
		return cSECmodel;
	}
	
	public List<Double> getRowWeights()
	{
		return row_weights;
	}
	
	public List<Double> getColumnWeights()
	{
		return column_weights;
	}

//	/**
//	 * Printing model SEC in matrix form. Note that modelSEC has the same for as DSEC
//	 */
//	@Override
//	public void printSEC() {
//		System.out.println("Model SEC:");
//		//print column labels
//		System.out.println("\t" + column_weights.toString());
//
//		//print row labels + data
//		for(int irow = 0; irow < row_weights.size(); irow++)
//		{
//			System.out.print(row_weights.get(irow).toString() + "\t");
//			System.out.println(this.dSECmodel.get(irow).toString());
//		}
//		
//	}
//	
//	/**
//	 * For printing a single (given) DSEC in a matrix like form
//	 * 
//	 * @param SEC
//	 */
//	public void printSEC(int isec) 
//	{
//		System.out.println("Derived matrix " + isec + ":");
//		
//		DerivedSEC cursec = SECs.get(isec);
//		cursec.printSEC();
//	}
}
