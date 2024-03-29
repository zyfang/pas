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
import org.jgrapht.ext.IntegerNameProvider;
import org.jgrapht.ext.VertexNameProvider;
import org.jgrapht.graph.DefaultWeightedEdge;
import org.jgrapht.graph.*;

/**
 * @author Zhou Fang, 04-2014, University of Bremen
 *
 * For storing semantic event chain matrices with row and column labels
 */
public class SemanticEventChains {

	List<SimpleWeightedGraph<String,DefaultWeightedEdge>> original_main_graphs;
	private OriginalSEC oSEC;
	private DerivedSEC dSEC;
	private CompressedSEC cSEC;

	/**
	 * Initializes an original SEC from a list of mainGraphs (the event graphs of timestamps where something changes). 
	 * Takes a list of main_graphs to get the number of rows and columns required and returns an originalSEC map with values set to 0.
	 * 
	 * TODO what to do if an object didn't exist yet or ceases to exist?! --> need to fill rows with 9's once encountered, or with 9 once not present anymore.
	 * 
	 * 
	 * @param main_graphs
	 * @param time_labels
	 */
	public SemanticEventChains(List<SimpleWeightedGraph<String,DefaultWeightedEdge>> main_graphs, List<Long> time_labels) //using generic type for labels because not sure yet whether row/column labels are always pairs, longs, or strings
	{
		//Initialize variables for SEC
		List<List<Integer>> matrix  = new ArrayList<List<Integer>>();
		List<Pair<Integer, Integer>> relation_labels = new ArrayList<Pair<Integer, Integer>>();
		Map<Integer,String> nodenamemap = new HashMap<Integer,String>();
		//copy timelabels (otherwise multiple objects will have the same object reference to time) //This actually only occurs if you give the same time_labels for different objects, but better to be safe
		List<Long> times = new ArrayList<Long>();
		for(int i=0; i< time_labels.size(); i++)
		{
			times.add(time_labels.get(i));
		}

		VertexNameProvider<String> nameProvider = new IntegerNameProvider<String>();
		//for every column/timestamp/graph
		for(SimpleWeightedGraph<String,DefaultWeightedEdge> igraph : main_graphs)
		{
			int nobjects = igraph.vertexSet().size();
			//System.out.println("nobjects: " + nobjects);
			for (String node : igraph.vertexSet())  //need to do this once to order them correctly
			{
				nameProvider.getVertexName(node);
			}

			//Add rows to the matrix for every possible relation by going through the nodes and adding relevant combinations with other nodes as rows
			int row_index = 0; //keeps track of which row we're on. Resets when we go to a different graph, i.e. a diferent column
			for(String node : igraph.vertexSet())
			{
				Integer current = Integer.parseInt(nameProvider.getVertexName(node));
				//add the name and number association to the nodenamemap
				nodenamemap.put(current, node);
				//the names of the relationships will be in the form of (1,1), (1,2), (1,3), (1,4), (2,3), (2,4), (3,4), so starting from your current node, you always count the second index upto how many objects you have in total
				for(int i = current+1; i <= nobjects; i++)
				{
					Pair<Integer, Integer> cur_row_label = new Pair<Integer,Integer>(current, i);
					//if this is the first column, need to initialize the list for the current row
					if(!relation_labels.contains(cur_row_label))
					{
						relation_labels.add(cur_row_label);
						matrix.add(new ArrayList<Integer>());
					}
					//add 0 to the right index
					matrix.get(row_index).add(new Integer(0));
					row_index++;
				}
			}
		}
		//Store graph and obtained SEC
		OriginalSEC original = new OriginalSEC(matrix, relation_labels, times, nodenamemap);
		System.out.println(times);
		this.oSEC = original;
		this.original_main_graphs = main_graphs; //store for later
	}
	
	/**
	 * Calls appropriate methods to fill Original, Derived and Compressed SEC variables according to the information
	 * in the main_graphs.
	 * 
	 * CALLS:		constructOriginalSEC(main_graphs);
	 * 				constructDerivedSEC();
	 * 				constructCompressedSEC();
	 * CALLED BY:	-
	 * 
	 * @param main_graphs
	 */
	public void constructAllSEC(List<SimpleWeightedGraph<String,DefaultWeightedEdge>> main_graphs)
	{
		constructOriginalSEC(main_graphs);
		this.dSEC = oSEC.constructDerivedSEC();
		this.cSEC = dSEC.constructCompressedSEC();
	}

	/**
	 * Fills OriginalSEC with values according to main_graphs. For now only encode contact yes/no (1/0).
	 * Later will add more types of relationships.
	 * 
	 * CALLS:		labelIndex
	 * CALLED BY:	constructAllSEC
	 * 
	 * @param main_graphs
	 */
	public void constructOriginalSEC(List<SimpleWeightedGraph<String,DefaultWeightedEdge>> main_graphs)
	{
		//can't use int or double here because primitives are not supported by Generics.
		VertexNameProvider<String> nameProvider = new IntegerNameProvider<String>();
		//for every important graph/timestep
		for(int i = 0; i< main_graphs.size(); i++)
		{
			org.jgrapht.graph.SimpleWeightedGraph<String,DefaultWeightedEdge> igraph = main_graphs.get(i);
			// assign ids in vertex set iteration order. Checked and the order of numbers will be WRONG if don't do this first.
			for (String node : igraph.vertexSet()) 
			{
				nameProvider.getVertexName(node);
			}

			//these are the only non-zero elements in this column
			Set<DefaultWeightedEdge> edges = igraph.edgeSet(); 
			//Collection<String> test = Collections.unmodifiableSet(new Set<String>());
			for(DefaultWeightedEdge iedge : edges)
			{
				String from = igraph.getEdgeSource(iedge);
				String to = igraph.getEdgeTarget(iedge);
				//convert to numbers
				Integer fromNumber = Integer.parseInt(nameProvider.getVertexName(from));
				Integer toNumber = Integer.parseInt(nameProvider.getVertexName(to));
				
				//because Pairs are ordered, need to have the right order for the key to work. The way the map is initialized, the smaller number always comes first
				Pair<Integer, Integer> cur_row_index;
				if(fromNumber>toNumber)
				{
					cur_row_index = new Pair<Integer,Integer>(toNumber,fromNumber); 
				}
				else
				{
					cur_row_index = new Pair<Integer,Integer>(fromNumber, toNumber); 
				}
				Integer rel_value = new Integer((int)igraph.getEdgeWeight(igraph.getEdge(from, to))); //TODO part of encoding more relations
				int currentindex = MyUtil.labelIndex(cur_row_index, this.oSEC.relationlabels);
				
//				if(i==3 || i==4)
//				{
//					System.out.println("i: " + i);
//					System.out.println(igraph.toString());
//					System.out.println("from: " + from + " - " + fromNumber);
//					System.out.println("to: " + to + " - " + toNumber);
//					System.out.println("Value: " + rel_value);
//				}
				
				this.oSEC.SECmatrix.get(currentindex).set(i, rel_value);
			}
		}
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
	
	
	/**
	 * Searches which row-column correspondences lead to the highest overall similarity in a greedy fashion, without assuming which dimension is smaller
	 * If there are equal values, it returns the results when selecting each of these values (recursive)
	 * 
	 * CALLS:		MaxValue
	 * 				removeFromMatrix
	 * CALLED BY: 	spatialSimilarityValueWith
	 * 
	 * @param permutations
	 * @param similarityvalues
	 * @param oneperm
	 * @param tempsimsum
	 * @param nrows
	 * @param spatial_sim_matrix_left
	 * @param SEC1labels_left
	 * @param SEC2labels_left
	 */
	public void greedySimCor(List<List<Pair<String, String>>> permutations, List<Double> similarityvalues, List<Pair<String,String>> oneperm, Double tempsimsum, int nrows, Double[][] spatial_sim_matrix_left, List<String> SEC1labels_left, List<String> SEC2labels_left)
	{
//		//print matrix to inspect progression on search
//		printMatrix(spatial_sim_matrix_left, SEC1labels_left, SEC2labels_left);
		//stop condition
		if(SEC1labels_left.size()==0)
		{
			permutations.add(oneperm);
			similarityvalues.add(tempsimsum/nrows);
//			System.out.println("tempsimsum/nrows: " + tempsimsum + "/" + nrows + "=" + tempsimsum/nrows);
			if(SEC2labels_left.size() != 0)
			{
				System.out.println("WARNING: No correspondences where possible for the following SEC2:\n"+SEC2labels_left.toString());
			}
		}
		else if(SEC2labels_left.size()==0)
		{
			permutations.add(oneperm);
			similarityvalues.add(tempsimsum/nrows);
			System.out.println("tempsimsum/nrows: " + tempsimsum + "/" + nrows + "=" + tempsimsum/nrows);
			//don't need to check whether SEC1labels is 0, because if it was, it would have never gotten in this condition
			System.out.println("WARNING: No correspondences where possible for the following SEC1:\n"+SEC1labels_left.toString());
		}
		else //there are still labels left to correspond
		{
			//need to first determine what the largest val is, and then go through row again to see which ones have the largest val (because there could be more than 1)
			Double largestval = maxValue(spatial_sim_matrix_left);

			for(int irow = 0; irow<spatial_sim_matrix_left.length; irow++)
			{
				for(int icol = 0; icol<spatial_sim_matrix_left[0].length; icol++)
				{
					Double curval = spatial_sim_matrix_left[irow][icol];
					if(curval.equals(largestval))
					{
						//which association does this largest val have?
						String rowind= SEC1labels_left.get(irow);
						String colind= SEC2labels_left.get(icol);
						Pair<String,String> curassoc = new Pair<String,String>(rowind, colind);

						//remove affected row and column, and then go on to the rest of the matrix
						Double[][] next_simmatrix = removeFromMatrix(spatial_sim_matrix_left, irow, icol);
						List<String> newSEC1 = new ArrayList<String>(SEC1labels_left);
						newSEC1.remove(irow);
						//					System.out.println("newSEC1: " + newSEC1.toString());
						List<String> newSEC2 = new ArrayList<String>(SEC2labels_left);
						newSEC2.remove(icol);
						//					System.out.println("newSEC2: " + newSEC2.toString());
						List<Pair<String,String>> addedPerm = new ArrayList<Pair<String,String>>(oneperm);
						addedPerm.add(curassoc);

						greedySimCor(permutations, similarityvalues, addedPerm, tempsimsum+largestval, nrows, next_simmatrix, newSEC1, newSEC2);
					}	
				}
			}
		}
	}

	/**
	 * Alternative function for finding correspondences from a similarity matrix. Instead of checking all child nodes with the same value
	 * (e.g. if there is more than one of the largest value), simply takes the first one it finds and goes with that.
	 * 
	 * CALLS:		maxValue
	 * 				removeFromMatrix
	 * CALLED BY:	temporalSimilarityValueWith
	 * 
	 * @param permutations
	 * @param oneperm
	 * @param tempsimsum
	 * @param nrows
	 * @param spatial_sim_matrix
	 * @param SEC1labels
	 * @param SEC2labels
	 * @return
	 */
	public List<Double> fastSimCor(List<List<Pair<String, String>>> permutations, List<Pair<String,String>> oneperm, Double tempsimsum, int nrows, Double[][] spatial_sim_matrix, List<String> SEC1labels, List<String> SEC2labels)
	{
		List<Double> similarityval = new ArrayList<Double>(); //for storing result
		//stop condition
		if(SEC1labels.size()==0)
		{
			permutations.add(oneperm);
			similarityval.add(tempsimsum/nrows);
			if(SEC2labels.size() != 0)
			{
				System.out.println("WARNING: No correspondences where possible for the following SEC2:\n"+SEC2labels.toString());
			}
			return similarityval;
		}
		else if(SEC2labels.size()==0)
		{
			permutations.add(oneperm);
			similarityval.add(tempsimsum/nrows);
			//don't need to check whether SEC1labels is 0, because if it was, it would have never gotten in this condition
			System.out.println("WARNING: No correspondences where possible for the following SEC1:\n"+SEC1labels.toString());
			return similarityval;
		}
		else //there are still rows to go through
		{
			//need to first determine what the largest val is, and then go through row again to see which ones have the largest val
			Double largestval = maxValue(spatial_sim_matrix);
			
			for(int irow = 0; irow<spatial_sim_matrix.length; irow++)
			{
				for(int icolumn=0; icolumn<spatial_sim_matrix[0].length;icolumn++)
				{
					if(spatial_sim_matrix[irow][icolumn].equals(largestval))
					{
						//which association does this largest val have?
						Pair<String,String> curassoc = new Pair<String,String>(SEC1labels.get(irow), SEC2labels.get(icolumn));

						//remove affected row and column, and then go on to the rest of the matrix
						Double[][] next_simmatrix = removeFromMatrix(spatial_sim_matrix, irow, icolumn);
						List<String> newSEC1 = new ArrayList<String>(SEC1labels);
						newSEC1.remove(irow);
						//					System.out.println("newSEC1: " + newSEC1.toString());
						List<String> newSEC2 = new ArrayList<String>(SEC2labels);
						newSEC2.remove(icolumn);
						//					System.out.println("newSEC2: " + newSEC2.toString());
						List<Pair<String,String>> addedPerm = new ArrayList<Pair<String,String>>(oneperm); //TODO I don't think it's necessary to make a new list here. Check with next update.
						addedPerm.add(curassoc);

						return fastSimCor(permutations, addedPerm, tempsimsum+largestval, nrows, next_simmatrix, newSEC1, newSEC2);
					}				
				}
			}
		}
		return similarityval;
	}
	
	public List<Double> simpleSimCor(List<List<Pair<String, String>>> permutation_res, List<Double> similarityval_res, Double[][] spatial_sim_matrix, CompressedSEC CSEC1, CompressedSEC CSEC2)
	{
		List<Pair<String,String>> permutation = new ArrayList<Pair<String,String>>();
		Double simsum = new Double(0);
		//cycle through all the rows of our CompressedSEC and look for correspondences. It doesn't really matter which one we cycle through, because once we're through one, all the other rows that may be left in the other SEC will necessarily not have any correspondences.
		List<String> csec1labels = CSEC1.getRelationStrings();
		List<String> csec2labels = CSEC2.getRelationStrings();
		for(int i=0; i<csec1labels.size(); i++)
		{
			for(int j=0; j<csec2labels.size(); j++)
			{
				String sec1name = CSEC1.getRelationNameStrings().get(i); //instead of numeric pairs, get a string with the name of the object in it
				String sec2name = CSEC2.getRelationNameStrings().get(j);
				if(sec1name.equals(sec2name)) //if the name of the relationship is the same
				{
					//add this correspondence to the permutation
					Pair<String,String> curassoc = new Pair<String,String>(csec1labels.get(i), csec2labels.get(j));
					permutation.add(curassoc);
					
					//add similarity to sum
					Double simval = spatial_sim_matrix[i][j];
					simsum += simval;
				}
			}
		}
		permutation_res.add(permutation); //Note that necessarily the permutations list will only contain one item. Kept it a list though to be compatible with the other functions using permutations, regardless of with SimCor was used
		Double similarityval = simsum/spatial_sim_matrix.length;
		similarityval_res.add(similarityval);
		return similarityval_res;
	}
	
	/**
	 * Help function for spatialSimilarityMatrix. Computes the maximum similarity between two rows (of different SECs),
	 * Assumes currow1 >= currow2.
	 * 
	 * @param currow1
	 * @param currow2
	 * @return
	 */
	private double max_spatial_row_similarity(List<String> currow1, List<String> currow2)
	{
		double max_similarity = 0;
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
			}
		}
		return max_similarity;
	}
	
	
	/**
	 * Computes the spatial similarity matrix for two compressed SEC, by comparing each row to each row.
	 * Similarity(i,j) = 100/max_length * SUM(identical entries over row i and j)
	 * If one row is shorter than the other, calculates similarity value for all possible shifts/matches and takes the maximum of that as
	 * the similarity value.
	 * 
	 * @param SEC1
	 * @param SEC2
	 * @return
	 */
	private Double [][] spatialSimilarityMatrix(CompressedSEC SEC1, CompressedSEC SEC2)
	{
		Double[][] similarity_matrix = new Double[SEC1.SECmatrix.size()][SEC2.SECmatrix.size()];
		System.out.println("Comparing: ");
		SEC1.printSEC();
		System.out.println("To: ");
		SEC2.printSEC();

		//Every row of SEC1 is compared to every row of SEC2 to find the highest similarity.
		int irow =0; //to keep track of where to store the similarity value in the matrix. 
		for(List<String> irow_SEC1 : SEC1.SECmatrix)
		{
			int icolumn =0;
			for(List<String> irow_SEC2 : SEC2.SECmatrix)
			{
				double max_similarity = 0; //maximum similarity for the comparison of the two rows
				if(irow_SEC1.size() > irow_SEC2.size())
				{					
					max_similarity = max_spatial_row_similarity(irow_SEC1, irow_SEC2);
				}
				else //currow2 is bigger or of equal size as currow1
				{
					max_similarity = max_spatial_row_similarity(irow_SEC2, irow_SEC1);
				}
				similarity_matrix[irow][icolumn]=max_similarity;
				icolumn++;
			}
			irow++;
		}
		return similarity_matrix;
	}
	
	/**
	 * Wrap for spatialSimilarityMatrix and greedySimCor function. Initializes variables and prints out results.
	 * In addition it will check whether the final similarity value for the different permutations are also the same and get rid of the permutations
	 * that in the end turn out to be suboptimal. (it may occur that for a specific row there are multiple, equally valid options, but then
	 * for later rows it turns out that a certain pick for the earlier row would have been better).
	 * 
	 * Unless we want to determine the similarity of actions using different objects (cup vs bowl or something like that), we don't have a problem
	 * matching it. But that might be nice to be able to do so I'll implement it here so that if there are equally largest values in one row, we
	 * will compute the similarity for both possibilities. If there is more than one permutation with equal final similarity value, will return
	 * multiple permutation assignment lists.
	 * 
	 * NOTE: rows that do not have any corresponding rows are not inside permutation_res. In the temporalSimilarityValue function, when reordering the rows, all the non-corresponding rows will be placed at the end.
	 * 
	 * NOTE: if exactMatch = true, will call simpleSimCor instead of greedySimCor; instead of looking at the chains for matching rows, we assume that the objects that should
	 * be matched have the same object name and this name is known (like it is in our simple cases). There is only one permutation inside that returned list.
	 * 
	 * Takes an additional argument nodenames, which contains the mapping from numbers to names.
	 * @param SEC2
	 * @return 
	 */
	public PermResults spatialSimilarityValue(CompressedSEC CSEC1, CompressedSEC CSEC2, boolean exactMatch)
	{
		Double[][] spatial_sim_matrix = spatialSimilarityMatrix(CSEC1, CSEC2);
		MyUtil.printMatrix(spatial_sim_matrix, CSEC1.getRelationStrings(), CSEC2.getRelationStrings(), "Spatial Similarity Matrix:");
		//nrows needs to be given as a constant because the recursive function will lose the rows in recursion. Changed to ndimension because if the input is not matched according to dimension, there might be more columns than rows and then we want to divide by that.
		int nmax_dimension = Math.max(spatial_sim_matrix.length, spatial_sim_matrix[0].length);
		//variables that need to be given to recurse on
		List<Pair<String,String>> oneperm = new ArrayList<Pair<String,String>>();

		//variables for storing results
		List<List<Pair<String, String>>> permutations_res = new ArrayList<List<Pair<String,String>>>();
		List<Double> similarityval_res = new ArrayList<Double>();
		if(exactMatch)
		{
			simpleSimCor(permutations_res, similarityval_res, spatial_sim_matrix, this.getCSEC(), CSEC2);
		}
		else
		{
			greedySimCor(permutations_res, similarityval_res, oneperm, 0.0, nmax_dimension, spatial_sim_matrix, CSEC1.getRelationStrings(), CSEC2.getRelationStrings());

		}
		PermResults result = new PermResults(permutations_res, similarityval_res);
		//it may be that some of the permutations eventually lead to a lower similarity value, so take these out
		//determine maximum value of row
		Double maxval = maxValue(similarityval_res);
		//throw out items that are not equal to the maximum value
		for(int i=similarityval_res.size()-1; i>=0; i--)//doing it the other way around to not mess up the indexes when removing something
		{
			if(similarityval_res.get(i) < maxval)
			{
				System.out.println("Removing value " + similarityval_res.get(i) + "\nPermutation " + permutations_res.get(i));
				result.removeItem(i);
			}
		}
		System.out.println(permutations_res.toString());
		return result;
	}

	/**
	 * Help function for temporalSimilarityMatrix. Computes the similarity of one column (of different SECs).
	 * Assumes curcol1.size == curcol2.size (because was filled with dummies if necessary)
	 * 
	 * CALLS:		-
	 * CALLED BY:	temporalSimilarityMatrix
	 * 
	 * @param curcol1
	 * @param curcol2
	 * @return
	 */
	private double single_temporal_similarity(List<String> curcol1, List<String> curcol2)
	{
		int sum = 0;
		for(int i=0; i<curcol1.size(); i++) //both columns should be of the same size
		{
			String el1 = curcol1.get(i);
			String el2 = curcol2.get(i);
			if(el2.equals(el1))
			{
				sum++;
			}
		}
		double theta_ab = (100.0/curcol1.size()) * sum;
		return theta_ab;
	}

	/**
	 * Computes the temporal similarity matrix for two derived SEC, by comparing each column to each column.
	 * Similarity(i,j) = 100/max_length * SUM(identical entries over columns i and j)
	 * Assumes curcol1.size == curcol2.size (because was filled with dummies if necessary)
	 * 
	 * CALLS: 		single_temporal_similarity
	 * 				getColumnFrom2DList
	 * CALLED BY:	temporalSimilarityValueWith
	 * 
	 * @param tSEC1
	 * @param tSEC2
	 * @return
	 */
	private Double[][] temporalSimilarityMatrix(DerivedSEC tSEC1, DerivedSEC tSEC2)
	{
		System.out.println("Comparing: ");
		tSEC1.printSEC();
		System.out.println("To: ");
		tSEC2.printSEC();
		
		//compare all columns with all columns to construct similarity matrix. No shuffling needed.
		Double[][] similarity_matrix = new Double[tSEC1.SECmatrix.get(0).size()][tSEC2.SECmatrix.get(0).size()];
		int irow =0; //to keep track of where to store the similarity value in the matrix. 
		for(int icol1=0; icol1<tSEC1.SECmatrix.get(0).size(); icol1++) //a of theta
		{
			int icolumn =0;//to keep track of where to store the similarity value in the matrix. 
			for(int icol2=0; icol2<tSEC2.SECmatrix.get(0).size(); icol2++) //b of theta
			{
				List<String> icol_SEC1 = getColumnFrom2DList(tSEC1.SECmatrix, icol1);
				List<String> icol_SEC2 = getColumnFrom2DList(tSEC2.SECmatrix, icol2);
				//take the two current SEC columns and calculate the similarity value for the current cell of the matrix for that
				similarity_matrix[irow][icolumn] = single_temporal_similarity(icol_SEC1, icol_SEC2);
				icolumn++;
			}
			irow++;
		}
		return similarity_matrix;
	}

	/**
	 * Compute maximum temporal similarity for all permutations given by spatialSimilarityValueWith. 
	 * 
	 * If number of rows are not equal, fill up with dummy rows. (use value 0, because in a derived matrix all numbers will have at least two digits, so it won't be similar to anything).
	 * 
	 * CALLS:		DerivedSEC.reorderDerivedSEC
	 * 				DerivedSEC.extendWithDummySEC
	 * 				MyUtil.printMatrix
	 * 				fastSimCor
	 * CALLED BY:	
	 * 
	 * @param derived_SEC2
	 * @param perm_res
	 * @return
	 */
	public SimTotalResults temporalSimilarityValueWith(DerivedSEC derived_SEC1, DerivedSEC derived_SEC2, PermResults perm_res)
	{
		//get the possible permutations from spatial similarity perspective
		List<List<Pair<String, String>>> permutations = perm_res.getPerms(); 

		//initialize variables for storing results
		Double max_temp_sim = new Double(0);
		Double max_spat_sim = new Double(0);
		List<List<Pair<String, String>>> max_spat_permutation = new ArrayList<List<Pair<String,String>>>(); //for storing exactly which permutations lead to the given similarity value
		List<List<Pair<String, String>>> max_temp_permutation = new ArrayList<List<Pair<String,String>>>();

		//how many rows is the max? need to fill smaller matrix with dummies
		int max_rows = Math.max(derived_SEC1.SECmatrix.size(), derived_SEC2.SECmatrix.size());

		//produce similarity matrices for all permutations
		for(int iperm=0; iperm < permutations.size(); iperm++)
		{
			//Create a SEC2 that is ordered such that the rows correspond to SEC1 according to the current permutation
			derived_SEC2.reorderDerivedSEC(derived_SEC1, permutations.get(iperm));

			//extend with dummy rows if necessary
			DerivedSEC tSEC1 = derived_SEC1.extendWithDummySEC(max_rows);
			DerivedSEC tSEC2 = derived_SEC2.extendWithDummySEC(max_rows);
			
			//compare all columns with all columns to construct similarity matrix. No shuffling needed.
			Double[][] similarity_matrix = temporalSimilarityMatrix(tSEC1, tSEC2);
			MyUtil.printMatrix(similarity_matrix, tSEC1.getTimeStrings(), tSEC2.getTimeStrings(), "temporal similarity matrix"); //SEC2 first because of transposition

			//compute final similarity value and correspondences
			int nmax_dimension = Math.max(similarity_matrix.length, similarity_matrix[0].length);
			List<Pair<String,String>> oneperm = new ArrayList<Pair<String,String>>(); //variable to recurse on
			//variables for storing results
			List<List<Pair<String, String>>> permutations_res = new ArrayList<List<Pair<String,String>>>();
			List<Double> similarityval_res = fastSimCor(permutations_res, oneperm, 0.0, nmax_dimension, similarity_matrix, tSEC1.getTimeStrings(), tSEC2.getTimeStrings());
//			//this finds better solutions but is just too slow: TODO maybe reimplementation can make it much faster
//			List<Double> similarityval_res = new ArrayList<Double>();
//			greedySimCor(permutations_res, similarityval_res, oneperm, 0.0, nrows, r_similarity_matrix, tSEC1.getTimeStrings(), tSEC2.getTimeStrings());
			
//			System.out.println("all permutationres temporal: " +permutations_res.toString());
//			System.out.println("all similarityres temporal: " + similarityval_res.toString());
			//compare with previous stored value: if it's greater, throw out all the other ones, if it's the same, add to the list, if it's smaller, ignore
			for(int j=0; j<similarityval_res.size(); j++) //the values in similarityval_res are not perse all equally large, so can't test them as a group, need to loop over
			{
				int compareval = similarityval_res.get(j).compareTo(max_temp_sim);
				if(compareval == 0) //if no previously stored value yet, will only be equal if the similarity is actually 0. If the highest similarity IS 0, maybe still want the permutation list no? so that's why it's not a special case here
				{
					max_temp_permutation.add(permutations_res.get(j)); //add current permutation, max_sim stays the same
					if(!max_spat_permutation.contains(permutations.get(iperm))) //has the current permutation been added to this? TODO probably more efficient using Set
					{
						max_spat_permutation.add(permutations.get(iperm));
					}
				}
				else if(compareval > 0) //similarityval_res(j) is larger than max_sim
				{
					//replace max_sim
					max_temp_sim = similarityval_res.get(j);
					max_spat_sim = perm_res.getSim().get(iperm);
					//replace permutation list
					max_temp_permutation = new ArrayList<List<Pair<String,String>>>();
					max_temp_permutation.add(permutations_res.get(j));
					max_spat_permutation = new ArrayList<List<Pair<String,String>>>();
					max_spat_permutation.add(permutations.get(iperm));
				}
				//if similarityval_res(j) smaller than max_sim, nothing happens
			}
		}
		System.out.println("Max. spatial similarity:" + max_spat_sim);
		System.out.println("Max. temporal similarity:" + max_temp_sim);
		System.out.println("for spatial permutations: " + max_spat_permutation.toString());
		System.out.println("and temporal permutations: " + max_temp_permutation.toString());
		
		SimTotalResults results = new SimTotalResults(max_spat_permutation, max_temp_permutation, max_spat_sim, max_temp_sim);
		return results;
	}

	/**
	 * Get the maximum value of the elements in a list
	 * 
	 * @param list
	 * @return
	 */
	public Double maxValue(List<Double> list)
	{
		Double maxval = list.get(0);
		for(int i=1; i<list.size(); i++)
		{
			if(list.get(i) >= maxval)
			{
				maxval = list.get(i);
			}
		}
		return maxval;
	}
	
	/**
	 * Get the maximum value of the elements in an array
	 * 
	 * @param array
	 * @return
	 */
	public Double maxValue(Double[] array)
	{
		Double maxval = array[0];
		for(int i=1; i<array.length; i++)
		{
			if(array[i] >= maxval)
			{
				maxval = array[i];
				//				System.out.println("Pos: " + i + "; Maxval: " + maxval);
			}
		}
		return maxval;
	}
	
	/**
	 * Get the maximum value of the elements in a 2d array
	 * 
	 * @param matrix
	 * @return
	 */
	public Double maxValue(Double[][] matrix)
	{
		Double maxval = matrix[0][0]; //not initializing to 0 but to a value of the parameters so behavior is OK also for negative valued input
		for(int irow=0; irow<matrix.length; irow++)
		{
			Double maxirow = maxValue(matrix[irow]);
			if(maxirow > maxval)
			{
				maxval = maxirow;
			}
		}
		return maxval;
	}
	
	/**
	 * Takes a matrix (2D array) and returns it with a specific row and column removed
	 * If no row or column should be removed, call with that set to -1
	 *  
	 * @param matrix
	 * @param rm_row
	 * @param rm_column
	 * @return 
	 */
	public Double[][] removeFromMatrix(Double[][] matrix, int rm_row, int rm_column)
	{
		int nrows = matrix.length;
		int ncolumns = matrix[0].length;
		//determine dimensions of the new matrix
		int newnr;
		int newnc;
		if(rm_row < 0)
			newnr=nrows;
		else
			newnr=nrows-1;
		if(rm_column < 0)
			newnc=ncolumns;
		else
			newnc=ncolumns-1;

		Double[][] newmatrix = new Double[newnr][newnc];
		int newi=0; //keep track of indexes of new matrix
		for(int i=0; i<nrows; i++)
		{
			if(i==rm_row) //skip this row in copying
			{
				continue;
			}
			int newj=0;
			for(int j=0; j<ncolumns;j++)
			{
				if(j==rm_column)//skip this column in copying
				{
					continue;
				}
				newmatrix[newi][newj]=matrix[i][j];
				newj++;
			}
			newi++;
		}
		return newmatrix;
	}
	
	/**
	 * Returns the column (2nd dimension of the nested arraylist) at index i
	 * @param arraylist
	 * @param index
	 * @return
	 */
	public static <T> List<T> getColumnFrom2DList(List<List<T>> arraylist, int index)
	{
		List<T> column = new ArrayList<T>();
		for(List<T> irow : arraylist)
		{
			column.add(irow.get(index));
		}
		return column;
	}
}
