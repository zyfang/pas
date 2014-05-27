/**
 * 
 */
package actSimG_prologMongo;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Zhou Fang, 05-2014, University of Bremen
 *
 */
public class MyUtil {
	
	/**
	 * Finds the index of the given relation in the label list (this is how we can retrace which row and column a value belongs to)
	 * @param index
	 * @return
	 */
	public static <T> int labelIndex(T label, List<T> labelslist)
	{
		int result = labelslist.indexOf(label);
		if(result < 0)
		{
			System.out.println("WARNING: label " + label.toString() + " was not found in the list.");
		}
		return result;
	}
	
	public static Double[][] transposeMatrix(Double[][] matrix)
	{
		Double[][] newmatrix = new Double[matrix[0].length][matrix.length]; //column and row length are reversed compared to old matrix
		//loop through rows
		for(int i=0; i<matrix.length; i++)
		{
			//loop through columns
			for(int j=0; j<matrix[0].length; j++)
			{
				newmatrix[j][i] = matrix[i][j];
			}
		}
		return newmatrix;
	}
	
	/**
	 * Converts a set of Pairs to a list of Strings
	 * 
	 * CALLED BY:	spatialSimilarityValue (with Pair set)
	 * 				temporalSimilarityValue (with Long set)
	 * CALLS:		-
	 * 
	 * @param pair
	 * @return
	 */
	public <T> List<String> listToStringList(List<T> pair)
	{
		List<String> result= new ArrayList<String>();
		for(T item : pair)
		{
			result.add(item.toString());
		}
		return result;
	}

	/**
	 * print a primitive two-dimensional matrix with row and column labels
	 * 
	 * @param m
	 */
	public static <T> void printMatrix(T[][] m, List<String> rlabels, List<String> clabels, String matrixname)
	{
		//In Java, 2D arrays are really arrays of arrays with possibly different lengths (there are no guarantees that in 2D arrays 
		//that the 2nd dimension arrays all be the same length)
		//You can get the length of any 2nd dimension array as z[n].length where 0 <= n < z.length.
		//If you're treating your 2D array as a matrix, you can simply get z.length and z[0].length, but note that you might be 
		//making an assumption that for each array in the 2nd dimension that the length is the same.
		if(matrixname != null)
		{
			System.out.println(matrixname);
		}
		System.out.println("\t" + clabels.toString());

		for(int i=0; i<m.length;i++)
		{
			System.out.print(rlabels.get(i).toString() + "\t");
			for(int j=0; j<m[0].length;j++)
			{
				System.out.print(m[i][j]+ " ");
			}
			System.out.println();
		}
	}

	/**
	 * print a primitive array
	 * 
	 * @param a
	 */
	public static void printArray(double[] a)
	{
		for(int i=0; i<a.length;i++)
			System.out.print(a[i]+ " ");
		System.out.println();
	}

}
