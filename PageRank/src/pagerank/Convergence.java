package pagerank;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

public class Convergence {

	//This function is use to calculate perplexity value for each iteration.
	public ArrayList<Double> calculatePerplexity(HashMap<String, Double> pageRank,ArrayList<Double> perplexity) {
	
		double entropy=(double)calculate_entropy(pageRank);
		//System.out.println("Entropy:\t"+entropy);
		double perplexi=Math.pow(2, (-entropy));
	//	System.out.println(perplexi);
		perplexity.add(perplexi);
	//	System.out.println("perlexi\t"+perplexi);
		return  perplexity;
		}
 //This function is use to calculate entropy for each iteration
	private double calculate_entropy(HashMap<String, Double> pageRank) {
		double entropy = 0;
		Set setOfk=pageRank.keySet();
		Iterator iter=setOfk.iterator();
		while (iter.hasNext())
		{
			String key=(String)iter.next();
	//		System.out.println("Page Rank"+pageRank);
			double pageRnk=pageRank.get(key);
		//	System.out.println("Page Rank"+pageRnk);
			entropy=entropy+pageRnk*((double)(Math.log(pageRnk)/(double)Math.log(2)));
		//	System.out.println("Entropy1"+entropy);
		}
	//	System.out.println(entropy);
		return entropy;
	}
	//Check for convergence after every iteration.
	public boolean isConvergence(ArrayList<Double> perplexity){
	double booleanValue = 0;
	boolean isConvergence=true;
	int counter=0;
	//System.out.println("In converergence");
//	System.out.println("Perplexity"+perplexity.size());
	if(perplexity.size()>=5)
	{
	//	System.out.println("Inside");
		int x=perplexity.size()-1;
		int y=perplexity.size()-5;
//		System.out.println("x and y"+x+y);
	for(int i=x;i>y;i--)	
	{
	//	System.out.println("inner inside");
     booleanValue=Math.abs(perplexity.get(i)-perplexity.get(i-1));
    // System.out.println("bool1"+booleanValue);
     if(booleanValue<1)
     {
    	//System.out.println("bool2"+booleanValue);
    	 counter ++;
    	// System.out.println("counter in convergence"+counter);
     }
	}
	}
	if(counter==4)
	{
		isConvergence=false;
	}
	
	return isConvergence;

	}
}
