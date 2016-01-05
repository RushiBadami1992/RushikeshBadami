package pagerank;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

public class PageRank {
	Convergence convergence = new Convergence();
//This function is use to get PageRank for the graph with 183811 pages
	public HashMap<String, Double> calculatePageRank(
			HashMap<String, HashSet<String>> inLinks,
			HashMap<String, Integer> numberOfOutLinks,
			HashMap<String, Double> pageRank, ArrayList<String> sinkNodes,
			ArrayList<String> getPages) {
		ArrayList<Double> perplexity = new ArrayList<Double>();

		double n = (1 / (double) inLinks.size());
		double d = 0.85;
		double lambda = (1 - d);
		boolean isConvergence = true;
		double pageRankq = 0;
		double N = inLinks.size();
		HashMap<String, Double> oldPageRank = new HashMap<String, Double>();
		// System.out.println("Initial page rank"+n);
		int ks = 0;
		Set setOfKeys = inLinks.keySet();
		 Iterator itmap = setOfKeys.iterator();
		while(itmap.hasNext())
		{
			String key=(String) itmap.next();
			oldPageRank.put(key, n);
		}
		// System.out.println(ks);
		// System.out.println(oldPageRank.size());

		int j = 1;

		// System.out.println("Page rank Size"+oldPageRank.size());
		System.out.println("Perplexity Values and iteration:");
		while (isConvergence) {
			
			double sinkPR = 0;
			for (int i = 0; i < sinkNodes.size(); i++) {
				String key = sinkNodes.get(i);
				// System.out.println(key);
				key = key.trim();
				// System.out.println(key);
				sinkPR = sinkPR + oldPageRank.get(key);
			}
			//System.out.println("sinkPR\t" + sinkPR);
			int pages = 0;
			Set setO = inLinks.keySet();
			 Iterator itr = setO.iterator();
			while (itr.hasNext()) {
				String k = (String) itr.next();
				// System.out.println(sinkPR);
				double newPR = lambda / N;
				newPR = newPR + (d * sinkPR) / N;
				Set<String> getChildNodes = new HashSet<String>();
				if (inLinks.containsKey(k)) {
					getChildNodes = inLinks.get(k);
				}
			
				for (String kchild : getChildNodes) {
					double outLinkCount = 0;
					String keyForOldRank = kchild;
					keyForOldRank = keyForOldRank.trim();
					// System.out.println(keyForOldRank);
					if(oldPageRank.containsKey(kchild))
					{
					pageRankq = oldPageRank.get(keyForOldRank);
					}
					if(oldPageRank.containsKey(kchild))
					{
					outLinkCount = numberOfOutLinks.get(keyForOldRank);
					}
					if (outLinkCount != 0) {
						newPR = newPR + d * pageRankq / (double) outLinkCount;
					}

				}
				// System.out.println(newPR);
				pageRank.put(k, newPR);
				pages++;
			}

			//System.out.println(pageRank);
			perplexity = convergence.calculatePerplexity(pageRank,perplexity);
			// System.out.println("Perplexity :"+perplexity.get(j));
			Set set = pageRank.keySet();
			Iterator i = set.iterator();
			oldPageRank.clear();
			while (i.hasNext()) {
				String key = (String) i.next();
				oldPageRank.put(key, pageRank.get(key));
			}
			pageRank.clear();

			isConvergence = convergence.isConvergence(perplexity);
			
			System.out.println("Perplexity:\t"+perplexity.get(j-1)+"\t"+"Iteration:\t"+j);
			j = j + 1;
			
		}

		return oldPageRank;
	}
	
	//This function is use to get page Rank values for test graph
	public HashMap<String, Double> calculatePageRankSmallGraph(
			HashMap<String, HashSet<String>> inLinks,
			HashMap<String, Integer> numberOfOutLinks,
			HashMap<String, Double> pageRank, ArrayList<String> sinkNodes,
			ArrayList<String> getPages,int iteration) {
		ArrayList<Double> perplexity = new ArrayList<Double>();

		double n = (1 / (double) inLinks.size());
		double d = 0.85;
		double lambda = (1 - d);
		boolean isConvergence = true;
		double pageRankq = 0;
		double N = inLinks.size();
		HashMap<String, Double> oldPageRank = new HashMap<String, Double>();
		// System.out.println("Initial page rank"+n);
		int ks = 0;
		Set setOfKeys = inLinks.keySet();
		 Iterator itmap = setOfKeys.iterator();
		while(itmap.hasNext())
		{
			String key=(String) itmap.next();
			oldPageRank.put(key, n);
		}
		// System.out.println(ks);
		// System.out.println(oldPageRank.size());

		int j = 0;

		// System.out.println("Page rank Size"+oldPageRank.size());

		while (j<iteration) {
			//System.out.println("Counter:" + j);
			double sinkPR = 0;
			for (int i = 0; i < sinkNodes.size(); i++) {
				String key = sinkNodes.get(i);
				// System.out.println(key);
				key = key.trim();
				// System.out.println(key);
				sinkPR = sinkPR + oldPageRank.get(key);
			}
		//	System.out.println("sinkPR\t" + sinkPR);
			int pages = 0;
			Set setO = inLinks.keySet();
			 Iterator itr = setO.iterator();
			while (itr.hasNext()) {
				String k = (String) itr.next();
				// System.out.println(sinkPR);
				double newPR = lambda / N;
				newPR = newPR + (d * sinkPR) / N;
				Set<String> getChildNodes = new HashSet<String>();
				if (inLinks.containsKey(k)) {
					getChildNodes = inLinks.get(k);
				}
			
				for (String kchild : getChildNodes) {
					double outLinkCount = 0;
					String keyForOldRank = kchild;
					keyForOldRank = keyForOldRank.trim();
					// System.out.println(keyForOldRank);
					if(oldPageRank.containsKey(kchild))
					{
					pageRankq = oldPageRank.get(keyForOldRank);
					}
					if(oldPageRank.containsKey(kchild))
					{
					outLinkCount = numberOfOutLinks.get(keyForOldRank);
					}
					if (outLinkCount != 0) {
						newPR = newPR + d * pageRankq / (double) outLinkCount;
					}

				}
				// System.out.println(newPR);
				pageRank.put(k, newPR);
				pages++;
			}

			
			Set set = pageRank.keySet();
			Iterator i = set.iterator();
			oldPageRank.clear();
			while (i.hasNext()) {
				String key = (String) i.next();
				oldPageRank.put(key, pageRank.get(key));
			}
			pageRank.clear();

			
			j = j + 1;
			
		}

		return oldPageRank;
	}


	
}
