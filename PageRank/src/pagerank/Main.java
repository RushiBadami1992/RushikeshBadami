package pagerank;

import java.io.IOException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;


public class Main {
	
	public static void main(String args[]) throws IOException
	{
		//Declaration of Data Structures Needed
		GeneralFunctions functions=new GeneralFunctions();
		PageRank rank=new PageRank();
		Convergence convergence=new Convergence();
		Main main=new Main();
		HashMap<String,HashSet<String>> inLinks=new HashMap<String,HashSet<String>>();
		HashSet<String> childNodes=new HashSet();
		ArrayList<String> sinkNodes=new ArrayList<String>();
		HashMap<String,Integer> inLinkCount=new HashMap<String, Integer>();
		HashMap<String, Integer> numberOfOutLinks=new HashMap<String, Integer>();
		HashMap<String,Double> pageRank=new HashMap<String, Double>();
		HashSet<String> getSourceNodes=new HashSet<String>();
		ArrayList<String> getpages=new ArrayList<String>();
		
		System.out.println("Page rank of small graph for one Iteration:");
		main.pageRankForTestGraph(1);
		System.out.println("Page rank of small graph for ten Iteration:");
		main.pageRankForTestGraph(10);
		System.out.println("Page rank of small graph for hundred Iteration:");
		main.pageRankForTestGraph(100);
		
		String file="wt2g_inlinks.txt";
		inLinks=functions.populateDataStructure(inLinks,file);
		childNodes=functions.getChildNodes(childNodes,file);
		sinkNodes=functions.getSinkNodes(inLinks, childNodes);
	    getpages=functions.getallpages(getpages,file);
	    numberOfOutLinks=functions.getNumberOfOutLinks(numberOfOutLinks,file,inLinks);
	    pageRank=rank.calculatePageRankSmallGraph(inLinks,numberOfOutLinks,pageRank,sinkNodes,getpages,1);
	    
	// 	inLinkCount=functions.getInLinkCount(inLinkCount, file);
	//	inLinkCount=functions.sortMap(inLinkCount);
	    int numberOfSourceLinks=functions.calculateSourceLinks(file);
	    int totlalPages=inLinks.size();
	    
		//System.out.println("Number Of sourcelinks:"+numberOfSourceLinks);
		int sinknodes=sinkNodes.size();
		numberOfOutLinks=functions.getNumberOfOutLinks(numberOfOutLinks,file,inLinks);
		System.out.println("numberof outlinks "+numberOfOutLinks.size());
	//	functions.printHashMap(numberOfOutLinks);
	//	functions.printHashMap(numberOfOutLinks);
	    pageRank=rank.calculatePageRank(inLinks,numberOfOutLinks,pageRank,sinkNodes,getpages);
	    pageRank=functions.sortMap(pageRank);
	    
	    
	    inLinkCount=functions.getInLinkCount(inLinkCount, file);
		inLinkCount=functions.sortMapByInLinkCount(inLinkCount);
		System.out.println("Top 50 pagerank by inlink of bigger graph:");
		functions.printHashMapInLinkCount(inLinkCount);
	    
		
		System.out.println("Top 50 PageRank of bigger graph:");
	    functions.printHashMap(pageRank);
	   
	    
	    System.out.println("The proportion of pages with no in-links (sources):\t"+numberOfSourceLinks/(double)totlalPages);
	    System.out.println("The proportion of pages with no out-links (sinks):\t"+sinknodes/(double)totlalPages);
	    
	    double intialRank=functions.proportionOfPages(pageRank,(1/(double)totlalPages));
	    System.out.println("The proportion of pages whose PageRank is less than their initial, uniform values:\t"+intialRank/(double)totlalPages);
	}
	
	public void pageRankForTestGraph(int i) throws IOException
	{
		GeneralFunctions functions=new GeneralFunctions();
		PageRank rank=new PageRank();
		Convergence convergence=new Convergence();
		HashMap<String,HashSet<String>> inLinks=new HashMap<String,HashSet<String>>();
		HashSet<String> childNodes=new HashSet();
		ArrayList<String> sinkNodes=new ArrayList<String>();
		HashMap<String,Integer> inLinkCount=new HashMap<String, Integer>();
		HashMap<String, Integer> numberOfOutLinks=new HashMap<String, Integer>();
		HashMap<String,Double> pageRank=new HashMap<String, Double>();
		HashSet<String> getSourceNodes=new HashSet<String>();
		ArrayList<String> getpages=new ArrayList<String>();
		
		String file="test.txt";
		inLinks=functions.populateDataStructure(inLinks,file);
		childNodes=functions.getChildNodes(childNodes,file);
		sinkNodes=functions.getSinkNodes(inLinks, childNodes);
	    getpages=functions.getallpages(getpages,file);
	    numberOfOutLinks=functions.getNumberOfOutLinks(numberOfOutLinks,file,inLinks);
	    pageRank=rank.calculatePageRankSmallGraph(inLinks,numberOfOutLinks,pageRank,sinkNodes,getpages,i);
	    functions.printHashMap(pageRank);
	}

}
