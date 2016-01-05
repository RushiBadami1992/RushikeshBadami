package pagerank;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.swing.text.html.InlineView;

public class GeneralFunctions {

	//Store the page and in links of page in a hashmap 
	public HashMap<String,HashSet<String>>  populateDataStructure(HashMap<String,HashSet<String>> inLinks,String file) throws IOException
	{
		
		BufferedReader reader=new BufferedReader(new FileReader(file));
		String line;
		String key=null;
		while ((line=reader.readLine())!= null)
		{
			String delims=" ";
			
		//	System.out.println("Example of line"+line );
			String[] tokens=line.split(delims);
			HashSet<String> list=new HashSet<String>();
			for(int i=0;i<tokens.length;i++)
			{
				if(i==0)
				{
				key=tokens[i];
				}
				else
				{
					list.add(tokens[i]);
				}
			}
			if(key!=null)
			{
			inLinks.put(key,list);
			}
		}
		return inLinks;
	}
	
	//This function is use to print the page rank of hash map
	public void printHashMap(HashMap<String,Double> inLinks)
	{
		 Set setOfKeys = inLinks.keySet();
		 Iterator iterator = setOfKeys.iterator();
		 //System.out.println(setOfKeys.size());
		 int count=0;
		while (iterator.hasNext()) {
			 String key = (String) iterator.next();
			 double list= inLinks.get(key);
			 System.out.println("Page:\t"+key+" "+"Page Rank:\t"+list );
			 if(count>50)
				 break;
				 
			 count ++;
			
			}
		 
		 }
	//Use to print top 50 pages having maximum inlink count
	public void printHashMapInLinkCount(HashMap<String,Integer> inLinks)
	{
		 Set setOfKeys = inLinks.keySet();
		 Iterator iterator = setOfKeys.iterator();
		 //System.out.println(setOfKeys.size());
		 int count=0;
		while (iterator.hasNext()) {
			 String key = (String) iterator.next();
			 double list= inLinks.get(key);
			 System.out.println("Page:\t"+key+" "+"InLinkCount:\t"+list );
			 if(count>50)
				 break;
				 
			 count ++;
			
			}
		 
		 }
	//Used for testing to check if the file was getting properly loaded in my datastructure
	public void printHashInLinks(HashMap<String,HashSet<String>> inLinks)
	{
		 Set setOfKeys = inLinks.keySet();
		 Iterator iterator = setOfKeys.iterator();
		 //System.out.println(setOfKeys.size());
		 int count=0;
		while (iterator.hasNext()) {
			 String key = (String) iterator.next();
			 HashSet<String> list= inLinks.get(key);
			 System.out.println("Key:"+key+" "+"list:"+list );
			 
			
			}
		 
		 }
	//Use to get childnodes of each page for sinknode calculation
	public HashSet<String> getChildNodes(HashSet<String> getChildNodes,String file) throws IOException
	{
		
		BufferedReader reader=new BufferedReader(new FileReader(file));
		String line = null;
		
		String delims=" ";
		String key=null;
	//	System.out.println("Example of line"+line );
		while ((line=reader.readLine())!= null)
		{
		String[] tokens=line.split(delims);
		for(int i=0;i<tokens.length;i++)
		{
			if(i==0)
			{
			key=tokens[i];
			}
			else
			{
		//		System.out.println(tokens[i]);
				getChildNodes.add(tokens[i]);
				
			}
		}
		}
		return getChildNodes;
	}
    //Use to get sink node of each graph 
	public ArrayList<String> getSinkNodes(HashMap<String,HashSet<String>> inLinks,HashSet<String>childNodes )
	{
	    ArrayList<String> sinkNodes=new ArrayList<String>();
		Set setOfKeys = inLinks.keySet();
		 Iterator iterator = setOfKeys.iterator();
		 while (iterator.hasNext())
		 {
			 String key = (String) iterator.next();
			 if(!(childNodes.contains(key)))
			 {
				sinkNodes.add(key);
			 }
		 }
		return sinkNodes;
		
	}
	//Use to get inlink count for each page
	public HashMap<String,Integer>  getInLinkCount(HashMap<String,Integer> inLinks ,String file) throws IOException
	{
		
		BufferedReader reader=new BufferedReader(new FileReader(file));
		String line;
		
		while ((line=reader.readLine())!= null)
		{
			String delims=" ";
			String key=null;
		//	System.out.println("Example of line"+line );
			String[] tokens=line.split(delims);
			HashSet<String> list=new HashSet<String>();
			for(int i=0;i<tokens.length;i++)
			{
				if(i==0)
				{
				key=tokens[i];
				}
				else
				{
					list.add(tokens[i]);
				}
			}
			
			inLinks.put(key, list.size());
		}
		return inLinks;
	}
	//Use to sort map according to page rank
	public  HashMap<String,Double> sortMap(HashMap<String,Double> map )
			{
		
		List<Map.Entry<String,Double>> list=new LinkedList<Map.Entry<String,Double>>(map.entrySet());
		list=callComparator(list);
		Map<String,Double> sortedMap=new LinkedHashMap<String,Double>();
		for (Iterator<Map.Entry<String,Double>> it= list.iterator();it.hasNext();)
		{
			Map.Entry<String,Double>entry=it.next();
			sortedMap.put(entry.getKey(),entry.getValue());
		}
		return (HashMap<String,Double>) sortedMap;
		}
	
	//Use to sort map according to in link count 
	public  HashMap<String,Integer> sortMapByInLinkCount(HashMap<String,Integer> map )
	{

		List<Map.Entry<String,Integer>> list=new LinkedList<Map.Entry<String,Integer>>(map.entrySet());
		list=callComparatorforInlinkCount(list);
		Map<String,Integer> sortedMap=new LinkedHashMap<String,Integer>();
		for (Iterator<Map.Entry<String,Integer>> it= list.iterator();it.hasNext();)
		{
			Map.Entry<String,Integer>entry=it.next();
			sortedMap.put(entry.getKey(),entry.getValue());
		}
		return (HashMap<String,Integer>) sortedMap;
	}

	//Use to comapare page rank value for sorting
	private List<Entry<String, Double>> callComparator(List<Entry<String, Double>> list) {
		// TODO Auto-generated method stub
		Collections.sort(list,new Comparator<Map.Entry<String,Double>>(){
		public int  compare (Map.Entry<String,Double> order1,Map.Entry<String,Double> order2){
			return(order2.getValue()).compareTo(order1.getValue());
		}});
		return list;
	}
	//Use to comapare inlink count value for sorting
	private List<Entry<String, Integer>> callComparatorforInlinkCount(List<Entry<String, Integer>> list) {
		// TODO Auto-generated method stub
		Collections.sort(list,new Comparator<Map.Entry<String,Integer>>(){
		public int  compare (Map.Entry<String,Integer> order1,Map.Entry<String,Integer> order2){
			return(order2.getValue()).compareTo(order1.getValue());
		}});
		return list;
	}
	
	//use to calculate source links 
	public int calculateSourceLinks(String file) throws IOException
	{
	BufferedReader reader=new BufferedReader(new FileReader(file));
	String line;
	int count=0;
	while ((line=reader.readLine())!= null)
	{
	String delims=" ";
		String key=null;
	//	System.out.println("Example of line"+line );
		String[] tokens=line.split(delims);
		ArrayList<String> list=new ArrayList<String>();
		for(int i=0;i<tokens.length;i++)
		{
			if(i==0)
			{
			key=tokens[i];
			}
			else
			{
				list.add(tokens[i]);
			}
		}
		
		if(list.size()==0)
		{
			count++;
		}
	}
	return count;
	}
	//Use to getallpages to calculate all page rank
	public ArrayList<String> getallpages(ArrayList<String> getpages,String file) throws IOException
	{
		BufferedReader reader=new BufferedReader(new FileReader(file));
		String line;
		while ((line=reader.readLine())!= null)
		{
		String delims=" ";
			String key=null;
		//	System.out.println("Example of line"+line );
			String[] tokens=line.split(delims);
			for(int i=0;i<tokens.length;i++)
			{
			getpages.add(tokens[i]);
		   }
		
		}
		 ArrayList<String> inlinks = new ArrayList<String>(new LinkedHashSet<String>(getpages)) {};
		return inlinks;
		
	}

//Use to calculate number of pages having pagerank less than initial value
public int proportionOfPages(HashMap<String, Double>pageRank,double initialRank )
{
	
	int count=0;
	Set setOfKeys = pageRank.keySet();
	 Iterator iterator = setOfKeys.iterator();
	 //System.out.println(setOfKeys.size());
	 
	 while (iterator.hasNext()) {
		 String key = (String) iterator.next();
		 double pageR= pageRank.get(key);
		 if(pageR<initialRank)
			 count++;
		 }
	 return count;
}
//Use to get the number of outlinks for each page or page pointing to other pages
public HashMap<String,Integer> getNumberOfOutLinks (HashMap<String,Integer>numberOfoutLinks,String file,HashMap<String,HashSet<String>> inlinks) throws IOException
{
	Set setOfKeys = inlinks.keySet();
	 Iterator iterator = setOfKeys.iterator();
	 while (iterator.hasNext())
	 {
		 String key=(String) iterator.next();
		 numberOfoutLinks.put(key, 0);
	 }
	BufferedReader reader=new BufferedReader(new FileReader(file));
	String line = null;
	String delims=" ";
	String key=null;
	while ((line=reader.readLine())!= null){
		String[] values = line.split(" ");
		String[] yourArray = Arrays.copyOfRange(values, 1, values.length);
		Set<String> hashSet = new HashSet<String>(Arrays.asList(yourArray));
		for (String s: hashSet)
		{
		
			s=s.trim();
			if(numberOfoutLinks.containsKey(s))
			{
			numberOfoutLinks.put(s,numberOfoutLinks.get(s)+1 );
			}
			else
			{
				numberOfoutLinks.put(s,1);
			}
		}
	//	System.out.println(numberOfoutLinks.size());
	}
	return numberOfoutLinks;
}
}